source("funcs.R")

### gathering data
x_source <- import_data(load_prev_result = T,addCountable = F)
x <- addCountable(x_source,plot_saudi = F)
exploreData(x,corPlotOrder = F)

### gathering data for regression
rx <- x[,c(30:ncol(x))] #30 = 1st counted column
corrPlot(rx[,c(8:13)],order=F)
rx <- rx[,-c(8:12)]
rx$disPfut <- c(rx$disP[2:nrow(rx)],NA)
rx <- addDiff(rx,l = c(1:ncol(rx)))
rx <- cbind(d_disPfut=rx[,ncol(rx)],rx[,-c(ncol(rx),ncol(rx)/2)])

### are stationary?
rad <- adftestM(rx[2:(nrow(rx)-1),]); rad
toChange <- (rad[,3]==F)*(1:nrow(rad)); toChange <- toChange[toChange!=0]
  
  # option 1: IF exclude not stationary
  rx<-rx[2:(nrow(rx)-1),-toChange]
  rad <- adftestM(rx); rad; nm <- names(rx)
  
  # option 2: IF change not stationary to diff(log(1+x)), trying make them stationary
  #    rx <- addDiffLog(rx,toChange,1)[2:nrow(rx),]
  #    rad <- adftestM(rx[2:(nrow(rx)-1),); rad
  #    rx<-rx[2:(nrow(rx)-1),rad[,3]==T]
  #    rad<-adftestM(rx);rad

### correlation problem
rxt<-rx[,-w(rx,c("PNLComm","PNLSpec"))]
rxt<-rxt[,-w(rx,c("d_STIndex","d_SSpecDegree","d_PercentDiff","d_PNLComm","d_PNLSpec"))]
rxt <- rxt[-w(rxt,c("STIndex","SpecDegree","TIndex"))]
rxt <- rxt[,-w(rxt,c("d_STIndex","d_PNLComm"))]
v <- vif_test(rxt)
rxb<-rx; rx<-rxt; # moving rx to backup; temp rx to rx
corrPlot(rx,order=F)


### building regression by step-by-step-excluding 
# place for trying different models
variablesException(rx[,],normalize=F)[1:3]
summary(lm(rx$d_disPfut~rx$BalWorld+rx$disP+rx[,2])) #...
summary(lm(rx$d_disPfut~rx$BalWorld+rx$d_PNLSpec+rx$TS14+rx$d_TS14+rx$disP))


# check every possible combination with at least 1 included variable
res <- lm_check(rx,c(3,6))

### moving window
movingFixed(rx[,c(1,3,6)],x$date,years = 5)
movingStepByStepExcluding(rx,x$date,5,par=c(3,2),plotPval = F,progressBar=T)



### NonReportable behavior

scatterPlots(x)

oi <- data.frame(date=x$date,Commercial = x$CommercialLong-x$CommercialShort,NonCommercial=x$NoncommercialLong-x$NoncommercialShort,NonReportable=x$NonreportablePositionsLong-x$NonreportablePositionsShort)
cor(oi[,2:4])
exploreNData(normalizeM(oi,save_sign = T),lwd = 1,main = 'Open Interest, relative')
corrplot.mixed(cor(oi[,2:4]),lower = "ellipse",upper = "number")#,tl.pos = "lt")

r <- NULL
w <- 52
for (i in w:nrow(x)) r<-c(r,cor(oi$NonCommercial[(i-w+1):i],oi$NonReportable[(i-w+1):i]))
oi_corr_dynamics <- data.frame(date=x$date[w:nrow(x)],NonCommercial=r)
r <- NULL
for (i in w:nrow(x)) r<-c(r,cor(oi$Commercial[(i-w+1):i],oi$NonReportable[(i-w+1):i]))
oi_corr_dynamics$Commercial <- r
exploreNData(data.frame(oi_corr_dynamics,price=nx2[w:nrow(x),5]),1,ymin=-1.5,main='Moving correlation between NonReportable Open Interest and others OI')
