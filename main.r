source("funcs.R")

x_source <- import_data(load_prev_result = T,addCountable = F)
x <- addCountable(x_source)
x_backup <- x
nm <- namesM(x)

# main
nx1 <- normalizeM(x,save_sign = F); nx2 <- normalizeM(x,save_sign = T)
exploreData(x)
exploreNData(cbind(x)[,c(1,44:45)],1,legend_position = 'topleft',main='S: 2 options')
scatterPlots(x)

# regression data
rx <- x[,c(30:ncol(x))]
rx <- rx[,-c(9:11,13)]
rx$disPfut <- c(rx$disP[2:nrow(rx)],NA)
rx <- addDiff(rx,l = c(1:ncol(rx)))
rx <- cbind(d_disPfut=rx[,ncol(rx)],rx[,-c(ncol(rx),ncol(rx)/2)])
nm <- namesM(rx); nm
rx[,1] <- log(1+rx[,1])
rx[,21] <- log(1+rx[,21])
rx[,22] <- log(1+rx[,22])
rx[,23] <- log(1+rx[,23])

corrPlot(rx[,c(2:13)],order=F)

# data root exploration
rad <- adftestM(rx[2:(nrow(rx)-1),]); rad
toChange <- (rad[,3]==F)*(1:nrow(rad)); toChange <- toChange[toChange!=0]

# IF exclude not stationary
rx<-rx[2:(nrow(rx)-1),-toChange]
rad <- adftestM(rx); rad; nm <- names(rx)

# IF change not stationary to diff(log(1+x)), trying make them stationary
    rx <- addDiffLog(rx,toChange,1)[2:nrow(rx),]
    rad <- adftestM(rx[2:(nrow(rx)-1),]); rad
    rx<-rx[2:(nrow(rx)-1),rad[,3]==T]
    rad<-adftestM(rx);rad

# step-by-step-excluding
# trying different models
variablesException(normalizeM(rx))[2:3]
variablesException(normalizeM(rx[,-c(7,8)]))[2:3]
variablesException(normalizeM(rx[,-c(22,23)]))[2:3]
variablesException(normalizeM(rx[,-c(21,23)]))[2:3]
variablesException(normalizeM(rx[,-c(21,22)]))[2:3]
variablesException(normalizeM(rx[,-c(21)]))[2:3]

variablesException(normalizeM(rx[,-c(22)]))[2:3]
variablesException(normalizeM(rx[,-c(23)]))[2:3]
variablesException(normalizeM(rx[,-c(10,23,17,7,8)]))[2:3]
variablesException(normalizeM(rx[,-c(10,22)]))[2:3]
variablesException(normalizeM(rx[,-c(7)]))[2:3]
variablesException(normalizeM(rx[,-c(8)]))[2:3]




# why nonreportable belong to noncommercial

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

