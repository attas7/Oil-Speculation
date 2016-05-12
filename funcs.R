library(xlsx)
library(tseries)
library(corrplot)

# funcs
addCountable <- function(x){
  l <- nrow(x)
  
  TS14 <- (x$price1-x$price4)/x$price1
  BalUS <- (x$demandUS - x$supplyUS)/(x$demandUS + x$supplyUS)
  BalWorld <- (x$demandWorld - x$supplyWorld)/(x$demandWorld + x$supplyWorld)
  
  volumeNC  <- x$NoncommercialLong + x$NoncommercialShort + 2*x$NoncommercialSpreads
  volumeC <- x$CommercialLong + x$CommercialShort
  volumeNR <- x$NonreportablePositionsLong + x$NonreportablePositionsShort
  volumeS <- volumeNC + volumeNR
  volumeH <- volumeC
  volume <- volumeNC + volumeC + volumeNR
  
  OI.Comm <- x$CommercialLong - x$CommercialShort
  OI.NonComm <- x$NoncommercialLong - x$NoncommercialShort
  OI.NonRept <- x$NonreportablePositionsLong - x$NonreportablePositionsShort
  OI.Spec <- OI.NonRept + OI.NonComm
  OI <- OI.Comm + OI.NonComm + OI.NonRept
  
  PercentC <- volumeC / volume
  PercentNC <- volumeNC / volume
  PercentNR <- volumeNR / volume
  PercentS <- volumeS / volume
  PercentH <- volumeH / volume
  
  
  TIndex <- 1+1/volumeC*(x$NoncommercialShort+(x$CommercialLong<x$CommercialShort)*(x$NoncommercialLong-x$NoncommercialShort))
  PNLComm <- OI.Comm / volumeC
  PNLSpec <- OI.Spec / volumeS
  
  OpenInterestDiff <- c(NA,diff(x$OpenInterest))
  
  SpecDegree <- volumeS / volumeC
  
  saudi <- unique(data.frame(year = x_source$year,revenue = x_source$SaudiRevenue,expenditure = x_source$SaudiExpenditure,production = x_source$SaudiOilProductionYearly))
  for (i in 1:nrow(saudi)) saudi$priceAvg[i] <- mean(x$price1[x$year==saudi$year[i]])
  saudi$revenueFromOil <- saudi$production*saudi$priceAvg/10^6
  sr <- lm(saudi$revenue ~ saudi$revenueFromOil)
  saudi$priceBreakEven <- (saudi$expenditure - sr$coefficients[1]) / (sr$coefficients[2]*saudi$production) * 10^6
  
  mar2 <- numeric()
  for (i in 1:nrow(saudi)) mar2[i]<-100*mean((saudi$priceAvg[max(1,(i-1)):i]-saudi$priceBreakEven[max(1,(i-2)):i])/saudi$priceBreakEven[max(1,(i-1)):i])
  saudi$priceFundamental <- saudi$priceBreakEven * (1+mar2/100)
  
  priceBreakEven1 <- numeric(); for (i in 1:nrow(x)) priceBreakEven1[i] <- saudi$priceBreakEven[saudi$year==x$year[i]]
  priceBreakEven2 <- numeric(); for (i in 1:nrow(x)) priceBreakEven2[i] <- saudi$priceFundamental[saudi$year==x$year[i]]
  
  
  #marS <- numeric()
  #mar3 <- numeric()
  #for (i in 1:nrow(saudi)) marS[i]<-100*mean((saudi$priceAvg[1:i]-saudi$priceBreakEven[1:i])/saudi$priceBreakEven[1:i])
  #for (i in 1:nrow(saudi)) mar3[i]<-100*mean((saudi$priceAvg[max(1,(i-2)):i]-saudi$priceBreakEven[max(1,(i-2)):i])/saudi$priceBreakEven[max(1,(i-2)):i])
  marR <- 100*(saudi$priceAvg-saudi$priceBreakEven)/saudi$priceBreakEven
  #res <- mar2*(sign(mar2)+1)/2
  
  excess <- data.frame(year=saudi$year,realPrice=marR,fundamentalPrice=mar2)
  exploreNData(excess,main='Excess of breakeven price, %')
  
  exploreNData(saudi[,c(1,5,7)],main='price yearly')
  exploreNData(saudi[,c(1,2,3)],main='Saudi Arabia budget',legend_position = "topleft")
  exploreNData(saudi[,c(1,2,5)],main='Saudi Arabia revenue & oil prices dynamics',legend_position = "topleft",normalize = T)
  exploreNData(saudi[,c(1,2,6)],main='Saudi Arabia revenue & oil revenue dynamics',legend_position = "topleft",normalize = T)
  exploreNData(saudi[,c(1,4)],main='Saudi Arabia oil production',legend_position = "topleft")
  plot(saudi[,5],saudi[,2],xlab='Oil price',ylab='Revenue',col='blue',main='Saudi Arabia revenue & oil prices dynamics')
  plot(saudi[,6],saudi[,2],xlab='Oil revenue',ylab='Revenue',col='blue',main='Saudi Arabia revenue & oil revenue dynamics')
  
  
  disP1 <- (x$price1 - priceBreakEven1) / priceBreakEven1
  disP2 <- (x$price1 - priceBreakEven2) / priceBreakEven2
  plot(disP1,type='l',col='blue')
  lines(disP2,col='red')
  
  priceBreakEven <- priceBreakEven2
  disP <- disP2
  
  SSpecDegree <- disP * SpecDegree
  STIndex <- disP * TIndex
  
  cbind(x,TS14,BalUS,BalWorld,TIndex,PNLComm,PNLSpec,PercentC,PercentNC,PercentNR,PercentH,PercentS,SpecDegree,priceBreakEven,disP,SSpecDegree,STIndex) 
}

namesM <- function(x) cbind(1:ncol(x),colnames(x))
normalize <- function(x,save_sign=F) {
  if (is.numeric(x))
    if (save_sign==F) (x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))
    else x/max(abs(x),na.rm = T)
  else x
}
normalizeM <- function(x,save_sign=F) { 
  for (i in 2:ncol(x)) 
    x[,i] <- normalize(x[,i],save_sign)
  x
}

exploreData <- function(x,lPos=(14:21),lCountablesFirst=30){
  exploreNData(main='Positions',ymin=0,x=x[,c(1,lPos)],legend_position = "topleft")
  exploreNData(main='Oil supply & demand, World',ymin=70,x=data.frame(date=x$date,demandWorld=x$demandWorld,supplyWorld=x$supplyWorld))
  exploreNData(main='Oil supply & demand, US',ymin=70,x=data.frame(date=x$date,demandUS=x$demandUS,supplyUS=x$supplyUS))
  exploreNData(main='Balance, World',ymin=70,x=data.frame(date=x$date,BalanceWorld=x$BalWorld))
  exploreNData(main='Balance, US',ymin=70,x=data.frame(date=x$date,BalanceUS=x$BalUS))
  exploreNData(main='Balance, World & US',ymin=-0.1,x=data.frame(date=x$date,BalanceWorld=x$BalWorld,BalanceUS=x$BalUS))
  exploreNData(main='WTI price dynamics\n 1, 3, 4 month maturity',ymin=0,x=data.frame(date=x$date,price1=x$price1,price3=x$price3,price4=x$price4),legend_position = "topleft")
  exploreNData(main='TS_1_4 dynamics',ymin=0,x=data.frame(date=x$date,TS_1_4=x$TS14))
  exploreNData(main='T-Index',ymin=1,x=data.frame(date=x$date,T_Index=x$TIndex))
  exploreNData(main='PNL_Spec & PNL_Comm dynamics',ymin=0,x=data.frame(date=x$date,PNL_Spec=x$PNLSpec,PNL_Comm=x$PNLComm))
  exploreNData(main='Speculation Degree',x=data.frame(date=x$date,SpecDegree=x$SpecDegree),legend_position = "topleft")
  exploreNData(main='Percents of total open interest',ymin=0,x=x[,c(1,36:38)],legend_position = "left")
  exploreNData(main='disPrice',ymin=0,x=data.frame(date=x$date,disP=x$disP))
  exploreNData(main='S, 2 methods',ymin=0,x=data.frame(date=x$date,S_SpecDegree=x$SSpecDegree,S_TIndex=x$STIndex))
  corrPlot(x[,c(lCountablesFirst:ncol(x))])
}
corrPlot <- function(x,normalize=T,save_sign=T,precision=1,order=T){
  if (normalize) x <- normalizeM(x,save_sign)
  if (order) corrplot.mixed(cor(x),lower = "ellipse",upper = "number",tl.pos = "lt", order="hclust",number.digits=precision)
  else corrplot.mixed(cor(x),lower = "ellipse",upper = "number",tl.pos = "lt",number.digits=precision)
}
adftestM <- function(x){
  r<-NULL
  for (i in c(1:ncol(x))) r<-c(r,as.numeric(adf.test(x[,i])$p.value))
  p.value <- as.numeric(round(r,2))
  stationary = (r<0.05)
  var <- colnames(x)[c(1:ncol(x))]
  data.frame(var,p.value,stationary)
}
exploreNData <- function(x,lwd=1,main='',ymin=10000000000,legend_position="bottomleft",normalize = F) {
  if (normalize) x<-normalizeM(x)
  #plots by colnames
  col <- c("black","dark red","red","orange","green","light green","blue","light blue","yellow")
  if (ncol(x)==3) col <- c("red","blue")
  if (ncol(x)==4) col <- c("black","red","blue")
  plot(x[,1],x[,2],type='l',xlab=colnames(x)[1],ylab='',col=col[1],ylim=c(min(x[,-1],ymin,na.rm=T),max(x[,-1],na.rm=T)),lwd=lwd,main=main)
  if (ncol(x)>2) for (i in 3:ncol(x)) lines(x[,1],x[,i],col=col[i-1],lwd=lwd)  
  if(min(x[,2:ncol(x)],ymin,na.rm=T)<0) lines(x[,1],rep(0,nrow(x)),col='yellow',lty=2)
  legend = colnames(x)[-1]
  if (length(legend) == 0) legend <- as.character(1:(ncol(x)-1))
  legend(x=legend_position, bty="n",col = col,lty = 1, legend = legend,lwd=lwd)
}

ma <- function(x,len){
  len <- max(len,2)
  if (is.numeric(x)==F) return(x)
  m <- NULL
  m[1:(len-1)] <- NA
  for (i in len:length(x)) m[i] = mean(x[(i-(len-1)):i])
  m
}
addLag <- function(x,l){
  r <- x
  for (i in 1:length(l)) r <- cbind(r,c(NA,r[-nrow(x),l[i]]))
  colnames(r)[(ncol(x)+1):(ncol(x)+length(l))] <- paste0("lag1_",colnames(x)[l])
  r
}
addDiff <- function(x,l){
  r <- x
  for (i in 1:length(l)) r <- cbind(r,c(NA,diff(r[,l[i]])))
  colnames(r)[(ncol(x)+1):(ncol(x)+length(l))] <- paste0("d_",colnames(x)[l])
  r
}
addLog <- function(x,l,a=0){
  r <- x
  for (i in 1:length(l)) r <- cbind(r,c(log(a+r[,l[i]])))
  if (a==0) colnames(r)[(ncol(x)+1):(ncol(x)+length(l))] <- paste0("l_",colnames(x)[l])
  else colnames(r)[(ncol(x)+1):(ncol(x)+length(l))] <- paste0("lp",a,"_",colnames(x)[l])
  r
}
addDiffLog <- function(x,l,a=0){
  nc <- ncol(x)
  ll <- length(l)
  x <- addLog(x,l,a)
  x <- addDiff(x,(nc+1):(nc+ll))
  x <- x[,c(1:nc,(nc+1+ll):(nc+2*ll))]
  x
}
addMA <- function(x,l,len){
  r <- x
  for (i in 1:length(l)) r <- cbind(r,ma(r[,l[i]],len))
  colnames(r)[(ncol(x)+1):(ncol(x)+length(l))] <- paste0("MA",len,"_",colnames(x)[l])
  r  
}

sPlot <- function(x1,x2,main='',smooth=F,line=F,xlab='',ylab='',square=F) {
  m <- max(abs(x1),abs(x2))
  if (!smooth) 
    if (square) plot(x1,x2,main=paste0(main,'\nСorrelation = ',round(cor(x1,x2),2)),xlim=c(-m,m),ylim=c(-m,m),xlab=xlab,ylab=ylab)
    else  plot(x1,x2,main=paste0(main,'\nСorrelation = ',round(cor(x1,x2),2)),xlab=xlab,ylab=ylab)
  else  
    if (square) smoothScatter(x1,x2,main=paste0(main,'\nСorrelation = ',round(cor(x1,x2),2)),xlim=c(-m,m),ylim=c(-m,m),xlab=xlab,ylab=ylab)
    else  smoothScatter(x1,x2,main=paste0(main,'\nСorrelation = ',round(cor(x1,x2),2)),xlab=xlab,ylab=ylab)
  if (line) lines(c(-m,m),c(-m,m),col='red',lty=2)
}
scatterPlots <- function(x){
  # scatter
  par(mfrow = c(3,3))
  
  # открытые позиции и цены
  sPlot(diff(log(x$price1)),diff(x$NoncommercialLong-x$NoncommercialShort),main='NonCommercial Open Interest changes and log price changes, weekly. ',smooth = T,xlab = 'log(price) change (t)',ylab='Open Interest change')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NoncommercialLong-x$NoncommercialShort)[2:(nrow(x)-1)],main='NonCommerical Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NoncommercialLong-x$NoncommercialShort)[1:(nrow(x)-2)],main='NonCommerical Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$CommercialLong-x$CommercialShort),main='Commercial Open Interest changes and log price changes, weekly. ',smooth = T,xlab = 'log(price) change (t)',ylab='Open Interest change')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$CommercialLong-x$CommercialShort)[2:(nrow(x)-1)],main='Commerical Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$CommercialLong-x$CommercialShort)[1:(nrow(x)-2)],main='Commerical Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$NonreportablePositionsLong-x$NonreportablePositionsShort),main='NonReportable Open Interest changes and log price changes, weekly. ',smooth = T,xlab = 'log(price) change (t)',ylab='Open Interest change')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NonreportablePositionsLong-x$NonreportablePositionsShort)[2:(nrow(x)-1)],main='NonReportable Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NonreportablePositionsLong-x$NonreportablePositionsShort)[1:(nrow(x)-2)],main='NonReportable Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  # длинные/короткие позиции и цены
  par(mfrow = c(2,3))
  
  sPlot(diff(log(x$price1)),diff(x$NoncommercialLong),main='NonCommerical Long changes (t) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NoncommercialLong)[2:(nrow(x)-1)],main='NonCommerical Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NoncommercialLong)[1:(nrow(x)-2)],main='NonCommerical Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$NoncommercialShort),main='NonCommerical Short changes (t) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Short Position change (t)')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NoncommercialShort)[2:(nrow(x)-1)],main='NonCommerical Short changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Short Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NoncommercialShort)[1:(nrow(x)-2)],main='NonCommerical Short changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Short Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$CommercialLong),main='Commerical Long changes (t) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$CommercialLong)[2:(nrow(x)-1)],main='Commerical Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$CommercialLong)[1:(nrow(x)-2)],main='Commerical Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$CommercialShort),main='Commerical Short changes (t) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Short Position change (t)')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$CommercialShort)[2:(nrow(x)-1)],main='Commerical Short changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Short Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$CommercialShort)[1:(nrow(x)-2)],main='Commerical Short changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Short Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$NonreportablePositionsLong),main='NonReportable Long changes (t) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NonreportablePositionsLong)[2:(nrow(x)-1)],main='NonReportable Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NonreportablePositionsLong)[1:(nrow(x)-2)],main='NonReportable Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$NonreportablePositionsShort),main='NonReportable Short changes (t) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Short Position change (t)')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NonreportablePositionsShort)[2:(nrow(x)-1)],main='NonReportable Short changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Short Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NonreportablePositionsShort)[1:(nrow(x)-2)],main='NonReportable Short changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Short Position change (t-1)')
  
  # длинные vs короткие позиции
  par(mfrow = c(1,3))
  
  sPlot(diff(x$CommercialLong),diff(x$CommercialShort),main='Commercial Long & Short changes weekly',smooth = T,line = T,xlab='Long',ylab='Short',square = T)
  sPlot(diff(x$NonreportablePositionsLong),diff(x$NonreportablePositionsShort),main='NonReportable Long & Short changes weekly',smooth = T,line = T,xlab='Long',ylab='Short',square = T)
  sPlot(diff(x$NoncommercialLong),diff(x$NoncommercialShort),main='NonCommercial Long & Short changes weekly',smooth = T,line = T,xlab='Long',ylab='Short',square = T)
  
  
  par(mfrow = c(1,1))
  
  hist(x$NoncommercialLong-x$NoncommercialShort,main='Histogram of NonCommercial Open Interest',breaks = 10,xlab='Open Position',ylab='weeks')
  hist(x$CommercialLong-x$CommercialShort,main='Histogram of Commercial Open Interest',breaks = 10,xlab='Open Position',ylab='weeks')
  par(mfrow = c(1,1))
}

import_data <- function(load_prev_result=T,addCountables=F){
  if (load_prev_result == F){
    sourceData <- read.xlsx("OIL data export.xlsx",sheetIndex=1)[1:3922,] #3986 - кол-во полных строк в файле с данными, без учёта заголовка # 2.5m
    
    # далее в sourceData выделяются только представители каждой недели (вторник и) переименовываются столбцы
    
    sourceData$year<-as.numeric(format(sourceData$date,'%Y'))
    sourceData$weekNum<-as.numeric(format(sourceData$date,'%U'))
    sourceData$weekDay<-weekdays(sourceData$date,abbreviate = T)
    
    calendar <- as.data.frame(unique(cbind(sourceData$year,sourceData$weekNum)))
    colnames(calendar) <- c('year','weekNum')
    calendar <- calendar[with(calendar,order(year,weekNum)),]
    data <- data.frame()
    for (i in 1:nrow(calendar)){
      daysInWeek <- (sourceData$year==calendar$year[i]) & (sourceData$weekNum==calendar$weekNum[i])
      calendar$weekDay[i] <- min(sourceData$weekDay[daysInWeek],na.rm = T)
      t <- sum((1:nrow(sourceData))*(daysInWeek & (sourceData$weekDay == calendar$weekDay[i])))
      data <- rbind(data,sourceData[t,])
    }
    
    # шлифовка
    data <- data[1:821,c(1:21,27:31,34:36)]
    colnames(data)[1:13] <- c("date","OI1","OI3","OI4","price1","price3","price4","supplyNonOPEC","supplyOPEC","supplyWorld","demandWorld","supplyUS","demandUS")
    
    #0.6m
    write.xlsx(data,"weeklyDataFromR.xlsx")
  }
  else {
    data <- read.xlsx("weeklyDataFromR.xlsx",sheetIndex = 1)
    data <- data[,2:ncol(data)]
    data$weekDay<-weekdays(data$date,abbreviate = T)
  }
  if (addCountables == T) data<-addCountable(data)
  data
}


## Create a formula for a model with a large number of variables:
#xnam <- paste0("x", 1:25)
#(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))

variablesException <- function(rx1){
  ols <- lm(rx1$d_disPfut ~ ., data = rx1)
  step.back <- step(ols, direction = "backward", trace = 0) 
  list(summary1=summary(ols),summary2=summary(step.back),AIC=extractAIC(step.back),ols=ols,step.back=step.back)
}
