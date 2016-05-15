library(xlsx)
library(tseries)
library(corrplot)
library('car')

# funcs
addCountable <- function(x,plot_saudi=F){
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
  PercentDiff <- PercentS - PercentH
  
  TIndex <- 1+1/volumeC*(x$NoncommercialShort+(x$CommercialLong<x$CommercialShort)*(x$NoncommercialLong-x$NoncommercialShort))
  PNLComm <- OI.Comm / volumeC
  PNLSpec <- OI.Spec / volumeS
  PNLDiff <- PNLSpec - PNLComm
  
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
  
  disP1 <- (x$price1 - priceBreakEven1) / priceBreakEven1
  disP2 <- (x$price1 - priceBreakEven2) / priceBreakEven2
  
  if (plot_saudi) {
    exploreNData(excess,main='Excess of breakeven price, %')
    exploreNData(saudi[,c(1,5,7)],main='price yearly')
    exploreNData(saudi[,c(1,2,3)],main='Saudi Arabia budget',legend_position = "topleft")
    exploreNData(saudi[,c(1,2,5)],main='Saudi Arabia revenue & oil prices dynamics',legend_position = "topleft",normalize = T)
    exploreNData(saudi[,c(1,2,6)],main='Saudi Arabia revenue & oil revenue dynamics',legend_position = "topleft",normalize = T)
    exploreNData(saudi[,c(1,4)],main='Saudi Arabia oil production',legend_position = "topleft")
    plot(saudi[,5],saudi[,2],xlab='Oil price',ylab='Revenue',col='blue',main='Saudi Arabia revenue & oil prices dynamics')
    plot(saudi[,6],saudi[,2],xlab='Oil revenue',ylab='Revenue',col='blue',main='Saudi Arabia revenue & oil revenue dynamics')
    exploreNData(data.frame(x[,1],disP1,disP2),main='disP1 vs disP2')
  }
  
  priceBreakEven <- priceBreakEven2
  disP <- disP2
  
  SSpecDegree <- disP * SpecDegree
  STIndex <- disP * TIndex
  
  cbind(x,TS14,BalUS,BalWorld,TIndex,PNLComm,PNLSpec,PNLDiff,PercentC,PercentNC,PercentNR,PercentH,PercentS,PercentDiff,SpecDegree,disP,SSpecDegree,STIndex) 
}

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

exploreData <- function(x,lPos=(14:21),lCountablesFirst=30,corPlotOrder=T){
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
  corrPlot(x[,c(lCountablesFirst:ncol(x))],order = corPlotOrder)
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
  if(min(x[,2:ncol(x)],ymin,na.rm=T)<0) lines(x[,1],rep(0,nrow(x)),col='grey',lty=2)
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
  
  # open interests and prices
  sPlot(diff(log(x$price1)),diff(x$NoncommercialLong-x$NoncommercialShort),main='NonCommercial Open Interest changes and log price changes, weekly. ',smooth = T,xlab = 'log(price) change (t)',ylab='Open Interest change')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NoncommercialLong-x$NoncommercialShort)[2:(nrow(x)-1)],main='NonCommerical Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NoncommercialLong-x$NoncommercialShort)[1:(nrow(x)-2)],main='NonCommerical Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$CommercialLong-x$CommercialShort),main='Commercial Open Interest changes and log price changes, weekly. ',smooth = T,xlab = 'log(price) change (t)',ylab='Open Interest change')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$CommercialLong-x$CommercialShort)[2:(nrow(x)-1)],main='Commerical Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$CommercialLong-x$CommercialShort)[1:(nrow(x)-2)],main='Commerical Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  sPlot(diff(log(x$price1)),diff(x$NonreportablePositionsLong-x$NonreportablePositionsShort),main='NonReportable Open Interest changes and log price changes, weekly. ',smooth = T,xlab = 'log(price) change (t)',ylab='Open Interest change')
  sPlot(diff(log(x$price1))[1:(nrow(x)-2)],diff(x$NonreportablePositionsLong-x$NonreportablePositionsShort)[2:(nrow(x)-1)],main='NonReportable Long changes (t) and log price changes (t-1)',T,xlab='log(price) change (t-1)',ylab = 'Long Position change (t)')
  sPlot(diff(log(x$price1))[2:(nrow(x)-1)],diff(x$NonreportablePositionsLong-x$NonreportablePositionsShort)[1:(nrow(x)-2)],main='NonReportable Long changes (t-1) and log price changes (t)',T,xlab='log(price) change (t)',ylab = 'Long Position change (t-1)')
  
  # long/short open interests and prices
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
    sourceData <- read.xlsx("OIL data export.xlsx",sheetIndex=1)[1:3922,] #3986 - num of full strings in the file, without header # 2.5m
    
    # gathering 1 observation from each week, columns renaming
    
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
    
    data <- data[1:821,c(1:21,27:31,34:36)]
    colnames(data)[1:13] <- c("date","OI1","OI3","OI4","price1","price3","price4","supplyNonOPEC","supplyOPEC","supplyWorld","demandWorld","supplyUS","demandUS")
    
    #0.6m
    write.csv(data,"weeklyDataFromR.csv")
  }
  else {
    # unlist doesn't help, horrible plots( but the best thing that partly helped: data.frame(sapply(x_source,c))
    #data <- read.csv('weeklyDataFromR.csv')[,-1]
    #data$weekDay<-weekdays(as.Date(data$date),abbreviate = T)
  
    data <- read.xlsx("weeklyDataFromR.xlsx",sheetIndex = 1)
    data <- data[,2:ncol(data)]
    data$weekDay<-weekdays(data$date,abbreviate = T)
  }
  if (addCountables == T) data<-addCountable(data)
  data
}


variablesException <- function(rx1,normalize=T){
  if (normalize) rx1 <- normalizeM(rx1)
  ols <- lm(rx1$d_disPfut ~ ., data = rx1)
  step.back <- step(ols, direction = "backward", trace = 0) 
  list(summary1=summary(ols),summary2=summary(step.back),AIC=extractAIC(step.back),ols=ols,step.back=step.back)
}

m1 <- function(x) sum(abs(x))/length(x)
m2 <- function(x) sqrt(sum(x^2))/length(x)
m <- function(x) list(m1=m1(x),m2=m2(x))

x_coef <- function(x,coef){
  x2 <- cbind(1,x)
  for (i in 1:ncol(x2)) x2[,i]<-x2[,i]*coef[i]
  y <- x2[,1]
  for (i in 2:ncol(x2)) y<-y+x2[,i]
  list(x=x2,y=y)
}

nm <- function(x) cbind(1:ncol(x),colnames(x))
w <- function(x,var) {
  res <- NULL
  for (i in 1:length(var)) res <- c(res,sum((1:length(x))*(names(x)==var[i])))
  res
}

vif_test<-function(rx){
  res <- matrix(,0,ncol(rx))
  colnames(res) <- colnames(rx)
  for (i in 1:ncol(rx)) { 
    t <- vif(lm(rx[,i]~.,data=rx[,-i]))
    if ((i>1) && (i<ncol(rx))) t <- c(t[1:(i-1)],0,t[i:length(t)])
    if (i==1) t<-c(0,t)
    if (i==ncol(rx)) t<-c(t,0)
    res <- rbind(res,t)
  }
  rownames(res) <- colnames(rx)
  image(1:ncol(rx),1:ncol(rx),res,col = terrain.colors(10000))
  r <- (which.max(res)-1) %% ncol(rx) + 1; c <- (which.max(res)-1) %/% ncol(rx) + 1
  cat("max V = ",max(res),' in (',r,';',c,')\n')
  cat("max V = ",max(res),' in (',colnames(rx)[r],'; ',colnames(rx)[c],')\n')
  res
}


# moving window - fixed model
movingFixed <- function(rx,dates,years=5,plotEst=T,plotPval=T,par=c(2,2),progressBar=F){
  
  # prework
  size_total <- nrow(rx)
  size_window <- years   *   (365 %/% 7) # N years
  cn <- colnames(rx)
  
  dates <- dates[(length(dates)-size_total+size_window):length(dates)]
  len <- NULL
  coef.estimates <- matrix(,nrow=0,ncol=ncol(rx)); colnames(coef.estimates) <- cn
  coef.pvalues <- matrix(,nrow=0,ncol=ncol(rx)); colnames(coef.pvalues) <- cn
  
  # coefficients calculating
  for (i in 1:(size_total - size_window + 1)){
    nrx <- rx[i:(i + size_window - 1),]
    model <- lm(nrx$d_disPfut ~ ., data = nrx)
    coeff <- coef(summary(model))[,c(1,4)]
    if (length(coeff)==2){
      temp.coef.estimates <- rep(NA,ncol(rx))
      temp.coef.pvalues <- rep(NA,ncol(rx))
      temp.len <- 1
    }
    else
    {
      temp.coef.estimates <- NULL
      temp.coef.pvalues <- NULL
      for (j in 1:ncol(rx)){
        temp.coef.estimates[j] <- coeff[,1][cn[j]]
        temp.coef.pvalues[j] <- coeff[,2][cn[j]]
      }
      temp.len <- nrow(coeff)
    }
    coef.estimates <- rbind(coef.estimates,temp.coef.estimates)
    coef.pvalues <- rbind(coef.pvalues,temp.coef.pvalues)
    len <- c(len,temp.len)
    if (progressBar) cat(i,' of ',size_total - size_window + 1,'completed\n')
    
  }
  
  # plots of estimation and p-values, color means meaningful
  par(mfrow = par)
  for (j in 1:ncol(rx)) 
    if (sum(1-is.na(coef.estimates[,cn[j]])) > 0){
      tmp <- (coef.pvalues[,cn[j]]<=0.05)
      
      if (plotEst){
        mcm <- coef.estimates[,cn[j]]; mcm[tmp] <- NA
        mcp <- coef.estimates[,cn[j]]; mcp[!tmp] <- NA
        plot(dates,mcp,col='green',main=paste0(cn[j],' est.'), ylim=c(min(coef.estimates[,cn[j]],na.rm = T),max(coef.estimates[,cn[j]],na.rm = T)), ylab = 'value' )    
        points(dates,mcm,col='red', pch=24)
        lines(dates,rep(0,nrow(coef.estimates)),col='grey',lty=2)        
      }
      
      if (plotPval){
      mcm <- coef.pvalues[,cn[j]]; mcm[tmp] <- NA
      mcp <- coef.pvalues[,cn[j]]; mcp[!tmp] <- NA
      plot(dates,mcp,col='green',main=paste0(cn[j],' p-value'), ylim=c(0,max(coef.pvalues,na.rm = T)), ylab = 'p-value')    
      points(dates,mcm,col='red', pch=24 )    
      lines(dates,rep(0.05,nrow(coef.estimates)),col='grey',lty=2)
      }
    }
  par(mfrow = c(1,1))
}



### moving window  -  fitting model every iteration

movingStepByStepExcluding <- function(rx,dates,years=5,columns=(2:ncol(rx)),plotEst=T,plotPval=T,par=c(2,2),progressBar=F){
  # prework
  size_total <- nrow(rx)
  size_window <- years   *   (365 %/% 7) # N years
  cn <- colnames(rx)
  
  dates <- dates[(length(dates)-size_total+size_window):length(dates)]
  len <- NULL
  coef.estimates <- matrix(,nrow=0,ncol=ncol(rx)); colnames(coef.estimates) <- cn
  coef.pvalues <- matrix(,nrow=0,ncol=ncol(rx)); colnames(coef.pvalues) <- cn
  
  # interesting for plotting regressors
  inm <- columns
  
  # coefficients calculating
  for (i in 1:(size_total - size_window + 1)){
    nrx <- rx[i:(i + size_window - 1),]
    model <- variablesException(normalizeM(nrx))$step.back
    coeff <- coef(summary(model))[,c(1,4)]
    if (length(coeff)==2){
      temp.coef.estimates <- rep(NA,ncol(rx))
      temp.coef.pvalues <- rep(NA,ncol(rx))
      temp.len <- 1
    }
    else
    {
      temp.coef.estimates <- NULL
      temp.coef.pvalues <- NULL
      for (j in 1:ncol(rx)){
        temp.coef.estimates[j] <- coeff[,1][cn[j]]
        temp.coef.pvalues[j] <- coeff[,2][cn[j]]
      }
      temp.len <- nrow(coeff)
    }
    coef.estimates <- rbind(coef.estimates,temp.coef.estimates)
    coef.pvalues <- rbind(coef.pvalues,temp.coef.pvalues)
    len <- c(len,temp.len)
    if (progressBar) cat(i,' of ',size_total - size_window + 1,'completed\n')
  }
    
  # plot of variables number (with constant and not meaningful variables)
  par(mfrow = c(1,2))
  plot(dates,len,ylim=c(0,max(len)),main=paste0('Number of variables in best regression on last ',size_window,' weeks'),ylab='# variables')
  lines(dates,rep(mean(len),length(len)),col='red',lty=2)
  hist(len,main='Number of variables',xlab='')
  par(mfrow = c(1,1))
  
  # plot of estimations and p-values, color means meaningul
  par(mfrow = par)
  for (j in inm) 
    if (sum(1-is.na(coef.estimates[,cn[j]])) > 0){
      tmp <- (coef.pvalues[,cn[j]]<=0.05)
      
      if (plotEst){
        mcm <- coef.estimates[,cn[j]]; mcm[tmp] <- NA
        mcp <- coef.estimates[,cn[j]]; mcp[!tmp] <- NA
        plot(dates,mcp,col='green',main=paste0(cn[j],' est.'), ylim=c(min(coef.estimates[,cn[j]],na.rm = T),max(coef.estimates[,cn[j]],na.rm = T)), ylab = 'value' )    
        points(dates,mcm,col='red', pch=24)
        lines(dates,rep(0,nrow(coef.estimates)),col='grey',lty=2)
      }
      
      if (plotPval){
        mcm <- coef.pvalues[,cn[j]]; mcm[tmp] <- NA
        mcp <- coef.pvalues[,cn[j]]; mcp[!tmp] <- NA
        plot(dates,mcp,col='green',main=paste0(cn[j],' p-value'), ylim=c(0,max(coef.pvalues,na.rm = T)), ylab = 'p-value')    
        points(dates,mcm,col='red', pch=24 )    
        lines(dates,rep(0.05,nrow(coef.estimates)),col='grey',lty=2)
      }
    }
  par(mfrow = c(1,1))
}


# check every combination of regressions for amount of significant regressors (without constant)
# must be at least 1 fixed included variable, not 1st; 1st column - y
lm_check <- function(rx,vfixed,progressBar=T,includeConst=T){
  nvar <- ncol(rx)-1-length(vfixed)
  vnew <- (1:ncol(rx))[-vfixed][-1]
  ordr <- order(c(vfixed,vnew))
  if (includeConst==T) q<-1 else q<-0
  
  rec <- NULL
  for (i in 0:(2^nvar-1)) rec <- rbind(rec,as.integer(intToBits(i))[1:nvar])
  rec <- cbind(0,cbind(matrix(1,2^nvar,length(vfixed)),rec)[,ordr])
  head(rec)
  
  nsign <- NULL
  for (i in 1:nrow(rec)){
    fmla <- paste0("rx[,1]~",q)
    for (j in 1:ncol(rec)) if (rec[i,j]==1) fmla <- paste0(fmla,'+rx[,',j,']')
    model <- summary(lm(as.formula(fmla)))
    nsign <- c(nsign,sum(coef(model)[-1,4]<=0.05)) # you can count smth else based on this code here
    if (progressBar) cat(i,' out of ',nrow(rec),' completed\n')
  }  
  plot(nsign,ylab='# significant regressors (without const)',xlab='Number of experiment',main='# significant regressors in different models')
  list(rec=rec,nsign=nsign)
}
