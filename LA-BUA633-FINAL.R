# Leslie Alhakim
# BUA 633 Term Project 2

# Import packages
library(YRmisc)
library(robust)

# Import datasets 
library(readxl)
spInfo <- read_excel("Downloads/BUA 633/spInfo-1 (1).xlsx")
View(spInfo)

spData <- read_excel("Downloads/BUA 633/spData-1 (1).xlsx")
View(spData)

# Preliminaries
names(spData)
names(spInfo)

data.class(spData)
spData<-as.data.frame(spData)
data.class(spData)

data.class(spInfo)
spInfo<-as.data.frame(spInfo)
data.class(spInfo)

# Merge dataframes
spdf<-merge(spData,spInfo,by="tkr")
dim(spdf)

names(spdf)

# Extract "year" variable
spdf$date<-as.numeric(substring(spdf$date,7,10))
names(spdf)[2]<-"year"
names(spdf)

# Choose a company tkr: AMAZON
unique(spdf$tkr)
names(spdf)
dim(spdf)
tsdf<-spdf[spdf$tkr=="AMZN",c("tkr", "name","price","eps","bvps",
                              "sales","year")]
tsdf
tsdf<-df.sortcol(tsdf,"year",FALSE)
dim(tsdf)
names(tsdf)
tsdf$obs<-1:23
names(tsdf)
tsdf[,3:6]<-round(tsdf[,3:6],3)

# Graphical techniques: HISTOGRAMS
histogram <-function(x,myTitle,xxlab,yylab){
  hist(x,main=myTitle,xlab=xxlab,ylab=yylab)}

par(mfrow=c(2,2))
histogram(tsdf$price,"Fig. 1 Hist of Price", "Price","Frequency")
histogram(tsdf$eps,"Fig. 2 Hist of EPS", "EPS","Frequency")
histogram(tsdf$bvps,"Fig. 3 Hist of BVPS", "BVPS","Frequency")
histogram(tsdf$obs,"Fig. 4 Hist of OBS", "OBS","Frequency")

# Graphical techniques: TIMESERIES PLOTS
plotts<- function(x,myTitle,yylab){
  ts.plot(x,main=myTitle,ylab=yylab)}

par(mfrow=c(2,2))
plotts(tsdf$price, "Fig. 5 Timeseries of Price", "Price")
plotts(tsdf$eps, "Fig. 6 Timeseries of EPS", "EPS")
plotts(tsdf$bvps, "Fig. 7 Timeseries of BVPS", "BVPS")
plotts(tsdf$obs, "Fig. 8 Timeseries of OBS", "OBS")

# Graphical techniques: SCATTERPLOTS
plotTkr<-function(x,y,z,xxlab,yylab,myTitle){
  scatter.smooth(x,y,type="n",xlab=xxlab,ylab=yylab,main=myTitle)
  text(x,y,z,cex=.6)  }

par(mfrow=c(2,2))
plotTkr(tsdf$eps,tsdf$price,tsdf$tkr,"EPS","Price","Fig. 9 Scatterplot of Price v EPS")
plotTkr(tsdf$bvps,tsdf$price,tsdf$tkr,"BVPS","Price","Fig. 10 Scatterplot of Price v BVPS")
plotTkr(tsdf$obs,tsdf$price,tsdf$tkr,"OBS","Price","Fig. 11 Scatterplot of Price v OBS")

# Analytical methods: DESCRIPTIVE STATISTICS
ds.summ(tsdf[,c("price","eps","bvps","obs")],2)

# Analytical methods: CORRELAION
round(cor(na.omit(tsdf[,c("price","eps","bvps")])),3)
      
# Analytical methods: REGRESSION RESULTS
fit<-lm(price ~ eps+bvps+obs,na.action=na.omit,data=tsdf)
summary(fit)

# Post regression validation
par(mfrow=c(2,2))
histogram(fit$residuals,"Fig. 12 Hist of Residuals", "Residuals","Frequency")
plotTkr(fit$model$price,fit$fitted.values,tsdf$tkr, "Predicted Price","Actual Price","Fig. 13 Act v Pred Price")
pl.2ts(tsdf$price,fit$fitted.values,"Fig. 14 Timeseries of Price")

# Graphical methods: ROBUST
fit<-lmRob(price ~ eps+bvps+obs,na.action=na.omit,data=tsdf)
cor(tsdf$price,predict(fit,tsdf))^2
summary(fit) 

par(mfrow=c(2,2))
histogram(fit$residuals,"Fig. 15 Hist of Residuals (With Robust)", "Residuals","Frequency")
plotTkr(fit$model$price,fit$fitted.values,tsdf$tkr, "Predicted Price","Actual Price","Fig. 16 Act v Pred Price (With Robust)")
abline(a = 0, b = 1, col = "red", lwd = 2)
pl.2ts(tsdf$price,fit$fitted.values,"Fig. 17 Timeseries of Price (With Robust)")

