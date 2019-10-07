library("ggplot2")
library("forecast")
library("graphics")

GS<-read.csv("Global Superstore.csv", header = T)
str(GS)
View(GS)
sum(is.na(GS))
View(na.omit(GS))

GS$Order.Date<-as.Date(GS$Order.Date, "%d-%m-%Y")
GS$Ship.Date<-as.Date(GS$Ship.Date, "%d-%m-%Y")
GS$Postal.Code<-as.factor(GS$Postal.Code)
levels(GS$Market)
levels(GS$Segment)

#Arrangement based on Market and Segment
Tprofit<-aggregate(GS$Profit, by=list(GS$Market, GS$Segment),FUN="sum")
names(Tprofit)<-c("Market","Segment","Total.profit")
Tsales<-aggregate(GS$Sales, by=list(GS$Market, GS$Segment),FUN="sum")
names(Tsales)<-c("Market","Segment","Total.sales")
Asales<-aggregate(GS$Sales, by=list(GS$Market, GS$Segment),FUN="mean")
names(Asales)<-c("Market","Segment","Avg.sales")
Nsales<-aggregate(GS$Sales, by=list(GS$Market, GS$Segment),FUN="length")
names(Nsales)<-c("Market","Segment","No.of.sales")

SalesSummaryTotal<-data.frame(Tprofit,Tsales,Asales,Nsales)
View(SalesSummaryTotal)
SalesSummaryTotal<-SalesSummaryTotal[ ,c(1,2,3,6,9,12)]
SalesSummaryTotal$ProfitP<-(SalesSummaryTotal$Total.profit/SalesSummaryTotal$Total.sales)*100

#Arrangement based on Market, Segment and Month
TMQuant<-aggregate(GS$Quantity,
                   by=list(GS$Market,GS$Segment,format(as.Date(GS$Order.Date),"%m%Y")),FUN="sum")
names(TMQuant)<-c("Market","Segment","Month","Total.Monthly.Quantity")
TMProfit<-aggregate(GS$Profit,
                   by=list(GS$Market,GS$Segment,format(as.Date(GS$Order.Date),"%m%Y")),FUN="sum")
names(TMProfit)<-c("Market","Segment","Month","Total.Monthly.Profit")
TMSales<-aggregate(GS$Sales,
                    by=list(GS$Market,GS$Segment,format(as.Date(GS$Order.Date),"%m%Y")),FUN="sum")
names(TMSales)<-c("Market","Segment","Month","Total.Monthly.Sales")  
AMSales<-aggregate(GS$Sales,
                   by=list(GS$Market,GS$Segment,format(as.Date(GS$Order.Date),"%m%Y")),FUN="mean")
names(AMSales)<-c("Market","Segment","Month","Avg.Monthly.Sales")
NMSales<-aggregate(GS$Sales,
                   by=list(GS$Market,GS$Segment,format(as.Date(GS$Order.Date),"%m%Y")),FUN="length")
names(NMSales)<-c("Market","Segment","Month","No.of.Monthly.Sales")  

SalesSummaryMonthly<-data.frame(TMQuant,TMProfit,TMSales,AMSales,NMSales)
SalesSummaryMonthly<-SalesSummaryMonthly[ ,c(1,2,3,4,8,12,16,20)]
View(SalesSummaryMonthly)
SalesSummaryMonthly$ProfitP<-(SalesSummaryMonthly$Total.Monthly.Profit/SalesSummaryMonthly$Total.Monthly.Sales)*100
View(SalesSummaryMonthly)

SalesSummaryMonthly<-SalesSummaryMonthly[order(SalesSummaryMonthly$Total.Monthly.Sales,decreasing=TRUE),]

#Calculating Monthly Profit Perent
AMProfitP<-aggregate(SalesSummaryMonthly$ProfitP,
                     by=list(SalesSummaryMonthly$Market,SalesSummaryMonthly$Segment),FUN="mean")
names(AMProfitP)<-c("Market","Segment","Avg.Monthly.Profit.Percent")
MProfitPSD<-aggregate(SalesSummaryMonthly$ProfitP,
                      by=list(SalesSummaryMonthly$Market,SalesSummaryMonthly$Segment),FUN="sd")
names(MProfitPSD)<-c("Market","Segment","Monthly.Profit.Percent.SD")

SalesSummaryTotal<-data.frame(SalesSummaryTotal,AMProfitP,MProfitPSD)
SalesSummaryTotal<-SalesSummaryTotal[ ,-c(8,9,11,12)]
SalesSummaryTotal$CV.Monthly.Profit.Percent<-SalesSummaryTotal$Monthly.Profit.Percent.SD/SalesSummaryTotal$Avg.Monthly.Profit.Percent
SalesSummaryTotal<-SalesSummaryTotal[order(SalesSummaryTotal$Total.profit,decreasing=TRUE),]
View(SalesSummaryTotal)
#We find that APAC Consumer and EU Consumer are the 2 most profitable segments.

#APAC Consumer
#-------------
APACConsumer_Sales<-subset(SalesSummaryMonthly,
                           (SalesSummaryMonthly$Market=="APAC")&(SalesSummaryMonthly$Segment=="Consumer"))
View(APACConsumer_Sales)
APACConsumer_Sales<-APACConsumer_Sales[order(APACConsumer_Sales$Month),]
APACConsumer_Sales$MonthNo<-c(1:nrow(APACConsumer_Sales))
APACConsumer_SalesTS<-APACConsumer_Sales[ ,c("MonthNo","Total.Monthly.Sales")]
APACConsumer_QtyTS<-APACConsumer_Sales[ ,c("MonthNo","Total.Monthly.Quantity")]

nrow(APACConsumer_Sales)
#Separating data for training and test
APACConsumer_SalesTS.test<-APACConsumer_SalesTS[(43:48),]
APACConsumer_SalesTS<-APACConsumer_SalesTS[(1:42),]
APACConsumer_QtyTS.test<-APACConsumer_QtyTS[(43:48),]
APACConsumer_QtyTS<-APACConsumer_QtyTS[(1:42),]

#APAC Consumer: Modelling Sales
#-------------------------------
#Creatig a tme series and decomposing it
APACConsumer_Sales.ts<-ts(APACConsumer_SalesTS[,2])
APACConsumer_Sales.ts.d<-ts(APACConsumer_SalesTS[,2],frequency=12)
APACConsumer_Sales.ts.decompose <- decompose(APACConsumer_Sales.ts.d)
#Plotting the decomposed time series
plot(APACConsumer_Sales.ts.decompose)
plot(APACConsumer_Sales.ts)

# Smoothening the curve 
APACConsumer_Sales.ts.smooth <- stats::filter(APACConsumer_Sales.ts,filter=rep(1/3,3),
                                              method='convolution', sides=2)
#First difference of time series
diff <- APACConsumer_Sales.ts.smooth[3] - APACConsumer_Sales.ts.smooth[2]
for (i in seq(1,1,-1)) {
  APACConsumer_Sales.ts.smooth[i] <- APACConsumer_Sales.ts.smooth[i+1] - diff
}
n <- length(APACConsumer_Sales.ts)

Ti <- APACConsumer_SalesTS[[1]]
Ti.test <- APACConsumer_SalesTS.test[[1]]

#Second difference of time series
diff <- APACConsumer_Sales.ts.smooth[n-1] - APACConsumer_Sales.ts.smooth[n-2]
for (i in seq(n-1+1, n)) {
  APACConsumer_Sales.ts.smooth[i] <- APACConsumer_Sales.ts.smooth[i-1] + diff
}
lines(APACConsumer_Sales.ts.smooth, col="yellow", lwd=2)

APACConsumer_Sales.ts.smdf <- as.data.frame(cbind(Ti,as.vector(APACConsumer_Sales.ts.smooth)))
colnames(APACConsumer_Sales.ts.smdf) <- c('MonthNo', 'Sales')
#converting to numeric
APACConsumer_Sales.ts.smdf$MonthNo<-as.numeric(APACConsumer_Sales.ts.smdf$MonthNo)
APACConsumer_Sales.ts.smdf$Sales<-as.numeric(APACConsumer_Sales.ts.smdf$Sales)
str(APACConsumer_Sales.ts.smdf)
#Model fitting for smoothening time series
model_fit <- lm(APACConsumer_Sales.ts.smdf$Sales ~ sin(0.5*APACConsumer_Sales.ts.smdf$MonthNo) *
              poly(APACConsumer_Sales.ts.smdf$MonthNo,2) 
            + cos(0.5*APACConsumer_Sales.ts.smdf$MonthNo) * 
              poly(APACConsumer_Sales.ts.smdf$MonthNo,2)
            + sin(0.05*APACConsumer_Sales.ts.smdf$MonthNo)*
              APACConsumer_Sales.ts.smdf$MonthNo, 
            data=APACConsumer_Sales.ts.smdf)
summary(model_fit)
#Checking accuracy of the model
accuracy(model_fit)

#Trend of the model to the timeseries data
trend <- predict(model_fit, data.frame(x=Ti))
lines(Ti, trend, col="orange", lwd=2)

#Auto Arima
aa <- auto.arima(APACConsumer_Sales.ts)
aa
tsdiag(aa)
plot(aa$x, col="black")
lines(fitted(aa), col="orange")
accuracy(aa)

#FORECASTING SALES
#HoltWinters function
APACConSalesF <- HoltWinters(APACConsumer_Sales.ts, beta=FALSE, gamma=FALSE)
APACConSalesF
plot(APACConSalesF)
APACConSalesF<-forecast:::forecast.HoltWinters(APACConSalesF,h=6)
APACConSalesF
plot(APACConSalesF)
# Comparing with Test data
accuracy(APACConSalesF,APACConsumer_SalesTS.test$Total.Monthly.Sales)[,"MAPE"]

#forecast function
forecast(APACConsumer_Sales.ts,h=6)
forecast(aa,h=6)


#APAC Consumer: Modelling Quantity
#---------------------------------

#Creating a timeseries of the data and decomposing it
APACConsumer_Qty.ts<-ts(APACConsumer_QtyTS[,2])
APACConsumer_Qty.ts.d<-ts(APACConsumer_QtyTS[,2],frequency=12)
APACConsumer_Qty.ts.decompose <- decompose(APACConsumer_Qty.ts.d)

#Plotting decomposed time series
plot(APACConsumer_Qty.ts.decompose)
plot(APACConsumer_Qty.ts)

# Smoothening the curve 
APACConsumer_Qty.ts.smooth <- stats::filter(APACConsumer_Qty.ts,filter=rep(1/3,3),method='convolution', sides=2)

#First difference
diff <- APACConsumer_Qty.ts.smooth[3] - APACConsumer_Qty.ts.smooth[2]
for (i in seq(1,1,-1)) {
  APACConsumer_Qty.ts.smooth[i] <- APACConsumer_Qty.ts.smooth[i+1] - diff
}
n <- length(APACConsumer_Qty.ts)

Ti <- APACConsumer_QtyTS[[1]]
Ti.test <- APACConsumer_QtyTS.test[[1]]

#Second difference
diff <- APACConsumer_Qty.ts.smooth[n-1] - APACConsumer_Qty.ts.smooth[n-2]
for (i in seq(n-1+1, n)) {
  APACConsumer_Qty.ts.smooth[i] <- APACConsumer_Qty.ts.smooth[i-1] + diff
}
lines(APACConsumer_Qty.ts.smooth, col="yellow", lwd=2)

APACConsumer_Qty.ts.smdf <- as.data.frame(cbind(Ti,as.vector(APACConsumer_Qty.ts.smooth)))
colnames(APACConsumer_Qty.ts.smdf) <- c('MonthNo', 'Quantity')

#converting columns to numeric
APACConsumer_Qty.ts.smdf$MonthNo<-as.numeric(APACConsumer_Qty.ts.smdf$MonthNo)
APACConsumer_Qty.ts.smdf$Quantity<-as.numeric(APACConsumer_Qty.ts.smdf$Quantity)
str(APACConsumer_Qty.ts.smdf)

#MOdel fitting of smoothened time series
model_fit <- lm(APACConsumer_Qty.ts.smdf$Quantity ~ sin(0.5*APACConsumer_Qty.ts.smdf$MonthNo) *
              poly(APACConsumer_Qty.ts.smdf$MonthNo,2) 
            + cos(0.5*APACConsumer_Qty.ts.smdf$MonthNo) * 
              poly(APACConsumer_Qty.ts.smdf$MonthNo,2)
            + sin(0.05*APACConsumer_Qty.ts.smdf$MonthNo)*
              APACConsumer_Qty.ts.smdf$MonthNo, 
            data=APACConsumer_Qty.ts.smdf)
summary(model_fit)
accuracy(model_fit)

#Plotting a trend line for fit data
trend <- predict(model_fit, data.frame(x=Ti))
lines(Ti, trend, col="orange", lwd=2)

#Auto Arima
aa <- auto.arima(APACConsumer_Qty.ts)
aa
tsdiag(aa)
plot(aa$x, col="black")
lines(fitted(aa), col="orange")
accuracy(aa)

#FORECASTING QUANTITY
#HoltWinters function
APACConQuanF <- HoltWinters(APACConsumer_Qty.ts, beta=FALSE, gamma=FALSE)
APACConQuanF
plot(APACConQuanF)
APACConQuanF<-forecast:::forecast.HoltWinters(APACConQuanF,h=6)
APACConQuanF
plot(APACConQuanF)

# Comparing with Test data
accuracy(APACConQuanF,APACConsumer_QtyTS.test$Total.Monthly.Quantity)[,"MAPE"]

#forecast function
forecast(APACConsumer_Qty.ts,h=6)
forecast(aa,h=6)

#-----------------------------------------------------------------------------------------------
#Similar process carried out for the second most profitable market-segment chosen

#EU Consumer
#-----------
EUConsumer_Sales<-subset(SalesSummaryMonthly,
                           (SalesSummaryMonthly$Market=="EU")&(SalesSummaryMonthly$Segment=="Consumer"))
View(EUConsumer_Sales)
EUConsumer_Sales<-EUConsumer_Sales[order(EUConsumer_Sales$Month),]
EUConsumer_Sales$MonthNo<-c(1:nrow(EUConsumer_Sales))
EUConsumer_SalesTS<-EUConsumer_Sales[ ,c("MonthNo","Total.Monthly.Sales")]
EUConsumer_QtyTS<-EUConsumer_Sales[ ,c("MonthNo","Total.Monthly.Quantity")]

nrow(EUConsumer_Sales)

#Creating training and test data for sales and quantity
EUConsumer_SalesTS.test<-EUConsumer_SalesTS[(43:48),]
EUConsumer_SalesTS<-EUConsumer_SalesTS[(1:42),]
EUConsumer_QtyTS.test<-EUConsumer_QtyTS[(43:48),]
EUConsumer_QtyTS<-EUConsumer_QtyTS[(1:42),]

#EU Consumer: Modelling Sales
#-----------------------------

#Creating time series and decomposing it
EUConsumer_Sales.ts<-ts(EUConsumer_SalesTS[,2])
EUConsumer_Sales.ts.d<-ts(EUConsumer_SalesTS[,2],frequency=12)
EUConsumer_Sales.ts.decompose <- decompose(EUConsumer_Sales.ts.d)

#Plotting decomposed time series into 4 parts
plot(EUConsumer_Sales.ts.decompose)
plot(EUConsumer_Sales.ts)

# Smoothening the curve 
EUConsumer_Sales.ts.smooth <- stats::filter(EUConsumer_Sales.ts,filter=rep(1/3,3), 
                                             method='convolution', sides=2)
#First Difference
diff <- EUConsumer_Sales.ts.smooth[3] - EUConsumer_Sales.ts.smooth[2]
for (i in seq(1,1,-1)) {
  EUConsumer_Sales.ts.smooth[i] <- EUConsumer_Sales.ts.smooth[i+1] - diff
}
n <- length(EUConsumer_Sales.ts)

Ti <- EUConsumer_SalesTS[[1]]
Ti.test <- EUConsumer_SalesTS.test[[1]]

#Second difference
diff <- EUConsumer_Sales.ts.smooth[n-1] - EUConsumer_Sales.ts.smooth[n-2]
for (i in seq(n-1+1, n)) {
  EUConsumer_Sales.ts.smooth[i] <- EUConsumer_Sales.ts.smooth[i-1] + diff
}
lines(EUConsumer_Sales.ts.smooth, col="yellow", lwd=2)

EUConsumer_Sales.ts.smdf <- as.data.frame(cbind(Ti, 
                                                            as.vector(EUConsumer_Sales.ts.smooth)))
colnames(EUConsumer_Sales.ts.smdf) <- c('MonthNo', 'Sales')
EUConsumer_Sales.ts.smdf$MonthNo<-as.numeric(EUConsumer_Sales.ts.smdf$MonthNo)
EUConsumer_Sales.ts.smdf$Sales<-as.numeric(EUConsumer_Sales.ts.smdf$Sales)
str(EUConsumer_Sales.ts.smdf)

model_fit <- lm(EUConsumer_Sales.ts.smdf$Sales ~ sin(0.5*EUConsumer_Sales.ts.smdf$MonthNo) *
              poly(EUConsumer_Sales.ts.smdf$MonthNo,2) 
            + cos(0.5*EUConsumer_Sales.ts.smdf$MonthNo) * 
              poly(EUConsumer_Sales.ts.smdf$MonthNo,2)
            + EUConsumer_Sales.ts.smdf$MonthNo, 
            data=EUConsumer_Sales.ts.smdf)
summary(model_fit)
accuracy(model_fit)

trend <- predict(model_fit, data.frame(x=Ti))
lines(Ti, trend, col="orange", lwd=2)

#Auto Arima
aa <- auto.arima(EUConsumer_Sales.ts)
aa
tsdiag(aa)
plot(aa$x, col="black")
lines(fitted(aa), col="orange")
accuracy(aa)

#FORECASTING SALES
#------------------
#HoltWinters function
EUConSalesF <- HoltWinters(EUConsumer_Sales.ts, beta=FALSE, gamma=FALSE)
EUConSalesF
plot(EUConSalesF)
EUConSalesF<-forecast:::forecast.HoltWinters(EUConSalesF,h=6)
EUConSalesF
plot(EUConSalesF)
# Comparing with Test data
accuracy(EUConSalesF,EUConsumer_SalesTS.test$Total.Monthly.Sales)[,"MAPE"]

#forecast function
forecast(EUConsumer_Sales.ts,h=6)
forecast(aa,h=6)


#EU Consumer: Modelling Quantity
#-------------------------------

#Creatinf time series data and decomposing it
EUConsumer_Qty.ts<-ts(EUConsumer_QtyTS[,2])
EUConsumer_Qty.ts.d<-ts(EUConsumer_QtyTS[,2],frequency=12)
EUConsumer_Qty.ts.decompose <- decompose(EUConsumer_Qty.ts.d)
plot(EUConsumer_Qty.ts.decompose)
plot(EUConsumer_Qty.ts)

# Smoothening the curve 
EUConsumer_Qty.ts.smooth <- stats::filter(EUConsumer_Qty.ts,filter=rep(1/3,3), 
                                           method='convolution', sides=2)
#First difference
diff <- EUConsumer_Qty.ts.smooth[3] - EUConsumer_Qty.ts.smooth[2]
for (i in seq(1,1,-1)) {
  EUConsumer_Qty.ts.smooth[i] <- EUConsumer_Qty.ts.smooth[i+1] - diff
}
n <- length(EUConsumer_Qty.ts)

Ti <- EUConsumer_QtyTS[[1]]
Ti.test <- EUConsumer_QtyTS.test[[1]]

#Second difference
diff <- EUConsumer_Qty.ts.smooth[n-1] - EUConsumer_Qty.ts.smooth[n-2]
for (i in seq(n-1+1, n)) {
  EUConsumer_Qty.ts.smooth[i] <- EUConsumer_Qty.ts.smooth[i-1] + diff
}
lines(EUConsumer_Qty.ts.smooth, col="yellow", lwd=2)

EUConsumer_Qty.ts.smdf <- as.data.frame(cbind(Ti,as.vector(EUConsumer_Qty.ts.smooth)))
colnames(EUConsumer_Qty.ts.smdf) <- c('MonthNo', 'Quantity')

#converting to numeric
EUConsumer_Qty.ts.smdf$MonthNo<-as.numeric(EUConsumer_Qty.ts.smdf$MonthNo)
EUConsumer_Qty.ts.smdf$Quantity<-as.numeric(EUConsumer_Qty.ts.smdf$Quantity)
str(EUConsumer_Qty.ts.smdf)

#Fitting Quantity time series data 
model_fit <- lm(EUConsumer_Qty.ts.smdf$Quantity ~ sin(0.5*EUConsumer_Qty.ts.smdf$MonthNo) *
              poly(EUConsumer_Qty.ts.smdf$MonthNo,2) 
            + cos(0.5*EUConsumer_Qty.ts.smdf$MonthNo) * 
              poly(EUConsumer_Qty.ts.smdf$MonthNo,2)
            + EUConsumer_Qty.ts.smdf$MonthNo, 
            data=EUConsumer_Qty.ts.smdf)
summary(model_fit)
accuracy(model_fit)

#Trend lines for the fitted time series
trend <- predict(model_fit, data.frame(x=Ti))
lines(Ti, trend, col="orange", lwd=2)

#Auto Arima
aa <- auto.arima(EUConsumer_Qty.ts)
aa
tsdiag(aa)
plot(aa$x, col="black")
lines(fitted(aa), col="orange")
accuracy(aa)

#FORECASTING QUANTITY/DEMAND
#---------------------
#HoltWinters function
EUConQuanF <- HoltWinters(EUConsumer_Qty.ts, beta=FALSE, gamma=FALSE)
EUConQuanF
plot(EUConQuanF)
EUConQuanF<-forecast:::forecast.HoltWinters(EUConQuanF,h=6)
EUConQuanF
plot(EUConQuanF)

# Comparing with Test data
accuracy(EUConQuanF,EUConsumer_QtyTS.test$Total.Monthly.Quantity)[,"MAPE"]

#forecast function
forecast(EUConsumer_Qty.ts,h=6)
forecast(aa,h=6)

