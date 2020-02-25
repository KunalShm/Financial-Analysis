library(prophet)
library(quantmod)
library(forecast)
library(xts)
library(tseries)
library(timeSeries)
library(dplyr)
library(fGarch)
library(xgboost)
library(astsa)
library(ggplot2)
library(dplyr)

getSymbols(("TCS.NS"), from="2012-01-01", to="2018-12-31")
#getSymbols("TCS.NS", src="yahoo", from="2004-08-25")
head(TCS.NS)
tcs_w <- data.frame(to.weekly(TCS.NS))
# xts has an which index must be a time-based class
plot(tcs_w$TCS.NS.Close)
tail(tcs_w)
chartSeries(tcs_w)
candleChart(tcs_w)
addEMA()
addSAR()

weeklyReturn_tcs <- weeklyReturn(tcs)
tail(weeklyReturn_tcs)
plot(weeklyReturn_tcs)


# get market data for multiple symbols making up an Index
symbol_list <- c("INFY", "WIT", "CTSH")
getSymbols(symbol_list)
# merge them all together
symbols <- data.frame(as.xts(merge(INFY, WIT, CTSH)))
head(symbols,2)

# display a simple bar chart
barChart((TCS.NS$TCS.NS.Close),type='line',theme = chartTheme("white"))
plot(TCS.NS$TCS.NS.Close)

# display a complex chart
chartSeries(TCS.NS, subset='last 3 months')
addBBands(n = 20, sd = 2, ma = "SMA", draw = 'bands', on = -1)


# Weekly forecast-------------------------------------------------------------------------
getSymbols(("TCS.NS"), from="2012-01-01", to="2018-12-31")
head(TCS.NS)
train_tcs_w <- data.frame(to.weekly(TCS.NS))

# training data for closing prices from 2012 to 2018
train_prices <- train_tcs_w$TCS.NS.Close
plot(train_prices,type='l')
acf(train_prices)
pacf(train_prices)

# testing prices for closing prices from 2019 to 2020 feb
getSymbols(("TCS.NS"), from="2019-01-01", to="2020-02-25")
test_tcs_w <- data.frame(to.weekly(TCS.NS))
test_prices <- test_tcs_w$TCS.NS.Close
plot(test_prices,type='l')

# difference taken to de trend the series
d1_prices = diff(train_prices)
plot(d1_prices,type="l")
acf(data)
pacf(data)

# due to increaing variance, we take a log transform
ld1_prices = diff(log(train_prices))
plot(ld1_prices,type="l")
acf(ld1_prices)
pacf(ld1_prices)

#seasonal component
sld1_prices = diff(diff(log(train_prices)),52)
plot(sld1_prices,type="l")
acf(sld1_prices)#q=0,1 Q= 2,3
pacf(seasonal_log_diff_data)#p= 0,1; P=2,3

#SARIMA(p,d,q,P,D,Q)52
# Compare all possible values of p and q
d=1 # non seasonal  difference
DD=1 # seasonal difference
per=52
for(p in 1:2){
  for(q in 1:2){
    for(i in 1:4){
      for(j in 1:4){
        if(p+d+q+i+DD+j<=12){
          model<-arima(x=log(train_prices), order = c((p-1),d,(q-1)), seasonal = list(order=c((i-1),DD,(j-1)), period=per))
          pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
          sse<-sum(model$residuals^2)
          cat(p-1,d,q-1,i-1,DD,j-1,per, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
        }
      }
    }
  }
}

sarima(log(train_prices), 0,1,0,2,1,1,52)
# q-q plot tells linearity
# l-J box statistics tells 
model <- arima(x=log(train_prices), order = c(0,1,0), seasonal = list(order=c(2,1,1), period=52))
plot(forecast(model))
val<- data.frame(forecast(model,62))
fcast <- exp(val$Point.Forecast)
plot(fcast,type="l",col="red")
lines(test_prices,type="l",col="blue")

