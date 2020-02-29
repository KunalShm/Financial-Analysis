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
tcs_m <- data.frame(to.monthly(TCS.NS))
# xts has an which index must be a time-based class
plot(tcs_m$TCS.NS.Close,type="l")
tail(tcs_m)
chartSeries(tcs_m)
candleChart(tcs_m)
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


# Monthly forecast-------------------------------------------------------------------------
getSymbols(("TCS.NS"), from="2012-01-01", to="2018-12-31")
head(TCS.NS)
train_tcs_m <- data.frame(to.monthly(TCS.NS))
attributes(train_tcs_m)
plot(decompose(train_tcs_m))

# training data for closing prices from 2012 to 2018
train_prices <- train_tcs_m$TCS.NS.Close
plot(train_prices,type='l')
acf(train_prices)
pacf(train_prices)
plot(decompose(ts(train_prices, frequency = 12)))

# testing prices for closing prices from 2019 to 2020 feb
getSymbols(("TCS.NS"), from="2019-01-01", to="2020-02-25")
test_tcs_m <- data.frame(to.monthly(TCS.NS))
test_prices <- test_tcs_m$TCS.NS.Close
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
sld1_prices = diff(diff(log(train_prices)),12)
plot(sld1_prices,type="l")
acf(sld1_prices)#q=0,1 Q= 4,5,6(j)
pacf(seasonal_log_diff_data)#p= 0,1,2; P=4,5(i)

#SARIMA(p,d,q,P,D,Q)12
# Compare all possible values of p and q
d=1 # non seasonal  difference
DD=1 # seasonal difference
per=12
for(p in 1:2){
  for(q in 1:3){
    for(i in 1:6){
      for(j in 1:6){
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

sarima(log(train_prices), 1,1,1,1,1,5,12)
# q-q plot tells linearity
# l-J box statistics tells 
model <- arima(x=log(train_prices), order = c(1,1,1), seasonal = list(order=c(1,1,5), period=12))
plot(forecast(model))
val<- data.frame(forecast(model,14))
fcast <- exp(val$Point.Forecast)
plot(fcast,type="l",col="red",main="Predicted vs Actual monthly price movments")
lines(test_prices,type="l",col="blue")
#legend("topleft", "red: Forecasted", title= "blue : Actual")
legend("topleft", legend=c("Forecasted", "Actual"),
       col=c("red", "Blue"), lty=1:2, cex=0.8)

#holt Winters Parameters
#Levels, with parameter "alpha"
#Trend, with parameter "beta", 
#Seasonal Component , with parameter "gamma"
train_prices_ts = ts(data = log(train_prices), start = c(2012,1), frequency = 12)
prices_hw = HoltWinters(train_prices_ts)
# Coeffiients a - last smoothed level, b- smoothed trend
# s(1:12)- Seasonality coefficients
prices_hw$SSE

price_forecast <- forecast:::forecast.HoltWinters(prices_hw,14)
# forecastes are given as point forecasts in 80% and 95% confidence intervals
# plotted as 2 different shades of grey

fcast_prices_val <- data.frame(price_forecast)
fcast <- (exp(fcast_prices_val$Point.Forecast)-100)
plot(fcast,type='l',col='red')
lines(test_prices,type="l",col="blue")
legend("topleft", legend=c("Forecasted", "Actual"),
       col=c("red", "Blue"), lty=1:2, cex=0.8)


