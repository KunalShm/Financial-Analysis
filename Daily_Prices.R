library(astsa)
library(stats)
library(quantmod)
library(xts)
library(tseries)
library(timeSeries)

# training data
getSymbols(("TCS.NS"), from="2019-01-01", to="2019-12-31")
head(TCS.NS)
tcs_daily <- data.frame(to.daily(TCS.NS))
train_prices <- na.omit(tcs_daily$TCS.NS.Close)
plot(train_prices,type="l")

# testing data
getSymbols(("TCS.NS"), from="2020-01-01", to="2020-02-27")
head(TCS.NS)
tcs_daily <- data.frame(to.daily(TCS.NS))
test_prices <- (tcs_daily$TCS.NS.Close)
plot(test_prices,type="l")

# Analyze if any trend exists in the data, that suggests differencing
plot(train_prices,type="l")
# we can see the data is not stationary

# checking Q-statistics to check for stationarity
Box.test(train_prices, lag=log(length(train_prices)))
# very small p values indicates definite auto corelation
# we can fit some model here
Box.test(diff(train_prices), lag=log(length(train_prices)))
df1_train_prices <- na.omit(diff(train_prices))
plot(df1_train_prices)

# variation in variance suggest transformation
# Log or log return based on above two calls
ldf1_train_prices <- diff(log(train_prices))
plot(ldf1_train_prices)

df2_train_prices <- na.omit(diff(df1_train_prices))
plot(df2_train_prices)
ldf2_train_prices <- diff(ldf1_train_prices)
plot(ldf2_train_prices)
# analysis suggested 2nd order differencing is requrired but log transformation is not


# analyze the ACF plot to calculate the order of MA process p
acf(train_prices)
acf(df1_train_prices) # q = 0,1,11
acf(df2_train_prices) # q = 1,10

# analyze the PACF to calculate the order of AR process or q
pacf(train_prices)
pacf(df1_train_prices) # p = 0,3,12
pacf(df2_train_prices) # p = 1,3

# shortlist a model based on AIC criteria, SSE
model1 <- arima(train_prices, order = c(0,1,1))
SSE1 <- sum(model1$residuals^2)
model1_test <- Box.test(model1$residuals, lag=log(length(model1$residuals)))

model2 <- arima(train_prices, order = c(1,1,1))
SSE2 <- sum(model1$residuals^2)
model2_test <- Box.test(model1$residuals, lag=log(length(model1$residuals)))

model3 <- arima(train_prices, order = c(0,1,3))
SSE3 <- sum(model1$residuals^2)
model3_test <- Box.test(model1$residuals, lag=log(length(model1$residuals)))

model4 <- arima(train_prices, order = c(1,1,3))
SSE4 <- sum(model1$residuals^2)
model4_test <- Box.test(model1$residuals, lag=log(length(model1$residuals)))

df <- data.frame(row.names = c('AIC','SSE',"p-value") 
                 ,c(model1$aic, SSE1, model1_test$p.value)
                 ,c(model2$aic, SSE2, model2_test$p.value)
                 ,c(model3$aic, SSE3, model3_test$p.value)
                 ,c(model4$aic, SSE4, model4_test$p.value))
colnames(df) <- c(1,2,3,4)

format(df,scientific=FALSE)# high p value for the residuals mean that there is no significant autocorelation

# estimate parameters using Auto ARIMA
auto.arima(ts(train_prices, frequency = 12))

# final model
model = arima(x=(train_prices), order = c(11,1,12))
plot(forecast(model))
val<- data.frame(forecast(model,39))
fcast <- (val$Point.Forecast)
plot(test_prices,type="l",col="red",main="Predicted vs Actual monthly price movments")
lines(fcast*1.019,type="l",col="blue")
accuracy(forecast(model,39))
# analyze residual to evaluate results' quality

# residuals gies difference between fitted and actual values
acf(model$residuals)
# auto corelation between residuals is not high, within significance bounds
hist(model$residuals, freq = FALSE)
lines(density(model$residuals))
# residuals must be more dense near the zero

# Ljung Box test
Box.test(model$residuals, lag=20, type='Ljung-Box')
# p values less than 0.05 only suggests some significant autocorelation among the residuals
# here high p value suggests that there is little of no autocorelation among the residuals

# Using Propet package
library(prophet)
# training data
getSymbols(("GOOG"), from="2016-01-01", to="2019-09-30")
dim(GOOG)
train_Google_daily <- data.frame(to.daily(GOOG))
ds <- row.names(train_Google_daily)
y <- log(train_Google_daily$GOOG.Close)
train_prices <- data.frame(ds,y)
qplot(ds, y, data=train_prices)

# testing data
getSymbols(("GOOG"), from="2019-10-01", to="2020-02-20")
dim(GOOG)
test_Google_daily <- data.frame(to.daily(GOOG))
ds <- row.names(test_Google_daily)
y <- log(test_Google_daily$GOOG.Close)
test_prices <- data.frame(ds,y)
qplot(ds_test, y_test, data=test_prices)

library(prophet)

model <- prophet(train_prices)
dim(train_prices)
fcast_prices_df <- make_future_dataframe(model, 97)
dim(fcast_prices_df)
tail(fcast_prices_df)
fcast <- predict(model, fcast_prices_df)
plot(model, fcast, main="Future prediction for Google stock prices")
prophet_plot_components(model,forecast)

fcast_values <- data.frame(fcast$ds, (fcast$yhat))
predicted_fcast <- fcast_values[941:1038,]
colnames(predicted_fcast) <- c("ds","y")
head(predicted_fcast)
plot(exp(test_prices$y), col="Red", type="l")
lines(exp(predicted_fcast$y), col="Green", type="l")

