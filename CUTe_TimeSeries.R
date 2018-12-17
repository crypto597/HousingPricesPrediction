rm(list=ls(all=TRUE))

library(ggplot2)
library(forecast)
library(DMwR)
# set the working directory 
setwd("C:\\Users\\PratapVN\\Documents\\INSOFE\\CUTe")
time_data <- read.csv("TS_Train-1540270465813.csv", header = T, sep = ",")

# what is the size of the data set ? 
dim(time_data)
# Look at the summary 
summary(time_data)
str(time_data)

ggplot(time_data,aes(timestamp,value)) + geom_line()


time_data$value_ma = ma(time_data$value, order=7)
time_data$value_ma30 = ma(time_data$value, order=30)
time_data$value_ma15 = ma(time_data$value, order=15)

ggplot() +
  geom_line(data = time_data, aes(x = timestamp, y = value, colour = "Price Value"),size = 1) +
  geom_line(data = time_data, aes(x = timestamp, y = value_ma,   colour = "Weekly Moving Average"), size = 1)  +
  geom_line(data = time_data, aes(x = timestamp, y = value_ma30, colour = "Monthly Moving Average"), size = 1)  +
  ylab('Price Value')



#To get count of the number of unique products in this data set. 
length(unique(time_data$timestamp))
length(unique(time_data$value))

#Splitting the data into Train and Test
Train = time_data[1:83,]
Test = time_data[84:nrow(time_data),]

# Converting into Time series object for visual inspection
Price <- ts(Train$value,frequency = 7)
# plotting the time series data 
plot(Price)


# decompising the time series into 3 components 
# 1. Trend 2. Seasonality 3. Random
pricedecomp <- decompose(Price)
# ploting the time series components 
plot(pricedecomp)


par(mfrow=c(1,2))
# Look at the Auto correlation plot
acf(Price)
# Look at the partial Auto correlation plot
pacf(Price)



# Applying the ARIMA models 
par(mfrow=c(1,2))
# Look at the Auto correlation plot
acf(Train$value)
#We notice that the large autocorrelations persist even after several lags. This indicates that either a trend should be
#removed or that the series should be differenced

# Look at the partial Auto correlation plot
pacf(Train$value)
#(PACF), as the name suggests, display correlation between a variable and its lags that is not explained by previous lags. 
#our PACF plot suggests the order of the AR(p) model to be 0.

# building the ARIMA model-1 
#As seems from the plots above we take p=0, q=0 and 
#We can start with the order of d = 1 and re-evaluate whether further differencing is needed
model1Arima <- arima(Train$value,c(0,1,7))
# Look at the model
model1Arima
# Look at the model summary
summary(model1Arima) #MAE is 0.036

# Checking for the Errors are normally distributed or not 
# Box test on a sample data set 
set.seed(12334)
# NULL hypothesis : Errors are normally distributed
# Alt hypothesis : Errors are "NOT" normally distributed

# Applying the Box test on the residuals from the ARIMA model
Box.test (model1Arima$residuals, lag = 1, type = "Ljung")

# ACF plot 
acf(model1Arima$residuals)
# PACF plot 
pacf(model1Arima$residuals)


# Applying the model on the Test Data set 
# h indicates the nuumber of periods ahead we want to predict 
pricearimaforecasts <- forecast(model1Arima,h=nrow(Test))
my_forecast <- forecast(model1Arima,level=c(95),h=7)
# converting into a data frame for visual inspection
pricearimaforecasts <- data.frame(pricearimaforecasts)

plot(my_forecast)
# Checking the error on Test data 
regr.eval(Test$value,pricearimaforecasts$Point.Forecast)

###############
# Applying the Auto-Arima forecasting 
# Step aic based apporach used to build the model
# useful when forecasting multiple time series
# Performance is sensitive to algo implementation
modelArima <- auto.arima(Train$value)
# Look at the summary 
summary(modelArima)
# Box test on auto.arima model
Box.test(modelArima$residuals, lag = 10, type = "Ljung-Box")
# Checking the ACF and pacf of residuals 
par(mfrow =c(1,2))
# ACF plot 
acf(modelArima$residuals)
# PACF plot 
pacf(modelArima$residuals)

# Applying the model on the Test Data set 
pricearima_autoforecasts <- forecast(modelArima,h=nrow(Test))
# converting into a data frame 
pricearima_autoforecasts <- data.frame(pricearima_autoforecasts)
# Checking the error on Test data 
library(DMwR)
regr.eval(Test$value,pricearima_autoforecasts$Point.Forecast) 
