#Loading the required libraries
library(forecast)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tseries)

#read transactional data into dataframe.

origData <- read.csv("Global Superstore.csv")

str(origData)


#Let check if there is any missing values.

sum(is.na(origData))
#there are 41296 missing values. lets check on column wise missing values.

sapply(origData, function(x) sum(is.na(x))) 

#We can see missing values are coming from postal code, we don't require postal code for analysis so we can ignore.

#Select columns which are required to the analysis.
Analysisdf <- origData[,c("Order.Date","Segment","Market","Category","Sales","Quantity","Profit")]
sum(is.na(Analysisdf)) # No missing values.
str(Analysisdf)

Analysisdf$Order.Date <- as.Date(Analysisdf$Order.Date,format="%d-%m-%Y")

sapply(Analysisdf,function(x) sum(!is.finite(x)))

market_seg_grid <- expand.grid(Market=unique(Analysisdf$Market),Segment=unique(Analysisdf$Segment))

segments_cv_profit <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(segments_cv_profit) <- c("Market","Segment","Total Profit","Profit CV")


for (i in seq(1,nrow(market_seg_grid))) {
  
    tempdf <- Analysisdf[(Analysisdf$Market==market_seg_grid$Market[i] & Analysisdf$Segment==market_seg_grid$Segment[i]),]
    tempdf$Order.Date <- format(tempdf$Order.Date,"%Y-%m")
    tempdf <- tempdf[order(tempdf$Order.Date),]
    Summdf <- tempdf %>% group_by(Order.Date) %>% summarise(Sales=sum(Sales),Quantity=sum(Quantity),Profit=sum(Profit))
    reccnt <- nrow(Summdf)
    Summdf <- Summdf[1:(reccnt - 6), ]
    segments_temp <- data.frame(Market=tempdf$Market[1],Segment=tempdf$Segment[1],"Total Profit" = sum(Summdf$Profit),"Profit CV" = sd(Summdf$Profit)/mean(Summdf$Profit))
    segments_cv_profit <- rbind(segments_cv_profit,segments_temp) 
}

# APAC Consumer and EU Consumer are most profitable and consistently profitable combinations
#   Region        Total Profit     COV
# APAC consumer   177389.251      0.6036334
# EU Consumer     152355.715      0.6553335
#

########################################FORCASTING######################################################

#####APAC CONSUMER forcasting############################

APAC_Consumer_df <- Analysisdf[which(Analysisdf$Market=="APAC" & Analysisdf$Segment=="Consumer"),c("Order.Date","Sales","Quantity")]
APAC_Consumer_df$Order.Date <- format(APAC_Consumer_df$Order.Date,"%Y-%m")
APAC_Consumer_df <- APAC_Consumer_df[order(APAC_Consumer_df$Order.Date),]
APAC_Consumer_df <- APAC_Consumer_df %>% group_by(Order.Date) %>% summarise(Sales=sum(Sales),Quantity=sum(Quantity))
reccnt <- nrow(APAC_Consumer_df)

APAC_Consumer_df_train <- APAC_Consumer_df[1:(reccnt - 6), ]
APAC_Consumer_df_test <- APAC_Consumer_df[(reccnt - 5) :reccnt , ]
plot(ts(APAC_Consumer_df$Sales), ylab= "Sales")
plot(ts(APAC_Consumer_df$Quantity),  ylab= "Quantity")

#################################################### APAC SALES forcasting###################################################

# Classical decomposition


timeser <- ts(APAC_Consumer_df_train$Sales)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                         filter=rep(1/(2*w+1),(2*w+1)), 
                         method='convolution', sides=2)
#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- c(1:42)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')


lmfit_Move_Avg <- lm(Sales ~ sin(0.5*Month) * poly(Month,3) + cos(0.5*Month) * poly(Month,3)
            + Month, data=smootheddf)
global_pred <- predict(lmfit_Move_Avg, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit_Move_Avg,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Consumer_df_test$Sales)[5]
MAPE_class_dec
# Moving Average MAPE - 31.074

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(APAC_Consumer_df[,2])
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col="blue")

# Exponential smoothing

smoothedseries_exp <- HoltWinters(timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(timeser, col="black")
lines(fitted(smoothedseries_exp)[,1], col="red", lwd=1)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- c(1:42)
#smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(fitted(smoothedseries_exp)[,1])))
colnames(smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Exp_smooth <- lm(Sales ~ sin(0.5*Month) + Month, data=smootheddf)
global_pred <- predict(lmfit_Exp_smooth, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- APAC_Consumer_df_test[,2]
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(APAC_Consumer_df[ ,2]), col = "black")
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col="blue")

# Auto Arima for Sales

APAC_Sales_train_arima <- auto.arima(ts(APAC_Consumer_df_train$Sales))
APAC_Sales_train_arima
tsdiag(APAC_Sales_train_arima)
plot(APAC_Sales_train_arima$x,col="black")
lines(fitted(APAC_Sales_train_arima),col="red")

adf.test(ts(APAC_Consumer_df_train$Sales) - fitted(APAC_Sales_train_arima),alternative = "stationary")
kpss.test(ts(APAC_Consumer_df_train$Sales) - fitted(APAC_Sales_train_arima))

fcast_auto_arima <- predict(APAC_Sales_train_arima, n.ahead = 6)

sum(abs((APAC_Consumer_df_test[,2]-as.vector(fcast_auto_arima$pred))/APAC_Consumer_df_test[,2]))*(100/6)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(APAC_Sales_train_arima),ts(fcast_auto_arima$pred))
plot(ts(APAC_Consumer_df[,2]), col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(APAC_Sales_train_arima), col="blue")

#------------------------------------------------------------------------------------------
#APAC Sales - MAPE for 3 models:
# Using moving average - 31.07
# Using Exponencial Smoothing - 21.22
# Using Auto Arima - 27.68

# Based on the MAPE Exponencial smoothing method as the least MAPE , so forecasting next 6 months sales.

timevals_out <- c(49:54)
APAC_sales_forecast <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(APAC_sales_forecast))
plot(ts(APAC_Consumer_df[,2]), col = "black", xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")

##################APAC QUANTITY Forecast###################################

# Quantity Classical decomposition

timeser <- ts(APAC_Consumer_df_train$Quantity)
plot(timeser)

#Moving Average smoothing


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- c(1:42)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')


lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

##################

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Consumer_df_test$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(APAC_Consumer_df$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(APAC_Consumer_df[,3]), col = "black" )
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col="blue")

# Exponential Smoothing

smoothedseries_exp <- HoltWinters(timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
lines(fitted(smoothedseries_exp)[,1], col="red", lwd=2)

timevals_in <- c(1:42)
#smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(fitted(smoothedseries_exp)[,1])))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month, data=smootheddf)
lmfit <- lm(Quantity ~ sin(0.5*Month) + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- APAC_Consumer_df_test[,3]
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out #+ predict(armafit,n.ahead = 6)$pred

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(APAC_Consumer_df[,3]), col = "black")
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col = "red")


# Quantity auto ARIMA

APAC_Quant_train_arima <- auto.arima(ts(APAC_Consumer_df_train$Quantity))
APAC_Quant_train_arima
tsdiag(APAC_Quant_train_arima)
plot(APAC_Quant_train_arima$x,col="black")
lines(fitted(APAC_Quant_train_arima),col="red")

adf.test(ts(APAC_Consumer_df_train$Quantity) - fitted(APAC_Quant_train_arima),alternative = "stationary")
kpss.test(ts(APAC_Consumer_df_train$Quantity) - fitted(APAC_Quant_train_arima))

fcast_auto_arima <- predict(APAC_Quant_train_arima, n.ahead = 6)

#MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer_df_test[,3])[5]
#MAPE_auto_arima

sum(abs((APAC_Consumer_df_test[,3]-as.vector(fcast_auto_arima$pred))/APAC_Consumer_df_test[,3]))*(100/6)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(APAC_Quant_train_arima),ts(fcast_auto_arima$pred))
plot(ts(APAC_Consumer_df[,3]), col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(APAC_Quant_train_arima), col = "blue")

#########APAC QUANTITY Forcasting##############
#APAC Quantity - MAPE for 3 models:
# Using moving average - 33.79
# Using Exponencial Smoothing - 25.00
# Using Auto Arima - 26.24

# Based on the MAPE Exponencial smoothing method as the least MAPE , so forecasting next 6 months sales.

timevals_out <- c(49:54)
APAC_quantity_forecast <- predict(lmfit,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(APAC_quantity_forecast))
plot(ts(APAC_Consumer_df[,3]), col = "black", xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")


#####EU CONSUMER forcasting############################

EU_Consumer_df <- Analysisdf[which(Analysisdf$Market=="EU" & Analysisdf$Segment=="Consumer"),c("Order.Date","Sales","Quantity")]
EU_Consumer_df$Order.Date <- format(EU_Consumer_df$Order.Date,"%Y-%m")
EU_Consumer_df <- EU_Consumer_df[order(EU_Consumer_df$Order.Date),]
EU_Consumer_df <- EU_Consumer_df %>% group_by(Order.Date) %>% summarise(Sales=sum(Sales),Quantity=sum(Quantity))
reccnt <- nrow(EU_Consumer_df)

EU_Consumer_df_train <- EU_Consumer_df[1:(reccnt - 6), ]
EU_Consumer_df_test <- EU_Consumer_df[(reccnt - 5) :reccnt , ]
plot(ts(EU_Consumer_df$Sales), ylab= "Sales")
plot(ts(EU_Consumer_df$Quantity),  ylab= "Quantity")

#################################################### EU SALES forcasting###################################################

# Classical decomposition


timeser <- ts(EU_Consumer_df_train$Sales)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)
#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- c(1:42)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')


lmfit_Move_Avg <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
                     + Month, data=smootheddf)
global_pred <- predict(lmfit_Move_Avg, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit_Move_Avg,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,EU_Consumer_df_test$Sales)[5]
MAPE_class_dec
# Moving Average MAPE - 31.074

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(EU_Consumer_df[,2])
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(total_timeser, col = "black")
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col="blue")

# Exponential smoothing

smoothedseries_exp <- HoltWinters(timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
plot(timeser, col="black")
lines(fitted(smoothedseries_exp)[,1], col="red", lwd=1)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
timevals_in <- c(1:42)
#smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(fitted(smoothedseries_exp)[,1])))
colnames(smootheddf) <- c('Month', 'Sales')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_Exp_smooth <- lm(Sales ~ sin(0.5*Month) + Month, data=smootheddf)
global_pred <- predict(lmfit_Exp_smooth, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- EU_Consumer_df_test[,2]
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(EU_Consumer_df[ ,2]), col = "black")
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col="blue")

# Auto Arima for Sales

EU_Sales_train_arima <- auto.arima(ts(EU_Consumer_df_train$Sales))
EU_Sales_train_arima
tsdiag(EU_Sales_train_arima)
plot(EU_Sales_train_arima$x,col="black")
lines(fitted(EU_Sales_train_arima),col="red")

adf.test(ts(EU_Consumer_df_train$Sales) - fitted(EU_Sales_train_arima),alternative = "stationary")
kpss.test(ts(EU_Consumer_df_train$Sales) - fitted(EU_Sales_train_arima))

fcast_auto_arima <- predict(EU_Sales_train_arima, n.ahead = 6)

sum(abs((EU_Consumer_df_test[,2]-as.vector(fcast_auto_arima$pred))/EU_Consumer_df_test[,2]))*(100/6)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(EU_Sales_train_arima),ts(fcast_auto_arima$pred))
plot(ts(EU_Consumer_df[,2]), col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(EU_Sales_train_arima), col="blue")

#------------------------------------------------------------------------------------------
#EU Sales - MAPE for 3 models:
# Using moving average - 34.35
# Using Exponencial Smoothing - 24.60
# Using Auto Arima - 28.92

# Based on the MAPE Exponencial smoothing method as the least MAPE , so forecasting next 6 months sales.

timevals_out <- c(49:54)
EU_sales_forecast <- predict(lmfit_Exp_smooth,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(EU_sales_forecast))
plot(ts(EU_Consumer_df[,2]), col = "black", xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")

##################EU QUANTITY Forecast###################################

# Quantity Classical decomposition

timeser <- ts(EU_Consumer_df_train$Quantity)
plot(timeser)

#Moving Average smoothing


#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                                filter=rep(1/(2*w+1),(2*w+1)), 
                                method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- c(1:42)
lines(smoothedseries, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Quantity')


lmfit <- lm(Quantity ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2)
            + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

##################

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,EU_Consumer_df_test$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit
total_timeser <- ts(EU_Consumer_df$Quantity)
class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(EU_Consumer_df[,3]), col = "black" )
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col="blue")

# Exponential Smoothing

smoothedseries_exp <- HoltWinters(timeser, alpha=0.5, beta=FALSE, gamma=FALSE)
lines(fitted(smoothedseries_exp)[,1], col="red", lwd=2)

timevals_in <- c(1:42)
#smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
smootheddf <- as.data.frame(cbind(timevals_in, as.vector(fitted(smoothedseries_exp)[,1])))
colnames(smootheddf) <- c('Month', 'Quantity')

#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

#lmfit <- lm(Sales ~ sin(0.5*Month) * poly(Month,2) + cos(0.5*Month) * poly(Month,2) + Month, data=smootheddf)
lmfit <- lm(Quantity ~ sin(0.5*Month) + Month, data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='orange', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

armafit
tsdiag(armafit)

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- EU_Consumer_df_test[,3]
timevals_out <- c(43:48)

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out #+ predict(armafit,n.ahead = 6)$pred

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(ts(EU_Consumer_df[,3]), col = "black")
lines(class_dec_pred, col = "red")
lines(ts(global_pred), col = "red")


# Quantity auto ARIMA

EU_Quant_train_arima <- auto.arima(ts(EU_Consumer_df_train$Quantity))
EU_Quant_train_arima
tsdiag(EU_Quant_train_arima)
plot(EU_Quant_train_arima$x,col="black")
lines(fitted(EU_Quant_train_arima),col="red")

adf.test(ts(EU_Consumer_df_train$Quantity) - fitted(EU_Quant_train_arima),alternative = "stationary")
kpss.test(ts(EU_Consumer_df_train$Quantity) - fitted(EU_Quant_train_arima))

fcast_auto_arima <- predict(EU_Quant_train_arima, n.ahead = 6)

#MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_df_test[,3])[5]
#MAPE_auto_arima

sum(abs((EU_Consumer_df_test[,3]-as.vector(fcast_auto_arima$pred))/EU_Consumer_df_test[,3]))*(100/6)

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(EU_Quant_train_arima),ts(fcast_auto_arima$pred))
plot(ts(EU_Consumer_df[,3]), col = "black")
lines(auto_arima_pred, col = "red")
lines(fitted(EU_Quant_train_arima), col = "blue")

#########EU QUANTITY Forcasting##############
#EU Quantity - MAPE for 3 models:
# Using moving average - 33.24
# Using Exponencial Smoothing - 27.40
# Using Auto Arima - 30.13

# Based on the MAPE Exponencial smoothing method as the least MAPE , so forecasting next 6 months sales.

timevals_out <- c(49:54)
EU_quantity_forecast <- predict(lmfit,data.frame(Month =timevals_out))
model_pred <- c(ts(global_pred),ts(global_pred_out))
forcast_pred <- c(ts(global_pred),ts(global_pred_out),ts(EU_quantity_forecast))
plot(ts(EU_Consumer_df[,3]), col = "black", xlim=c(0, 55))
lines(forcast_pred, col = "blue")
lines(model_pred, col = "red")



