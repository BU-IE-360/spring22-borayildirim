#######################################################
#Bora Yıldırım

#IE360 HW 2 

#######################################################

setwd("/Users/bora.yildirim/Desktop")
library(ggplot2)
library(forecast)
library(readxl)
library(zoo)
install.packages("fpp2")
library(fpp2)

#I converted the csv data to the excel file to have numeric values
data_gasoline <- read_excel("IE360_Spring22_HW2_data.xlsx")

#Converting quarters into date objects
new_quarter <- seq(as.Date("2000-01-01"), by="quarter", length.out =32)
data_gasoline$Quarter <- new_quarter

#Plotting of UGS time series
UGS_time_series <- ts(data_gasoline$`Unleaded Gasoline Sale (UGS)`,start = c(2000,1), frequency = 4 )

autoplot(UGS_time_series)
#Data seems to has a decreasing trend line, and it is not stationary
UGS_decomposed <- decompose(UGS_time_series, type="multiplicative")
plot(UGS_decomposed)

#I will take the difference between observations to obtain a stationary time series

Diff_UGS_time_series <- diff(UGS_time_series)
autoplot(Diff_UGS_time_series)

#Time series now seems stationary
#Now we look at seasonal plot, and the graph shows us that every q2 change between UGS sales is increasing, and every q4 it is decreasing.
#Our data also has seasonality
ggseasonplot(Diff_UGS_time_series)

#autocorrelation plot shows us at lag 1 and lag 4 acf is out of the boundary which shows us the seasonality effect.
acf(data_gasoline$`Unleaded Gasoline Sale (UGS)`[1:28], 8)



## forecasting

#seasonal naive method, p-value = 9.137e-09, residual SD=223671.49, residuals seem random, all acf is between %95 conf. interval boundaries

snaive_model_1 <- snaive(Diff_UGS_time_series[1:28])
summary(snaive_model_1)
checkresiduals(snaive_model_1)


#exponential smooting, p-value = 9.203e-11, residual SD(sigma)= 0.1281, residuals seem to have a pattern, some acf crosses %95 conf interval boundaries

es_model_2 <- ets(UGS_time_series[1:28])
summary(es_model_2)
checkresiduals(es_model_2)

#ARIMA method, d for eliminating the trend, and D for eliminating seasonality, p-value= 2.971e-11, residual SD= 149799.9 , some acf are out of boundaries, residuals seem to have a strong pattern

arima_model_3 <- auto.arima(UGS_time_series[1:28], d= 1, D= 1)
summary(arima_model_3)
checkresiduals(arima_model_3)

#linear regression with other independent variables, p-value= 7.027e-09, residual SD= 47250, all acf in the boundaries and relatively have lower values, residuals seem random

lm_model_4 <- lm(UGS_time_series[1:28]~ data_gasoline$RNUV[1:28] + data_gasoline$`Price of Unleaded Gasoline (PU)`[1:28] + data_gasoline$`Price of Diesel Gasoline (PG)`[1:28] + data_gasoline$`GNP Agriculture`[1:28] + data_gasoline$`GNP Commerce`[1:28] + data_gasoline$`GNP Total`[1:28] + data_gasoline$`# LPG Vehicles (NLPG)`[1:28] + data_gasoline$`# Unleaded Gasoline Vehicles (NUGV)`[1:28] + data_gasoline$`# of Diesel Gasoline Vehicles (NDGV)`[1:28])
summary(lm_model_4)
checkresiduals(lm_model_4)


#I will use the fourth model due to lower SD, randomness among residuals, lower acf values. Proceeding to forecast

data_29 <- data_gasoline[29,c("RNUV", "Price of Unleaded Gasoline (PU)", "Price of Diesel Gasoline (PG)", "GNP Agriculture", "GNP Commerce", "GNP Total", "# LPG Vehicles (NLPG)", "# Unleaded Gasoline Vehicles (NUGV)", "# of Diesel Gasoline Vehicles (NDGV)")]
data_30 <- data_gasoline[30,c("RNUV", "Price of Unleaded Gasoline (PU)", "Price of Diesel Gasoline (PG)", "GNP Agriculture", "GNP Commerce", "GNP Total", "# LPG Vehicles (NLPG)", "# Unleaded Gasoline Vehicles (NUGV)", "# of Diesel Gasoline Vehicles (NDGV)")]
data_31 <- data_gasoline[31,c("RNUV", "Price of Unleaded Gasoline (PU)", "Price of Diesel Gasoline (PG)", "GNP Agriculture", "GNP Commerce", "GNP Total", "# LPG Vehicles (NLPG)", "# Unleaded Gasoline Vehicles (NUGV)", "# of Diesel Gasoline Vehicles (NDGV)")]
data_32 <- data_gasoline[32,c("RNUV", "Price of Unleaded Gasoline (PU)", "Price of Diesel Gasoline (PG)", "GNP Agriculture", "GNP Commerce", "GNP Total", "# LPG Vehicles (NLPG)", "# Unleaded Gasoline Vehicles (NUGV)", "# of Diesel Gasoline Vehicles (NDGV)")]

forecast_29 <- predict(lm_model_4, newdata =data_29)
forecast_30 <- predict(lm_model_4, newdata =data_30)
forecast_31 <- predict(lm_model_4, newdata =data_31)
forecast_32 <- predict(lm_model_4, newdata =data_32)
