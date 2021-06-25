################################
#Created by: Alyssa Onorario || 2BSAM
#Final Requirement for AMAT132 || Introductory Forecasting
#This data involves the Total Daily Number of Vaccinated Individuals in Southeast Asia
################################
#First, we clear all variables in workspace
rm(list=ls())

#BEGIN

#Check Working Directory
getwd()

#Import .csv file to global environment
vac <- read.csv("SoutheastAsia_Vaccinations.csv",header = TRUE)

#Check the Content of Imported File
head(vac)
tail(vac)

################################
##Feature Construction

#Library
install.packages('fpp2')
library(fpp2)

#_Create Time-series
vac.ts <- ts(vac[,14],start=c(2021,1), frequency=365.25)
print(vac.ts)

#Note: We set the frequency as 365.25, since we are using a daily data.


#_Create Time-plot
autoplot(vac.ts) +
  ggtitle("Time Plot: Total Daily Vaccinations in Southeast Asian Countries") +
  xlab("Days") +
  ylab("Number of Vaccinated Individuals (thousands)")


#Note: Since our data is not seasonal, there will be no seasonal plots included.
#Findings: We can observe that there is a trend in the data, so, we can say that it is not stationary. 
#Thus, we need to transform our data.

################################
##Feature Selection and Transformation

#Library
library(urca)

#To determine if differencing is required, we use UNIT ROOT TEST.
vac.ts %>% ur.kpss() %>% summary()

#Findings: The value of the test-statistic is 3.1258, which is much bigger that the 1% critical value of 0.739. 
#Therefore, our data is not stationary.
#Hence, we need to take the first difference of the data.

#Taking the first difference
vac.ts %>% diff() %>% ur.kpss() %>% summary()

#Findings: The value of the test-statistic after differencing is 0.0294 which is lesser than 0.739.
#Therefore, the differenced data is stationary.

#Next, we determine the appropriate number of first differences
ndiffs(vac.ts)

#Findings: [1]1 . Therefore,we must do first-order differencing.


##Differencing

#To store the values for the first-order differencing
vac.diff <- diff(vac.ts)

#Then, we will look at the time-plot of the first-order difference
autoplot(vac.diff) +
  ggtitle("First Difference Total Daily Vaccinations in Southeast Asian Countries") +
  xlab("Days") +
  ylab("Number of Vaccinated Individuals (thousands)")

#Findings: We can observe in the graph that the differenced data is already stationary. 

################################
#REMARKS:
#Our series, vac.ts, has trend but no seasonality.
#In order to remove the trend, we took its first difference.
#The first difference series is already stationary.
################################

###############################
###Model Training

###
#First Forecasting Method
#_Using the simple forecasting methods:

#AverageMethod
fit_Average <- meanf(vac.diff)
print(summary(fit_Average))           #$sd: 45.33597
checkresiduals(fit_Average)


#Naive
fit_Naive <- naive(vac.diff)
print(summary(fit_Naive))           #Residual SD: 50.3478
checkresiduals(fit_Naive)


#Drift
fit_Drift <- rwf(vac.diff, drift= TRUE)
print(summary(fit_Drift))           #Residual SD: 50.4592
checkresiduals(fit_Drift)


#Findings: All Simple Forecasting Methods showed autocorrelation overtime in their ACF,
#the lags are not within the two dashed lines. Thus, they are not ideal. 
#Therefore, there must be a better model we can use than the ones above.

###
#Second Forecasting Method
#_Using Exponential Smoothing (ETS)

#Note: We use the original data series
fit_ets <- ets(vac.ts)
print(summary(fit_ets))       #Residual SD / sigma : 43.5318 
checkresiduals(fit_ets)

#Findings: Have smaller Residual SD compared to the residuals of the simple forecasting methods.
#Also, the ACF is better than the previous ones.
#However, there are still some data not used efficiently.

###
#Third Forecasting Method
#_Using ARIMA Model

#Note: The data used must be stationary
fit_arima <- auto.arima(vac.ts, d=1, stepwise= FALSE, approximation= FALSE, trace = TRUE)
print(summary(fit_arima))     #Residual SD = 39.39543
checkresiduals(fit_arima)

#Findings: Best Model: ARIMA (1,1,4)
#Findings: Fits best - based on the residual and SD. Also, there's not much autocorrelation in the ACF.
#There are only 2 lags that are outside the 95% interval.


#Note:Smaller numbers of SD represents a better fit.
#########################################
#CONCLUSION: Out of all the models tried, ARIMA model is the best to use. 
#It has the lowest Residual among the models used, and the ACF shows little autocorrelation. 
##########################################
##Forecast with ARIMA Model

#Forecast the next 6 months (remaining days of the year)
frcst <- forecast(fit_arima, h=180)
autoplot(frcst)

#Forecast after 1 year
frcst2 <- forecast(fit_arima, h=365)
autoplot(frcst2, include=180)


#END