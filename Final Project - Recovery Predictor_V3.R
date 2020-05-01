
library(quantmod)
library(stargazer)
library(fredr)
library(tidyr)
library(PASWR2)
library(MASS)
library(repmis)
library(latex2exp)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(RCurl)
library(haven)
library(forecast)
library(depmixS4)
library(randomForest)
fredr_set_key('7ed57ed908bbcfe6f306db34ebbd7fa4') # Sarah McCann FREDR key.


#Step 1: Gather data on whether we are in or not in a recession - Recession Predictor

rm(list = ls())

recession = fredr('USREC')


#Step 2: Gather Relevant data that may be predictors of the state of economy

#The Yield Curve 
yieldcurve = fredr('T10Y3MM')

#Credit spreads tend to spike when we enter a recession 

bbbspreads = fredr('BAMLC0A4CBBB')

#Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity
baaspread = fredr(series_id = "BAA10Y")

#University of Michigan: Consumer Sentiment declines when we are in a recession
sentiment = fredr(series_id="UMCSENT")
summary(sentiment)

#Weekly UI Claims spikes when we are in a recession
claims = fredr(series_id = "ICSA", observation_start = as.Date("1980-01-01"))
claims$value = claims$value / 10000000
plot(claims$date, claims$value, col = 'blue', main="Weekly UI Claims",
     pch=16, ylab = "Claims (Millions)", xlab = "Date")
grid(lw=2)
lines(claims$date, claims$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")
# Question - how to turn into monthly data in code? 


#Nominal Value of the Fed Balance Sheet rises in a recession
balance = drop_na(fredr("WALCL", observation_start = as.Date("1990-01-01")))
balance$value = balance$value * 1000
plot(balance$date, balance$value, pch=16, col='blue',
     xlab="Date", ylab="Nominal Value of Fed's Balance Sheet",
     main="Powell's Condundrum")
grid(lw=2)
lines(balance$date, balance$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")


#Market Expectations of Inflation in 10 Years drops in a recession
breakeven = drop_na(fredr("T10YIE", observation_start = as.Date("1990-01-01")))
plot(breakeven$date, breakeven$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Market Expectations of Inflation in 10 Years")
grid(lw=2)
lines(breakeven$date, breakeven$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")
abline(h=2, col="black")



# Question: HOW TO MERGE THE DATA ABOVE INTO A SINGLE TIME-SERIES DATASET? 
data = merge(recession, yieldcurve, bbbspreads, baaspread, by.x='date', by.y='date')  ## Merge into single time-series dataset
data = subset(data, select = c(date, value.x, value.y))
names = c("date", "recession", "yieldcurve", "bbbspreads", "baaspread")
colnames(data) = names
data$yieldcurve = data$yieldcurve * 100
summary(data)

data$yieldcurve.l1 = lag(data$yieldcurve,1)


stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)


data$yieldcurve.l1 = lag(data$yieldcurve, 1)
#creates a lagged value on the yieldcurve variable

#Step 3: Determine the Model to Use

# Random Forest? 


