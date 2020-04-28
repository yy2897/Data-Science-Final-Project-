
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
fredr_set_key('7ed57ed908bbcfe6f306db34ebbd7fa4') # Sarah McCann FREDR key.

# we want to define as a recovery predictor
# 1s being recovery
# 0s being recession
# algorithms know these are classified differently
# think about the types of macro measures to use in addition to the yield curve
# still using the yield curve - cam harvey
# think about other macro measures you might put in there 
# thinking about spreads - spreads in private bond markets

bbbspreads = fredr('BAMLC0A4CBBB')
# spreads are seen everywhere
# aaa bonds relative to junk bonds
# inversions in measures are same as yield curve

# training algorithm on UK data, use US data as test data 
# if you have a weekly series on unemployment insurance claims (weekly) - could create a total for the month, flow measure
# sum the weeks to get monthly claims
# stock prices on a daily basis - calculate monthly high, monthly low, monthly average - 3 different measures to include

# step 1
# identify potential influential variables / data and plot to assess trends
# recovery indicator - how to define a recovery = "state of growth" per NBER data - how to transform so indicator '1'
# when we are in a state of recovery / growth, the following is likely true:
# 1) the yield curve is upward-sloping (not inverted)
# 2) personal consumption is growing
# 3) fed balance sheet = ??
# 4) unemployment is dropping - employment is growing
# identifying variables that have same time increments (monthly) and that are released at relevant points 
# gdp growth will be too late 
# do we have monthly changes in personal consumption?

# step 2
# identify algorithm to use to predict
# how to implement holt-winters algorithm - does this work with multiple variables?

# these lines of code ingest the NBER data and changes the 1s to 0s and 0s to 1s
recession = fredr('USREC')
recovery = recession * 

# data sourcing from class notes below

rm(list = ls())
recession = fredr('USREC')
yieldcurve = fredr('T10Y3MM')

data = merge(recession, yieldcurve, by.x='date', by.y='date')  ## Merge into single time-series dataset
data = subset(data, select = c(date, value.x, value.y))
names = c("date", "recession", "yieldcurve")
colnames(data) = names
data$yieldcurve = data$yieldcurve * 100
summary(data)
data$yieldcurve.l1 = lag(data$yieldcurve, 1)
#creates a lagged value on the yieldcurve variable

#merged two data sources together, adapted into bps
#will spend more time with this code when talking with individual groups 
#recession is an indicator value (value 1 when US is in recession)


#GDP per Capita data
# From In-Class Assignment: Read in GDP Per Capita from the first session.
# Lag the value.  Rename the variables and run the regresion.
# Now you have fit a perfect model.

gdp_pc = fredr('A229RX0A048NBEA') # Grab GDP per capita
attach(gdp_pc) # Attach and plot.
plot(date, value, pch=16, col="darkblue", main="GDP Per Capita (2009 $)", 
     xlab="Date", ylab="GDP Per Capita")  ## time series plot in R  
lines(date, value, col="darkblue")
grid(lw=2)

gdp_pc$lag.gdp_pc = lag(gdp_pc$value)

names = c("date", "id", "gdp_pc", "lag_gdp_pc")
colnames(gdp_pc) = names
perfectmodel = lm('gdp_pc ~ lag_gdp_pc', data = gdp_pc)
stargazer(perfectmodel, type="text", title="Perfect Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

# Use quarterly changes in personal consumption.
pcchange = drop_na(fredr("DPCERL1Q225SBEA"))
plot(pcchange$date, pcchange$value, pch=16, col="blue",
     main="Quarterly Change in Personal Consumption", 
     xlab="Date", ylab="Quarterly Change (%)")
grid(lw=2)
lines(pcchange$date, pcchange$value, col="blue")
abline(h=0, lw=2)
abline(h=mean(pcchange$value), lw=2, col="red")

arima = Arima(pcchange$value, order=c(1, 0, 1))
summary(arima)
plot(forecast(arima, h=8), include=80, col="blue")
grid(lw=2)
abline(h=0, lw=2)
abline(h=mean(pcchange$value), lw=2, col="red")

# Notes:
# The graph shows the last 80 quarters and forecast for 8 quarters.
# The forecast is essentially the long-run average growth of about 3.3%.



# Common Economic Time Series Data in CRE
threemonth = drop_na(fredr("DGS3MO", observation_start = as.Date("1990-01-01")))
tenyear = drop_na(fredr("DGS10", observation_start = as.Date("1990-01-01")))
fedfunds = drop_na(fredr(series_id = "FEDFUNDS", observation_start = as.Date("1990-01-01")))


plot(threemonth$date, threemonth$value, pch=16, col='blue',
     xlab="Date", ylab="%", 
     main="Three-Month US Treasurys since 1990")
grid(lw=2)
lines(threemonth$date, threemonth$value, col='blue')


plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", 
     main="10-Year US Treasurys since 1990")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue')
abline(h=3, col='red')


plot(fedfunds$date, fedfunds$value, col = 'blue', pch=16, 
     ylab = "Rate", xlab = "Date", main="Fed Funds Rate since 1990")
grid(lw=2)
lines(fedfunds$date, fedfunds$value, col='blue')


unrate = fredr(series_id = "UNRATE", observation_start = as.Date("1990-01-01"))
plot(unrate$date, unrate$value, col = 'blue', main="US U-3 Unemployment Rate since 1990",
     pch=16, ylab = "%", xlab = "Date")
grid(lw=2)
lines(unrate$date, unrate$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")


empgrowth = fredr(series_id = "PRS85006012", observation_start = as.Date("1980-01-01"))
plot(empgrowth$date, empgrowth$value, col = 'blue', main="US Employment Growth since 1980",
     pch=16, ylab = "%", xlab = "Date")
grid(lw=2)
lines(empgrowth$date, empgrowth$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")
abline(h=0, col="black")


claims = fredr(series_id = "ICSA", observation_start = as.Date("1980-01-01"))
claims$value = claims$value / 10000000
plot(claims$date, claims$value, col = 'blue', main="Weekly UI Claims",
     pch=16, ylab = "Claims (Millions)", xlab = "Date")
grid(lw=2)
lines(claims$date, claims$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")


yield_curve = fredr(series_id = "T10Y3M", observation_start=as.Date('2017-01-01'))
plot(yield_curve$date, yield_curve$value, col = 'blue', main='Yield Curve since 2017', 
     pch=16, ylab="%", xlab="Date")
grid(lw=2)
lines(yield_curve$date, yield_curve$value, col = 'blue')
abline(h=0, col='red')


tenyear = drop_na(fredr("DGS10", observation_start = as.Date("1990-01-01")))
plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", ylim=c(0, 10),
     main="Bernanke's Condundrum")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue')
lines(fedfunds$date, fedfunds$value, col = 'red', pch=16)


balance = drop_na(fredr("WALCL", observation_start = as.Date("1990-01-01")))
balance$value = balance$value * 1000
plot(balance$date, balance$value, pch=16, col='blue',
     xlab="Date", ylab="Nominal Value of Fed's Balance Sheet",
     main="Powell's Condundrum")
grid(lw=2)
lines(balance$date, balance$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")


breakeven = drop_na(fredr("T10YIE", observation_start = as.Date("1990-01-01")))
plot(breakeven$date, breakeven$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Market Expectations of Inflation in 10 Years")
grid(lw=2)
lines(breakeven$date, breakeven$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")
abline(h=2, col="black")


cash = drop_na(fredr("MMMFFAQ027S", observation_start = as.Date("1990-01-01")))
plot(cash$date, cash$value, pch=16, col='blue',
     xlab="Date", ylab="Millions ($)",
     main="Total Assets in Money Market Funds")
grid(lw=2)
lines(cash$date, cash$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")


libor = drop_na(fredr("USD3MTD156N", observation_start = as.Date("1990-01-01")))
plot(libor$date, libor$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="LIBOR and Other Important Benchmark Rates")
grid(lw=2)
lines(libor$date, libor$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")
lines(fedfunds$date, fedfunds$value, col = 'red', pch=16)
ed = drop_na(fredr("DED3", observation_start = as.Date("1990-01-01")))
lines(ed$date, ed$value, col = "green", pch=16)


wilshire5000 = drop_na(fredr("WILL5000INDFC", observation_start = as.Date("1990-01-01")))
plot(wilshire5000$date, wilshire5000$value, pch=16, col='blue',
     xlab="Date", ylab="Index",
     main="Wilshire 5000")
grid(lw=2)
lines(wilshire5000$date, wilshire5000$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")


rm(list=ls())
# Real Estate?
data = read.csv('nyc.csv')
names = c("geo", "date", "rent", "vacancy", "cap_rate", "gross_rent")
colnames(data) = names


attach(data)
plot(date, rent, col="darkblue", main="NYC Office Rent Index (2008 US $)", pch=16, 
     xlab="Date", ylab="Rents", xlim=c(1980, 2019), ylim=c(20, 80))
grid(lw=2)
lines(date, rent, pch=16)
abline(v=2008.3, col="red")


plot(date, vacancy, col="darkred", main="NYC Office Vacancy", pch=16, 
     xlab="Date", ylab="Vacancy Rate (%)", xlim=c(1990, 2019), ylim=c(0, 20))
grid(lw=2)
lines(date, vacancy, col="darkred", pch=16)
abline(v=2008.3, col="red")


plot(date, cap_rate, pch=16, col='blue',
     xlab="Date", ylab="%", xlim=c(2006, 2016), ylim=c(3, 8),
     main="NYC Office Cap Rates (%)")
grid(lw=2)
lines(date, cap_rate, col='blue')
abline(v=2008.3, col="red")
detach(data)
rm(list=ls())







# Volatility (a measure of risk) with Amazon, NASDAQ and S&P
getSymbols(c('AMZN','^IXIC'), from="2006-01-01")
AMZN = AMZN$AMZN.Adjusted
NASDAQ = IXIC$IXIC.Adjusted
plot(AMZN, main="AMZN ($/Share)", col = "darkblue")
plot(NASDAQ, main="NASDAQ Index", col = "darkblue")
data = merge(as.zoo(AMZN), as.zoo(NASDAQ))  ## Merge into single time-series dataset
names = c("AMZN", "NASDAQ")
colnames(data) = names


data.level = as.xts(data)  ## Levels 
data.returns = diff(log(data.level), lag=1)  ## Log returns
data.returns = na.omit(data.returns)  ## Dump missing values


data.returns$AMZNvol = sqrt(data.returns$AMZN ** 2)
data.returns$NASDAQvol = sqrt(data.returns$NASDAQ ** 2)
plot(data.returns$AMZNvol, main="AMZN Volatility", col="darkblue")
plot(data.returns$NASDAQvol, main="NASDAQ Volatility", col="darkblue")


getSymbols(c('^GSPC'), from="2006-01-01")
SnP = GSPC$GSPC.Adjusted
plot(SnP, main="S&P 500 Index", col = "darkblue")
SnPVol = sqrt(diff(log(SnP), lag=1) ** 2)
plot(SnPVol, main="S&P 500 Index Volatility", col = "darkblue")








#OTHER RESOURCES

# An CRE Application (thanks to Duke Professor Campbell Harvey)
# Note that his publication examines Gross Domestic Production (GDP) and the yield curve.
# See https://faculty.fuqua.duke.edu/~charvey/Term_structure/Harvey.pdf
rm(list = ls())
recession = fredr('USREC')
yieldcurve = fredr('T10Y3MM')



data = merge(recession, yieldcurve, by.x='date', by.y='date')  ## Merge into single time-series dataset
data = subset(data, select = c(date, value.x, value.y))
names = c("date", "recession", "yieldcurve")
colnames(data) = names
data$yieldcurve = data$yieldcurve * 100
summary(data)


#merged two data sources together, adapted into bps
#will spend more time with this code when talking with individual groups 
#recession is an indicator value (value 1 when US is in recession)


data$yieldcurve.l1 = lag(data$yieldcurve, 1)
#creates a lagged value on the yieldcurve variable



plot(data$date, data$yieldcurve, col="darkblue", main="U.S. Yield Curve (10-Year Minus 3-Month)", pch=16, 
     xlab="Date", ylab="Basis Points", ylim=c(-100, 450))
lines(data$date, data$yieldcurve, col="darkblue")
grid(lw=2)
abline(h=0, col="red")
abline(h=mean(data$yieldcurve), col='darkgreen')

#yield curve running back to early 1980s
#monthly averages, not daily values
#yield curve became slightly negative in 1990 (SL crisis)
#negative in late 1990s/early 2000s (dot com, 9/11 attacks)
#negative in 2007 and after LB bankruptcy (GFC)
#and yield curve is roughly negative now 


#harvey developed a recession predictor
#use case from CS used when classifying spam/not spam

camharvey = lm(recession ~ yieldcurve + yieldcurve.l1, data=data)
#relates whether we're in a recession to today's yield curve and last month's yield curve
#extended linear probability to predicting whether or not we're in recession

stargazer(camharvey, type="text", title="Recession Predictor", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)
#fundamental research - yield curve strong predictor of recession






# In-Class Assignment: Read in GDP Per Capita from the first session.
# Lag the value.  Rename the variables and run the regresion.
# Now you have fit a perfect model.

gdp_pc = fredr('A229RX0A048NBEA') # Grab GDP per capita
attach(gdp_pc) # Attach and plot.
plot(date, value, pch=16, col="darkblue", main="GDP Per Capita (2009 $)", 
     xlab="Date", ylab="GDP Per Capita")  ## time series plot in R  
lines(date, value, col="darkblue")
grid(lw=2)

gdp_pc$lag.gdp_pc = lag(gdp_pc$value)

names = c("date", "id", "gdp_pc", "lag_gdp_pc")
colnames(gdp_pc) = names
perfectmodel = lm('gdp_pc ~ lag_gdp_pc', data = gdp_pc)
stargazer(perfectmodel, type="text", title="Perfect Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

#past explains present well 
#there is no causality in this model at all
#things change slowly over time
#forecasting is less difficult than we think


# Indeed the point is clear here.
# How difficult is it to predict long-term interest rates?
# Another example using interest rates (courtesy of Matt Rader).
threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2000-01-01")))
tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2000-01-01")))

plot(tenyear$date, tenyear$value, col = 'darkblue', pch=16, 
     ylab = "Rate", ylim = c(0, 8), xlab = "Date", 
     main="Daily Interest Rates (2000 to Present)")
lines(tenyear$date, tenyear$value, col = 'darkblue')
grid(lw=2)
lines(threemonth$date, threemonth$value, col = 'red', pch=16)

threemonthdelta = diff(threemonth$value, lag=1)
tenyeardelta = diff(tenyear$value, lag=1)

plot(threemonthdelta, tenyeardelta, 
     xlab="3 Month Yield Changes", ylab="10 Year Yields",
     main="Daily Interest Rate Changes (2000 to Present)", pch=16, col='darkblue')
grid(lw = 2)
abline(lm(tenyeardelta ~ threemonthdelta), col='red')

ols = lm(tenyeardelta ~ threemonthdelta)
stargazer(ols, type="text", title="Interest Rate Results", single.row=TRUE, 
          ci=TRUE, ci.level=0.95)

#null is that there's no relationship
#there's obviously a positive relationship
#10 bps move in short term rate leads to 2.3 bps move in long term rate
#model where we can say, reject null hypothesis
#movements in the three month lead to movements in 10 year
#r squared metric is 4%
#interest rate movements are really difficult to explain
