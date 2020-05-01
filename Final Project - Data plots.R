
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


#recession indicator
recession = fredr('USREC')
head(recession)
dim(recession)
tail(recession)


#Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity
baaspread = fredr(series_id = "BAA10Y")


#University of Michigan: Consumer Sentiment
sentiment = fredr(series_id="UMCSENT")
summary(sentiment)

#US U-3 Unemployment Rate
unrate = fredr(series_id = "UNRATE", observation_start = as.Date("1990-01-01"))
plot(unrate$date, unrate$value, col = 'blue', main="US U-3 Unemployment Rate",
     pch=16, ylab = "Rate", xlab = "Date")
grid(lw=2)
lines(unrate$date, unrate$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")


#US Employment Growth Rate
empgrowth = fredr(series_id = "PRS85006012", observation_start = as.Date("1980-01-01"))
plot(empgrowth$date, empgrowth$value, col = 'blue', main="US Employment Growth",
     pch=16, ylab = "Rate", xlab = "Date")
grid(lw=2)
lines(empgrowth$date, empgrowth$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")


#Weekly UI Claims
claims = fredr(series_id = "ICSA", observation_start = as.Date("1980-01-01"))
claims$value = claims$value / 10000000
plot(claims$date, claims$value, col = 'blue', main="Weekly UI Claims",
     pch=16, ylab = "Claims (Millions)", xlab = "Date")
grid(lw=2)
lines(claims$date, claims$value, col = 'blue')
abline(v=as.Date("2008-09-30"), col="red")

#Yield Curve
yield_curve = fredr(series_id = "T10Y3M", observation_start=as.Date('1980-01-01'))
plot(yield_curve$date, yield_curve$value, col = 'blue', main='Yield Curve', 
     pch=16, ylab="%", xlab="Date")
grid(lw=2)
lines(yield_curve$date, yield_curve$value, col = 'blue')
abline(h=0, col='red')

#Ten Year
tenyear = drop_na(fredr("DGS10", observation_start = as.Date("1990-01-01")))
plot(tenyear$date, tenyear$value, pch=16, col='blue',
     xlab="Date", ylab="%", ylim=c(0, 10),
     main="Bernanke's Condundrum")
grid(lw=2)
lines(tenyear$date, tenyear$value, col='blue')
lines(fedfunds$date, fedfunds$value, col = 'red', pch=16)

#Nominal Value of the Fed Balance Sheet
balance = drop_na(fredr("WALCL", observation_start = as.Date("1990-01-01")))
balance$value = balance$value * 1000
plot(balance$date, balance$value, pch=16, col='blue',
     xlab="Date", ylab="Nominal Value of Fed's Balance Sheet",
     main="Powell's Condundrum")
grid(lw=2)
lines(balance$date, balance$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")

#Market Expectations of Inflation in 10 Years
breakeven = drop_na(fredr("T10YIE", observation_start = as.Date("1990-01-01")))
plot(breakeven$date, breakeven$value, pch=16, col='blue',
     xlab="Date", ylab="%",
     main="Market Expectations of Inflation in 10 Years")
grid(lw=2)
lines(breakeven$date, breakeven$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")
abline(h=2, col="black")

#Total Assets in Money Market Funds
cash = drop_na(fredr("MMMFFAQ027S", observation_start = as.Date("1990-01-01")))
plot(cash$date, cash$value, pch=16, col='blue',
     xlab="Date", ylab="Millions ($)",
     main="Total Assets in Money Market Funds")
grid(lw=2)
lines(cash$date, cash$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")

#Libor
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

# bbb spreads
bbbspreads = fredr('BAMLC0A4CBBB')
plot(bbbspreads$date, bbbspreads$value, pch=16, col='blue',
     xlab="Date", ylab="Rate",
     main="BBB Spreads")
grid(lw=2)
lines(wilshire5000$date, wilshire5000$value, col='blue')
abline(v=as.Date("2008-09-30"), col="red")
# spreads are seen everywhere
# aaa bonds relative to junk bonds
# inversions in measures are same as yield curve

# three month US treasury rate
threemonth = drop_na(fredr(series_id = "DGS3MO", observation_start = as.Date("2000-01-01")))

# ten year US treasury rate
tenyear = drop_na(fredr(series_id = "DGS10", observation_start = as.Date("2000-01-01")))


# gdp per capita quarterly
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


#merge the data
data = merge(recession, yield_curve, bbbspreads, by.x='date', by.y='date')  ## Merge into single time-series dataset
data = subset(data, select = c(date, value.x, value.y))
names = c("date", "recession", "yieldcurve", "bbbspreads")
colnames(data) = names
data$yieldcurve = data$yieldcurve * 100
summary(data)


# lagged yield curve
data$yieldcurve.l1 = lag(data$yieldcurve, 1)
