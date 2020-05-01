
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

#The Yield Curve tends to invert when we are in a recession or before a recession
yieldcurve = fredr('T10Y3MM')

#Credit spreads tend to spike before we enter a recession 
bbbspreads = fredr('BAMLC0A4CBBB')
#Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity
baaspread = fredr(series_id = "BAA10Y")

#University of Michigan: Consumer Sentiment declines before / while we are in a recession
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

# linear probability model option:

# The Linear Probability Model From Class 
# The linear algorithm is a very powerful tool with many applications.
# One of its core strengths is the ease with which we can interpret its results.
# Let's use another application.
rm(list = ls())
admit = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
attach(admit)
summary(admit)
head(admit)

#admit is an indicator variable 
#1 indicates they did, 0 indicates they did not


# Notes:
# Admit is categorical: 0 is "Applicant Not Admitted", 1 is "Applicant Is Admitted".
# GRE is continuous measure of the applicant's Graduate Record Exam score.
# GPA is continuous measure of the applicant's Grade Point Average.
# Rank is the categorical ranking of the school the applicant attended as an undergraduate.

admit$rank = as.factor(rank) # Designate Rank as a categorical for R.
model = lm('admit ~ gre + gpa + rank', data = admit)

#estimates a simple linear model

stargazer(model, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)

#within stargazer, we can interpret results in the same manner as we interpreted results from linear model looking at cap rates


# Notes:
# Let's interpret the results from the linear probability model.
# GRE: Graduate Record Examine: a one point increase in GRE increases probability of admission by 0.0004 or 0.04%
# GPA: Grade Point Average: a one point increase in GPA increases probability of admission by 0.156 or 15.6%.
# rank: Going to a tier 2 school reduces probabilty of admission by 0.1624 or 16.24% (relative to a tier 1 school).


model$coefficients #R stores results from a model in coefficients, combines model with coefficients

mean(gre)
mean(gpa)

model$coefficients[1] + model$coefficients[6] + model$coefficients[2] * mean(gre) +
        model$coefficients[3] * mean(gpa)

# What about a tier 3 school?
model$coefficients[1] + model$coefficients[5] + model$coefficients[2] * mean(gre) +
        model$coefficients[3] * mean(gpa)

# What about a tier 2 school?
model$coefficients[1] + model$coefficients[4] + model$coefficients[2] * mean(gre) +
        model$coefficients[3] * mean(gpa)

# What about a tier 1 school?
model$coefficients[1] + model$coefficients[2] * mean(gre) +
        model$coefficients[3] * mean(gpa)

#if you look at results from stargazer, can see that the measures relative to rank 1 measure bears out
#new use case: linear probability model
#prediction versus evaluating impact of an intervention


# other model considerations
# Random Forest Classifier?

#notes from Class 8
# The Random Forecast Classifier
# See Jupyter Notes

# An example using our usual suspect.
# Create momentum factors.
# quantmod creates datasets as time series objects.
# We must manipulate them to create dataframes.  
rm(list = ls())
getSymbols(c('AAPL'), from="2006-01-01", to="2017-12-31")
AAPL$OmC = (AAPL$AAPL.Open - AAPL$AAPL.Close) / AAPL$AAPL.Close
AAPL$HmL = (AAPL$AAPL.High - AAPL$AAPL.Low) / AAPL$AAPL.Low
AAPL$volume = AAPL$AAPL.Volume / 1000000000
AAPL$returns = diff(log(AAPL$AAPL.Adjusted), lag=1)
AAPL$gain = 2
AAPL = na.omit(AAPL)



aapl = fortify(AAPL)
aapl = subset.data.frame(aapl, select=c("OmC", "HmL", "volume", "returns", "gain"))
aapl = mutate(aapl, gain = ifelse(returns > sd(returns), 3, gain), 
               gain = ifelse(returns < -sd(returns),1, gain))
aapl$gain = factor(aapl$gain)
aapl = subset.data.frame(aapl, select=c("OmC", "HmL", "volume", "gain"))



# Create train and test data by converting to standard dataframe
set.seed(04142020)
sample = sample.split(aapl, SplitRatio = 0.80)
train = subset(aapl, sample == TRUE)
test = subset(aapl, sample == FALSE)

#evaluate how well the random forest algorithm performs on the test set

train.data = subset.data.frame(train, select=c("OmC", "HmL", "volume"))
train.target = subset.data.frame(train, select=c("gain"))
test.data = subset.data.frame(test, select=c("OmC", "HmL", "volume"))
test.target = subset.data.frame(test, select=c("gain"))

#target is an indicator variable of extreme loss or gain


# Summary statistics in training and test sets.
summary(train)
summary(test)

#because we've done randomization, summary statistics look similar in each set
#deploying random forest algorithm is installed with one of hte earlier libraries


rf = randomForest(gain ~ ., data=train, ntree=1000, treesize=100)
print(rf) # Note that this is the confusion matrix in the training set.
prediction = predict(rf, newdata=test.data)

#gain is equalt to . where dot just means everything on the right hand side
#explaining gain using momentum factors
#tree size here is rule of thumb we talked about before
#print gives us a confusion matrix and an error rate (1-accuracy rate)
#in this world, on diagonal elements are things correctly predicted within the sample
#could classify 154 extremely loss days, 158 extreme gain days
#38% of the time, it is misclassifying the 1s as a 2 or a 3
#can't talk about true positive and false positive because we're dealing with a multiclass problem
#now we want to deploy the algorithm on the test set (data it has not seen)

# Create confusion matrix in the test set 
  # This compares prediction from the fitted Random Forest algorithm to actual outcomes.
  # The machine does not know the test set, so this is equivalent to out of sample (OOS).
cm = as.matrix(table(prediction, test$gain))
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
sumdiag = sum(diag) # sum of correct predictions
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
class.error = 1 - diag / rowsums
acc = sumdiag / n # accuracy

#need to figure out why this isn't workign 
#if we set a discriminant threshold of 50%, accuracy is 68%

# In Sample
print(rf)

#this is the in sample confusion matrix - not very useful
#look at performance of algorithm as a predictor


# Out of Sample
cm 
class.error
acc

#confusion matrix with classification errors
#and accuracy
#things on off diagonal elements are the misclassifications
#error rates are 30%, 10% and 30%
#overall accuracy of the algorithm is 86%


# Notes:
# There is the confusion matrix for both the training and test sets.
# I largely ignore the confusion matrix for the training set.
# I focus on the confusion matrix for the test set.
# Consider random guessing as a benchmark: 
  # Class error would be ~2 out of 3 or ~67%.
  # Accuracy would be ~1 out of 3 or ~33%.
# This is why we deploy algorithms rather than guessing.



# Logistic Regression - Questions how to read in the data to the model and use it with multiple variables? 

# Logistic Regression: Where CS and CRE merge.
# CS had a key insight: train/test split.
# Probability of default (PD) models in MBS and other securitizations.
# Almost any problem in which one has two discrete outcomes.
# Start with email spam

# In machine learning, we split data into a training set and a test set.
# In this example, I will use an 80% / 20% train / test split.


rm(list = ls())
target = "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
spam = read.csv(target, header = FALSE)
library(readr)
spam = read.csv("spambase.data", header = FALSE)
names(spam)[58] = c("spam")



set.seed(04142020)
sample = sample.split(spam, SplitRatio = 0.80)
train = subset(spam, sample == TRUE)
test = subset(spam, sample == FALSE)



# Summary statistics on the train and test dataframes.
summary(train$spam)
summary(test$spam)



logit = glm(spam ~ V1 + V2 + V3 + V4 + V5, data = train, family = "binomial")
stargazer(logit, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)


# Fit the logit on the training set.  Apply the fitted logit to the test set.
# Evaluation.
test$spamhat = predict(logit, newdata = test, type = "response")



rm(list = ls())
target = "https://archive.ics.uci.edu/ml/machine-learning-databases/spambase/spambase.data"
spam = read.csv(target, header = FALSE)
library(readr)
spam = read.csv("spambase.data", header = FALSE)
names(spam)[58] = c("spam")



sentiment = fredr(series_id="UMCSENT")
summary(sentiment)

set.seed(04142020)
sample = sample.split(sentiment, SplitRatio = 0.80)
train = subset(sentiment, sample == TRUE)
test = subset(sentiment, sample == FALSE)



# Summary statistics on the train and test dataframes.
summary(train$sentiment)
summary(test$sentiment)



logit = glm(sentiment ~ V1 + V2 + V3 + V4 + V5, data = train, family = "binomial")
stargazer(logit, type="text", title="Model", single.row=TRUE, 
          ci=TRUE, ci.level=0.95, digits = 4)


# Fit the logit on the training set.  Apply the fitted logit to the test set.
# Evaluation.
test$spamhat = predict(logit, newdata = test, type = "response")




