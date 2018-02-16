# probability simulations in r
# feb 15, 2018
# bmv

library(ggplot2)
testData=runif(1000)
qplot(x=testData)

#----------
# function histo
# better histogram plot
# input xData= any kind of numeric vector
# input fColor= fill color
# 2 inputs that are going to be provided by the user
# output = corrected ggplot histogram
# summary statistics
# output = 95% interval (what range of x values encompass 95% of the mass)

# start function with the inputs and set default values. can change when
# the function is called. 
# the open curly bracket is the start of the function
# result of the function has to go into another object, here called z
# I sets the identity of a variable...need to look into this more.
# color here is the outside color of the bars... not the fill.
# boundary here fixes the little bars at 0 and 1. can use xlim in other functions
# to set the limit from 0-1. 

Histo <- function(xData=runif(1000), fColor="indianred") { 
  z<-qplot(x=xData, color=I("black"), fill=I(fColor), xlab="X", boundary=0)
print(z)
print(summary(xData))
print(quantile(xData, probs=c(0.025, 0.975))) } # 95% of the area under the curve (CI)
# not 95% chance that the mean falls within the interval===wrong
# if you did the run 100 times, 95 times the mean would fall between the those quantiles

#------
#Function iHisto (integer histo that is discrete)
# input xData = a vector of integers
# input fColor = named color
# output = histogram of integters
# output = summary of xData
# summary statistics

iHisto <- function(xData=runif(1000), fColor="indianred") { 
  z<-qplot(x=factor(xData), color=I("black"), fill=I(fColor), xlab="X", boundary=0)
  print(z)
  print(summary(xData))
  print(quantile(xData, probs=c(0.025, 0.975))) } 

# test
Histo()
temp=rnorm(1000)
Histo(xData = temp, fColor="goldenrod")
test=rgamma(1000, shape=4, rate = 0.5)
Histo(xData = test)


# poison requires 2 parms, remember lambda is the rate. count data. rare events
# think of bombs falling during a blitz within a fixed window, say 1 week in london
# rate is largely going to determine the probability of the same place getting bombed
# poison bounded at 0, but no real upper boundary, though adding on really high
# values after a certain value is unlikely.
temp2=rpois(n=1000, lambda=0.5)
#Histo(temp2) # unnatural binning process
iHisto(xData = temp2)
mean(temp2==0) # coerce boolean to 1=TRUE and 0=FALSE, then take mean of ones/1000


# poison doesn't have a lot of variability 
# there are other discrete distributions with > variability

# binomial
# integer from 0 to number of trials
# parm size= number of trials
# parm p= prob of success (e.g. tossing a coin =0.5)

# do 40 trails, and on each trial, the coin has a prob of being heads 75%
# generate 1000 numbers ranging from 0-40 with each representing the number of times
# that we got heads.
# max: on one trial we got 38/40 heads.
# min: least number of heads obtained out of 40 is 21.
x=rbinom(n=1000, size=40, prob = 0.75)
iHisto(xData = x)


