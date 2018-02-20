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
  z<-qplot(x=factor(xData), color=I("black"), fill=I(fColor), xlab="X")
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
x=rbinom(n=1000, size=400, prob = 0.75)
iHisto(xData = x)


# poisson
# range from 0 to infinity
# constant rate process
# lambda- avg number of events or hits within the range
z<-rpois(n=1000, lambda = 1)
iHisto(z)
mean(z==0)

# negative binomial
# range from 0 to infinity
# number of failures until we get to a certain number of successes.
# more variation than a simple poisson distribution
# e.g. want 2 heads. how many tails do we have until we reach the success.
# two heads sequentially within first to throws= 0 failures 
# tails head head = 1 failure
# heads tail head = 1 failure
# n = number of replicates 
# size= number of trials 
# prob = probability of success with 1 trial
# 12 failures before we got 2 heads is much lower than 0 and 1 failures 
z<-rnbinom(n=1000, size=2, p=0.5)
iHisto(z)

# alternatively call with mu = mean rate
# size= index of overdispersion
# small size = high dispersion 
# use this to make a very skewed distribution
# smaller values less than 1 lead to a more skewed right tail
z<-rnbinom(n=1000, mu=1.1, size=0.7)
iHisto(z)

# number of trials equal to 1
z<-rnbinom(n=1000, size = 1, prob = 0.05)
iHisto(z)
# geometric series as a result. 
# 95% of the trials we will have to toss again to have a success.

z<-rnbinom(n=1000, size = 1, prob = 0.95)
iHisto(z)
# 95% of the time we will have a success on the first trial.
# 5% we will have to toss again
# and of that last 5% we will have to toss a second time

# multinomial distribution
# in binomial---only 2 outcomes
# multiple "balls in urns"
# each ball will land in one of the urns and each urns has a different probability 
# associated with it. 
# size is the number of balls tossed at the urns
# probabilities is vector of the number of urns. set of positive numbers
# system rescales the probs to equal 1 if they do not 
# n is the number of sets we repeat this. each set =size number
z<- rmultinom(n=1000, size=20, prob=c(0.2, 0.7, 0.1))
z
rowSums(z) # match the probs we gave each urn (*x2 because of 20)
rowMeans(z) # sample size by total probs to observed counts

# creating multinomial with sample function

z<-sample(x=LETTERS[1:3], size=20, prob=c(0.2, 0.7, 0.1), replace=TRUE)
z
table(z)


#####continuous variables

# uniform 
# no prior information in the system
# the most null case
# equal probable distribution across range specified by min and max
z<- runif(n=1000, min=3, max=10.2)
Histo(z)

# normal distribution
# unbounded 
# symmetric distribution if enough sampling
# problematic because we often do not have negative values when we record data
# e.g. mass, transfer distance.

z<- rnorm(n=1000, mean =2.2, sd=6)
Histo(z)

# gamma distribution 
# dist. of waiting times for failure
# similar to negative binomial but for continuous vars.
# bounded at 0 (no negative values)
# shape and scale parms
# together determine what the mean and variance of the data look like
# mean= shape * scale
# variance= shape * scale ^2

z<-rgamma(n=1000, shape=1, scale=10)
Histo(z)

# beta distribution
# bounded between 0 and 1
# change boundary by add or multiplying final vector 
# conjugate prior for a binomial distribution 
# previous expectations of what we should be getting for our parms


# binomal
# begin with underlying prob (parm that goes into the binomial)
# generate number of successes and failures in a sample

# we would like to go in the other direction. we have the number of successes and
# failures in the data. we would like to understand the probability of getting a 
# success or failure
# frequentist: prob = number of success/ (success + failure)
# problem at small sample sizes. doesn't accurately show uncertainty in data
# think about flipping coin 1, 10, and 1000 times. better estimate with increase sample

# beta
# shape1= number of successes + 1
# shape2= number of failures + 1
# update the estiamte of the actual parm (prob) based on the number we have

# e.g. zero data and try to make inference about what dist of p values look like
# prob is the value between 0 and 1
# equal prob that the value is anywhere between 0 and 1

# 0 successes and 0 failures
z<-rbeta(n=1000, shape1=1, shape2 = 1)
Histo(z) # uniform distribution 
# x axis is not the number of success. estimate of what the underlying prob is
# we don't know what the underlying prob is

# 1 toss
# frequentist: toss 1- prob is 1/10

# i don't think the total number of tosses actually should match the shape parms
# but the ideas here still stand. 

# 1 head and 0 failures
z<-rbeta(n=1000, shape1=2, shape2 = 1)
Histo(z)
# more prob mass is at the higher end. just a little bit of data. 
# we are going to bet that it is going to be heads rather than tails

# say we do two tosses. 1 head and 1 tail
z<-rbeta(n=1000, shape1=2, shape2 = 2)
Histo(z)
# intuition here is that the probs should be around 0.5. 
# dist. is no longer uniform because we have more data
# uniform was between 0 and 1 with a mean of 0.5 but doesnt tell us anything

# say we do twenty tosses. 10 head and 10 tail
z<-rbeta(n=1000, shape1=10, shape2 = 10)
Histo(z)
# the more information we have the more the dist is shrinking towards the center 
# of the data 
# narrowing on a prob estimate

# say we do 110 tosses.
z<-rbeta(n=1000, shape1=100, shape2 = 10)
Histo(z)
# shift of the distribution towards the right because we have more heads than tails
# out of 110 tosses.

z<-rbeta(n=1000, shape1=0.1, shape2 = 0.3)
Histo(z)

# max like estimation in r

x<- rnorm(n=1000, mean=92.5, sd=2.5)
Histo(x)

# this function takes the data (vector x) and what dist what you want to fit the data to

library(MASS)
zFit<-fitdistr(x, "normal")
str(zFit)
zFit$estimate
# the most likely means and sd are in the estimate
# trivial because they very closely match the mean and sd that we assigned in x

# now fit gamma distribution to data
zFit<-fitdistr(x, "gamma")
zFit$estimate
zNew<-rgamma(n=1000, shape=1461, rate=15.8)
Histo(zNew)

summary(x)
# min and max values in a data set and the min and max like estimates in a uniform

z<-runif(n=1000, min=84.57, max=100.51)
Histo(z)
