# Randomization test for regression data
# April 3, 2018
# EAT

setwd("~/Documents/UVM_2018/FirstProject")
# preliminaries
library("ggplot2")
library("TeachingDemos")
char2seed("Cruel April")

###################################
# Function: readData
# read in or generate data frame
# input: file name (or nothing for demo)
# output: 3-column data frame of observed data (ID, x, y)
#---------------------------------
readData <- function(z=NULL) {
  if (is.null(z)) {
    xVar <- 1:20
    yVar <- xVar + 10*rnorm(20)
    dF <- data.frame(ID=seq_along(xVar),xVar,yVar)
  }
  return(dF)
}
readData()


###################################
# Function: getMetric
# calculate metric for randomization test
# input: 3-column data frame for regression
# output: regression slope
#---------------------------------
getMetric <- function(z=NULL) {
  if (is.null(z)) {
    xVar <- 1:20
    yVar <- xVar + 10*rnorm(20)
    z <- data.frame(ID=seq_along(xVar),xVar,yVar)
  }
  . <- lm(z[,3]~z[,2])
  . <- summary(.)
  . <- .$coefficients[2,1]
  slope <- .
  return(slope)
}
getMetric()

###################################
# Function: shuffleData
# randomize data for regression analysis
# input: 3-column data frame (ID, xVar, yVar)
# output: 3-column data frame (ID, xVar, yVar) 
#---------------------------------
shuffleData <- function(z=NULL) {
  if (is.null(z)) {
    xVar <- 1:20
    yVar <- xVar + 10*rnorm(20)
    z <- data.frame(ID=seq_along(xVar),xVar,yVar)
  }
  z[,3] <- sample(z[,3])
  return(z)
}
shuffleData()

###################################
# Function: getPVal
# calculate p value for observed, simulated data
# input: list of observed metric and vector of simulated metric
# output: lower, upper tail probability vector
#---------------------------------
getPVal <- function(z=NULL) {
  if (is.null(z)) {
    z <- list(xObs=runif(1),xSim=runif(1000))
  }
  pLower <- mean(z[[2]] <= z[[1]])
  pUpper <- mean(z[[2]] >= z[[1]])
  return(c(pL=pLower,pU=pUpper))
}
getPVal()

###################################
# Function: plotRanTest
# ggplot graph
# input: list of observed metric and vector of simulated metric
# output: ggplot graph
#---------------------------------
plotRanTest <- function(z=NULL) {
  if (is.null(z)) {
    z <- list(xObs=runif(1),xSim=runif(1000))
  }
  dF <- data.frame(ID=seq_along(z[[2]]),simX=z[[2]])
  p1 <- ggplot(data=dF, mapping = aes(x=simX))
  p1 + geom_histogram(mapping = aes(fill=I("goldenrod"),color=I("black"))) + geom_vline(aes(xintercept=z[[1]],col="blue"))
}
plotRanTest()

# ------------------------------------
# Main body of code
nSim <- 1000 # number of simulations
XSim <- rep(NA, nSim) # will hold simulated slopes

dF <- readData()
Xobs <- getMetric(dF)

for (i in seq_len(nSim)) {
  XSim[i] <- getMetric(shuffleData(dF))
}

slopes <- list(Xobs,XSim)
getPVal(slopes)
plotRanTest(slopes)

