# illustrated break function with a program for a random walk

library(ggplot2)
library(tcltk)


############################################################
# FUNCTION: RanWalk
# stochastic random walk
# input: times: number of time steps
# n1= initial population size= n[1]
# lambda = finite rate of increase
# noise sd= sd of a normal distribution with a mean of 0
# output: vector n with pop sizes > 0
# ----------------------------------------------------------
RanWalk= function (times=100,
                    n1=50,
                    lambda=1.0,
                    noiseSD=10){
  n=rep(NA, times)
  n[1]=n1
  noise=rnorm(n=times, mean=0, sd=noiseSD)
  for(i in 1:(times-1)) {
    n[i + 1]= n[i]*lambda + noise[i]
   #n[i+1]= n[1] + noise[i] # not a random walk. just adding noise to each 
  # value. not taking steps in a "meaningful" way
  if(n[i +1] <=0) {
    n[i +1]=NA # reset to NA
    cat("popualtion extinction at time", i, "\n")
    tkbell() # ring the bell on my computer when pop is going extinct
    break }
  }
  n=n[complete.cases(n)]
  return(n)
}

z=RanWalk(lambda = 1.1)
qplot(x=seq_along(z), y=z, geom=c("line", "point"))
