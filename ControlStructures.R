# Illustrates control strucutres for programming flow
# 20 March 2018
# NJG

# basic if statements

z <- signif(runif(1),digits=2)
print(z)
z > 0.5 # spits out TRUE
if(z > 0.5) cat(z, "is a bigger than average number","\n")

if (z > 0.8) cat(z, "is a bigger than average number","\n") else
  if (z < 0.2) cat(z, "is a smaller than average number","\n") else
  {cat(z,"is a number of typical size","\n")
    cat("z^2 =",z^2,"\n")}

# Put block statements into functions before using with an if structure
# "Condition" in if statement returns only a single true or false value!

z <- 1:10

if (z > 7) print(z) # nothing happens
if (z < 7) print(z) # not what we want (prints the whole vector because the first and only value it looks at is the  first one)
# Use subsetting!
print(z[z<7])

# insect clutch size poisson with lambda =  10.2
# parasitism probablility = 0.35 with 0 eggs laid

tester <- runif(1000) # random uniform
eggs <- ifelse(tester > 0.35, rpois(n=1000,lambda=10.2),0)
hist(eggs)

# Use to create vector of states for plotting

pVals <- runif(1000)
z <- ifelse(pVals <= 0.025, "lowerTail","nonSig")
z[pVals>=0.975] <- "upperTail"
head(z)
table(z)

# alternative method
z1 <- rep("nonSig",length(pVals))
z1[pVals <= 0.025] <- "lowerTail"
z1[pVals >= 0.975] <- "upperTail"
table(z1)

# simple for loops

myDat <- signif(runif(10),digits = 2)

# for ( i in 1:length(myDat)) # not so good
for (i in seq_along(myDat)) {
  cat("loop number =",i,"vector element = ", myDat[i], "\n")
}
print(i)

# use a constant to define the lenght of the loop

zz <- 50
myDat <- signif(runif(zz),digits=2)
for (i in seq_len(zz)){
  cat("loop number =",i,"vector element = ", myDat[i], "\n")
}

# This allows you to not start at one
for (i in 2:5){
  cat("loop number =",i,"vector element = ", myDat[i], "\n")
}

zz2 <- 4:6
for (i in zz2){
  cat("loop number =",i,"vector element = ", myDat[i], "\n")
}

# don't do anything in the for loop unless you have to!
myDat <- vector(mode = "numeric", length=10)

for (i in seq_along(myDat)) {
  myDat[i] <- signif(runif(1),digits=2)
  # don't do this in a loop! (above)
  cat("loop number =",i,"vector element =",myDat[i],"\n")
}

# don't change object dimension in loop
# functions c, cbind, rbind, list change the dimenstions in the loop


myDat <- runif(1)

# loop from second element to number 10
for (i in 2:10) {
  temp <- signif(runif(1),digits=2)
  myDat <- c(myDat,temp)
  cat("loop number =",i,"vector element =",myDat[i],"\n")
}

# do not write a loop if you can vectorize

myDat=1:10
for(i in seq_along(myDat)){
  myDat[i]=myDat[i] + myDat[i]^2
  cat("loop number =",i,"vector element =",myDat[i],"\n")
}
z=1:10
z=z + z^2

z=c(10, 2, 4)
for(i in seq_along(z)){
  cat("i=", i, "z[i]=", z[i], "\n")
}

# counter variable retains its final value
i

# use next to slip elements in the loop
# only operate on odd-numbered elements
# when remainder is 0 or even the code after next is skipped.
z=1:20
zstore=rep(NA, length=length(z))
for(i in seq_along(z)){
  if( i %% 2==0)next
  print(i)
  zstore[i]=i
}
zstore=zstore[complete.cases(zstore)]

z=1:20
zstore=NULL
zsub=z[z %% 2!=0]
length(zsub)
for ( i in seq_along(zsub)){
  zstore[i]=zsub[i]
  cat("i=", i, "zsub[i]=",zsub[i], "\n")
}

