# plotting functions, sweeping parms
# 03.29.2018
# BMV

# use differing numbers of matrix rows and columns
z=matrix(runif(9), nrow=3)
z[3,3]
z=matrix(runif(20), nrow=5)
z[5,4]
z[4,5]

# using double for loops
m=matrix(round(runif(20), digits=2), nrow=5)
print(m)

# loop over rows
for(i in 1:nrow(m)){
  m[i,]= m[i,] + i
}

print(m)

# loop by cols
m=matrix(round(runif(20), digits=2), nrow=5)
print(m)

for(j in 1:ncol(m)){
  m[,j]=m[,j] + j
}

print(m)

# double loop, over rows and cols
m=matrix(round(runif(20), digits=2), nrow=5)
print(m)

for(i in 1:nrow(m)){
  # insert code here if you want to only modify the rows before moving on
  # to the cols
  for(j in 1:ncol(m)){
    m[i,j]= m[i,j] + i + j
  }
}

print(m)

# sweeping over parms in an equation
# S = cA^ z (the number of species on an island, size of an island)

############################################################
# FUNCTION: SpeciesAreaCurve
# creates power function for relationship of S and A
# input: A= vector of islands areas (x axis on graph)
#        C= intercept constant
#        z= slope constant
# output: S= vector of species richness values (y axis on graph)
# ----------------------------------------------------------
SpeciesAreaCurve= function (A=1:5000,
                    z=0.5,
                      c=0.26){
  S = c*(A^z)
  return(S)
}
# head(SpeciesAreaCurve())
############################################################
# FUNCTION: SpeciesAreaPlot
# plot curve in base graphs
# input: A= vector of islands areas (x)
#        c= intercept
#        z= slope 
# output: base graph with parm values
# ----------------------------------------------------------
SpeciesAreaPlot= function (A=1:5000,
                            z=0.5,
                            c=0.26){
  plot(x=A, y=SpeciesAreaCurve(A,c,z),
       type = "l",
       xlab="Island size",
       ylab="S",
       ylim =c(0,2500))
  mtext(paste("c=", c, "z=", z), cex=0.7)
return()
  }
SpeciesAreaPlot()


# now build a grid of plots
# each with different parm values
# global variables

cParms=c(100, 150, 175)
zParms=c(0.10, 0.16, 0.26, 0.3)
par(mfrow=c(3,4))

# enter into a double loop for plotting
for(i in 1:length(cParms)){
  for(j in 1:length(zParms)){
    SpeciesAreaPlot(c=cParms[i], z=zParms[j])
  }
}

par(mfrow=c(1,1))

# plotting redux with ggplot
library(ggplot2)

cParms=c(100, 150, 175)
zParms=c(0.10, 0.16, 0.26, 0.3)
Area=1:5

# set up model frame
modelFrame= expand.grid(c=cParms, z=zParms, A=Area)
modelFrame$S=NA
head(modelFrame)

# tricky double for loop for filling new data frame
for (i in 1:length(cParms)) {
  for (j in 1:length(zParms)) {
    modelFrame[modelFrame$c==cParms[i] & modelFrame$z==zParms[j], "S"] = 
      SpeciesAreaCurve(A=Area, c=cParms[i], z=zParms[j])
  }
}

p1=ggplot(data=modelFrame)
p1 + geom_line(mapping=aes(x=A, y=S)) + facet_grid(c~z) + theme_linedraw()

p2=p1
p2 + geom_line(mapping=aes(x=A, y=S, group=z)) + facet_grid(.~c) + theme_linedraw()
