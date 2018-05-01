#prelims

library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)
library(TeachingDemos)
char2seed("Sharpei")

species=5
sites=8
abundanceRange=1:10
mFill=0.4

vec=rep(0, species*sites)
abund=sample(x=abundanceRange, size=round(mFill*length(vec)), replace = TRUE)
vec[seq_along(abund)]=abund
vec
vec=sample(vec)
vec

aMat=matrix(vec, nrow=species)
aMat
rownames(aMat)=rownames(aMat,
                        do.NULL = FALSE,
                        prefix = "Species")
aMat
colnames(aMat)=colnames(aMat,
                        do.NULL = FALSE,
                        prefix = "Site")
aMat

# look at the smallest unit of obs. must be in every row in the data
# e.g. abundance counts. the values in each cell in the matrix.
# each col is then an attribute to that value
# the rows and columns (species and sites) are attributes

# melt from reshape2
# works the best with matrices
. = melt(aMat)
head(.)

# get names formatted correctly
#value.name is the new variable being created. e.g. abundance
. <-melt(aMat, varnames = c("Species, Site"), value.name = "Abundance")
head(.) # didn't work just right

aFrame=data.frame(cbind(Species=rownames(aMat), aMat))
head(aFrame)

# use gather from tidyr for dataframe 
# needs to know which cols we want (do not need to know all)
.=gather(aFrame,Site1:Site8, key="Site", value="Abundance")
head(.)

.$Abundance=as.numeric(.$Abundance)

aFrameL=.

ggplot(aFrameL, aes(x=Site, y=Abundance, fill=Species)) +
  geom_bar(position = "dodge", stat="identity", color="black")

# build a subject x time expt. matrix
Treatment=rep(c("Control", "Treatment"), each=5)
Subject=1:10
T1=rnorm(10)
T2=rnorm(10)
T3=rnorm(10)

eFrame=data.frame(Treatment=Treatment, 
                  Subject=Subject, 
                  T1=T1, 
                  T2=T2, 
                  T3=T3)

head(eFrame)

.=gather(eFrame, T1:T3, key="Time", value="Response")
head(.)
.$Time=as.factor(.$Time)
eFrameL=.
ggplot(eFrameL, aes(x=Treatment, y=Response, fill=Time)) +
  geom_boxplot()

# go the other way

.=dcast(aFrameL, Species ~ Site, value="Abundance")
head(.)

.=spread(aFrameL, key=Site, value=Abundance)
head(.)

.=spread(eFrameL, key=Time, value=Response)
print(.)

# summarize and by_group
# take an entire data frame and creates a new df based on the functions called
# important that you do not recycle variable name. 
as.data.frame(summarize(mpg, ctyM=mean(cty), ctySD=sd(cty)))
. <- group_by(mpg, fl)
head(.)
as.data.frame(summarize(., ctyM=mean(cty), ctySD=sd(cty), n=length(cty)))

# filter 
.=filter(mpg, class!="suv")
.=group_by(., fl, class)
as.data.frame(summarize(., ctyM=mean(cty), ctySD=sd(cty),n=length(cty)))

# replicate(n, expression, simplify)
# n= number of replication
# expression is any r expression or function call
# simplify what we will do with the output
# simplify- default=array with 1 more dimension than the og output
# simplify=TRUE gives a vector or matrix
# simplify=FALSE gives a list of the results

myOut=matrix(data=0, nrow=3, ncol=5)
myOut
for(i in 1:nrow(myOut)){
  for(j in 1:ncol(myOut)) {
    myOut[i,j]=runif(1)
  }
}
print(myOut)

myOut=matrix(data=runif(15), nrow = 3)
myOut

# do not actually write the exp= part
mo=replicate(n=5,
             100 + runif(3),
             simplify = TRUE
             )
mo

mo=replicate(n=5,
             matrix(runif(6),3,2),
             simplify = "array"
)
mo
