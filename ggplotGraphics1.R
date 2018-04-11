# ggplot graphics
# April 5, 2018
# EAT

setwd("~/Documents/UVM_2018/FirstProject")

# Preliminaries
library(ggplot2)
install.packages("ggthemes")
library(ggthemes)
library(patchwork)
library(TeachingDemos)
char2seed("10th Avenue Freeze-Out")
d <- mpg
str(d)

# qplots for use while coding (don't require data to be in a data frame)

# Basic histogram
qplot(x=d$hwy)
qplot(x=d$hwy,fill=I("khaki"),color=I("black"))

# density plot
qplot(x=d$hwy,geom="density")

# basic scatterplot
qplot(x=d$displ,y=d$hwy,geom=c("smooth","point"))
qplot(x=d$displ,y=d$hwy,geom=c("smooth","point"),method="lm")

# basic boxplot
qplot(x=d$fl,y=d$cty,geom="boxplot",fill=I("green"))

# basic barplot
qplot(x=d$fl,geom="bar",fill=I("green"))

# not what you expect (setting vs mapping a variable)
qplot(x=d$fl,geom="bar",fill="green")

# plotting curves and functions
myVec <- seq(1,100, by=0.1)
myFun <- function(x) sin(x) + 0.1*x

# plot built in functions
qplot(x=myVec,y=sin(myVec),geom="line")

# plot density distributions for probability functions
qplot(x=myVec,y=dgamma(myVec,shape=5,scale=3),geom="line")

# plot user-defined functions
qplot(x=myVec,y=myFun(myVec),geom = "line")

# ------------------------------------------------------------
p1 <- ggplot(data=d, mapping = aes(x=displ,y=cty)) + geom_point()
print(p1)

p1 + theme_classic() # no grid lines
p1 + theme_linedraw() # black frame 
p1 + theme_dark() # good with brigh points (good for slides)
p1 + theme_base() # looks like base R
p1 + theme_par() # uses current par setting
p1 + theme_void() # only data
p1 + theme_solarized() # good colors
p1 + theme_economist() # many specialized themes
p1 + theme_grey() # base theme/graphic for ggplot

# use theme parameters to modify font and font size
p1 + theme_classic(base_size = 30,base_family = "serif")

p2 <- ggplot(data=d,mapping=aes(x=fl,fill=fl)) + geom_bar() # the fill fl gives you a color for each of the different values in the fl column
print(p2)
# flip the two coordinate axis
p2 + coord_flip() + theme_gray(base_size = 20,base_family = "Courier")

# minor theme modifications
p1 <- ggplot(data = d,mapping = aes(x=displ,y=cty)) + geom_point(size=5,shape=21,color="black",fill="coral") + ggtitle("My graph title here") + xlab("My x axis label") + ylab("My y axis label") + xlim(0,4) + ylim(0,20)
print(p1)

# create four individual graphs
g1 <- ggplot(data=d,mapping = aes(x=displ,y=cty)) + 
  geom_point() + 
  geom_smooth()
print(g1)

# second graph
g2 <- ggplot(data = d,mapping = aes(x=fl,fill=I("tomato"), color=I("black"))) + 
  geom_bar(stat="count") + 
  theme(legend.position = "none")
print(g2)
