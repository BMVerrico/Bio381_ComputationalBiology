# ggplot graphics
# 04.12.2018

library(ggplot2)
library(patchwork)
#library(TeachingDemos)
#library(ggthemes)
#char2seed("Sienna")
data('mtcars')

d=mpg

# standard plot with all of the data
p1=ggplot(data=d, mapping=aes(x=displ, y=hwy)) +
  geom_point() + geom_smooth()
print(p1)

# break out the drive types
p1=ggplot(data=d, mapping=aes(x=displ, y=hwy, group=drv, fill=drv)) +
  geom_point() + geom_smooth()
print(p1)

# see effect of color and what changes.
# color maps to points and lines not but to other objects (e.g. ci)
p1=ggplot(data=d, mapping=aes(x=displ, y=hwy, color=drv)) +
  geom_point() + geom_smooth()
print(p1)

p1=ggplot(data=d, mapping=aes(x=displ, y=hwy, group=drv, fill=drv, color=drv)) +
  geom_point() + geom_smooth()
print(p1)

# map aes in the geoms called. this will override the calls in the first aes block
# e.g. highlight only one of the groups and keep other elements the same
# subset the data frame within the ggplot code
# this only adds the points when the drv = 4
p1=ggplot(data=d, mapping=aes(x=displ, y=hwy, color=drv)) +
  geom_point(data=d[d$drv=="4",]) + geom_smooth()
print(p1)

# instead of subsetting, just map the aes
# map points based on group drv, but smooth is based on all?
p1=ggplot(data=d, mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=drv)) + geom_smooth()
print(p1)

p1=ggplot(data=d, mapping=aes(x=displ, y=hwy)) +
  geom_point() + geom_smooth(mapping=aes(color=drv))
print(p1)

# do not show all of the data
# subset in the first layer to eliminate some data entirely
p1=ggplot(data=d[d$drv!="4",], mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=drv)) + geom_smooth()
print(p1)

# geoms have additional attributes that can be set,
# in addition to any aes that are mapped

p1=ggplot(data=d[d$drv!="4",], mapping=aes(x=displ, y=hwy)) +
  geom_point(mapping=aes(color=drv), size=1) + 
  geom_smooth(color="black", size=2, fill="gold", method="lm") +
  scale_color_manual(values=c("red", "blue"))
print(p1)

# use var plots for categoric variables
table(d$drv)
p1=ggplot(data=d, mapping = aes(x=drv)) +
  geom_bar(color="black", fill="goldenrod")
print(p1)

# same as above. the stat_count is actually creating the stat var that
# we are plotting. 
p1=ggplot(data=d, mapping = aes(x=drv)) +
  stat_count(color="black", fill="goldenrod")
print(p1)

# get the proportion
p1=ggplot(data=d, mapping = aes(x=drv, y=..prop.., group=1)) +
  stat_count(color="black", fill="goldenrod")
print(p1)

# aesthetic mapping for multiple groups of bars
p1=ggplot(data=d, mapping = aes(x=drv, fill=fl)) +
  stat_count()
print(p1)

# unstack the bars
p1=ggplot(data=d, mapping = aes(x=drv, fill=fl)) +
  stat_count(position = "identity")
print(p1)

# change the transparency with alpha
p1=ggplot(data=d, mapping = aes(x=drv, fill=fl)) +
  stat_count(alpha=0.6, position = "identity")
print(p1)

# make the bar transparent
p1=ggplot(data=d, mapping = aes(x=drv, fill=fl, color=fl)) +
  stat_count(fill=NA, position = "identity")
print(p1)

# use position=fill for stacking but with equivalent height
p1=ggplot(data=d, mapping = aes(x=drv, fill=fl)) +
  stat_count(position = "fill")
print(p1)

#multiple bars
p1=ggplot(data=d, mapping = aes(x=drv, fill=fl)) +
  stat_count(position = "dodge")
print(p1)

# more typical bar plot for mean values of a continuous variable
dTiny=tapply(X=d$hwy, INDEX=as.factor(d$fl), FUN=mean)
# calculate group means for each hwy type
# put in data frame
dTiny=data.frame(hwy=dTiny)
dTiny=cbind(fl=row.names(dTiny),dTiny)

p2=ggplot(data=dTiny, mapping=aes(x=fl, y=hwy, fill=fl))+ 
  geom_bar(stat="identity")
print(p2)

# use the stats geom to create the classic barplot
p1=ggplot(data=d, mapping=aes(x=fl, y=hwy)) + 
  stat_summary(fun.y=mean, 
               fun.ymin = function(x) mean(x)-sd(x),
               fun.ymax = function(x) mean(x) + sd(x)) 
# give a point 1 sd below and above the mean
# the points are the means
print(p1)

# now put the two plots together
p1=ggplot(data=d, mapping=aes(x=fl, y=hwy)) + 
  stat_summary(fun.y=mean, 
               fun.ymin = function(x) mean(x)-sd(x),
               fun.ymax = function(x) mean(x) + sd(x)) +
  geom_bar(data=dTiny, mapping=aes(x=fl, fill=fl, alpha=0.6), stat="identity")
print(p1)

# use a boxplot instead!
p1=ggplot(data=d, mapping=aes(x=fl, y=hwy)) + geom_boxplot(fill="thistle") +
  geom_point(position=position_jitter(width=0.2,height=0.7),
             color="grey60", alpha=0.6)
print(p1)
