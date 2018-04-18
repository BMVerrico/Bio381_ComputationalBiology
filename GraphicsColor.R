# ggplot graphics
# 04.17.2018

library(ggplot2)
library(ggthemes)
library(wesanderson)
library(TeachingDemos)
char2seed("short capo")

# colors in graphics
# aesthetics
  # large geom (fills) -- pale colors
  # small geom (points, lines) -- brighter colors
  # dichromat() -- visible with people with RG color blindness
  # emphasize or de-emphasize graphic elements
    # pale, grey to de-emphasize
    # bright and saturated colors to emphasize

# how to use color to convey additional important information about the data

# discrete scale
# fills in boxplot, histo
# same color = similar treatments 
# neutral colors (black, white, gray) = control treatments
# symbolic colors (heat: red, orange; cool: blues)
# photosynthesis/growth: green tones
# low nutrients: blue tones
# over enriched: brown
# disease dynamics : infected ind--red.

# monochromatic scale: different degrees/scales/shades of that color
# dichromatic scale: colors go from low to high value
# divergent scale: low, baseline, high colors

d=mpg # get data set
p1=ggplot(data= d, 
          mapping = aes(x=fl, y=hwy, group=fl))
p1 +geom_violin(fill="red")

myColors=c("red", "green", "pink", "blue", "orange")
p1 +geom_boxplot(fill=myColors)
myGray=gray(0.7)
p1 +geom_boxplot(fill=myGray)
print(myGray) # hexidecimal representation of the color
# red green blue tones in plot.  in 16s
test="#C9B4B5"
p1 +geom_boxplot(fill=test)

.=col2rgb("red")
print(.)
. = . /255
print(.)
.= rgb(t(.), alpha=0.5)
print(.) # the 80 is the transparancy value in the hexidecimal 
myPaleRed=.
p1 +geom_boxplot(fill=myPaleRed)
p1 +geom_boxplot(fill=gray(seq(from=0.1,to=0.9,length=5)))

x1 <- rnorm(n=100,mean=0)
x2 <- rnorm(n=100,mean=3)
dFrame <- data.frame(v1=c(x1,x2))
lab <- rep(c("Control","Treatment"),each=100)
dFrame <- cbind(dFrame,lab)
str(dFrame)
h1 <- ggplot(data=dFrame,mapping=aes(x=v1,fill=lab))
h1 + geom_histogram(position="identity",alpha=0.5,color="black") 

p1 + geom_boxplot(fill=wes_palettes[["Rushmore"]])
redmono = c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A", "#FC9272")
p1 + geom_boxplot(fill=redmono)

p1 + geom_boxplot(fill=c(gray(0.5), canva_palettes[[1]]))

p2=ggplot(data=d, 
          mapping=aes(x=fl, y=hwy, fill=fl)) + geom_boxplot() +
  scale_fill_manual(values=wes_palettes[["Rushmore"]]) # override colors. # fill is different than color
print(p2)

p2 + scale_fill_brewer(palette = "Blues")

# continuous scale colors
p3 = ggplot(data=d, 
            mapping=aes(x=displ, y=hwy, color=fl)) + 
  geom_point() + 
  scale_color_brewer(palette = "Spectral")
print(p3)

p3 <- ggplot(data=d, mapping=aes(x=displ,y=hwy,color=cty)) +
  geom_point()    
print(p3)

p3 +
  scale_color_gradient(low="red",high="blue")

p3 + scale_color_gradient2(midpoint=20,low="red",
                       mid=grey(0.5),high="darkblue")

z=mean(d$cty)
p3 +
  scale_color_gradient2(midpoint=z,low="red",mid="pink",
                        high="purple",space="Lab")
# space is required. but no real definition as to what it is. why it is needed.

# use scale_color_gradientn for multicolored changes
p3+scale_color_gradientn(colours = rainbow(5))

# heat map
xVar <- 1:30
yVar <- 1:5
myData <- expand.grid(xVar=xVar,yVar=yVar)
head(myData)
zVar <- myData$xVar + myData$yVar + 2*rnorm(n=150)
myData <- cbind(myData,zVar)
head(myData)
p4 <- ggplot(data=myData,mapping=aes(x=xVar,y=yVar,fill=zVar))
p4 + geom_tile() + scale_fill_gradient2(midpoint=19,low="red",mid="pink",
                        high="purple")
