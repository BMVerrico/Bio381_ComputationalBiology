# ggplot graphics
# 04.10.2018

library(ggplot2)
library(patchwork)
library(TeachingDemos)
library(ggthemes)
char2seed("Sienna")
data('mtcars')

d=mpg

# create four individual graphs
g1 <- ggplot(data=d,mapping = aes(x=displ,y=cyl)) + 
  geom_point() + 
  geom_smooth()
print(g1)

# second graph
g2 <- ggplot(data = d,mapping = aes(x=fl,fill=I("tomato"), color=I("black"))) + 
  geom_bar(stat="count") + 
  theme(legend.position = "none")
print(g2)

g3=ggplot(data=d, 
          mapping = aes(x=displ, fill=I("royalblue"), color=I("black"))) +
  geom_histogram()
print(g3)

g4=ggplot(data=d,
          mapping=aes(x=fl, y=cty, fill=fl)) +
  geom_boxplot() + theme(legend.position = "none")
print(g4)

# patchwork
# place two plots horizontally
 g1 + g2
 
 g1 + g2 + g3 + plot_layout(ncol=1)
 
 g1 + g2 + plot_layout(ncol=1, heights=c(2,1))
 
 g1 + g2 + plot_layout(ncol=2, heights=c(1,2))
 
 g1 + plot_spacer() + g2
 
 g1 + {
   g2 + {
     g3 + 
       g4 + 
         plot_layout(ncol=1)
       }
     }
      + plot_layout(ncol=1)

 g1 + g2 - g3 + plot_layout(ncol=1)
 
 (g1 | g2 | g3)/g4
 
 g3a= g3 + scale_x_reverse()
 g3b =g3 + scale_y_reverse()
 g3c =g3 + scale_y_reverse() + scale_x_reverse()
 
 (g3 | g3a) / ( g3b | g3c)
 
 (g3 + coord_flip() | g3a + coord_flip())/ (g3b + coord_flip() | g3c + coord_flip())
 
 # save 
 #ggsave(filename="MyPlot.pdf", plot=g3, device="pdf", width=20, height=20,
#        units="cm")
 
 # mapping of vars to aes
 
m1= ggplot (data=mpg, mapping=aes(x=displ, y=cty, color=class))+
  geom_point(size=2, shape=21)
print(m1) 

m2= ggplot (data=mpg, mapping=aes(x=displ, y=cty, shape=class))+
  geom_point(size=2)
print(m2) 

m3= ggplot (data=mpg, mapping=aes(x=displ, y=cty))+
  geom_point(fill="coral", color="royalblue", shape=21)
print(m3)

m4= ggplot (data=mpg, mapping=aes(x=displ, y=cty, size=class))+
  geom_point()
print(m4) 

# 2 vars
m5= ggplot (data=mpg, mapping=aes(x=displ, y=cty, size=hwy, color=hwy))+
  geom_point()
print(m5)

m6= ggplot (data=mpg, mapping=aes(x=displ, y=cty, shape=class, color=hwy))+
  geom_point()
print(m6)

# 3 vars
m7= ggplot (data=mpg, mapping=aes(x=displ, y=cty, 
                                  shape=drv, size=hwy,color=fl))+
  geom_point()
print(m7)

# mapping a var to the same aes for two different geoms
m7= ggplot (data=mpg, mapping=aes(x=displ, y=cty, 
                                  color=drv))+
  geom_point() + geom_smooth(method="lm")
print(m7)

# facetting for visualization in a set of related plots

m8= ggplot (data=mpg, mapping=aes(x=displ, y=cty, color=hwy)) + 
  geom_point() 
print(m8)
# classification for rows, for cols
m8 + facet_grid(class ~ fl)
m8 + facet_grid(class ~ fl, scales="free_y")

# facet on only 1 var
m8 + facet_grid(. ~ class)

# use facet wrap for unordered graphs
m8 + facet_wrap(~ class)

# combine vars in a facet wrap
m8+ facet_wrap(~class + fl, drop=FALSE)

m9= ggplot (data=mpg, mapping=aes(x=displ, y=cty, color=drv)) + 
  geom_smooth(method="lm", se=FALSE)
print(m9)
m9 + facet_grid(. ~ class)

# fitting with boxplots over a cont. var
m10= ggplot (data=mpg, mapping=aes(x=displ, y=cty)) + 
  geom_boxplot()
print(m10)
m10 + facet_grid(. ~ class)

m10= ggplot (data=mpg, mapping=aes(x=displ, y=cty, group=drv, fill=drv)) + 
  geom_boxplot()
print(m10)
m10 + facet_grid(. ~ class)


