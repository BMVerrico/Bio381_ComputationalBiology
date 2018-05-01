#MADE4 PACKAGE
#source("https://bioconductor.org/biocLite.R")
#biocLite("made4")
library(made4)
#biocLite("BiocUpgrade")

# read in the data
res_mat <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/res_mat_abun.csv")
bac_mat <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/bac_mat_abun.csv")
vf_mat <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/vf_mat_abun.csv")

# structure the data so it is in the correct format for the analysis.
str(res_mat)
#so clean thus up by setting all values to numeric, except not the gene names
res_mat_all <- res_mat[,-c(1)]
res_mat_all[] <- lapply(res_mat_all[], as.numeric)
rownames(res_mat_all) <- res_mat$Name
head(res_mat_all)
str(res_mat_all)

none <- lapply(res_mat_all, function(x) all(x == 0))
which(none == "TRUE")
# remove those 6 columns
res_mat2 <- res_mat_all[,-c(4,10,11,15,21,24)]
head(res_mat_all)
str(res_mat_all)

bac_mat_all <- bac_mat[,-c(1)]
bac_mat_all[] <- lapply(bac_mat_all[], as.numeric)
rownames(bac_mat_all) <- bac_mat$Name
bac_mat2 <- bac_mat_all[,-c(4,10,11,15,21,24)]
head(bac_mat2)
str(bac_mat2)

vf_mat_all <- vf_mat[,-c(1)]
vf_mat_all[] <- lapply(vf_mat_all[], as.numeric)
rownames(vf_mat_all) <- vf_mat$Name
vf_mat2 <- vf_mat_all[,-c(4,10,11,15,21,24)]
head(vf_mat2)
str(vf_mat2)

#the ord function makes it east to run ordination methods to
#explore structure of a data matrix - the methods include 
#correspondence (coa, the default), non-symmetric correspondence
#analysis (nsc) or principal component (pca).

res_coa<-ord(res_mat2, type= "coa")

summary(res_coa$ord)

# plot the data
plot(res_coa)

plotgenes(res_coa, nlab=1) #if you include "nlab" 
plotarrays(res_coa, graph="groups") #"simple" gives the points, "groups" adds the labels

#you can generate a list of the variables (genes) at the end of the axis or gradient
topgenes(res_coa, axis=1, n=5, end="pos")

# and compare lists of top genes at the ends of gradients
a<-topgenes(res_coa, axis=2, n=10, end="pos")
b<-topgenes(res_coa, axis=3, n=10, end="pos")
z<-comparelists(a,b)
print.comparelists(z)
z$intersect
z$Set.Diff


#including generating in 3d
do3d(res_coa$ord$li)
do3d(res_coa$ord$co)

#Coinertia Analysis
#exploring trends or co-relationships between multiple datasets

#bacteria and ARGs
c <- cia(bac_mat2, res_mat2)
c$coinertia$RV
#0.445
plot.cia(c)

# virulence and ARGs
c2 <- cia(vf_mat2, res_mat2)
c2$coinertia$RV
#0.647
plot.cia(c2)


# virulence and bacteria
c3 <- cia(vf_mat2, bac_mat2)
c3$coinertia$RV
#0.358
plot.cia(c3)

##############################################################
# LEAFLET PACKAGE
library(leaflet)
library(maps)
library(TeachingDemos)
char2seed("Professor Looney")
dF <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/leafletData30.csv")
dF2 <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/leafletData500.csv")
cities <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/cities.csv")

# a way to create interactive maps
# leaflet(): function that creates a map widget that can store vars to be modified later on
# addTiles(): adds mapping data from "open street map"
# %>%: piping notation. takes an output that adds or pipes it ot the next command
# as the first argument and reassign it to the var.

# create a simply map
my_map=leaflet() %>%
addTiles()
my_map

# without piping
my_map=leaflet()
my_map=addTiles(my_map)
my_map

# adding different types of maps onto my_map
my_map=leaflet() %>%
  addTiles() %>%
  addProviderTiles(providers$Esri.WorldImagery)
my_map

#Esri.NatGeoWorldMap

# adding markers
map=my_map %>%
  addMarkers(lat=44.4764, lng=-73.1955)
map

# give a label
map=my_map %>%
  addMarkers(lat=44.4764, lng=-73.1955,
             popup = "Bio381 Classroom")
map

df=data.frame(lat=runif(20, min=44.4770, max=44.4793),
              lng=runif(20, min=-73.18788, max=-73.18203))
head(df)

df %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

markers=data.frame(lat=dF$lat, lng=dF$lng)

markers %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()

# adding legends
df=data.frame(col=sample(c("red", "blue", "green"),
                         20, replace=TRUE), stringsAsFactors = FALSE)

markers %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers(color=df$col) %>%
  addLegend(labels=c("A.rubrum", "T. canadensis", "P. strobulus"),
            colors=c("red", "blue", 'green'))

# making clusters
cluster=data.frame(lat=dF2$lat, lng=dF2$lng)

cluster %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers()
  
# solution to hot mess above
cluster %>%
  leaflet() %>%
  addTiles() %>%
  addMarkers(clusterOptions = markerClusterOptions())

# add custom markers
uvmIcon <- makeIcon(iconUrl = "UVM.jpg", # call the image
                    iconWidth = 31*215/230,
                    iconHeight= 31,
                    iconAnchorX= 31*215/230/2,
                    iconAnchorY= 16
) # what i found to be the best length,height,width for marker
UVMLatLong <- data.frame(
  lat= c(44.4779),
  lng= c(-73.1965)) #lat & lng for your data point 
UVMLatLong %>%
  leaflet()%>%
  addTiles()%>%
  addMarkers(icon= uvmIcon) # what icon do u want

# adding shapes
# polygons
mapStates=map("state", fill=TRUE, plot=FALSE)

leaflet(data = mapStates) %>% 
  addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = 0.9), stroke = FALSE)

leaflet(cities) %>% addTiles() %>%
  addCircles(lng = ~long, lat = ~lat, weight = 2,
             radius = ~sqrt(pop) * 30, popup = ~city)

##############################################################
library(metafor)
# package made for metaanalyses
# multiple papers and pull out a specific test statistic
# aggregate papers from literature
# example is legume and nitrogen fixing bacteria and warming
# obs. effect (eg. change in abundance of legumes) = true effect + sampling var
# true effect is unknown
# you know sampling var--obtained from package metafor
# obs. effect is also known from literature
# true effect= random effect that comes from the study + mu (true mean of the effect)
# random: (e.g. differences in sampling design, norm dist. mu=0, sd=ta^2, heteogenity among studies)
# standardized mean differences
  # mean biomass control- mean biomass treatment = what you get from the lit. 
  # divide by pool std. to get standardization

dat=dat.normand1999
dat
# 2 groups
# n tally
# m mean
# sd 

# calculate standardized mean difference- this is needed for analysis
smdDat=escalc(measure = "SMD", m1i = m1i, sd1i=sd1i, n1i=n1i,
              m2i = m2i,
              sd2i = sd2i, n2i=n2i, data = dat)

myModel=rma(yi, vi, data=smdDat)
print(myModel)
# i2 is the heterogenity because of treatment and not between studies
# want this number to be as close to 1 as possible
# tau is the heterogeneity 
# test for hetero: if <0.05--- good to proceed. if not-- says too much noise
# in the data

dat=dat.curtis1998
head(dat)
dat=dat.mcdaniel1994
Zdat=escalc(measure="ZCOR", ri=ri, ni=ni, data=dat)
myModel=rma(yi, vi, data=Zdat)
print(myModel)

##############################################################
# prelims
library(openair)
library(dplyr)

# analyze air pollution 
# good for time series
# 40 functions--review of 4 plotting functions
# summaryPlot-- good for seeing a summary
# timePlot-- good for line graphs of the time series
# windRose-- wind data (plots in a compass)
# calanderPlot-- wind and water quality parms

dbBuoy <- read.csv("/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/HF_MB_forOpenAir.csv")
head(dbBuoy)

# need date and time in the correct format
dbBuoy$date=as.POSIXct(strptime(dbBuoy$timestamp,
                                format="%m/%d/%Y %H:%M", 
                                tz="Etc/GMT-4"))

# summaryPlot
# making a summary plot to see days missing data, distribution of the data, and stats 
summaryPlot(select(dbBuoy, date, windSpeed, CHL_CONC, PC_RFU, wTEMP))
# can change the time period of the stats to "months" 
summaryPlot(select(dbBuoy, date, windSpeed, CHL_CONC, PC_RFU, wTEMP), period = "months")
# can change what values the graph displays: avg.time = "sec", "hour", "day" (default), "week", "month", "quarter", "year", or "2 month" etc. 
summaryPlot(select(dbBuoy, date, windSpeed, CHL_CONC, PC_RFU, wTEMP), period = "months", avg.time="week")

# timePlot 
# first with different scales 
timePlot(dbBuoy,pollutant=c("wTEMP", "PC_RFU", "CHL_CONC"))

# all on one plot
timePlot(dbBuoy,pollutant=c("wTEMP", "PC_RFU", "CHL_CONC"), group = TRUE)

# have different scales for y axis
timePlot(dbBuoy,  pollutant=c("wTEMP","PC_RFU", "CHL_CONC"), y.relation="free")
timePlot(dbBuoy,  pollutant=c("wTEMP","PC_RFU", "CHL_CONC"),
         y.relation="free", ref.y=list(h=9, lty=5), 
         name.pol=c("temp (C)", "PC (RFU)", "Chl(ug/L"))

# windRose 
# can see the proportion of wind that was blowing in a certain direction for the dataset
windRose(dbBuoy, ws="windSpeed", wd="windDir")

# can split it up by season and add "pollution" to see how the wind direction correlated to a parameter you're measuring
pollutionRose(dbBuoy, ws="windSpeed", wd="windDir", pollutant="PC_RFU", type="season")

# calendarPlot
dbBuoy <- rename(dbBuoy,wd = windDir,  ws = windSpeed)

# with the default colors 
calendarPlot(dbBuoy, pollutant = "ws")

# customizing the colors and adding in wind vectors 
calendarPlot(dbBuoy, pollutant = "PC_RFU", annotate = "ws", cols = c("white", "dodgerblue", "dodgerblue3", "dodgerblue4"))

# pollutant is also with windRose

##############################################################
library("cluster")
library("factoextra")
library(ggplot2)
library("NbClust")

#  prepared data
iris.scaled=scale(iris[,-5])
head(iris.scaled)

# step 1: do clusters exist?

#factoextra package
Hopkins=get_clust_tendency(iris.scaled,
                           n=nrow(iris.scaled)-1,
                           seed=123)
Hopkins$hopkins_stat
# 1-stat (anything above 0.5 means there are clusters in the data)


# step 2: calculate distance

# cluster package
distgower=daisy(iris.scales, metric="gower",
                stand=TRUE)

#factoextra package
DistanceMap=fviz_dist(dist.obj = distgower,
                      show_labels = TRUE, 
                      lab_size = 4)

# step 3: cluster using K-means

# not the right way to go about this
set.seed(123)
km.res=kmeans(iris.scaled, 
              centers=6,
              iter.max = 250,
              nstart = 25)
head(km.res)

fviz_cluster(km.res,
             data=iris,
             choose.vars = c("Sepal.Length",
                             "Petal.Width"), stand=TRUE)

# what is the right number of clusters?

# method 1: elbow method (kind of subjective)

fviz_nbclust(iris.scaled,
             FUNcluster = kmeans,
             method="wss") # make clusters as compact as possible

# method 2: silhouette method

fviz_nbclust(iris.scaled,
             FUNcluster = kmeans,
             method="silhouette") + theme_classic()

# method 3: gap statistic

fviz_nbclust(iris.scaled,
             kmeans,
             nstart=25,
             method="gap_stat",
             nboot=500) + theme_classic()

# method 4: test all

nb=NbClust(iris.scaled,
           distance="euclidean",
           min.nc=2,
           max.nc=10,
           method="kmeans")


km.res=ecluster(iris.scaled,
                stand=FALSE,
                "kmeans",
                hc_metric="manhattan",
                k=3)

fviz_silhouette(km.res, palette="jco")

# approach 2: heirarchical cluster
res.agnes=agnes(x=distgower,
                diss=TRUE,
                stand=TRUE,
                metric="euclidean",
                method="ward")

fviz_dend(res.agnes)

fviz_dend(res.agnes, cex=0.6, k=2, rect=TRUE)

fviz_dend(res.agnes, k=2, k_color="jco", 
          type="phylogenic",
          repel=TRUE,
          phylo_layout = "layout.gem")

# calculate p-values
# library(pvclust)
data("lung")
ss=sample(1:73, 30)
newData=lung[,ss]
res.pv=pvclust(newData,
               method.hclust="average",
               method.dist="correlation")

##############################################################

library(ggmap) # automatically loads ggplot
library(rgdal) # automatically loads sp (Cheat sheet for sp : http://rspatial.r-forge.r-project.org/gallery/)
#sp is also used for plotting spatial information
library(foreign)

head(read.dbf(file ="/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/Countyboundaries/VTCountyBoundaries.dbf"))

# what is ggmap?
# goes out to different map servers (like google)
# raster objects
# lets you plot on those maps
# similar to ggplot
# can change your coord system and datums

# some example map fetching
avl=get_map(location="Asheville, North Carolina")
ggmap(avl)

# Different maptypes: "terrain", "toner", "watercolor +. 
avl <- get_map(location="Asheville",maptype="toner")
ggmap(avl)

# different sources include google, stamen, OSM 
avl=get_map(location="Asheville",
            maptype = "watercolor", zoom=15)
ggmap(avl)

# change the zoom
# whole numbers 1-21 for google
# 1-18 for stamen
# 1 is the world, 21 is house level
YD=get_map("Yaounde, Cameroom", source="google",
           maptype = "terrain", zoom=11)
ggmap(YD)


# Vermont Example shapefile -----------------------------------------------

# get a raster object, overset it with a shapefile, and add GPS coords
# 1.Get the map of VT

vt=get_map("Salisbury, Vermont", zoom=8, maptype = "roadmap")
VTmap=ggmap(vt, extent = "normal")

# read in vermont database file
VTcountyB  <- readOGR(dsn = "/Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/Countyboundaries/VTCountyBoundaries.shp", 
                      layer = "VTCountyBoundaries")
plot(VTcountyB)

# reproject your data's projection/datum
VTcountyB=spTransform(VTcountyB, CRS("+proj=longlat + datum=WGS84"))

#fortify command (from the package ggplot2).  
#Converts spatial data into a data frame.
fortify(VTcountyB)

# Plot the shape file on raster object!
VTcountyMap <- VTmap + geom_polygon(aes(x=long, y=lat, group=group), 
                                    fill='green', size=.2,
                                    color='black', data=VTcountyB,
                                    alpha=0.3)
VTcountyMap

# Then you can play with alpha and colors
VTcountyMap <- VTmap+geom_polygon(data = VTcountyB, 
                                  aes(x=long,y=lat,group=group),
                                  fill="purple",alpha=.5)
VTcountyMap

VTcountyMap <- VTmap+geom_polygon(data = VTcountyB,
                                  aes(x=long,y=lat,group=group),
                                  fill="red",alpha=.2,color="brown",
                                  size=0.5)
VTcountyMap

#command shift r
# Add GPS coords ----------------------------------------------------------

gps_coords=read.table("//Volumes/Macintosh HD/Volumes/Macintosh HD/Users/Brittany/Downloads/GPS_coords.csv",
                      header = TRUE, sep=",", stringsAsFactors = FALSE)

# use geom_point (scatter plots) to plot pts on map
VTcountyMap + geom_point(data=gps_coords,
                         mapping=aes(x=Lon, y=Lat),
                         color="yellow", size=3)

##############################################################
library(EpiModel)

param <- param.dcm (inf.prob=0.3, act.rate=0.5)
init <- init.dcm(s.num=1000, i.num=1)
control <- control.dcm(type="SI", nsteps=200)

mod <- dcm(param, init, control)
mod

plot(mod) # basic plot
comp_plot(mod, at=50, digits=1) # flow diagram

#SIR mod
param <- param.dcm (inf.prob=0.5, act.rate=0.3, rec.rate= 1/40)
init <- init.dcm(s.num=1000, i.num=1, r.num=0)
control <- control.dcm(type="SIR", nsteps=500, dt=.5) 

mod <- dcm(param, init, control)
mod

plot(mod) # basic plot
comp_plot(mod, at=70, digits=1)

# plot for disease incidence
plot(mod, y="si.flow")
plot(mod, y="si.flow", col="goldenrod", legend= "n")


#SIS 
param <- param.dcm (inf.prob=0.2, act.rate=0.5, rec.rate=0.02)
init <- init.dcm(s.num=1000, i.num=1)
control <- control.dcm(type="SIS", nsteps=200)

mod <- dcm(param, init, control)
mod

plot(mod) # basic plot
comp_plot(mod, at=59, digits=1)

# w/ Demography
#SIR
param <- param.dcm(inf.prob = 0.33, act.rate = 1.5, rec.rate = 1/10,
                   b.rate = 1/95, ds.rate = 1/100, di.rate = 1/60, dr.rate = 1/100)
init <- init.dcm(s.num = 1000, i.num = 1, r.num = 0) 
control <- control.dcm(type = "SIR", nsteps = 500, dt = 0.5) 
mod <- dcm(param, init, control)

plot(mod) # basic plot
comp_plot(mod, at=70, digits=1)

#plotting comparment size
plot(mod, y="si.flow", col="goldenrod")

# SI
param <- param.dcm (inf.prob=0.3, act.rate=0.5,
                    b.rate=1/95, ds.rate=1/100, di.rate=1/60)
init <- init.dcm(s.num=1000, i.num=1)
control <- control.dcm(type="SI", nsteps=200)

mod <- dcm(param, init, control)
mod
plot(mod)
comp_plot(mod, at=50)

#Sensitivity Analyses
param <- param.dcm (inf.prob=0.2, act.rate=seq(0.25,0.5,0.05), rec.rate=0.02)
init <- init.dcm(s.num=1000, i.num=1)
control <- control.dcm(type="SIS", nsteps=200)

mod <- dcm(param, init, control)
mod

plot(mod) # basic plot
plot(mod, run=2, legend="n")
comp_plot(mod, at=59)

#plotting with color palette
plot(mod, col=1:6)
plot(mod, y="si.flow", col="Blues")
plot(mod, col=rainbow(3), lty=rep(1:2, each=3))

#variation in more than one parameter:
act.rates <- c(0.2, 0.2, 0.4, 0.4, 0.6, 0.6)
inf.probs <- c(0.1,0.2,0.1,0.2,0.1,0.2)
param <- param.dcm (inf.prob=inf.probs, act.rate=act.rates, rec.rate=0.02)
init <- init.dcm(s.num=1000, i.num=1)
control <- control.dcm(type="SIS", nsteps=200)

mod <- dcm(param, init, control)
mod
plot(mod)

#to extract the data of a specific run
z <- head(as.data.frame(mod, run=4))
z


# Bipartite or two group models
param <- param.dcm(inf.prob=0.4, inf.prob.g2 = 0.1, act.rate=0.25, balance= "g1",
                   b.rate=1/100, b.rate.g2 = NA, ds.rate=1/100, ds.rate.g2=1/100,
                   di.rate =1/50, di.rate.g2=1/50)

# here we establish the birth and death rates for the group groups. Incorporating a balance to g1 enables us to derive the act.rate for group 2 over time
init <- init.dcm(s.num = 500, i.num = 1, s.num.g2 = 500, i.num.g2 = 0) #same principal here
control <- control.dcm(type = "SI", nsteps = 500)
mod <- dcm(param, init, control)
mod
plot(mod)


#ICMs
param <- param.icm(inf.prob = 0.2, act.rate = 0.25)# note the function is now param.icm
init <- init.icm(s.num = 500, i.num = 1)
control <- control.icm(type = "SI", nsims = 10, nsteps = 300)
mod <- icm(param, init, control)
mod

summary(mod, at = 125) # to request summary of individual timestep 
head(as.data.frame(mod, out= "mean"))
tail(as.data.frame(mod, out = "vals", sim = 1))

plot(mod) # standard plot
plot(mod, sim.lines = TRUE, mean.smooth = FALSE, qnts.smooth = FALSE)


#comparing dcm and icm
#dcm
param <- param.dcm(inf.prob = 0.3, act.rate = 0.92, rec.rate = 1/50,
                   b.rate = 1/50, ds.rate = 1/60, di.rate = 1/40, dr.rate = 1/60)
init <- init.dcm(s.num = 800, i.num = 200, r.num = 0)
control <- control.dcm(type = "SIR", nsteps = 300)
mD <- dcm(param, init, control)

# icm
param <- param.icm(inf.prob = 0.3, act.rate = 0.92, rec.rate = 1/50,
                   b.rate = 1/50, ds.rate = 1/60, di.rate = 1/40,
                   dr.rate = 1/60)
init <- init.icm(s.num = 800, i.num = 200, r.num = 0)
control <- control.icm(type = "SIR", nsteps = 300, nsims = 10)
mI <- icm(param, init, control)

#to get rid of stocastic birth and death rates
control <- control.icm(type = "SIR", nsteps = 300, nsims = 10,
                       b.rand = FALSE, d.rand = FALSE) #this should cause less variability
mI_2 <- icm(param, init, control)

# plotting using ```add```
plot(mD, alpha = 0.75, lwd = 4, main = "DCM and ICM Comparison")
plot(mI, qnts = FALSE, sim.lines = FALSE, add = TRUE, mean.lty = 2, legend = FALSE)
plot(mI_2, qnts = FALSE, sim.lines = FALSE, add = TRUE, mean.lty = 3, legend = FALSE)

#SIS bipartite with demography
param <- param.icm(inf.prob = 0.3, inf.prob.g2 = 0.1, act.rate = 0.5, balance = "g1",
                   rec.rate = 1/25, rec.rate.g2 = 1/50, b.rate = 1/100, b.rate.g2 = NA,
                   ds.rate = 1/100, ds.rate.g2 = 1/100, di.rate = 1/90, di.rate.g2 = 1/90)
init <- init.icm(s.num = 500, i.num = 1, s.num.g2 = 500, i.num.g2 = 1)
control <- control.icm(type = "SIS", nsteps = 500, nsims = 10)

mod <- icm(param, init, control)
plot(mod)


print(mod_SIR_1g_op)


# Part II
SEIR <- function(t, t0, parms) {
  with(as.list(c(t0, parms)), {
    
    # Population size
    num <- s.num + e.num + i.num + r.num
    
    # Effective contact rate and FOI from a rearrangement of Beta * c * D
    ce <- R0 / i.dur 
    lambda <- ce * i.num/num
    
    dS <- -lambda*s.num
    dE <- lambda*s.num - (1/e.dur)*e.num
    dI <- (1/e.dur)*e.num - (1 - cfr)*(1/i.dur)*i.num - cfr*(1/i.dur)*i.num
    dR <- (1 - cfr)*(1/i.dur)*i.num
    
    # Compartments and flows  part of the derivative vector
    # Other calculations to be output are outside the vector, but within the containing list
    list(c(dS, dE, dI, dR, 
           se.flow = lambda * s.num,
           ei.flow = (1/e.dur) * e.num,
           ir.flow = (1 - cfr)*(1/i.dur) * i.num,
           d.flow = cfr*(1/i.dur)*i.num),
         num = num,
         i.prev = i.num / num,
         ei.prev = (e.num + i.num)/num)
  })
}
#running SEIR
param <- param.dcm(R0 = 2.9, e.dur = 12, i.dur = 14, cfr =  0.95)
init <- init.dcm(s.num = 1e6, e.num = 50, i.num = 12, r.num = 0,
                 se.flow = 0, ei.flow = 0, ir.flow = 0, d.flow = 0)
control <- control.dcm(nsteps = 500, dt = 1, new.mod = SEIR)
mod <- dcm(param, init, control)
mod

#plotting
par(mfrow = c(1, 2))
plot(mod, y = "i.num", main = "Prevalence")
plot(mod, y = "se.flow",  main = "Incidence")
