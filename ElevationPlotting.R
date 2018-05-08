library(elevatr)  # for elevation inqueries
library(sp)  # for spatial dataset classification
library(rgdal)
library(raster)
library(lattice)
library(rasterVis)

set_api_key("https://aws.amazon.com/public-datasets/terrain/")

#northwest coordinates
maxlat<- 44.534168
minlong<- -72.818231
#southeast coordinates
minlat<-44.515098
maxlong<- -72.780376

#determine time zone for transformation
epsg<-"+init=EPSG::3855"

# set seed for reproducible results
set.seed(76)
# set number of iterations (the higher the greater resolution)
n<- 200

#Creating your spatial dataset from scratch

spCoord <- SpatialPoints(data.frame(
  x = runif(n, min = minlong, max = maxlong), 
  y = runif(n, min = minlat, max = maxlat)),
  proj4string = CRS("+proj=longlat"))
summary(spCoord) # notice projection = false

#Transform coordinate system

# Set Coordinate Reference System using the epsg code, based UTM zone
sp <-spTransform(spCoord,CRS(epsg))
summary(sp)    # notice projection now = true

#Now let’s set up a spatial dataframe to access elevation data

spdf<-SpatialPointsDataFrame(sp, data.frame(sp),
                             proj4string=proj4string(sp),
                             bbox = NULL)
summary(spdf)

# now our dataset is in a format that can be used to query elevation
elev<-get_elev_point(spdf,
                     prj = proj4string(spdf),
                     src = "epqs")
#note: defaults to mapzen, but as of this past January this
#source is no longer an option, thus we will use the 
#Elevation Point Query Service (epqs)

# tripple check projection
is.projected(elev)

# ensure there are no NA elevation values from query
!is.na(elev$elevation)

summary(elev)

raster<-get_elev_raster(spdf,elev$elevation, 
                        prj = proj4string(elev), 
                        src = "aws",
                        set_api_key("https://aws.amazon.com/public-datasets/terrain/"),
                        z=5)   

hist(raster, main="Distribution of Elevation Values", 
     col= "purple", 
     maxpixels=10000)

plot(raster)
contour<-contour(raster,nlevels=8)
#image(raster, zlim=c(1500,2000),col=terrain.colors(5)) 

points=read.csv("/Volumes/kellrlab/datashare/Spruce/MansfieldCG/ConeCollections/nsfConeCollections2018_bmvCopy.csv")
points2=subset(points, points$Site=="MMF")
plot(raster)
points(points2$Longitude, points2$Latitude, col = "red")

plot(points2$Longitude, points2$Latitude, col = "red")
