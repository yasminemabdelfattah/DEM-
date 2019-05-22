##SRTM 90m Digital Elevation Data	
##http://srtm.csi.cgiar.org/
##http://www.diva-gis.org/gdata
##SRTM30 dataset. CGIAR-SRTM data aggregated to 30 seconds	
library(rgdal)

GDALinfo("C:/Users/yassmin/Dropbox/PhD Thesis & Analysis/My PhD R work/Ethiopia Shapefile/ETH_msk_alt/ETH_msk_alt.grd") # prints out info about the raster
alt = readGDAL("C:/Users/yassmin/Dropbox/PhD Thesis & Analysis/My PhD R work/Ethiopia Shapefile/ETH_msk_alt/ETH_msk_alt.vrt") # reads in the whole raster

image(alt) # does a plot
class(alt)
str(alt)
spplot(alt) # does a plot with a legend
# Using the class provided by raster
library(raster)
altr <- raster(alt)
spplot(altr)
summary(altr)
res(altr)
ncol(altr)
nrow(altr)
## Change resolution by a factor
altr<- aggregate(altr, fact=c(30,30), expand=TRUE)
res(altr)
ncol(altr)
nrow(altr)
spplot(altr)
summary(altr)




# We can use the digital elevation model to derive other interesting variables.
# The terrain function allows us to compute many variables: slope, aspect, topographic position index, etc. 
# The aspect variable is a circular variable (0° = 360°) 
# and can therefore cause some troubles in a statistical analysis. 
# One possibility is to decompose the variable into 2 orthogonal variables: 
# northness and eastness (both variables go from -1 to +1).
rSlope <- terrain(altr, opt = "slope", unit = "degrees")
rAspect <- terrain(altr, opt = "aspect")

# We can use mathematical functions directly on the raster objects
rEastness <- sin(rAspect)
rNorthness <- cos(rAspect)

# If several rasters have the same extent and resolution, we can use
# raster algebra to combine them and derive new variables
rMix <- (rSlope + (rNorthness + 2)^5)/cellStats(altr, stat = "mean") + 42

# We can store them in a RasterStack object (see also ?brick)
rTerrain <- stack(list(slope = rSlope, aspect = rAspect, eastness = rEastness, 
                       northness = rNorthness))

plot(rTerrain)

#convert the raster to points for plotting
alt.p <- rasterToPoints(altr)

#Make the points a dataframe for ggplot
df <- data.frame(alt.p)
#Make appropriate column headings
colnames(df)<- c("lon", "lat", "alt")

#convert the raster to points for plotting
rTerrain.p<- rasterToPoints(rTerrain)

#Make the points a dataframe for ggplot
rTerrain.df <- data.frame(rTerrain.p)
#Make appropriate column headings
colnames(rTerrain.df)<- c("lon", "lat", "slope", "aspect", "eastness", "northness")
alt.terrain <- merge(df, rTerrain.df,by=c("lon","lat"))
summary(alt.terrain)

