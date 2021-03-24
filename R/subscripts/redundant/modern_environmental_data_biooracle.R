library(raster)
library(spatialEco)

max_sst <- raster("C:\\Users/Lewis Jones/OneDrive/Reef ENM paper/data/layers/Present.Surface.Temperature.Max.asc")
min_sst <- raster("C:\\Users/Lewis Jones/OneDrive/Reef ENM paper/data/layers/Present.Surface.Temperature.Min.asc")
DEM <- raster("C:\\Users/Lewis Jones/OneDrive/Reef ENM paper/data/layers/ETOPO1_Bed_c_geotiff.tif")
DEM[DEM > 0] <- NA
DEM <- abs(DEM)
DEM <- raster::aggregate(DEM, fact = 1/res(DEM)[1], fun = 'min')
DEM[DEM > 200] <- NA
plot(DEM)



DEM <- mask(DEM, max_sst)
plot(DEM)
stk <- stack(max_sst, min_sst, DEM)
names(stk) <- c("max_sst", "min_sst", "dem")

paths <- paste("./data/enm/layers/Modern/", sep = "")
dir.create(paths)
paths <- paste(rep(paths, nlayers(stk)), names(stk), ".asc", sep = "")
writeRaster(x = stk, filename = paths, bylayer = TRUE, overwrite = TRUE)

pts <- read.csv("./data/occurrences/UNEP_pts.csv")

ras <- rasterize(x = pts, y = DEM, field = 1) 
#plot raster
plot(ras)
#spatial subsample
pts_ras <- rasterToPoints(ras)
pts_ras <- data.frame(pts_ras[,c("x","y")])

ext <- extract(x = stk, y = pts_ras[,c("x","y")])
pts_ras <- cbind.data.frame(pts_ras, ext)
pts_ras <- subset(pts_ras, dem > 200)
pts_ras <- na.omit(pts_ras)
pts_ras$species <- "reef"
pts_ras <- pts_ras[,c("species", "x","y")]
write.csv(pts_ras, "./data/occurrences/UNEP_pts_subsample.csv", row.names = FALSE)

#pearson stats

pearson<-layerStats(stk,'pearson',na.rm=T)
# Check correlation results
print(pearson)
# Transform correlation results in a dataframe
cor.df<-as.data.frame(pearson[[1]])
# Check dataframe structure
head(cor.df)

var.dist <- abs(as.dist(cor.df))
# Calculate dendrogram based on distance (less distance = more correlation)
var.cluster <- hclust(1-var.dist)
# Plot dendrogram
plot(var.cluster)
abline(h=0.25, lty=2, lwd=2) # variables that have a correlation < 0.6

beepr::beep(2)
