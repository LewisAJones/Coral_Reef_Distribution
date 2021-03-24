#prepare occurrence data
#load libraries
library(sf)
library(dplyr)
library(raster)
source("./R/options.R")

#load data
data <- st_read("C:\\Users/Lewis Jones/OneDrive/Reef ENM paper/data/WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Py_v4.shp")
#extract geometry
pts <- st_geometry(data)
#format coordinates
pts <- st_coordinates(pts)
#filter data
pts <- pts[,c("X", "Y")]

r <- raster(res = res)
#rasterize points
ras <- rasterize(x = pts, y = r, field = 1) 
#plot raster
plot(ras)
#spatial subsample
pts_ras <- rasterToPoints(ras)
pts_ras <- data.frame(pts_ras[,c("x","y")])
pts_ras$species <- "reef"
pts_ras <- pts_ras[,c("species", "x", "y")]

#plot points
#plot(pts_ras[,2:3])
#save data
#write.csv(pts, "./data/occurrences/UNEP_pts.csv", row.names = FALSE)
write.csv(pts_ras, "./data/occurrences/UNEP_pts_subsample.csv", row.names = FALSE)
writeRaster(ras, "./data/occurrences/UNEP_raster.asc", overwrite = TRUE)

beepr::beep(2)
