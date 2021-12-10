#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# prepare_ReefBase_data.R
#
# Script description:
# Prepare modern occurrence data
#
#-------------------------------------------------
#load libraries
library(dplyr)
library(raster)
source("./R/options.R")
#-------------------------------------------------
#read data
data <- read.csv("./data/occurrences/ReefBase_24_08_2021.csv")
#remove non-reef coral community data
remove <- c("",
              "Non-reef coral community",
              "Non-Reef coral community",
              "Non-Reef Coral Community",
              "Non reef coral community",
              "Non-reef Coral community" ,
              "Fringing/Patch/Non-reef coral community",
              "Fringing/Patch/Non-reef",
              "Fringing/Non-reef coral community",
              "Coralline algae/Vermetid reef",
              "Patch Reef/Non-reef coral community",
              "Fringing/Patch/Non-reef",
              "Fringing/Parch Reef/Non-reef Coral community",
              "Fringing/Patch reefs/non-reef coral community",
              "Barrier/Fringing/Non-reef coral community",
              "Pseudo-atoll"
              )
data <- data %>% filter(!REEF_TYPE %in% remove)

#reduce to coordinate data
pts <- data[,c("LON", "LAT")]
#remove data points without coordinates
pts <- na.omit(pts)
#plot data
plot(pts)
#create raster at desired resolution
r <- raster(res = res)
#rasterize points
ras <- rasterize(x = pts, y = r, field = 1) 
#plot raster
plot(ras)
#spatial subsampling
pts_ras <- rasterToPoints(ras)
pts_ras <- data.frame(pts_ras[,c("x","y")])
pts_ras$species <- "reef"
pts_ras <- pts_ras[,c("species", "x", "y")]

colnames(pts) <- c("x", "y")
pts$species <- "reef"
pts <- pts[,c("species", "x", "y")]

#remove data outside mask
vars <- raster::stack(paste("./data/enm/layers/Modern/", list.files("./data/enm/layers/Modern/", pattern = ".asc"), sep = ""))
ext <- extract(x = vars, y = pts_ras[,c("x", "y")])
pts_ras$ext <- ext
pts_ras <- na.omit(pts_ras)
pts_ras <- pts_ras[,c("species", "x", "y")]

#plot presence points
plot(pts_ras[,2:3], 
     ylab = "Latitude", xlab = "Longitude",
     pch = 22, 
     col = "black", bg = "blue", 
     xlim = c(-180, 180), ylim = c(-90, 90))


#write data
write.csv(pts_ras, "./data/occurrences/ReefBase_pts_subsample.csv", row.names = FALSE)
writeRaster(ras, "./data/occurrences/ReefBase_raster.asc", overwrite = TRUE)

file.remove("./data/enm/layers/Modern/dem.asc")

beepr::beep(2)


