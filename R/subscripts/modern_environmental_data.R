#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# modern_environmental_data.R
#
# Script description:
# Prepare modern environmental layers for HSM
#
#-------------------------------------------------
#Load libraries and analyses options
library(raster)
library(ncdf4)
library(spatialEco)
source("./R/options.R")
#-------------------------------------------------
#load simulation codes
simulations <- read.csv("./data/exp_codes.csv", sep = ";")
#retain only pre-industrial code
simulations <- subset(simulations, interval_name == "Pre-Industrial_hadcm3l")
#transform to character string
exp_code <- as.character(simulations$exp_code)
dem_code <- as.character(simulations$dem_code)
#generate empty raster for resampling
r <- raster(res = res)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
#-------------------------------------------------
#set up syntax for reading data
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", "djf", "mam", "jja", "son")
irr <- c(paste("_sed_solar_mm_s3_srf_", months, "_formatted_180.nc", sep = ""))
sst <- c(paste("o.pfcl", months, "_formatted_sst_180.nc", sep = ""))
bathy <- c("_bathymetry_180.nc")
# DEM----------------------------------------------
#prepare bathymetry data
DEM <- raster(paste("./data/dem/", dem_code, bathy, sep = ""))
DEM <- mask(x = DEM, mask = world, inverse = TRUE)
#set absolute values
DEM[DEM > 0] <- NA
DEM <- abs(DEM)
DEM[DEM > 200] <- NA
#aggregate to desired resolution, retaining minimum cell value
DEM <- raster::aggregate(DEM, fact = res/res(DEM)[1], fun = 'min')
#resample data to fit same extent as climate data
DEM <- raster::resample(x = DEM, y = r)
#name data and plot
names(DEM) <- "dem"
plot(DEM)
# CLIMATE------------------------------------------
#prepare climate data

#read irradiance data and get summary layers
sol <- raster::stack(paste("./data/climate/", exp_code, irr, sep = ""), varname = "solar_mm_s3_srf")
#resample to desired resolution
sol <- raster::resample(sol, r)
#assign layer names
names(sol) <- paste("irr_", months, sep = "")
#calculate cell by cell maximum
max_irr <- raster::calc(sol[[1:12]], function(x){max(x, na.rm = FALSE)})
#calculate cell by cell minimum
min_irr <- raster::calc(sol[[1:12]], function(x){min(x, na.rm = FALSE)})
#calculate range
range_irr <- max_irr - min_irr
#calculate mean 
mean_irr <- raster::calc(sol[[1:12]], function(x){mean(x, na.rm = FALSE)})

#assign layer names
names(max_irr) <- c("max_irr")
names(min_irr) <- c("min_irr")
names(range_irr) <- c("range_irr")
names(mean_irr) <- c("mean_irr")

#read sst data and get summary layers
sst <- raster::stack(paste("./data/climate/", exp_code, sst, sep = ""), varname = "temp_mm_dpth")
#interpolate via nearest neighbors to replace NAs within shallow marine mask
sst <- lapply(1:nlayers(sst), function(x){focal(x = sst[[x]], w = matrix(1,3,3), fun = mean, NAonly = TRUE, na.rm = TRUE)})
sst <- stack(sst)
#resample to desired resolution
sst <- raster::resample(sst, r)
#assign layer names
names(sst) <- paste("sst_", months, sep = "")
#calculate cell by cell maximum
max_sst <- calc(sst[[1:12]], function(x){max(x)})
#calculate cell by cell minimum
min_sst <- calc(sst[[1:12]], function(x){min(x)})
#calculate range
range_sst <- max_sst - min_sst
#calculate mean
mean_sst <- raster::calc(sst[[1:12]], function(x){mean(x, na.rm = FALSE)})

#name layers
names(max_sst) <- c("max_sst")
names(min_sst) <- c("min_sst")
names(range_sst) <- c("range_sst")
names(mean_sst) <- c("mean_sst")

#stack of all layers
stk <- stack(sst, sol, DEM,
             max_sst, min_sst, range_sst, mean_sst, 
             max_irr, min_irr, range_irr, mean_irr)

#MASK---------------------------------------------
#mask environmental data by DEM and ocean layers
stk <- raster::mask(stk, DEM)
stk <- raster::mask(stk, stk$mean_sst)
#MULTICOLLINEARITY--------------------------------
#check for multicollinearity 

#pearson stats
pearson <- layerStats(stk,'pearson',na.rm=TRUE)
# Transform correlation results in a dataframe
cor.df <- as.data.frame(pearson[[1]])
# Check dataframe structure
head(cor.df)
# Calculate dendrogram based on distance (less distance = more correlation)
var.dist <- abs(as.dist(cor.df))
var.cluster <- hclust(1-var.dist)
# plot dendrogram
plot(var.cluster)
abline(h=0.25, lty=2, lwd=2) # variables that have a correlation <= 0.75

##subset selected variables
stk <- stack(DEM, stk$max_sst, stk$min_sst,
             stk$min_irr, stk$max_irr)

#mask stack by ocean layers and DEM
stk <- raster::mask(stk, max_sst)
stk <- raster::mask(stk, DEM)

#plot data
plot(stk)

plot(stk$max_sst)

#WRITE---------------------------------------------
#save environmental data
paths <- paste("./data/enm/layers/Modern/", sep = "")
#create file paths
dir.create(paths)
paths <- paste(rep(paths, nlayers(stk)), names(stk), ".asc", sep = "")
#save rasters
writeRaster(x = stk, filename = paths, bylayer = TRUE, overwrite = TRUE)
#notification
beepr::beep(2)

