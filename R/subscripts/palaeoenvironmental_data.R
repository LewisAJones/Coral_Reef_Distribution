#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# palaeoenvironmental_data.R
#
# Script description:
# Prepare palaeoenvironmental layers for HSM
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
#retain most realistic simulations
simulations <- subset(simulations, most_realistic == "x")
#remove pre-industrial data
simulations <- subset(simulations, !stage == "PI")
#generate empty raster for resampling
r <- raster(res = res)
#-------------------------------------------------
#set up syntax for reading files
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", "djf", "mam", "jja", "son")
irr_path <- c(paste("_sed_solar_mm_s3_srf_", months, "_formatted_180.nc", sep = ""))
sst_path <- c(paste("o.pfcl", months, "_formatted_sst_180.nc", sep = ""))
bathy <- c("_bathymetry_180.nc")
#-------------------------------------------------
#prepare data
for(i in 1:nrow(simulations)){
  #read stage-level DEM code
  abbrev <- as.vector(simulations$dem_code[i])
  #read stage-level simulation code
  exp <- as.vector(simulations$exp_code[i])
  #get stage name
  stage <- as.vector(simulations$interval_name[i])
  #read irradiance data
  sol <- raster::stack(paste("./data/climate/", exp, irr_path, sep = ""), varname = "solar_mm_s3_srf")
  #resample data to desired resolution
  sol <- raster::resample(sol, r)
  #add layer names
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
  
  #read sst data
  sst <- raster::stack(paste("./data/climate/", exp, sst_path, sep = ""), varname = "temp_mm_dpth")
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
  #caclulate mean
  mean_sst <- raster::calc(sst[[1:12]], function(x){mean(x, na.rm = FALSE)})
  
  #assign layer names
  names(max_sst) <- c("max_sst")
  names(min_sst) <- c("min_sst")
  names(range_sst) <- c("range_sst")
  names(mean_sst) <- c("mean_sst")
  
  #read DEM
  DEM <- raster(paste("./data/dem/", abbrev, bathy, sep = ""))
  #assign layer name
  names(DEM) <- "dem"
  #make shallow marine mask
  DEM[DEM > 200] <- NA
  #aggregate data to desired resolution based on minimum cell value
  DEM <- raster::aggregate(DEM, fact = res/res(DEM), fun = 'min')
  #resample data
  DEM <- raster::resample(x = DEM, y = r)
  
  #stack layers
  stk <- stack(DEM, max_sst, min_sst,
               max_irr, min_irr)
  
  #mask stack by ocean layers and DEM
  stk <- raster::mask(stk, mean_sst)
  stk <- raster::mask(stk, DEM)
  
  #generate paths
  paths <- paste("./data/enm/layers/", stage, "/", sep = "")
  dir.create(paths)
  paths <- paste(rep(paths, nlayers(stk)), names(stk), ".asc", sep = "")
  
  #save layers
  writeRaster(x = stk, filename = paths, bylayer = TRUE, overwrite = TRUE)

}

#notification
beepr::beep(2)
