# Palaeoenvironmental data processing
# Lewis A. Jones
# March 2020
#---------------------------------
library(raster)
library(ncdf4)
library(spatialEco)
source("./R/options.R")
#---------------------------------
simulations <- read.csv("./data/exp_codes.csv")
simulations <- subset(simulations, most_realistic == "x")
simulations <- subset(simulations, !interval_name == "Pre-Industrial")
r <- raster(res = res)
#---------------------------------
#set up syntax
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", "jja", "djf", "mam", "son", "ann")
irr_path <- c(paste("_sed_solar_mm_s3_srf_", months, "_formatted_180.nc", sep = ""))
sst_path <- c(paste("o.pfcl", months, "_formatted_sed_sst_180.nc", sep = ""))
bathy <- c("_bathymetry_180.nc")
#---------------------------------

for(i in 1:nrow(simulations)){
  abbrev <- as.vector(simulations$dem_code[i])
  exp <- as.vector(simulations$exp_code[i])
  stage <- as.vector(simulations$interval_name[i])
  
  sol <- raster::stack(paste("./data/climate/", exp, irr_path, sep = ""), varname = "solar_mm_s3_srf")
  sol <- raster::resample(sol, r)
  names(sol) <- paste("irr_", months, sep = "")
  max_irr <- raster::calc(sol, function(x){max(x, na.rm = FALSE)})
  min_irr <- raster::calc(sol, function(x){min(x, na.rm = FALSE)})
  range_irr <- max_irr - min_irr
  
  names(max_irr) <- c("max_irr")
  names(min_irr) <- c("min_irr")
  names(range_irr) <- c("range_irr")
  
  sst <- raster::stack(paste("./data/climate/", exp, sst_path, sep = ""), varname = "temp_mm_dpth")
  sst <- raster::resample(sst, r)
  names(sst) <- paste("sst_", months, sep = "")
  max_sst <- calc(sst[[1:12]], function(x){max(x)})
  min_sst <- calc(sst[[1:12]], function(x){min(x)})
  range_sst <- max_sst - min_sst
  
  names(max_sst) <- c("max_sst")
  names(min_sst) <- c("min_sst")
  names(range_sst) <- c("range_sst")
  
  DEM <- raster(paste("./data/dem/", abbrev, bathy, sep = ""))
  names(DEM) <- "dem"
  DEM <- raster::aggregate(DEM, fact = res/res(r), fun = 'min')
  DEM <- raster::resample(x = DEM, y = r)
  
  stk <- stack(sst, sol, DEM, max_sst, min_sst, range_sst, max_irr, min_irr, range_irr)
  #stk <- raster::stack(max_sst, min_sst, DEM)
  
  stk <- raster::mask(stk, sst$sst_ann)
  stk <- stack(stk$sst_ann, stk$range_sst, stk$irr_djf, stk$irr_jja, stk$dem)
  
  paths <- paste("./data/enm/layers/", stage, "/", sep = "")
  dir.create(paths)
  paths <- paste(rep(paths, nlayers(stk)), names(stk), ".asc", sep = "")
  
  writeRaster(x = stk, filename = paths, bylayer = TRUE, overwrite = TRUE)
}

beepr::beep(2)