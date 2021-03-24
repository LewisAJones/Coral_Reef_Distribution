# Environmental data processing
# Lewis A. Jones
# March 2020
#---------------------------------
library(raster)
library(ncdf4)
library(spatialEco)
source("./R/options.R")
#---------------------------------
simulations <- read.csv("./data/exp_codes.csv")
simulations <- subset(simulations, interval_name == "Pre-Industrial")
exp_code <- as.character(simulations$exp_code)
dem_code <- as.character(simulations$dem_code)
r <- raster(res = res)
#---------------------------------
#set up syntax
months <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec", "jja", "djf", "mam", "son", "ann")
irr <- c(paste("_sed_solar_mm_s3_srf_", months, "_formatted_180.nc", sep = ""))
sst <- c(paste("o.pfcl", months, "_formatted_sed_sst_180.nc", sep = ""))
bathy <- c("_bathymetry_180.nc")
#---------------------------------

#DEM <- raster("C:\\Users/Lewis Jones/OneDrive/Reef ENM paper/data/layers/ETOPO1_Bed_c_geotiff.tif")
DEM <- raster(paste("./data/dem/", dem_code, bathy, sep = ""))
DEM[DEM > 0] <- NA
DEM <- abs(DEM)
DEM <- raster::aggregate(DEM, fact = res/res(DEM)[1], fun = 'min')
DEM <- raster::resample(x = DEM, y = r)
names(DEM) <- "dem"
plot(DEM)

################

data <- read.csv("./data/occurrences/UNEP_pts_subsample.csv")

ext <- extract(x = DEM, y = data[,c("x","y")])
sum(is.na(ext))

ext <- which(ext <= 200)

data <- data[ext,]

sol <- stack(paste("./data/climate/", exp_code, irr, sep = ""))
sol <- resample(sol, r)
names(sol) <- paste("irr_", months, sep = "")
max_irr <- calc(sol[[1:12]], function(x){max(x)})
min_irr <- calc(sol[[1:12]], function(x){min(x)})
range_irr <- max_irr - min_irr

names(range_irr) <- c("range_irr")

names(max_irr) <- c("max_irr")
names(min_irr) <- c("min_irr")

sst <- stack(paste("./data/climate/", exp_code, sst, sep = ""))
sst <- resample(sst, r)
names(sst) <- paste("sst_", months, sep = "")
max_sst <- calc(sst[[1:12]], function(x){max(x)})
min_sst <- calc(sst[[1:12]], function(x){min(x)})
range_sst <- max_sst - min_sst

names(max_sst) <- c("max_sst")
names(min_sst) <- c("min_sst")
names(range_sst) <- c("range_sst")

stk <- stack(sst, sol, DEM, max_sst, min_sst, range_sst, max_irr, min_irr, range_irr)
stk <- raster::mask(stk, sst$sst_ann)

plot(stk)

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

stk <- stack(stk$sst_ann, stk$range_sst, stk$irr_djf, stk$irr_jja, stk$dem)

paths <- paste("./data/enm/layers/Modern/", sep = "")
dir.create(paths)
paths <- paste(rep(paths, nlayers(stk)), names(stk), ".asc", sep = "")
writeRaster(x = stk, filename = paths, bylayer = TRUE, overwrite = TRUE)

ext <- extract(x = stk, y = data[,c("x","y")])
data <- cbind.data.frame(data, ext)
data <- na.omit(data)
data <- data[,c("species", "x","y")]

write.csv(data, "./data/occurrences/UNEP_pts_subsample.csv", row.names = FALSE)

beepr::beep(2)
