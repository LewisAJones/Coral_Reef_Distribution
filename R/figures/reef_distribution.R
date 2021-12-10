#world map
library(sf)
library(raster)
library(ggplot2)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(rgdal)
library(maps)

world <- ne_countries(scale = "medium", returnclass = "sf")
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

data <- read.csv("./data/occurrences/ReefBase_pts_subsample.csv")

DEM <- raster("./data/dem/pre_ind_bathymetry_180.nc")
#prepare bathymetry data
DEM <- mask(x = DEM, mask = world, inverse = TRUE)
#set absolute values
DEM[DEM > 0] <- NA
DEM <- abs(DEM)
DEM[DEM > 200] <- NA
#aggregate to desired resolution, retaining minimum cell value
DEM <- raster::aggregate(DEM, fact = 1/res(DEM)[1], fun = 'min')
#resample data to fit same extent as climate data
DEM <- raster::resample(x = DEM, y = raster(res = 1))
#name data and plot
names(DEM) <- "dem"
plot(DEM)
DEM <- as(DEM, "SpatialPixelsDataFrame")
DEM <- data.frame(DEM)
DEM <- na.omit(DEM)

p1 <- ggplot() +
  geom_tile(data = DEM, aes(x = x, y = y), fill = "#a6cee3", colour = "darkgrey", alpha = 1) +
  geom_tile(data = data, aes(x=x, y=y), fill = "#d73027", colour = "black", alpha = 1) + 
  geom_sf(data = world, fill = "grey80", colour = "grey80", alpha = 1) +
  geom_sf(data = coastline, colour = "black", alpha = 0.5) +
  #geom_point(data = data, aes(x = x, y = y), shape = 21, colour = "black", fill = "#377eb8") +
  theme_map() +
  theme(legend.position="") +
  theme(legend.key.width=unit(2, "cm"),
  plot.background=element_rect(fill = "white", colour = NA))

p1

ggsave("./figures/reef_distribution.png", plot = p1, units = "mm", width = 300, height = 150, dpi = 300)
