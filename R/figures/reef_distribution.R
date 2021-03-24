#world map
library(sf)
library(raster)
library(ggplot2)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)

world <- ne_countries(scale = "medium", returnclass = "sf")
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

#data <- st_read("C:\\Users/Lewis Jones/OneDrive/Reef ENM paper/data/WCMC008_CoralReefs2018_v4/01_Data/WCMC008_CoralReef2018_Py_v4.shp")

data <- read.csv("./data/occurrences/UNEP_pts_subsample.csv")

p1 <- ggplot() +
  geom_sf(data = world, fill = "grey80", colour = "grey80") +
  geom_sf(data = coastline, colour = "grey50") +
  geom_point(data = data, aes(x=x, y=y), shape = 21, size = 2, fill = "#0868ac", colour = "black")  +
  #geom_sf(data = data, fill = "#99000d", colour = "#99000d") +
  theme_map() +
  theme(legend.position="") +
  theme(legend.key.width=unit(2, "cm"))

p1

ggsave("./figures/reef_distribution.png", plot = p1, units = "mm", width = 300, height = 150, dpi = 300)
