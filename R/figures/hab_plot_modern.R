# Hab plot modern
# Lewis A. Jones
# March 2021
#---------------------------------
library(raster)
library(ggplot2)
library(ggpubr)
source("./R/functions/hab_plot.R")

dir.create("./figures/hab_plots/")

world <- ne_countries(scale = "medium", returnclass = "sf")
coastline <- ne_coastline(scale = "medium", returnclass = "sf")

r1 <- raster(paste("./results/Predictions/Binary/LPT/Modern.asc", sep =""))
r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/Modern.asc", sep =""))

DEM <- raster(paste("./data/dem/pre_ind_bathymetry_180.nc", sep =""))
DEM <- mask(x = DEM, mask = world, inverse = TRUE)
#set absolute values
DEM[DEM > 0] <- NA
DEM <- abs(DEM)
#aggregate to desired resolution, retaining minimum cell value
DEM <- raster::aggregate(DEM, fact = 1/res(DEM)[1], fun = 'min')
#resample data to fit same extent as climate data
DEM <- raster::resample(x = DEM, y = r1)

plot(DEM)
#set absolute values
DEM[is.na(DEM)] <- -9999
DEM[DEM != -9999] <- NA
DEM[DEM == -9999] <- 1
plot(DEM)
  
r1[r1 == 0] <- NA
r2[r2 == 0] <- NA
  
r1 <- na.omit(as.data.frame(r1, xy = TRUE))
r2 <- na.omit(as.data.frame(r2, xy = TRUE))
colnames(r1) <- c("x", "y", "value")
colnames(r2) <- c("x", "y", "value")
DEM <- as.data.frame(DEM, xy = TRUE)
colnames(DEM) <- c("x", "y", "value")
  
  
  #plots
LPT <- ggplot() +
    #geom_tile(data = DEM, aes(x=x, y=y), fill = "darkgrey", colour = "darkgrey")  +
    geom_tile(data = subset(DEM, is.na(value)), aes(x=x, y=y), fill = "darkgrey", colour = "darkgrey")  +
    geom_tile(data = subset(DEM, !is.na(value)), aes(x=x, y=y), fill = "white", colour = "white")  +
    geom_tile(data = r1, aes(x=x, y=y), fill = "darkcyan", colour = "darkcyan")  +
    geom_sf(data = world, fill = "white", colour = "white", alpha = 1) +
    geom_sf(data = coastline, colour = "black", alpha = 0.5) +
    geom_text(aes(x = -175, y = -86, label = "Modern"), size = 14, hjust = "left") +
    scale_fill_manual(values = c("aliceblue", "darkgrey", col)) +
    scale_colour_manual(values = c("aliceblue", "darkgrey", col)) +
    xlim(-180, 180) +
    theme_map() +
    theme(legend.position="") +
    theme(plot.margin = margin(0,0,0,0, "cm"))
  
  MaxSSS <- ggplot() +
    #geom_tile(data = DEM, aes(x=x, y=y), fill = "darkgrey", colour = "darkgrey")  +
    geom_tile(data = subset(DEM, is.na(value)), aes(x=x, y=y), fill = "darkgrey", colour = "darkgrey")  +
    geom_tile(data = subset(DEM, !is.na(value)), aes(x=x, y=y), fill = "white", colour = "white")  +
    geom_tile(data = r2, aes(x=x, y=y), fill = "#225ea8", colour = "#225ea8")  +
    geom_sf(data = world, fill = "white", colour = "white", alpha = 1) +
    geom_sf(data = coastline, colour = "black", alpha = 0.5) +
    geom_text(aes(x = -175, y = -86, label = "Modern"), size = 14, hjust = "left") +
    scale_fill_manual(values = c("aliceblue", "darkgrey", col)) +
    scale_colour_manual(values = c("aliceblue", "darkgrey", col)) +
    xlim(-180, 180) +
    theme_map() +
    theme(legend.position="") +
    theme(plot.margin = margin(0,0,0,0, "cm"))
  
  p <- ggarrange(LPT, MaxSSS, ncol=2, nrow=1)
  ggsave(paste("./figures/hab_plots/Modern.jpg", sep = ""), plot = p, width = 300, height = 80, units = "mm", dpi = 220, scale = 3)

