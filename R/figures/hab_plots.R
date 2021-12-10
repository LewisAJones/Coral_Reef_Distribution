# Hab plot
# Lewis A. Jones
# March 2021
#---------------------------------
library(raster)
library(ggplot2)
library(ggpubr)
source("./R/functions/hab_plot.R")

dir.create("./figures/hab_plots/")

collections <- read.csv("./data/occurrences/PARED_subsampled.csv")
world <- ne_countries(scale = "medium", returnclass = "sf")

stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,]
stages <- c(stages$interval_name)

num <- 0

for(i in stages){
  num <- num + 1
  #heat <- raster(paste("./results/Predictions/Median/", i, ".asc", sep =""))
  r1 <- raster(paste("./results/Predictions/Binary/LPT/", i, ".asc", sep =""))
  r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, ".asc", sep =""))
  #load continent reconstruction
  dem_name <- tolower(substr(i, 1, 3))
  if(dem_name == "cam"){dem_name <- "cmp"}
  dem <- raster(paste("./data/dem/", dem_name, "_bathymetry_180.nc", sep =""))
  dem <- raster::aggregate(dem, fact = res(r1)/res(dem), fun = 'min')
  dem <- resample(x = dem, y = r1)
  dem[is.na(dem)] <- -9999
  dem[dem != -9999] <- NA
  dem[dem == -9999] <- 1
  plot(dem)

  #subset collection data
  name <- i
  xy <- subset(collections, interval_name == name)[,c("P.Long", "P.Lat")]
  ext <- extract(x = r1, y = xy)
  vec <- which(!is.na(ext))
  xy <- xy[vec,]
  
  r1[r1 == 0] <- NA
  r2[r2 == 0] <- NA
  
  r1 <- na.omit(as.data.frame(r1, xy = TRUE))
  r2 <- na.omit(as.data.frame(r2, xy = TRUE))
  colnames(r1) <- c("x", "y", "value")
  colnames(r2) <- c("x", "y", "value")
  dem <- as.data.frame(dem, xy = TRUE)
  colnames(dem) <- c("x", "y", "value")
  

  #plots
  LPT <- ggplot() +
    geom_sf(data = world, fill = NA, colour = NA) +
    geom_tile(data = subset(dem, is.na(value)), aes(x=x, y=y), fill = "darkgrey", colour = "darkgrey")  +
    geom_tile(data = subset(dem, !is.na(value)), aes(x=x, y=y), fill = "white", colour = "white")  +
    geom_tile(data = r1, aes(x=x, y=y), fill = "darkcyan", colour = "darkcyan")  +
    geom_point(data = xy, aes(x=P.Long, y=P.Lat), shape = 21, size = 4.5, stroke = 2, fill = "red", colour = "black", alpha = 0.75)  +
    geom_text(aes(x = -175, y = -85, label = name), size = 14, hjust = "left") +
    scale_fill_manual(values = c("aliceblue", "darkgrey", col)) +
    scale_colour_manual(values = c("aliceblue", "darkgrey", col)) +
    xlim(-180, 180) +
    theme_map() +
    theme(legend.position="") +
    theme(plot.margin = margin(0,0,0,0, "cm"))
  
  MaxSSS <- ggplot() +
    geom_sf(data = world, fill = NA, colour = NA) +
    geom_tile(data = subset(dem, is.na(value)), aes(x=x, y=y), fill = "darkgrey", colour = "darkgrey")  +
    geom_tile(data = subset(dem, !is.na(value)), aes(x=x, y=y), fill = "white", colour = "white")  +
    geom_tile(data = r2, aes(x=x, y=y), fill = "#225ea8", colour = "#225ea8")  +
    geom_point(data = xy, aes(x=P.Long, y=P.Lat), shape = 21, size = 4.5, stroke = 2, fill = "red", colour = "black", alpha = 0.75)  +
    geom_text(aes(x = -175, y = -85, label = name), size = 14, hjust = "left") +
    scale_fill_manual(values = c("aliceblue", "darkgrey", col)) +
    scale_colour_manual(values = c("aliceblue", "darkgrey", col)) +
    xlim(-180, 180) +
    theme_map() +
    theme(legend.position="") +
    theme(plot.margin = margin(0,0,0,0, "cm"))
  
  p <- ggarrange(LPT, MaxSSS, ncol=2, nrow=1)
  ggsave(paste("./figures/hab_plots/",num,"_", name, ".jpg", sep = ""), plot = p, width = 300, height = 80, units = "mm", dpi = 220, scale = 3)
  print(i)
}  
  