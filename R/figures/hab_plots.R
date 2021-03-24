# Hab plot
# Lewis A. Jones
# March 2021
#---------------------------------
library(raster)
library(ggplot2)
library(ggpubr)
source("./R/functions/hab_plot.R")

collections <- read.csv("./data/occurrences/subsampled_PBDB_collections.csv")
stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,]
stages <- c("Modern", stages$interval_name)

master <- data.frame()

for(i in stages){
  #heat <- raster(paste("./results/Predictions/Median/", i, ".asc", sep =""))
  r1 <- raster(paste("./results/Predictions/Binary/LPT/", i, ".asc", sep =""))
  r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, ".asc", sep =""))
  #extract stage name
  name <- tools::file_path_sans_ext(i)
  #subset collection data
  xy <- subset(collections, stage == name)[,c("x", "y")]
  ext <- extract(x = r1, y = xy)
  vec <- which(!is.na(ext))
  xy <- xy[vec,]
  
  LPT <- hab_plot(x = r1, xy = xy, col = "darkcyan", name = name, lab = "")
  MaxSSS <- hab_plot(x = r2, xy = xy, col = "#225ea8", name = name, lab = "")
  #heat <- hab_plot(x = heat, xy = xy, heat = TRUE, name = name, lab = "Heat")
  p <- ggarrange(LPT, MaxSSS, ncol=2, nrow=1)
  ggsave(paste("./figures/hab_plots/", name, ".png", sep = ""), plot = p, width = 75, height = 20, units = "mm", dpi = 300, scale = 3)
  print(i)
}  
  