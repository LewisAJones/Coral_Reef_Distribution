library(raster)
library(ggplot2)
library(ggpubr)
source("./R/functions/hab_plot.R")

collections <- read.csv("./data/occurrences/subsampled_PARED_collections_extract.csv")
stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,]
stages <- c("Modern", stages$interval_name)

Norian <- raster("./results/Predictions/Binary/MaxSSS/Norian.asc")
Norian_xy <- subset(collections, stage == "Norian")[,c("x", "y")]
ext <- extract(x = Norian, y = Norian_xy)
vec <- which(!is.na(ext))
Norian_xy <- Norian_xy[vec,]
p1 <- hab_plot(x = Norian, xy = Norian_xy, col = "darkcyan", name = "", lab = "Norian (Triassic)")
#p1

Tithonian <- raster("./results/Predictions/Binary/MaxSSS/Tithonian.asc")
Tithonian_xy <- subset(collections, stage == "Tithonian")[,c("x", "y")]
ext <- extract(x = Tithonian, y = Tithonian_xy)
vec <- which(!is.na(ext))
Tithonian_xy <- Tithonian_xy[vec,]
p2 <- hab_plot(x = Tithonian, xy = Tithonian_xy, col = "darkcyan", name = "", lab = "Tithonian (Jurassic)")
#p2

Albian <- raster("./results/Predictions/Binary/MaxSSS/Albian.asc")
Albian_xy <- subset(collections, stage == "Albian")[,c("x", "y")]
ext <- extract(x = Albian, y = Albian_xy)
vec <- which(!is.na(ext))
Albian_xy <- Albian_xy[vec,]
p3 <- hab_plot(x = Albian, xy = Albian_xy, col = "darkcyan", name = "", lab = "Albian (Cretaceous)")
#p3

Lutetian <- raster("./results/Predictions/Binary/MaxSSS/Lutetian.asc")
Lutetian_xy <- subset(collections, stage == "Lutetian")[,c("x", "y")]
ext <- extract(x = Lutetian, y = Lutetian_xy)
vec <- which(!is.na(ext))
Lutetian_xy <- Lutetian_xy[vec,]
p4 <- hab_plot(x = Lutetian, xy = Lutetian_xy, col = "darkcyan", name = "", lab = "Lutetian (Paleogene)")
#p4

p <- ggarrange(p1, p2, p3, p4, ncol=2, nrow=2)
#p
ggsave("./figures/hab_plot_combi.png", plot = p, width = 78, height = 40, units = "mm", dpi = 300, scale = 3)
