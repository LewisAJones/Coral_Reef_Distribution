#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# centroid_calc.R
#
# Script description:
# Calculate centroid for discrete predictions
#
#-------------------------------------------------
#load libraries
library(raster)
source("./R/functions/gCentroid_weight.R")
#-------------------------------------------------

files <- list.files("./results/Predictions/Binary/LPT/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
master <- data.frame()

for(i in files){
  r1 <- raster(paste("./results/Predictions/Binary/LPT/", i, sep =""))
  r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, sep =""))
  
  name <- tools::file_path_sans_ext(i)
  
  LPT <- gCentroid_weight(x = r1)
  MaxSSS <- gCentroid_weight(x = r2)
  
  df <- cbind.data.frame(name, LPT, MaxSSS)
  colnames(df) <- c("stage", "LPT_Nx", "LPT_Ny", "LPT_Sx", "LPT_Sy", "MaxSSS_Nx", "MaxSSS_Ny", "MaxSSS_Sx", "MaxSSS_Sy")
  
  master <- rbind.data.frame(master, df)
  
}

dir.create("./results/Centroid/")

write.csv(master, "./results/Centroid/centroid_calc.csv", row.names = FALSE)

beepr::beep(2)
