#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# calculate_boyce.R
#
# Script description:
# Calculate boyce index
#
#-------------------------------------------------
#load libraries
library(ecospat)
#load occurrence data
xy <- read.csv("./data/occurrences/ReefBase_pts_subsample.csv")

xy <- xy[,c("x", "y")]

files <- paste("reef_", seq(0, 99, 1), ".asc", sep = "")

v <- vector()

for(i in files){
  #load model prediction
  r <- raster(paste("./outputs/", i, sep =""))
  #calculate continuous boyce index Hirzel et al. (2006) 
  CBI <- ecospat.boyce(fit = r, obs = xy, PEplot = FALSE)
  
  v <- append(v, CBI$Spearman.cor)
}

#create file directory
dir.create("./results/Boyce_index/")

CBIfile <- file("./results/Boyce_index/CBI.txt")

meanCBI <- mean(v)
sdCBI <- sd(v)

writeLines(paste("The continuous boyce index for this model is: ", meanCBI, " ± ", sdCBI, sep = ""), CBIfile)

close(CBIfile)

beepr::beep(2)
