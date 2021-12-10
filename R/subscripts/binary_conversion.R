#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# binary_conversion.R
#
# Script description:
# Generate discrete predictions
#
#-------------------------------------------------
#load libraries
library(raster)
library(dismo)
library(dplyr)
#-------------------------------------------------
#load threshold function
source("./R/functions/threshold.R")
#how many reps
reps <- 99
#get thresholds per run
thresholds <- lapply(0:reps, function(x){
  p <- read.csv(paste("./outputs/reef_", x, "_samplePredictions.csv", sep = ""))
  p <- p[,c("Logistic.prediction")]
  a <- read.csv(paste("./outputs/reef_", x, "_backgroundPredictions.csv", sep = ""))
  a <- a[,c("Logistic")]
  threshold(p, a)
})
#bind data
thresholds <- bind_rows(thresholds)
#get thresholds
LPT <- median(thresholds$LPT)
MaxSSS <- median(thresholds$MaxSSS)

files <- list.files("./outputs/", pattern = "_median.asc")

dir.create("./results/Predictions/")
dir.create("./results/Predictions/Median/")
dir.create("./results/Predictions/Binary/")
dir.create("./results/Predictions/Binary/LPT/")
dir.create("./results/Predictions/Binary/MaxSSS/")

for(i in files){
  
  r <- raster(paste("./outputs/", i, sep =""))
  #plot(r)
  
  LPT_ras <- r
  LPT_ras[LPT_ras < LPT] <- 0
  LPT_ras[LPT_ras >= LPT] <- 1
  
  #plot(LPT_ras)
  
  maxSSS_ras <- r
  maxSSS_ras[maxSSS_ras < MaxSSS] <- 0
  maxSSS_ras[maxSSS_ras >= MaxSSS] <- 1
  
  #plot(maxSSS_ras)
  name <- sub("^[^_]*_([^_]*).*", "\\1", i)
  if(name == "median.asc"){name <- "Modern"}
  
  writeRaster(x= r, filename = paste("./results/Predictions/Median/", name, ".asc", sep = ""), overwrite = TRUE)
  writeRaster(x= LPT_ras, filename = paste("./results/Predictions/Binary/LPT/", name, ".asc", sep = ""), overwrite = TRUE)
  writeRaster(x= maxSSS_ras, filename = paste("./results/Predictions/Binary/MaxSSS/", name, ".asc", sep = ""), overwrite = TRUE)
}

beepr::beep(2)
