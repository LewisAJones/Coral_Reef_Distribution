#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# model_performance_cont.R
#
# Script description:
# Calculate model performance
#
#-------------------------------------------------
library(raster)
library(dplyr)
source("./R/options.R")
#---------------------------------
collections <- read.csv("./data/occurrences/PARED_subsampled.csv")
stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,] #subset to stages of interest
stages <- paste(unique(stages$interval_name), ".asc", sep = "")
stk <- stack(paste("./results/Predictions/Median/", stages, sep = ""))

dir.create("./results/Performance/")
dir.create("./results/Performance/plots/")

vec <- vector()

for(i in 1:nlayers(stk)){
  #load rasters
  r <- stk[[i]]
  #get raster name for later use
  name <- names(r)
  #subset collection data
  xy <- subset(collections, interval_name == name)[,c("P.Long", "P.Lat")]
  
  observed <- raster::extract(x = r, y = xy)
  
  #observed[is.na(observed)] <- 0
  
  observed_median <- median(observed)
  
  vals <- raster::sampleRandom(x = r, size = 1000, xy = FALSE, na.rm = TRUE)
  
  random_median <- median(vals)
    
  p <- round(wilcox.test(x = observed, y = vals, alternative = "greater")$p.value, 3)
  
  W <- wilcox.test(x = observed, y = vals, alternative = "greater")$statistic
  
  if(p == 0){p <- 0.001}
  
  jpeg(paste("./results/Performance/plots/", i, "_", name, ".jpg", sep = ""), width = 3000, height = 3000, res = 600)
  
  hist(vals, xlim = c(0, 1), ylim = c(0, 1000), col= "grey50", main = name, xlab = "Habitat suitability", ylab = "Frequency")
  box()
  abline(v = observed_median, col = "red", lty = 2, lwd = 2)
  points(x = observed_median, y = 0, pch = 23, col = "black", bg = "red", cex = 1.3)
  abline(v = random_median, col = "blue", lty = 2, lwd = 2)
  points(x = random_median, y = 0, pch = 23, col = "black", bg = "blue", cex = 1.3)
  #text(x = 0.85, y = 950, paste('P-val', " = ", paste(p)))
  #text(x = 0.85, y = 900, paste('n', " = ", nrow(xy)))
  legend("topright", 
         legend = c(paste('W', " = ", paste(W)), paste('P', " = ", paste(p)), paste('n', " = ", nrow(xy))),
         bty = "n")
  
  dev.off()

  vec <- append(vec, p)
  
  names(vec)[i] <- name
  
}

results <- file("./results/Performance/performance.txt")

writeLines(paste(names(vec), vec), results)

close(results)
  
beepr::beep(2)
