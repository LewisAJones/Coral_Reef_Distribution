library(raster)
dir.create("./results/Predictions/Clamping/")
dir.create("./results/Predictions/Clamping/Mask/")
dir.create("./results/Predictions/Clamping/Clamped/")
dir.create("./results/Predictions/Clamping/Clamped/LPT/")
dir.create("./results/Predictions/Clamping/Clamped/MaxSSS/")
stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,]

for(i in 1:nrow(stages)){
  r <- raster(paste("./results/RUN/reef_0_", stages$interval_name[i], "_novel_limiting.asc", sep = ""))
  r[r != 5] <- 0
  r[r == 5] <- 1
  writeRaster(r, paste("./results/Predictions/Clamping/Mask/", stages$interval_name[i], ".asc", sep = ""), overwrite = TRUE)
  
  LPT <- raster(paste("./results/Predictions/Binary/LPT/", stages$interval_name[i], ".asc", sep =""), overwrite = TRUE)
  MaxSSS <- raster(paste("./results/Predictions/Binary/MaxSSS/", stages$interval_name[i], ".asc", sep =""), overwrite = TRUE)
  
  LPT <- mask(LPT, mask = r, maskvalue = 0, updatevalue = 0)
  MaxSSS <- mask(MaxSSS, mask = r, maskvalue = 0, updatevalue = 0)
  
  writeRaster(LPT, paste("./results/Predictions/Clamping/Clamped/LPT/", stages$interval_name[i], ".asc", sep = ""), overwrite = TRUE)
  writeRaster(MaxSSS, paste("./results/Predictions/Clamping/Clamped/MaxSSS/", stages$interval_name[i], ".asc", sep = ""), overwrite = TRUE)
}
