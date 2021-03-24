#centroid calculation
library(raster)
source("./R/functions/gCentroid_weight.R")

files <- list.files("./results/Predictions/Binary/LPT/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
master <- data.frame()

for(i in files){
  r1 <- raster(paste("./results/Predictions/Clamping/Clamped/LPT/", i, sep =""))
  r2 <- raster(paste("./results/Predictions/Clamping/Clamped/MaxSSS/", i, sep =""))
  
  name <- tools::file_path_sans_ext(i)
  
  LPT <- gCentroid_weight(x = r1)
  MaxSSS <- gCentroid_weight(x = r2)
  
  df <- cbind.data.frame(name, LPT, MaxSSS)
  colnames(df) <- c("stage", "LPT_Nx", "LPT_Ny", "LPT_Sx", "LPT_Sy", "MaxSSS_Nx", "MaxSSS_Ny", "MaxSSS_Sx", "MaxSSS_Sy")
  
  master <- rbind.data.frame(master, df)
  
}

dir.create("./results/Centroid/")

write.csv(master, "./results/Centroid/centroid_calc_clamped.csv", row.names = FALSE)

beepr::beep(2)