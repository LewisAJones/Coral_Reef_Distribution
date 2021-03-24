#reef zone calculation
library(raster)
source("./R/functions/lat_zone.R")

files <- list.files("./results/Predictions/Binary/LPT/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
master <- data.frame()

for(i in files){
  r1 <- raster(paste("./results/Predictions/Binary/LPT/", i, sep =""))
  r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, sep =""))
  
  name <- tools::file_path_sans_ext(i)
  
  names(r1) <- "layer"
  names(r2) <- "layer"
  
  LPT <- lat_zone(x = r1)
  MaxSSS <- lat_zone(x = r2)
  
  df <- cbind.data.frame(name, LPT, MaxSSS)
  colnames(df) <- c("stage", "LPT_max", "LPT_min", "LPT_lower", "LPT_upper", "MaxSSS_max", "MaxSSS_min", "MaxSSS_lower", "MaxSSS_upper")
  
  master <- rbind.data.frame(master, df)
  
}

dir.create("./results/Reef_zone/")

write.csv(master, "./results/Reef_zone/reef_zone_calc.csv", row.names = FALSE)

beepr::beep(2)
