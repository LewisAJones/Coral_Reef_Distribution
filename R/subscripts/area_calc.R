#area calculation

library(raster)
source("./R/functions/lat_bins.R")
source("./R/options.R")

area_ras <- area(x = raster(res = res))

files <- list.files("./results/Predictions/Binary/LPT/", pattern = ".asc")
files <- files[!files == "Modern.asc"]


#global

master <- data.frame()

for(i in files){
  r1 <- raster(paste("./results/Predictions/Binary/LPT/", i, sep =""))
  r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, sep =""))
  
  r1[r1 != 1] <- NA
  r2[r2 != 1] <- NA
  
  name <- tools::file_path_sans_ext(i)
  
  LPT <- mask(x = area_ras, mask = r1)
  MaxSSS <- mask(x = area_ras, mask = r2)
  
  LPT_global <- cellStats(x= LPT, stat = 'sum')
  MaxSSS_global <- cellStats(x= MaxSSS, stat = 'sum')
  
  
  df <- cbind.data.frame(name, LPT_global, MaxSSS_global)
  colnames(df) <- c("stage", "LPT_global", "MaxSSS_global")
  
  master <- rbind.data.frame(master, df)
  
}

dir.create("./results/Area/")

write.csv(master, "./results/Area/area_global.csv", row.names = FALSE)


#latitudinal analyses

master <- data.frame()

for(i in files){
  lats <- lat_bins(size = lat_bin_size)
  lats$LPT_area <- NA
  lats$MaxSSS_area <- NA
  
  r1 <- raster(paste("./results/Predictions/Binary/LPT/", i, sep =""))
  r2 <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, sep =""))
  
  r1[r1 != 1] <- NA
  r2[r2 != 1] <- NA
  
  name <- tools::file_path_sans_ext(i)
  
  LPT <- mask(x = area_ras, mask = r1)
  MaxSSS <- mask(x = area_ras, mask = r2)
  LPT <- data.frame(rasterToPoints(LPT))
  MaxSSS <- data.frame(rasterToPoints(MaxSSS))
  
  for(j in 1:nrow(lats)){
    tmp <- subset(LPT, y <= lats$max[j] & y >= lats$min[j])
    lats$LPT_area[j] <- sum(tmp$layer)
    tmp <- subset(MaxSSS, y <= lats$max[j] & y >= lats$min[j])
    lats$MaxSSS_area[j] <- sum(tmp$layer)
  }
  
  lats$stage <- name
  
  master <- rbind.data.frame(master, lats)
  
}

write.csv(master, "./results/Area/area_lat.csv", row.names = FALSE)

beepr::beep(2)