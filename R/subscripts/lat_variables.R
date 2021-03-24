#environmental analysis
library(raster)
source("./R/functions/lat_bins.R")
source("./R/options.R")
area_ras <- area(x = raster(res = res))

files <- list.files("./data/enm/layers/")
files <- files[!files == "Modern.asc"]

master <- data.frame()

for(i in files){
  name <- i
  
  lats <- lat_bins(size = lat_bin_size)
  lats$sst <- NA
  lats$dem <- NA
  
  r1 <- raster(paste("./data/enm/layers/", name, "/sst_ann.asc", sep =""))
  r2 <- raster(paste("./data/enm/layers/", name, "/dem.asc", sep =""))
  
  r2[r2 >= 200] <- NA
  r2[!is.na(r2)] <- 1
  #plot(r2)
  r2 <- mask(x = area_ras, mask = r2)
  
  r1 <- data.frame(rasterToPoints(r1))
  r2 <- data.frame(rasterToPoints(r2))
  
  for(j in 1:nrow(lats)){
    tmp <- subset(r1, y <= lats$max[j] & y >= lats$min[j])
    lats$sst[j] <- mean(tmp$sst_ann)
    
    tmp <- subset(r2, y <= lats$max[j] & y >= lats$min[j])
    lats$dem[j] <- sum(tmp$layer)
  }
  
  lats$stage <- name
  
  master <- rbind.data.frame(master, lats)
  
}

write.csv(master, "./results/Variables/lat_variables.csv", row.names = FALSE)

beepr::beep(2)