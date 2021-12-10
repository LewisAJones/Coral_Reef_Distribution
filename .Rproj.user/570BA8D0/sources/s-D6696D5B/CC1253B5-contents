#MESS analyses
library(raster)
library(dplyr)

collections <- read.csv("./data/occurrences/PARED_subsampled.csv")
stages <- unique(collections$interval_name)
files <- list.files("./results/RUN/", pattern = "_novel.asc")

master <- data.frame()

for(i in stages){
  r <- raster(paste("./results/Predictions/Binary/LPT/", i, ".asc", sep =""))
  #extract stage name
  name <- i
  #subset collection data
  xy <- subset(collections, interval_name == name)[,c("P.Long", "P.Lat")]
  ext <- extract(x = r, y = xy)
  #vec <- which(ext == 0)
  #xy <- xy[vec,]
  
  if(nrow(xy) == 0){next}
  
  files <- list.files(paste("./data/enm/layers/", i, "/", sep = ""), pattern = ".asc")
  
  r <- stack(paste("./data/enm/layers/", i, "/", files, sep = ""))
  ext <- extract(x = r, y = xy)
  
  df <- cbind.data.frame(xy, ext)
  df$stage <- name

  master <- rbind.data.frame(master, df)
}

beepr::beep(2)
