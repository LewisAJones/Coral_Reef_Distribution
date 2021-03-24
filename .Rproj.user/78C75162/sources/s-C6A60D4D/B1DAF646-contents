library(raster)

stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,]
collections <- read.csv("./data/occurrences/subsampled_PBDB_collections.csv")

master <- data.frame()

for(i in 1:nrow(stages)){
  #extract stage name
  name <- stages$interval_name[i]
  #subset collection data
  xy <- subset(collections, stage == name)[,c("x", "y")]
  
  r <- raster(paste("./results/RUN/reef_0_", stages$interval_name[i], "_novel_limiting.asc", sep = ""))
  #r[r != 5] <- 1
  r[r == 5] <- NA
  
  ext <- extract(x = r, y = xy)
  vec <- which(!is.na(ext))
  df <- cbind.data.frame(xy, ext)
  df <- df[vec,]
  
  if(nrow(df) == 0){next}
  
  df$stage <- name
  
  master <- rbind.data.frame(master, df)

}
