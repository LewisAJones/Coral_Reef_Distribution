library(raster)

stages <- read.csv("./data/stage_bins.csv")
stages <- stages[6:50,]
collections <- read.csv("./data/occurrences/PARED_subsampled.csv")

master <- data.frame()

for(i in 1:nrow(stages)){
  #extract stage name
  name <- stages$interval_name[i]
  #subset collection data
  xy <- subset(collections, interval_name == name)[,c("P.Long", "P.Lat")]
  
  r <- raster(paste("./outputs/reef_0_", stages$interval_name[i], "_novel_limiting.asc", sep = ""))
  #r[r != 5] <- 1
  #r[r == 5] <- NA
  
  ext <- extract(x = r, y = xy)
  vec <- which(!is.na(ext))
  df <- cbind.data.frame(xy, ext)
  df <- df[vec,]
  
  if(nrow(df) == 0){next}
  
  df$stage <- name
  
  master <- rbind.data.frame(master, df)

}
test <- subset(master, ext != 4)
nrow(subset(master, ext == 3)) #minimum sst
nrow(subset(master, ext == 1)) #maximum sst
nrow(subset(master, ext == 2)) #minimum irr
nrow(subset(master, ext == 0)) #maximum irr
