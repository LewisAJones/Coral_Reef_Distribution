#model performance
library(raster)
source("./R/functions/rand_samp.R")
source("./R/options.R")

collections <- read.csv("./data/occurrences/subsampled_PBDB_collections.csv")

#LPT

files <- list.files("./results/Predictions/Binary/LPT/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
master <- data.frame()

for(i in files){
  #load rasters
  r <- raster(paste("./results/Predictions/Binary/LPT/", i, sep =""))
  #extract stage name
  name <- tools::file_path_sans_ext(i)
  #subset collection data
  xy <- subset(collections, stage == name)[,c("x", "y")]
  
  #skip if no points
  if(nrow(xy) == 0){success <- NA; failure <- NA; n <- 0; perf <- NA; buffer_perf <- NA; upper <- NA; mean <- NA; lower <- NA; pval <- NA
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, upper, mean, lower, pval)
  master <- rbind.data.frame(master, df)
  next}
  
  #remove points falling inside continents
  ext <- extract(x = r, y = xy)
  vec <- which(!is.na(ext))
  xy <- xy[vec,]
  
  #calculate predictive performance
  ext <- extract(x = r, y = xy) #extract data
  success <- sum(ext == 1, na.rm = TRUE) #success
  failure <- sum(ext == 0, na.rm = TRUE) #failed 
  n <- nrow(xy) #number of points
  perf <- (success/n)*100 #calculate percentage
  
  #calculate buffered predictive performance
  buffer_ext <- sapply(1:nrow(xy), function(x){
    pts <- xy[x,] 
    cells <- cellFromXY(object = r, xy = pts)
    cells <- append(cells, adjacent(x = r, cells = cells, directions = 8, pairs = FALSE))
    cells <- extract(x = r, y = cells)
    }, simplify = TRUE)
  buffer_ext <- colSums(buffer_ext, na.rm = TRUE)
  buffer_ext[buffer_ext > 1] <- 1
  buffer_perf <- (sum(buffer_ext)/length(buffer_ext))*100

  #calculate random predictive performance
  
  xy_rand <- rand_samp(x = r, n = length(ext), reps = reps)
  
  rand_perf <- sapply(1:length(xy_rand), function(x){extract(x = r, y = xy_rand[[x]])}, simplify = FALSE)
  names(rand_perf) <- 1:length(rand_perf)
  rand_perf <- data.frame(dplyr::bind_cols(rand_perf, .name_repair = "unique"))
  vals <- (colSums(rand_perf)/length(ext))*100
  rand_perf <- t(data.frame(Rmisc::CI(vals, ci = 0.95)))
  
  #wilcox test
  pval <- wilcox.test(x = vals, mu = perf, alternative = "less")$p.value
  
  #bind data
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, rand_perf, pval)
  rownames(df) <- NULL
  master <- rbind.data.frame(master, df)
}

dir.create("./results/Performance/")

write.csv(master, "./results/Performance/LPT_results.csv", row.names = FALSE)

#maxSSS

files <- list.files("./results/Predictions/Binary/MaxSSS/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
master<- data.frame()

for(i in files){
  #load rasters
  r <- raster(paste("./results/Predictions/Binary/MaxSSS/", i, sep =""))
  #extract stage name
  name <- tools::file_path_sans_ext(i)
  #subset collection data
  xy <- subset(collections, stage == name)[,c("x", "y")]
  
  #skip if no points
  if(nrow(xy) == 0){success <- NA; failure <- NA; n <- 0; perf <- NA; buffer_perf <- NA; upper <- NA; mean <- NA; lower <- NA; pval <- NA
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, upper, mean, lower, pval)
  master <- rbind.data.frame(master, df)
  next}
  
  #remove points falling inside continents
  ext <- extract(x = r, y = xy)
  vec <- which(!is.na(ext))
  xy <- xy[vec,]
  
  #calculate predictive performance
  ext <- extract(x = r, y = xy) #extract data
  success <- sum(ext == 1, na.rm = TRUE) #success
  failure <- sum(ext == 0, na.rm = TRUE) #failed 
  n <- nrow(xy) #number of points
  perf <- (success/n)*100 #calculate percentage
  
  #calculate buffered predictive performance
  buffer_ext <- sapply(1:nrow(xy), function(x){
    pts <- xy[x,] 
    cells <- cellFromXY(object = r, xy = pts)
    cells <- append(cells, adjacent(x = r, cells = cells, directions = 8, pairs = FALSE))
    cells <- extract(x = r, y = cells)
  }, simplify = TRUE)
  buffer_ext <- colSums(buffer_ext, na.rm = TRUE)
  buffer_ext[buffer_ext > 1] <- 1
  buffer_perf <- (sum(buffer_ext)/length(buffer_ext))*100
  
  #calculate random predictive performance
  
  xy_rand <- rand_samp(x = r, n = length(ext), reps = reps)
  
  rand_perf <- sapply(1:length(xy_rand), function(x){extract(x = r, y = xy_rand[[x]])}, simplify = FALSE)
  names(rand_perf) <- 1:length(rand_perf)
  rand_perf <- data.frame(dplyr::bind_cols(rand_perf, .name_repair = "unique"))
  vals <- (colSums(rand_perf)/length(ext))*100
  rand_perf <- t(data.frame(Rmisc::CI(vals, ci = 0.95)))
  
  #wilcox test
  pval <- wilcox.test(x = vals, mu = perf, alternative = "less")$p.value
  
  #bind data
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, rand_perf, pval)
  rownames(df) <- NULL
  master <- rbind.data.frame(master, df)
}

write.csv(master, "./results/Performance/MaxSSS_results.csv", row.names = FALSE)

beepr::beep(2)
