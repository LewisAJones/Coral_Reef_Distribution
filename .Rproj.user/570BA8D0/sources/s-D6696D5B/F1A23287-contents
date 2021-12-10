#model performance
library(raster)
source("./R/options.R")
collections <- read.csv("./data/occurrences/PARED_subsampled.csv")[,c("interval_name","P.Long", "P.Lat")]
stages <- read.csv("./data/stage_bins.csv")
stages <- stages[1:52,] #subset to stages of interest
#---------------------------------
#LPT
files <- list.files("./results/Predictions/Binary/LPT/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
stk <- stack(paste("./results/Predictions/Binary/LPT/", files, sep = ""))

master <- data.frame()

for(i in 1:nlayers(stk)){
  #load rasters
  r <- stk[[i]]
  #get raster name for later use
  name <- names(r)
  #subset collection data
  xy <- subset(collections, interval_name == name)[,c("P.Long", "P.Lat")]
  #remove points falling inside continents
  ext <- extract(x = r, y = xy)
  vec <- which(!is.na(ext))
  xy <- xy[vec,]
  
  #skip if no points for testing
  if(nrow(xy) == 0){success <- NA; failure <- NA; n <- 0; perf <- NA; buffer_perf <- NA; upper <- NA; mean <- NA; lower <- NA; pval <- NA
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, upper, mean, lower, pval)
  master <- rbind.data.frame(master, df)
  next}
  
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
    cells <- r[cells]
  }, simplify = TRUE)
  buffer_ext <- colSums(buffer_ext, na.rm = TRUE)
  buffer_ext[buffer_ext > 1] <- 1
  buffer_perf <- (sum(buffer_ext)/length(buffer_ext))*100
  
  #calculate random predictive performance
  r <- getValues(r)
  #r <- na.omit(r)
  r[is.na(r)] <- 0
  xy_rand <- replicate(n = 1000, sample(x = r, size = n, replace = TRUE), simplify = FALSE)
  rand_perf <- data.frame(dplyr::bind_cols(xy_rand))
  vals <- (colSums(rand_perf)/length(ext))*100
  rand_perf <- t(data.frame(Rmisc::CI(vals, ci = 0.95)))
  
  #wilcox test
  pval <- wilcox.test(x = perf, y = vals, alternative = "greater")$p.value
  
  #bind data
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, rand_perf, pval)
  rownames(df) <- NULL
  master <- rbind.data.frame(master, df)
}

dir.create("./results/Performance/")
master <- left_join(x = master, y = stages, by = c("name" = "interval_name"))
master <- master[order(master$mid_ma),]
write.csv(master, "./results/Performance/LPT_results.csv", row.names = FALSE)

#maxSSS
files <- list.files("./results/Predictions/Binary/MaxSSS/", pattern = ".asc")
files <- files[!files == "Modern.asc"]
stk <- stack(paste("./results/Predictions/Binary/MaxSSS/", files, sep = ""))

master <- data.frame()

for(i in 1:nlayers(stk)){
  #load rasters
  r <- stk[[i]]
  #get raster name for later use
  name <- names(r)
  #subset collection data
  xy <- subset(collections, interval_name == name)[,c("P.Long", "P.Lat")]
  #remove points falling inside continents
  ext <- extract(x = r, y = xy)
  vec <- which(!is.na(ext))
  xy <- xy[vec,]
  
  #skip if no points for testing
  if(nrow(xy) == 0){success <- NA; failure <- NA; n <- 0; perf <- NA; buffer_perf <- NA; upper <- NA; mean <- NA; lower <- NA; pval <- NA
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, upper, mean, lower, pval)
  master <- rbind.data.frame(master, df)
  next}
  
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
    cells <- r[cells]
  }, simplify = TRUE)
  buffer_ext <- colSums(buffer_ext, na.rm = TRUE)
  buffer_ext[buffer_ext > 1] <- 1
  buffer_perf <- (sum(buffer_ext)/length(buffer_ext))*100
  
  #calculate random predictive performance
  r <- getValues(r) 
  #r <- na.omit(r)
  r[is.na(r)] <- 0
  xy_rand <- replicate(n = 1000, sample(x = r, size = n, replace = TRUE), simplify = FALSE)
  rand_perf <- data.frame(dplyr::bind_cols(xy_rand))
  vals <- (colSums(rand_perf)/length(ext))*100
  rand_perf <- t(data.frame(Rmisc::CI(vals, ci = 0.95)))
  
  #wilcox test
  pval <- wilcox.test(x = perf, y = vals, alternative = "greater")$p.value
  
  #bind data
  df <- cbind.data.frame(name, success, failure, n, perf, buffer_perf, rand_perf, pval)
  rownames(df) <- NULL
  master <- rbind.data.frame(master, df)
}
master <- left_join(x = master, y = stages, by = c("name" = "interval_name"))
master <- master[order(master$mid_ma),]
write.csv(master, "./results/Performance/MaxSSS_results.csv", row.names = FALSE)

beepr::beep(2)
