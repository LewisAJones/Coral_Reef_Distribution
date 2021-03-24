#data preparation
library(dplyr)
library(raster)
source("./R/options.R")

#PARED data

data <- read.csv("./data/occurrences/PARED/PARED_2021_02_19_text.csv")
collections <- read.csv("./data/occurrences/Getech_rotated_collections_09_02_2021.csv")
data <- subset(data, collection != "")
data <- subset(data, biota_main_t == "Corals" | biota_sec_text == "Corals")
data <- subset(data, max_ma < 300)

colls <- unique(data$collection)

collections <- collections %>% filter(collection_no %in% colls)

r <- raster(res = res)

stages <- unique(collections$stage_bin)

master <- data.frame()

for(i in stages){
  stage <- i
  df <- subset(collections, stage_bin == i)
  ras <- rasterize(x = df[,c("PLong", "PLat")], y = r, field = 1) 
  pts_ras <- data.frame(rasterToPoints(ras))
  pts_ras <- data.frame(pts_ras[,c("x","y")])
  pts_ras <- cbind.data.frame(pts_ras, stage)
  master <- rbind.data.frame(master, pts_ras)
}

write.csv(master, "./data/occurrences/PARED/subsampled_collections.csv", row.names = FALSE)


#PBDB data

data <- read.csv("./data/occurrences/pbdb_data_23_02_2021.csv")
collections <- read.csv("./data/occurrences/Getech_rotated_collections_09_02_2021.csv")

colls <- unique(data$collection_no)

collections <- collections %>% filter(collection_no %in% colls)
collections$x <- collections$PLong
collections$y <- collections$PLat
collections$stage <- collections$stage_bin

write.csv(collections, "./data/occurrences/PBDB_collections.csv", row.names = FALSE)

r <- raster(res = res)

stages <- unique(collections$stage_bin)

master <- data.frame()

for(i in stages){
  stage <- i
  df <- subset(collections, stage_bin == i)
  ras <- rasterize(x = df[,c("PLong", "PLat")], y = r, field = 1) 
  pts_ras <- data.frame(rasterToPoints(ras))
  pts_ras <- data.frame(pts_ras[,c("x","y")])
  pts_ras <- cbind.data.frame(pts_ras, stage)
  master <- rbind.data.frame(master, pts_ras)
}

write.csv(master, "./data/occurrences/subsampled_PBDB_collections.csv", row.names = FALSE)


