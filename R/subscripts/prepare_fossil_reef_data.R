#-------------------------------------------------
# Project: Coral_Reef_Distribution
#
# Date: 2021-10-29
# Author: Lewis A. Jones
# Copyright (c) Lewis A. Jones, 2021
# Email: LewisA.Jones@outlook.com
#
# Script name:
# prepare_fossil_data.R
#
# Script description:
# Prepare fossil occurrence data
#
#-------------------------------------------------
#Load libraries, functions and analyses options
library(dplyr)
library(stringr)
library(raster)
library(chronosphere)
source("./R/options.R")
source("./R/functions/bin_assignment.R")
#-------------------------------------------------
#load stage bins
bins <- read.csv("./data/stage_bins.csv")
#round mid age
bins$mid_ma <- round(bins$mid_ma, 3)
#Load PARED data
PARED <- read.csv("./data/occurrences/PARED_06_10_2021.csv")
collections <- as.integer(PARED$collection)
collections <- na.omit(collections)
r_number <- as.integer(PARED$r_number)
PBDB <- read.csv("./data/occurrences/PBDB_data_12_10_2021.csv")
#-------------------------------------------------
#PREPARE PARED DATA
#retain only coral reefs
PARED <- subset(PARED, biota_main_t == "Corals" | biota_sec_text == "Corals")
#retain only outcropping reefs
PARED <- subset(PARED, subsurface_text == "Outcropping reef")
#retain only true reefs
PARED <- subset(PARED, type_text == "True reef")
#remove cold water/temperate coral reefs
PARED <- subset(PARED, tropical_text == "Tropical or unknown")
#create empty cells for populating
PARED$mid_ma <- NA
PARED$prob <- NA

#assign bin based on probability duration
for(i in 1:nrow(PARED)){
  print(round(i/nrow(PARED)*100)) #print percentage
  tmp <- assign_bins_prob(max = PARED$max_ma[i], min = PARED$min_ma[i], bins = bins$min_ma) #assign bins based on age duration
  PARED$mid_ma[i] <- as.numeric(tmp$mid_ma)
  PARED$prob[i] <- as.numeric(tmp$prob)
}

#round mid age
PARED$mid_ma <- round(PARED$mid_ma, 3)

#retain data with 0.5 probability of being in assigned stage
PARED <- subset(PARED, prob >= 0.5)
#drop columns to avoid duplication
PARED <- subset(PARED, select=-c(max_ma, min_ma))
#join stage names based on assigned mid age
PARED <- inner_join(x = PARED, y = bins, by = c("mid_ma")) 
#remove data younger than 2.588 Ma
PARED <- subset(PARED, min_ma >= 2.588)
#remove data older than 247.2 Ma
PARED <- subset(PARED, max_ma <= 247.2)
#load PARED rotations
rotations <- read.csv("/Users/lewis/Documents/Data/Rotations/PARED_rotated_02_11_2021.csv")
#add id columns for joining data
rotations$join <- paste(rotations$r_number, rotations$stage, sep = "_")
PARED$join <- paste(PARED$r_number, PARED$interval_name, sep = "_")
#join data to Getech rotations
PARED <- left_join(x = PARED, rotations[,c("join", "P.Long", "P.Lat")], by = "join")
#remove data without GETECH palaeocoordinates
PARED <- subset(PARED, !is.na(P.Long) & !is.na(P.Lat))
#-------------------------------------------------
#PREPARE PBDB DATA
#retain only true reefs based on environment and lithology
PBDB <- PBDB %>% filter(!environment %in% c("perireef or subreef"))
PBDB <- PBDB %>% filter(!lithology1 %in% c("shale", 
                                           "marl", 
                                           "claystone", 
                                           "wackestone",
                                           "breccia", 
                                           "phosphorite", 
                                           "volcaniclastic", 
                                           "tuff",
                                           "siliciclastic",
                                           "conglomerate",
                                           "sandstone",
                                           "siltstone",
                                           "not reported"))

#filter by reef number for pre-removed reef data
remove <- r_number[!r_number %in% PARED$r_number]
remove <- paste("Reef ", remove, sep = "")
PBDB <- PBDB %>% filter(!collection_aka %in% remove)

PBDB$mid_ma <- NA
PBDB$prob <- NA

#assign bin based on probability duration
for(i in 1:nrow(PBDB)){
  print(round(i/nrow(PBDB)*100)) #print percentage
  tmp <- assign_bins_prob(max = PBDB$max_ma[i], min = PBDB$min_ma[i], bins = bins$min_ma) #assign bins based on age duration
  PBDB$mid_ma[i] <- tmp$mid_ma
  PBDB$prob[i] <- tmp$prob
}

#round mid age
PBDB$mid_ma <- round(PBDB$mid_ma, 3)

#retain data with 0.5 probability of being in assigned stage
PBDB <- subset(PBDB, prob >= 0.5)
#drop columns to avoid duplication
PBDB <- subset(PBDB, select=-c(max_ma, min_ma))
#join stage names based on assigned mid age
PBDB <- inner_join(x = PBDB, y = bins, by = c("mid_ma")) 
#remove data younger than 2.588 Ma
PBDB <- subset(PBDB, min_ma >= 2.588)
#remove data older than 247.2 Ma
PBDB <- subset(PBDB, max_ma <= 247.2)
#load PBDB rotations
rotations <- read.csv("/Users/lewis/Documents/Data/Rotations/Getech_rotated_collections_04_10_2021.csv")
#add id columns for joining data
rotations$join <- paste(rotations$collection_no, rotations$stage_bin, sep = "_")
PBDB$join <- paste(PBDB$collection_no, PBDB$interval_name, sep = "_")
#join data to Getech rotations
PBDB <- left_join(x = PBDB, rotations[,c("join", "P.Long", "P.Lat")], by = "join")
#remove data without GETECH palaeocoordinates
PBDB <- subset(PBDB, !is.na(P.Long) & !is.na(P.Lat))
#-------------------------------------------------
#subset and tidy data
PBDB$r_number <- NA
PBDB <- PBDB[,c("collection_no","r_number", "interval_name", "max_ma","mid_ma", "min_ma", "lng", "lat", "P.Long", "P.Lat")]
PBDB$data_source <- c("PaleoBioDB")
PARED$collection_no <- PARED$collection
PARED$collection_no[PARED$collection_no == ""] <- NA
PARED$collection_no[PARED$collection_no == 0] <- NA
PARED$lng <- PARED$longit
PARED$lat <- PARED$lat
PARED <- PARED[,c("collection_no", "r_number", "interval_name", "max_ma", "mid_ma", "min_ma", "lng", "lat", "P.Long", "P.Lat")]
PARED$data_source <- c("PaleoReefDB")
#bind data
PARED <- rbind.data.frame(PARED, PBDB)
#-------------------------------------------------
#Rotate data with PALEOMAP for sensitivity testing
#get plate model
pm <- fetch("paleomap", "model", datadir="./data/model/") #download plate model

PARED$paleolng <- NA
PARED$paleolat <- NA


pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                     max = nrow(PARED), # Maximum value of the progress bar
                     style = 3,    # Progress bar style (also available style = 1 and style = 2)
                     width = 50,   # Progress bar width. Defaults to getOption("width")
                     char = "=")   # Character used to create the bar

for(i in 1:nrow(PARED)){
  
  coords <- reconstruct(x = PARED[i, c("lng", "lat")], #coordinates of data
                        age = PARED$mid_ma[i], #age of data 
                        model=pm, #plate model
                        dir = "./data/model/", #directory of plate model
                        #path.gplates="/Volumes/GPlates-2.2.0-Darwin-x86_64/",
                        cleanup = TRUE)
  
  PARED$paleolng[i] <- coords[,c("lng")]
  PARED$paleolat[i] <- coords[,c("lat")]
  
  setTxtProgressBar(pb, i)
}

#write data
write.csv(PARED, "./data/occurrences/PARED_cleaned.csv", row.names = FALSE)
#-------------------------------------------------
#filter data on continents
stages <- unique(PARED$interval_name)
#create empty dataframe
master <- data.frame()
#run for loop
for(i in stages){
  tmp <- subset(PARED, interval_name == i)
  DEM <- raster(paste("./data/enm/layers/", i, "/dem.asc", sep = ""))
  ext <- extract(x = DEM, y = tmp[,c("P.Long","P.Lat")], df = TRUE)
  ext <- which(!is.na(ext$dem))
  tmp <- tmp[ext,]
  master <- rbind.data.frame(master, tmp)
}
#write data
write.csv(master, "./data/occurrences/PARED_clip.csv", row.names = FALSE)
#assign data
PARED <- master
#spatially subsample data
r <- raster(res = res)
#create empty data frame
master <- data.frame()
#get unique stages of data
stages <- unique(PARED$interval_name)
#run for loop
for(i in stages){
  interval_name <- i
  df <- subset(PARED, interval_name == i)
  ras <- rasterize(x = df[,c("P.Long", "P.Lat")], y = r, field = 1) 
  pts_ras <- data.frame(rasterToPoints(ras))
  pts_ras <- data.frame(pts_ras[,c("x","y")])
  pts_ras <- cbind.data.frame(pts_ras, interval_name)
  master <- rbind.data.frame(master, pts_ras)
}
#update column names
colnames(master) <- c("P.Long", "P.Lat", "interval_name")
#write data
write.csv(master, "./data/occurrences/PARED_subsampled.csv", row.names = FALSE)
#notification
beepr::beep(2)

