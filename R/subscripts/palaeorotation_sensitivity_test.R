#load libraries

#sensitivity between models
library(chronosphere)
library(dplyr)
#collection_no 34647 coordinates
xy <-cbind(long=c(74.6667), lat=c(37.3333))
#approx. mid age of collection
age <- 218
#try different rotation models
models <- c("PALEOMAP", "MULLER2016", "SETON2012", "GOLONKA", "MATTHEWS2016")
#run across different models
df <- lapply(models, function(x){
  tmp <- as.data.frame(reconstruct(x = xy, age = age, model = x))
  tmp$model <- as.character(x)
  tmp
})
names(df) <- models
df <- bind_rows(df)

#sensitivity between plate boundaries
#load libraries
library(chronosphere)
library(dplyr)
#collection_no 34647 coordinates
xy <-cbind(long=c(rep(74.6667, 11)), lat=c(seq(37, 38, 0.1)))
#approx. mid age of collection
age <- 218
models <- c("PALEOMAP")
#run across different coordinates
df <- as.data.frame(reconstruct(x = xy, age = age, model = models))
df <- cbind.data.frame(xy, df)
