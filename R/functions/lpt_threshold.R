lpt_threshold <- function(x, xy){
  threshold <- raster::extract(x = x, y = xy)
  threshold <- min(threshold, na.rm = TRUE)
  return(threshold)
}

#example
#x <- raster(res = 1, val = 1)
#xy <- c(12, 49)
#lpt_threshold(x = x, xy = xy)
