gCentroid_weight <- function(x){
  area_ras <- suppressWarnings(raster::area(x))
  area_ras <- area_ras/raster::cellStats(area_ras, stat = 'max')
  r <- x * area_ras
  r <- data.frame(raster::rasterToPoints(r))
  r <- subset(r, layer > 0)
  
  Nr <- subset(r, y >= 0)
  Sr <- subset(r, y <= 0)
  
  Nr.y <- weighted.mean(x = Nr$y, w = Nr$layer)
  Nr.x <- weighted.mean(x = Nr$x, w = Nr$layer)
  
  Sr.y <- weighted.mean(x = Sr$y, w = Sr$layer)
  Sr.x <- weighted.mean(x = Sr$x, w = Sr$layer)
  
  wm <- cbind.data.frame(Nr.x, Nr.y, Sr.x, Sr.y)
  colnames(wm) <- c("n_x", "n_y", "s_x", "s_y")
  return(wm)
}

#example
#x <- raster("./results/Predictions/Binary/MaxSSS/Modern.asc")
#gCentroid_weight(x = x)
