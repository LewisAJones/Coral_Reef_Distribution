lat_zone <- function(x){
  r <- data.frame(raster::rasterToPoints(x))
  r <- subset(r, layer > 0)
  max_lat <- max(r$y, na.rm = TRUE)
  min_lat <- min(r$y, na.rm = TRUE)
  lower <- quantile(r$y, probs = c(0.025), names = FALSE)
  upper <- quantile(r$y, probs = c(0.975), names = FALSE)
  lats <- cbind.data.frame(max_lat, min_lat, lower, upper)
  return(lats)
}
#example
#x <- raster(res = 1, val = 1)
#lat_zone(x = x)
