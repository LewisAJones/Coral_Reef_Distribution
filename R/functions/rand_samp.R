rand_samp <- function(x, n, reps){
  df <- lapply(1:reps, function(i){
    r <- raster::sampleRandom(x = x, size = n, xy = TRUE, na.rm = TRUE)[,c("x", "y")]
    
    if(length(r) == 2){r <- t(data.frame(r))}
    
    row.names(r) <- NULL
    
    r
  })
  return(df)
}

#example
#x <- raster(res = 1, val = 1)
#n <- 10
#reps <- 5
#rand_samp(x = x, n = n, reps = reps)
