lat_bins <- function(size){
  bins <- 180/size
  df <- seq(from = -90, to = 90, by = size)
  min <- df[1:bins]
  max <- df[1:bins]+size
  mid <- (max+min)/2
  bins <- 1:bins
  df <- cbind(max, mid, min)
  df <- df[order(-max),]
  df <- cbind.data.frame(bins, df)
  return(df)
}
