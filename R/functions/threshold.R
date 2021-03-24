threshold <- function(p, a){
  e <- dismo::evaluate(p = p, a = a)
  LPT <- dismo::threshold(x = e, stat = "no_omission")
  MaxSSS <- dismo::threshold(x = e, stat = "spec_sens")
  return(cbind.data.frame(LPT, MaxSSS))
}
