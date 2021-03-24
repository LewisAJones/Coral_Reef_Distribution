library(ggplot2)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)

hab_plot <- function(x, xy, col = "darkcyan", name = "name", lab = "label", heat = FALSE){

  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  if(heat == TRUE){
    df <- as.data.frame(x, xy = TRUE)
    colnames(df) <- c("x", "y", "value")
    
    p <- ggplot() +
      geom_sf(data = world, fill = "white", colour = "white") +
      geom_tile(data = df, aes(x=x, y=y, fill = value))  +
      geom_point(data = xy, aes(x=x, y=y), shape = 21, size = 1, fill = "white", colour = "black", alpha =0.75)  +
      scale_fill_viridis_c(option = "A", begin = 0.1, na.value = "darkgrey") +
      geom_text(aes(x = -130, y = -82, label = name), colour = "white") +
      geom_text(aes(x = -170, y = 82, label = lab), colour = "white", hjust = "left") +
      #scale_fill_gradient2(mid = "#2166ac", high = "#a50026", na.value = "darkgrey") +
      xlim(-180, 178) +
      theme_map() +
      theme(legend.position="") +
      #theme(legend.key.width=unit(2, "cm"))
      theme(plot.margin = margin(0,0,0,0, "cm"))
    
    p
    
  }
  
  else{
    x[is.na(x)] <- 0.5
    df <- as.data.frame(x, xy = TRUE)
    colnames(df) <- c("x", "y", "value")
    
  p <- ggplot() +
    geom_sf(data = world, fill = "white", colour = "white") +
    geom_tile(data = df, aes(x=x, y=y, fill = as.factor(value)))  +
    geom_point(data = xy, aes(x=x, y=y), shape = 21, size = 1.5, fill = "white", colour = "black", alpha = 0.75)  +
    geom_rect(aes(xmin = -180, xmax = -50, ymin = 73, ymax = 90), fill = "white", alpha = 0.75) +
    #geom_text(aes(x = -175, y = -82, label = name), hjust = "left") +
    geom_text(aes(x = -175, y = 82, label = lab), hjust = "left") +
    #labs(subtitle = lab) +
    scale_fill_manual(values = c("aliceblue", "darkgrey", col)) +
    xlim(-180, 178) +
    theme_map() +
    theme(legend.position="") +
    #theme(plot.subtitle = element_text(hjust = 0.08, vjust = -0.75, size = 10)) +
    #theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
    #theme(legend.key.width=unit(2, "cm"))
    theme(plot.margin = margin(0,0,0,0, "cm"))
  
  p
  
  }
  
  return(p)
}

#example
#collections <- read.csv("./data/occurrences/subsampled_PBDB_collections.csv")
#xy <- subset(collections, stage == "Albian")
#x <- raster("./results/Predictions/Median/Albian.asc")
#hab_plot(x = x, xy = xy, heat = TRUE)
