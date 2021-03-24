#area plot

# Lewis A. Jones
# March 2020
#---------------------------------
library(ggplot2)
library(ggpubr)
library(viridis)
library(grid)
stages <- read.csv("./data/stage_bins.csv")
stages <- subset(stages, max_ma < 250)
stages <- subset(stages, max_ma >= 3.6)
stages$duration <- stages$max_ma - stages$min_ma
stages$periodcol <- NA
periods <- read.csv("./data/period_bins.csv")
periods$duration <- periods$max_ma - periods$min_ma
#stages <- stages[1:52,] #subset to stages of interest
periods <- periods[2:6,] #subset to periods of interest

periods$color <- "white"

data <- read.csv("./results/Area/area_lat.csv")
data <- left_join(x = data, y = stages, by = c("stage" = "interval_name"))

data$LPT_area <- data$LPT_area/max(data$LPT_area, na.rm = TRUE)
data$MaxSSS_area <- data$MaxSSS_area/max(data$MaxSSS_area, na.rm = TRUE)

#---------------------------------
for(i in 1:nrow(periods)){
  for(j in 1:nrow(stages)){
    if(stages$mid_ma[j] <= periods$max_ma[i] & stages$mid_ma[j] >= periods$min_ma[i]){
      stages$periodcol[j] <- paste(periods$color[i])}
  }
}
#---------------------------------


p1 <- ggplot() + 
  geom_tile(data = data, aes(x = mid_ma, y = mid, fill = LPT_area, width = duration, height = 20), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits = c(0, 1), breaks=seq(0, 1, by= 0.2), labels = seq(0, 1, by= 0.2)) +
  #geom_tile(data = NULL, aes(x = 298.900/2, y = 95, width = 298.900, height = 10), colour = "black", fill= "black")+
  geom_tile(data = stages, aes(x = mid_ma, y = 95, width = duration, height = 10), colour = NA, fill= stages$color)+
  geom_text(data = periods, aes(x = mid_ma, y = 95, label = abbrev), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 60)) +
  scale_x_reverse(expand=c(0,0)) +
  #coord_fixed(ratio = 0.8) +
  labs(x = "", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "LTP") +
  theme(
    axis.text.x=element_text(size = 16, vjust = -1, angle = 0),
    axis.text.y=element_text(size = 16, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 16, face = "bold", vjust = -4, colour = "Black"),
    axis.title.y=element_text(size = 16, face = "bold", vjust = 4, colour = "black"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 10, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(2.5), "cm"),
    legend.key.height = unit(c(0.8), "cm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.title = element_text(size = 16, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.5,1,0.5,0.5), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    aspect.ratio = 0.5)

p1 <- p1 + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Standardised habitat area", title.position = "top", title.hjust = 0.5, title.vjust = -7))

p1

p2 <- ggplot() + 
  geom_tile(data = data, aes(x = mid_ma, y = mid, fill = MaxSSS_area, width = duration, height = 20), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits = c(0, 1), breaks=seq(0, 1, by= 0.2), labels = seq(0, 1, by= 0.2)) +
  #geom_tile(data = NULL, aes(x = 298.900/2, y = 95, width = 298.900, height = 10), colour = "black", fill= "black")+
  geom_tile(data = stages, aes(x = mid_ma, y = 95, width = duration, height = 10), colour = NA, fill= stages$color)+
  geom_text(data = periods, aes(x = mid_ma, y = 95, label = abbrev), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 60)) +
  scale_x_reverse(expand=c(0,0)) +
  #coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "MaxSSS") +
  theme(
    axis.text.x=element_text(size = 16, vjust = -1, angle = 0),
    axis.text.y=element_text(size = 16, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 16, face = "bold", vjust = -4, colour = "Black"),
    axis.title.y=element_text(size = 16, face = "bold", vjust = 4, colour = "black"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 10, face = "bold"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(2.5), "cm"),
    legend.key.height = unit(c(0.8), "cm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.title = element_text(size = 16, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.5,1,0.5,0.5), "cm"),
    panel.background=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank(),
    aspect.ratio = 0.5)


p2 <- p2 + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = "Standardised habitat area", title.position = "top", title.hjust = 0.5, title.vjust = -7))

p2

p <- ggarrange(p1, p2, ncol=1, nrow=2, labels = "AUTO", align = "v", font.label = list(size = 18), common.legend = TRUE, legend = "bottom")
p

ggsave("./figures/habitat_area_lat_standard.png",width = 230, height = 280, units = "mm", dpi = 600)
