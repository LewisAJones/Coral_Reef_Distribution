#area plot

# Lewis A. Jones
# March 2020
#---------------------------------
library(ggplot2)
library(ggpubr)
library(viridis)
library(grid)
library(dplyr)
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

data$LPT_area <- data$LPT_area/100000
data$MaxSSS_area <- data$MaxSSS_area/100000

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
  scale_fill_viridis(option = "D", direction = 1, limits = c(0, 200), breaks=seq(0, 200, by= 25), labels = seq(0, 200, by=25)) +
  #geom_tile(data = NULL, aes(x = 298.900/2, y = 95, width = 298.900, height = 10), colour = "black", fill= "black")+
  geom_tile(data = stages, aes(x = mid_ma, y = 95, width = duration, height = 10), colour = NA, fill= stages$color)+
  geom_text(data = periods, aes(x = mid_ma, y = 95, label = abbrev), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 60)) +
  scale_x_reverse(expand=c(0,0)) +
  #coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "") +
  theme(
    axis.text.x=element_text(size = 14, vjust = -1, angle = 0),
    axis.text.y=element_text(size = 14, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = "Black"),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = "black"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0, vjust = 1, size = 12, face = "plain"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3), "cm"),
    legend.key.height = unit(c(0.8), "cm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    panel.background=element_rect(fill = "white"),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill = "white"),
    aspect.ratio = 0.5)

p1 <- p1 + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = expression(bold(paste("Suitable habitat area (",10^6~km^2,")",sep=""))), title.position = "top", title.hjust = 0.5, title.vjust = -8.5))
ggsave("./figures/habitat_area_lat_LPT.pdf", plot = p1, width = 200, height = 140, units = "mm", dpi = 600)

p1 <- p1 + labs(subtitle = "Least training presence")

p2 <- ggplot() + 
  geom_tile(data = data, aes(x = mid_ma, y = mid, fill = MaxSSS_area, width = duration, height = 20), colour = "black") +
  scale_fill_viridis(option = "D", direction = 1, limits = c(0, 200), breaks=seq(0, 200, by= 25), labels = seq(0, 200, by=25)) +
  #geom_tile(data = NULL, aes(x = 298.900/2, y = 95, width = 298.900, height = 10), colour = "black", fill= "black")+
  geom_tile(data = stages, aes(x = mid_ma, y = 95, width = duration, height = 10), colour = NA, fill= stages$color)+
  geom_text(data = periods, aes(x = mid_ma, y = 95, label = abbrev), color = "black", size = 4) +
  scale_y_continuous(expand = c(0,0), breaks = seq(-90, 90, 60)) +
  scale_x_reverse(expand=c(0,0)) +
  #coord_fixed(ratio = 0.8) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")")))) +
  theme(
    axis.text.x=element_text(size = 14, vjust = -1, angle = 0),
    axis.text.y=element_text(size = 14, hjust = 1, angle = 0),
    axis.title.x=element_text(size = 14, face = "bold", vjust = -4, colour = "Black"),
    axis.title.y=element_text(size = 14, face = "bold", vjust = 4, colour = "black"),
    legend.position="bottom",
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0, vjust = 1, size = 12, face = "plain"),
    legend.text = element_text(size = 12),
    legend.key.width = unit(c(3), "cm"),
    legend.key.height = unit(c(0.8), "cm"),
    legend.background = element_blank(),
    legend.margin = margin(0, 0, 0, 0, "cm"),
    legend.title = element_text(size = 14, face = "bold", vjust = -6, colour = "white"),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
    panel.background=element_rect(fill = "white"),
    panel.grid.minor=element_blank(),
    plot.background=element_rect(fill = "white"),
    aspect.ratio = 0.5)


p2 <- p2 + guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black", label = TRUE, title = expression(bold(paste("Suitable habitat area (",10^6~km^2,")",sep=""))), title.position = "top", title.hjust = 0.5, title.vjust = -8.5))
ggsave("./figures/habitat_area_lat_MaxSSS.png", plot = p2, width = 200, height = 140, units = "mm", dpi = 600)

p2 <- p2 + labs(subtitle = "Maximising the sum of sensitivity and specificity")

p <- ggarrange(p1, p2, ncol=1, nrow=2, labels = "auto", align = "v", font.label = list(size = 18), common.legend = FALSE, hjust = -2)
p

ggsave("./figures/habitat_area_lat.png",width = 210, height = 280, units = "mm", dpi = 600)

