# reef zone plot
# Lewis A. Jones
# March 2021
#---------------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)
stages <- read.csv("./data/stage_bins.csv")
periods <- read.csv("./data/period_bins.csv")
stages <- stages[1:52,] #subset to stages of interest
periods <- periods[2:6,] #subset to periods of interest
periods$color <- "white"
data <- read.csv("./results/Reef_zone/reef_zone_calc.csv")
data <- left_join(x = data, y = stages, by = c("stage" = "interval_name"))
occ <- read.csv("./data/occurrences/PARED_subsampled.csv")
occ <- left_join(x = occ, y = stages, by = c("interval_name" = "interval_name"))
#---------------------------------

throwing_shade <- periods[seq(1, nrow(periods), 2),]

pts <- c("Reef localities" = 19)

cols <- c("Max" = "black",
          "95% quantiles" = "red")

lty <- c("Max" = 1,
         "95% quantiles" = 2)


p1 <- ggplot(data = data) +
  geom_rect(data = periods, mapping=aes(xmin=min(data$min_ma), xmax=0, ymin= -68, ymax= -60), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = throwing_shade, aes(xmin=min_ma, xmax=max_ma, ymin=-68, ymax=75), color= "grey95", alpha=0.2)  +
  geom_rect(data = periods, mapping=aes(xmin=min_ma, xmax=max_ma, ymin= -68, ymax= -60), colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_ma+max_ma)/2, y= -64, label = abbrev), colour = "black", alpha=1)  +
  geom_hline(aes(yintercept = 0), size = 1, colour = "black", alpha = 0.7) +
  geom_point(data = occ, aes(x = mid_ma, y = P.Lat, shape = "Reef localities"), colour = "#0c2c84", size = 1.5, alpha = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(LPT_max), colour = "Max", linetype = "Max"), size = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(LPT_min)), linetype = 1, size = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(LPT_upper), colour = "95% quantiles",  linetype = "95% quantiles"), size = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(LPT_lower)), linetype = 2, colour = "red", size = 1) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-68, 75), breaks = seq(-60, 75, 30)) +
  scale_colour_manual(name = "L1", values = cols) +
  scale_linetype_manual(name = "L1", values = lty) +
  scale_shape_manual(name = "L1", values = pts) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")"))), subtitle = "") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = NA),
        legend.position = c(0.88, 0.91),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-0.2, "cm"),
        legend.key.size = unit(0.75, "lines"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 5, hjust = 0.6),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 0.5)
p1
ggsave("./figures/reef_zone_plot_LPT.png", plot = p1, width = 200, height = 110, units = "mm", dpi = 600)
p1 <- p1 + labs(subtitle = "Least training presence")


p2 <- ggplot(data = data) +
  geom_rect(data = periods, mapping=aes(xmin=min(data$min_ma), xmax=0, ymin= -68, ymax= -60), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = throwing_shade, aes(xmin=min_ma, xmax=max_ma, ymin=-68, ymax=75), color= "grey95", alpha=0.2)  +
  geom_rect(data = periods, mapping=aes(xmin=min_ma, xmax=max_ma, ymin= -68, ymax= -60), colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_ma+max_ma)/2, y= -64, label = abbrev), colour = "black", alpha=1)  +
  geom_hline(aes(yintercept = 0), size = 1, colour = "black", alpha = 0.7) +
  geom_point(data = occ, aes(x = mid_ma, y = P.Lat, shape = "Reef localities"), colour = "#0c2c84", size = 1.5, alpha = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(MaxSSS_max), colour = "Max", linetype = "Max"), size = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(MaxSSS_min)), linetype = 1, size = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(MaxSSS_upper), colour = "95% quantiles",  linetype = "95% quantiles"), size = 1) +
  geom_line(aes(x = mid_ma, y = as.numeric(MaxSSS_lower)), linetype = 2, colour = "red", size = 1) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-68, 75), breaks = seq(-60, 75, 30)) +
  scale_colour_manual(name = "L1", values = cols) +
  scale_linetype_manual(name = "L1", values = lty) +
  scale_shape_manual(name = "L1", values = pts) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = NA),
        legend.position = c(0.88, 0.91),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-0.2, "cm"),
        legend.key.size = unit(0.75, "lines"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 5, hjust = 0.6),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 0.5)
p2
ggsave("./figures/reef_zone_plot_MaxSSS.png", plot = p2, width = 200, height = 110, units = "mm", dpi = 600)

p2 <- p2 + labs(subtitle = "Maximising the sum of sensitivity and specificity")

p <- ggarrange(p1, p2, ncol=1, nrow=2, labels = "AUTO", align = "v", font.label = list(size = 18))
p
ggsave("./figures/reef_zone_plot.jpg", plot = p, width = 65, height = 75, units = "mm", dpi = 600, scale = 3)
