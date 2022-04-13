# Performance plot
# Lewis A. Jones
# March 2021
#---------------------------------
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(dplyr)
stages <- read.csv("./data/stage_bins.csv")
periods <- read.csv("./data/period_bins.csv")
stages <- stages[1:52,] #subset to stages of interest
periods <- periods[2:6,] #subset to periods of interest
periods$color <- "white"

#---------------------------------

throwing_shade <- periods[seq(1, nrow(periods), 2),]

data <- read.csv("./results/Performance/LPT_results.csv")
data <- data[order(data$mid_ma),]

cols <- c("standard" = "#b10026",
          "buffer" = "#0c2c84",
          "random" = "black")

labels <- c("standard", "buffer", "random")
#labels <- factor(labels, levels = c("standard", "buffer", "random"))

p1 <- ggplot(data = data) +
  geom_rect(data = periods, mapping=aes(xmin=min(data$min_ma), xmax=0, ymin= -7, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = throwing_shade, aes(xmin=min_ma, xmax=max_ma, ymin=-7, ymax=105), color= "grey95", alpha=0.2)  +
  geom_rect(data = periods, mapping=aes(xmin=min_ma, xmax=max_ma, ymin= -7, ymax= 0), colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_ma+max_ma)/2, y= -3.5, label = abbrev), colour = "black", alpha=1)  +
  #geom_hline(aes(yintercept = 0), size = 1, colour = "black", alpha = 0.7) +
  #geom_ribbon(aes(x = mid_ma, ymin = LPT_min, ymax = LPT_max), fill = NA, colour = "black", size = 1, alpha = 0.75) +
  #geom_line(aes(x = mid_ma, y = as.numeric(LPT_max)), colour = "black", size = 1.25) +
  #geom_line(aes(x = mid_ma, y = as.numeric(LPT_min)), colour = "black", size = 1.25) +
  geom_point(aes(x = mid_ma, y = perf, colour = "standard"), size = 2.5, shape = 20, fill = "#0c2c84", alpha = 0.75) +
  geom_line(aes(x = mid_ma, y = perf, colour = "standard"), size = 1, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = buffer_perf, colour = "buffer"), size = 2.5, shape = 20, alpha = 0.75) +
  geom_line(aes(x = mid_ma, y = buffer_perf, colour = "buffer"), size = 1, alpha = 0.75) +
  geom_ribbon(aes(x = mid_ma, ymin = lower, ymax = upper), fill = "black", colour = NA, size = 1, alpha = 0.25) +
  geom_line(aes(x = mid_ma, y = mean, colour = "random"), size = 1, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = mean, colour = "random"), size = 2.5, shape = 20, alpha = 0.75) +
  #geom_text_repel(mapping=aes(x=(min_ma+max_ma)/2, y = buffer_perf, label = success+failure), fontface = "bold", nudge = 3, size = 3, colour = "black", alpha=1)  +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-7, 105), breaks = seq(0, 100, 25)) +
  scale_colour_manual(values = cols, breaks = labels) +
  labs(x = "Time (Ma)", y = "Predictive performance (%)", subtitle = "") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = NA),
        legend.position = c(0.1, 0.22),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.size = unit(0.75, "lines"),
        legend.key.width = unit(0.75, "cm"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 5, hjust = 0.6),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 0.5)
p1
ggsave("./figures/performance_plot_LTP.png", plot = p1, width = 200, height = 110, units = "mm", dpi = 600)

p1 <- p1 + labs(subtitle = "Least training presence")

data <- read.csv("./results/Performance/MaxSSS_results.csv")
data <- data[order(data$mid_ma),]

p2 <- ggplot(data = data) +
  geom_rect(data = periods, mapping=aes(xmin=min(data$min_ma), xmax=0, ymin= -7, ymax= 0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = throwing_shade, aes(xmin=min_ma, xmax=max_ma, ymin= -7, ymax=105), color= "grey95", alpha=0.2)  +
  geom_rect(data = periods, mapping=aes(xmin=min_ma, xmax=max_ma, ymin= -7, ymax= 0), colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_ma+max_ma)/2, y= -3.5, label = abbrev), colour = "black", alpha=1)  +
  #geom_hline(aes(yintercept = 0), size = 1, colour = "black", alpha = 0.7) +
  #geom_ribbon(aes(x = mid_ma, ymin = LPT_min, ymax = LPT_max), fill = NA, colour = "black", size = 1, alpha = 0.75) +
  #geom_line(aes(x = mid_ma, y = as.numeric(LPT_max)), colour = "black", size = 1.25) +
  #geom_line(aes(x = mid_ma, y = as.numeric(LPT_min)), colour = "black", size = 1.25) +
  geom_point(aes(x = mid_ma, y = perf, colour = "standard"), size = 2.5, shape = 20, fill = "#0c2c84", alpha = 0.75) +
  geom_line(aes(x = mid_ma, y = perf, colour = "standard"), size = 1, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = buffer_perf, colour = "buffer"), size = 2.5, shape = 20, alpha = 0.75) +
  geom_line(aes(x = mid_ma, y = buffer_perf, colour = "buffer"), size = 1, alpha = 0.75) +
  geom_ribbon(aes(x = mid_ma, ymin = lower, ymax = upper), fill = "black", colour = NA, size = 1, alpha = 0.25) +
  geom_line(aes(x = mid_ma, y = mean, colour = "random"), size = 1, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = mean, colour = "random"), size = 2.5, shape = 20, alpha = 0.75) +
  #geom_text_repel(mapping=aes(x=(min_ma+max_ma)/2, y= buffer_perf, label = success+failure), fontface = "bold", size = 3, colour = "black", nudge_y = 5, alpha=1)  +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-7, 105), breaks = seq(0, 100, 25)) +
  scale_colour_manual(values = cols, breaks = labels) +
  labs(x = "Time (Ma)", y = "Predictive performance (%)") +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = NA),
        legend.position = c(0.1, 0.22),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.size = unit(0.75, "lines"),
        legend.key.width = unit(0.75, "cm"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 5, hjust = 0.6),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 0.5)
p2
ggsave("./figures/performance_plot_MaxSSS.png", plot = p2, width = 200, height = 110, units = "mm", dpi = 600)

p2 <- p2 + labs(subtitle = "Maximising the sum of sensitivity and specificity")


p <- ggarrange(p1, p2, ncol=1, nrow=2, labels = "auto", align = "v", font.label = list(size = 18), common.legend = FALSE)
p
ggsave("./figures/performance_plot.jpg", plot = p, width = 65, height = 75, units = "mm", dpi = 600, scale = 3)
