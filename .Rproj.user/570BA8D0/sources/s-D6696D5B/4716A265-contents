# reef zone plot
# Lewis A. Jones
# October 2021
#---------------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)
stages <- read.csv("./data/stage_bins.csv")
periods <- read.csv("./data/period_bins.csv")
stages <- stages[1:52,] #subset to stages of interest
periods <- periods[2:6,] #subset to periods of interest
periods$color <- "white"
occ <- read.csv("./data/occurrences/PARED_cleaned.csv")
#---------------------------------

throwing_shade <- periods[seq(1, nrow(periods), 2),]

rots <- data.frame()

for(i in 1:nrow(stages)){
  tmp <- subset(occ, interval_name == stages$interval_name[i])
  mid_ma <- stages$mid_ma[i]
  max_getech <- max(tmp$P.Lat, na.rm = TRUE)
  min_getech <- min(tmp$P.Lat, na.rm = TRUE)
  max_scotese <- max(tmp$pal_lat_scotese, na.rm = TRUE)
  min_scotese <- min(tmp$pal_lat_scotese, na.rm = TRUE)
  max_pbdb <- max(tmp$paleolat, na.rm = TRUE)
  min_pbdb <- min(tmp$paleolat, na.rm = TRUE)
  tmp <- cbind.data.frame(max_getech, min_getech, max_scotese, min_scotese, max_pbdb, min_pbdb, mid_ma)
  rots <- rbind.data.frame(rots, tmp)
}

rots <- left_join(x = stages, y = rots, by = c("mid_ma" = "mid_ma"))
rots[rots == "Inf"] <- NA
rots[rots == "-Inf"] <- NA
rots$min_scotese[rots$min_scotese > 0] <- NA
rots$min_pbdb[rots$min_pbdb > 0] <- NA
rots$min_getech[rots$min_getech > 0] <- NA
rots$max_scotese[rots$max_scotese < 0] <- NA
rots$max_pbdb[rots$max_pbdb < 0] <- NA
rots$max_getech[rots$max_getech < 0] <- NA


cols <- c("Getech" = "#e41a1c",
          "PALEOMAP" = "#0570b0")

p1 <- ggplot() +
  geom_rect(data = periods, mapping=aes(xmin=min(min_ma), xmax=0, ymin= -70, ymax= -60), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = throwing_shade, aes(xmin=min_ma, xmax=max_ma, ymin=-70, ymax=75), color = "grey95", alpha=0.2)  +
  geom_rect(data = periods, mapping=aes(xmin=min_ma, xmax=max_ma, ymin= -70, ymax= -60), colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_ma+max_ma)/2, y= -65, label = abbrev), colour = "black", alpha=1)  +
  geom_hline(aes(yintercept = 0), size = 1, colour = "black", alpha = 0.7) +
  geom_point(data = occ, aes(x = mid_ma, y = P.Lat, colour = "Getech"), size = 1.5, alpha = 0.5) +
  geom_point(data = occ, aes(x = mid_ma, y = paleolat, colour = "PALEOMAP"), size = 1.5, alpha = 0.5) +
  geom_line(data = rots, aes(x = mid_ma, y = min_getech, colour = "Getech"), linetype = 1, size = 0.75, alpha = 1) +
  geom_line(data = rots, aes(x = mid_ma, y = max_getech, colour = "Getech"), linetype = 1, size = 0.75, alpha = 1) +
  geom_line(data = rots, aes(x = mid_ma, y = min_pbdb, colour = "PALEOMAP"), linetype = 1, size = 0.75, alpha = 1) +
  geom_line(data = rots, aes(x = mid_ma, y = max_pbdb, colour = "PALEOMAP"), linetype = 1, size = 0.75, alpha = 1) +
  scale_colour_manual(name = "L1", values = cols) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-70, 75), breaks = seq(-60, 75, 30)) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Palaeolatitude (",degree,")")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = NA),
        legend.position = c(0.9, 0.91),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.spacing.y = unit(-0.2, "cm"),
        legend.key.size = unit(0.75, "lines"),
        legend.key.width = unit(1, "cm"),
        legend.text = element_text(size = 8),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 5, hjust = 0.6),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 0.5)
p1

ggsave("./figures/rotation_plot.png", plot = p1, width = 200, height = 110, units = "mm", dpi = 600)
