# Area plot
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
#---------------------------------

throwing_shade <- periods[seq(1, nrow(periods), 2),]

data <- read.csv("./results/Area/area_global.csv")
reefs <- read.csv("./data/occurrences/PARED_subsampled.csv")
reefs <- data.frame(table(reefs$interval_name))
colnames(reefs) <- c("interval_name", "n")
data <- left_join(x = data, y = stages, by = c("stage" = "interval_name"))
reefs <- left_join(x = reefs, y = stages, by = c("interval_name"))
reefs <- reefs[order(reefs$mid_ma),]
data <- data[order(data$mid_ma),]
data$LPT_global <- data$LPT_global/100000
data$MaxSSS_global <- data$MaxSSS_global/100000
data <- left_join(x = data, y = reefs[,c("interval_name", "n")], by = c("stage" = "interval_name"))

lm_eqn <- function(df, x, y){
  m <- lm(y ~ x, df);
  eq <- substitute(italic(R)^2 == r2~~~~~italic(P)~"="~pval, 
                   list(r2 = format(summary(m)$r.squared, nsmall = 3, digits = 0, scientific = FALSE),
                        pval = format(coef(summary(m))[2, "Pr(>|t|)"], digits = 3)))
  as.character(as.expression(eq));
}


# Value used to transform the data
coeff <- 5
p1 <- ggplot(data = data, aes()) +
  geom_rect(data = periods, mapping=aes(xmin=min(data$min_ma), xmax=0, ymin= -40, ymax= -0), linetype = 1, colour = "black", fill="black", alpha=1)  +
  geom_rect(data = throwing_shade, aes(xmin=min_ma, xmax=max_ma, ymin=-40, ymax=550), color= "grey95", alpha=0.2)  +
  geom_rect(data = periods, mapping=aes(xmin=min_ma, xmax=max_ma, ymin= -40, ymax= 0), colour = "black", fill=periods$color, alpha=1)  +
  geom_text(data = periods, mapping=aes(x=(min_ma+max_ma)/2, y= -20, label = abbrev), colour = "black", alpha=1)  +
  geom_line(aes(x = mid_ma, y = LPT_global, colour = "LTP"), size = 1, alpha = 0.75) +
  geom_line(aes(x = mid_ma, y = MaxSSS_global, colour = "MaxSSS"), size = 1, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = LPT_global, colour = "LTP"), size = 1.5, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = MaxSSS_global, colour = "MaxSSS"), size = 1.5, alpha = 0.75) +
  geom_line(aes(x = mid_ma, y = n*coeff, colour = "Reef sites"), size = 1, alpha = 0.75) +
  geom_point(aes(x = mid_ma, y = n*coeff, colour = "Reef sites"), size = 1.5, alpha = 0.75) +
  scale_colour_manual(values = c("#0c2c84", "#1d91c0", "black"), labels = c("LTP", "MaxSSS", "Reef sites")) +
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(-40, 550), breaks = seq(0, 550, 100),   
                     sec.axis = sec_axis(~./coeff, name = expression(bold(paste("Fossil coral reef sites (", italic("n"), ")", sep = ""))))) +
  labs(x = "Time (Ma)", y = expression(bold(paste("Suitable habitat area (",10^6~km^2,")",sep="")))) +
  theme(panel.background = element_blank(),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"),
        panel.grid.minor.y = element_line(colour = NA),
        panel.grid.minor.x = element_line(colour = NA),
        panel.grid.major.y = element_line(colour = NA),
        panel.grid.major.x = element_line(colour = NA),
        legend.position = c(0.1, 0.925),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.key.size = unit(0.75, "lines"),
        legend.key.width = unit(0.75, "cm"),
        legend.text = element_text(size = 10),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
        axis.text.y.right = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 2),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 0.5)
#p1 <- p1 + geom_text(x = -33, y = 430, label = lm_eqn(df = data, x = data$LPT_global, y = data$n), colour = "#0c2c84", face = "bold", size = 3.5, parse = TRUE)
#p1 <- p1 + geom_text(x = -33, y = 404, label = lm_eqn(df = data, x = data$MaxSSS_global, y = data$n), colour = "#1d91c0", face = "bold", size = 3.5, parse = TRUE)
#p1
ggsave("./figures/global_hab_area.pdf", plot = p1, width = 70, height = 35, units = "mm", dpi = 600, scale = 3)

