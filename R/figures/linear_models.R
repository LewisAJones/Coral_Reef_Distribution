# linear model plot
# Lewis A. Jones
# Feb 2022
#---------------------------------
library(ggplot2)
library(ggpubr)
library(dplyr)
stages <- read.csv("./data/stage_bins.csv")
#---------------------------------

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

#LPT

p1 <- ggplot(data = data, aes()) +
  geom_point(aes(x = LPT_global, y = n), shape = 21, fill = data$color, colour = "black", size = 2, alpha = 1) +
  geom_smooth(aes(x = LPT_global, y = n), method='lm', colour = "black", formula= y~x) +
  labs(x = expression(bold(paste("Suitable habitat area (",10^6~km^2,")",sep=""))), y = "Number of reef sites", subtitle = "Least training presence") +
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
        plot.subtitle = element_text(size = 10, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
        axis.text.y.right = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 2),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 1)
p1 <- p1 + geom_text(x = 340, y = 45, label = lm_eqn(df = data, x = data$LPT_global, y = data$n), colour = "black", size = 3, parse = TRUE)
#p1

#MAXSSS

p2 <- ggplot(data = data, aes()) +
  geom_point(aes(x = MaxSSS_global, y = n), shape = 21, fill = data$color, colour = "black", size = 2, alpha = 1) +
  geom_smooth(aes(x = MaxSSS_global, y = n), method='lm', colour = "black", formula= y~x) +
  labs(x = expression(bold(paste("Suitable habitat area (",10^6~km^2,")",sep=""))), y = "Number of reef sites", subtitle = "Maximising the sum of sensitivity and specificity") +
  theme(panel.background = element_rect(colour = "white", fill = "white"),
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
        plot.subtitle = element_text(size = 10, angle = 0, hjust = 0.5),
        axis.text.x = element_text(size = 14, angle = 0, hjust = 0.5),
        axis.text.y = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y = element_text(size = 14, face = "bold", vjust = 2),
        axis.text.y.right = element_text(size = 14, angle = 0, hjust = 0),
        axis.title.y.right = element_text(size = 14, face = "bold", vjust = 2),
        axis.title.x = element_text(size = 14, face = "bold", vjust = -1),
        aspect.ratio = 1)
p2 <- p2 + geom_text(x = 250, y = 45, label = lm_eqn(df = data, x = data$MaxSSS_global, y = data$n), colour = "black", size = 3, parse = TRUE)
#p2

p <- ggarrange(p1, p2, labels = c("A", "B"), ncol = 2) 
#p

ggsave("./figures/linear_models.jpg", plot = p, width = 70, height = 35, units = "mm", dpi = 600, scale = 3)

