setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption")

library(ggplot2)
library(tidyverse)
library(scales)
library(gridExtra)
library(grid)
library(cowplot)
library(sf)
library(USAboundaries)
library(rnaturalearth)
library(dplyr)
library(gtable)
library(corrplot)

#### Extended Data Figure 1 ####
pdf("Extended Data Figure 1.pdf",width=88/25.4,height=70/25.4)

Zone <- readRDS("REVIEW Climate Zone range 3.RDS")

Zone$group<-factor(Zone$group, 
                   levels=c("Marine", "Cold/Very Cold", "Hot-Dry/Mixed-Dry", "Mixed-Humid", "Hot-Humid"), order=T) 

dataset <- data.frame()

Zone$start <- ifelse(Zone$range == "0-12", 0,
                     ifelse(Zone$range == "12-30", 12,
                            ifelse(Zone$range == "30+", 30, NA)))

for (race in unique(Zone$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Zone[Zone$group == race & Zone$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Zone[Zone$group == race & Zone$start == 0, ]$c + (30-12)*Zone[Zone$group == race & Zone$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Zone[Zone$group == race & Zone$start == 0, ]$c + (30-12)*Zone[Zone$group == race & Zone$start == 12, ]$c + (35-30)*Zone[Zone$group == race & Zone$start == 30, ]$c, 0))),
                                                 ll = ifelse(tas == 12, 12*Zone[Zone$group == race & Zone$start == 0, ]$ll,
                                                             ifelse(tas == 30, 12*Zone[Zone$group == race & Zone$start == 0, ]$ll + (30-12)*Zone[Zone$group == race & Zone$start == 12, ]$ll,
                                                                    ifelse(tas == 35, 12*Zone[Zone$group == race & Zone$start == 0, ]$ll + (30-12)*Zone[Zone$group == race & Zone$start == 12, ]$ll + (35-30)*Zone[Zone$group == race & Zone$start == 30, ]$ll, 0))),
                                                 ul = ifelse(tas == 12, 12*Zone[Zone$group == race & Zone$start == 0, ]$ul,
                                                             ifelse(tas == 30, 12*Zone[Zone$group == race & Zone$start == 0, ]$ul + (30-12)*Zone[Zone$group == race & Zone$start == 12, ]$ul,
                                                                    ifelse(tas == 35, 12*Zone[Zone$group == race & Zone$start == 0, ]$ul + (30-12)*Zone[Zone$group == race & Zone$start == 12, ]$ul + (35-30)*Zone[Zone$group == race & Zone$start == 30, ]$ul, 0))) )))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Marine", "Cold/Very Cold", "Hot-Dry/Mixed-Dry", "Mixed-Humid", "Hot-Humid"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Temperature", y = "Changes in added sugar consumption (g / person / day)", color = "Climate Zone") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) + 
  scale_color_manual(values = c(rgb(99/255, 177/255, 191/255), rgb(103/255, 128/255, 188/255), rgb(179/255, 83/255, 19/255), rgb(139/255, 181/255, 52/255), rgb(240/255, 162/255, 51/255)))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Zone[Zone$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) +
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c(rgb(99/255, 177/255, 191/255), rgb(103/255, 128/255, 188/255), rgb(179/255, 83/255, 19/255), rgb(139/255, 181/255, 52/255), rgb(240/255, 162/255, 51/255)))+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.8, 2.4), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Zone[Zone$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) + 
  scale_color_manual(values = c(rgb(99/255, 177/255, 191/255), rgb(103/255, 128/255, 188/255), rgb(179/255, 83/255, 19/255), rgb(139/255, 181/255, 52/255), rgb(240/255, 162/255, 51/255)))+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(2.5, 1))

dev.off()