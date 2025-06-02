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

#### Figure 2 ####
pdf("Figure 2.pdf",width=180/25.4,height=180/25.4)

# Male Education
Education<-readRDS("REVIEW Male Education range 3.RDS")

Education$group<-factor(Education$group, 
                        levels=c("Grade School", "Some High School", "Graduated High School", 
                                 "Some College", "Graduated College", "Post College Grad"), order=T) 

dataset <- data.frame()

Education$start <- ifelse(Education$range == "0-12", 0,
                          ifelse(Education$range == "12-30", 12,
                                 ifelse(Education$range == "30+", 30, NA)))

for (race in unique(Education$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Education[Education$group == race & Education$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Education[Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$group == race & Education$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Education[Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$group == race & Education$start == 12, ]$c + (35-30)*Education[Education$group == race & Education$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Grade School", "Some High School", "Graduated High School", 
                               "Some College", "Graduated College", "Post College Grad"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "", y = "Changes in added sugar consumption \n(g / person / day)", color = "Male Head Education") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) + 
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Education[Education$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Education[Education$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot1 <- plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(2.5, 1))

# Female Education
Education<-readRDS("REVIEW Female Education range 3.RDS")

Education$group<-factor(Education$group, 
                        levels=c("Grade School", "Some High School", "Graduated High School", 
                                 "Some College", "Graduated College", "Post College Grad"), order=T) 

dataset <- data.frame()

Education$start <- ifelse(Education$range == "0-12", 0,
                          ifelse(Education$range == "12-30", 12,
                                 ifelse(Education$range == "30+", 30, NA)))

for (race in unique(Education$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Education[Education$group == race & Education$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Education[Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$group == race & Education$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Education[Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$group == race & Education$start == 12, ]$c + (35-30)*Education[Education$group == race & Education$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Grade School", "Some High School", "Graduated High School", 
                               "Some College", "Graduated College", "Post College Grad"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "", y = "", color = "Female Head Education") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) + 
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Education[Education$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  xlab(NULL) + ylab(NULL)+
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Education[Education$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  xlab(NULL) + ylab(NULL)+
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v", rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot2 <- plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(3, 1))

# Income
Income<-readRDS("REVIEW Income range 3.RDS")

Income$group<-factor(Income$group, 
                     levels=c("Very Low", "Low", "Medium","High", "Very High"), order=T) 

dataset <- data.frame()

Income$start <- ifelse(Income$range == "0-12", 0,
                       ifelse(Income$range == "12-30", 12,
                              ifelse(Income$range == "30+", 30, NA)))

for (race in unique(Income$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Income[Income$group == race & Income$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Income[Income$group == race & Income$start == 0, ]$c + (30-12)*Income[Income$group == race & Income$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Income[Income$group == race & Income$start == 0, ]$c + (30-12)*Income[Income$group == race & Income$start == 12, ]$c + (35-30)*Income[Income$group == race & Income$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Very Low", "Low", "Medium","High", "Very High"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "", y = "Changes in added sugar consumption \n(g / person / day)", color = "Income") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) + 
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Income[Income$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Income[Income$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot3 <- plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(2.5, 1))

# Ethnic
Ethnic<-readRDS("REVIEW Ethnic range 3.RDS")

Ethnic$group<-factor(Ethnic$group, 
                     levels=c("White/Caucasian", "Other", "Black/African American", "Hispanic", "Asian" ), order=T) 

dataset <- data.frame()

Ethnic$start <- ifelse(Ethnic$range == "0-12", 0,
                       ifelse(Ethnic$range == "12-30", 12,
                              ifelse(Ethnic$range == "30+", 30, NA)))

for (race in unique(Ethnic$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$c + (30-12)*Ethnic[Ethnic$group == race & Ethnic$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$c + (30-12)*Ethnic[Ethnic$group == race & Ethnic$start == 12, ]$c + (35-30)*Ethnic[Ethnic$group == race & Ethnic$start == 30, ]$c, 0))),
                                                 ll = ifelse(tas == 12, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$ll,
                                                             ifelse(tas == 30, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$ll + (30-12)*Ethnic[Ethnic$group == race & Ethnic$start == 12, ]$ll,
                                                                    ifelse(tas == 35, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$ll + (30-12)*Ethnic[Ethnic$group == race & Ethnic$start == 12, ]$ll + (35-30)*Ethnic[Ethnic$group == race & Ethnic$start == 30, ]$ll, 0))),
                                                 ul = ifelse(tas == 12, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$ul,
                                                             ifelse(tas == 30, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$ul + (30-12)*Ethnic[Ethnic$group == race & Ethnic$start == 12, ]$ul,
                                                                    ifelse(tas == 35, 12*Ethnic[Ethnic$group == race & Ethnic$start == 0, ]$ul + (30-12)*Ethnic[Ethnic$group == race & Ethnic$start == 12, ]$ul + (35-30)*Ethnic[Ethnic$group == race & Ethnic$start == 30, ]$ul, 0))) )))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("White/Caucasian", "Other", "Black/African American", "Hispanic", "Asian" ), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "", y = "", color = "Ethnic") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.25, "cm"),
        legend.key.height = unit(0.3, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) + 
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Ethnic[Ethnic$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) +
  xlab(NULL) + ylab(NULL)+
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a"))+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Ethnic[Ethnic$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  xlab(NULL) + ylab(NULL)+
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a"))+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot4 <- plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(3, 1))

# Male Work Environment
Environment <- readRDS("REVIEW Male Environment range 3.RDS")

Environment$group <- factor(Environment$group, 
                            levels=c("Indoor", "Outdoor"), order=T) 

dataset <- data.frame()

Environment$start <- ifelse(Environment$range == "0-12", 0,
                            ifelse(Environment$range == "12-30", 12,
                                   ifelse(Environment$range == "30+", 30, NA)))

for (race in unique(Environment$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Environment[Environment$group == race & Environment$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Environment[Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$group == race & Environment$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Environment[Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$group == race & Environment$start == 12, ]$c + (35-30)*Environment[Environment$group == race & Environment$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Indoor", "Outdoor"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Temperature", y = "Changes in added sugar consumption \n(g / person / day)", color = "Male Head Work Environment") + 
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
  scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Environment[Environment$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Environment[Environment$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot5 <- plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(2.5, 1))

# Female Work Environment
Environment <- readRDS("REVIEW Female Environment range 3.RDS")

Environment$group <- factor(Environment$group, 
                            levels=c("Indoor", "Outdoor"), order=T) 

dataset <- data.frame()

Environment$start <- ifelse(Environment$range == "0-12", 0,
                            ifelse(Environment$range == "12-30", 12,
                                   ifelse(Environment$range == "30+", 30, NA)))

for (race in unique(Environment$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Environment[Environment$group == race & Environment$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Environment[Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$group == race & Environment$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Environment[Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$group == race & Environment$start == 12, ]$c + (35-30)*Environment[Environment$group == race & Environment$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Indoor", "Outdoor"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.8) +
  labs(x = "Temperature", y = "", color = "Female Head Work Environment") + 
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
  scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Environment[Environment$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  xlab(NULL) + ylab(NULL)+
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Environment[Environment$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 0.5, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 0.5, show.legend = FALSE) + 
  xlab(NULL) + ylab(NULL)+
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30°C")  +
  theme(plot.title = element_text(size = 6, hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        text = element_text(size = 6))+
  scale_y_continuous(limits = c(-0.813, 2.42), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.1, 1, 0.1, 1, 0.5))
plot6 <- plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(3, 1))

plot_grid(plot1, plot2, plot3, plot4, plot5, plot6, 
          labels = c("a", "b", "c", "d", "e", "f"), label_size = 8, nrow = 3, ncol = 2, align = "vh")

dev.off()
