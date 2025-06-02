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

pdf("Figure 3.pdf",width=180/25.4,height=170/25.4)

# drink consumption
Range <- readRDS("REVIEW coefficient R focused covert monthly Range3.RDS")

Range$group <- Range$variable

Range$group <- ifelse(Range$variable == "Sweetened Beverages", "Sugar added Beverages", Range$variable)

Range$est <- as.numeric(Range$est)
Range <- spread(Range, coef, est)

Range$start <- ifelse(Range$range == "0-12", 0,
                      ifelse(Range$range == "12-30", 12,
                             ifelse(Range$range == "30+", 30, NA)))

dataset <- data.frame()

for (food in unique(Range$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = food,
                                                 y = ifelse(tas == 12, 12*Range[Range$group == food & Range$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Range[Range$group == food & Range$start == 0, ]$c + (30-12)*Range[Range$group == food & Range$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Range[Range$group == food & Range$start == 0, ]$c + (30-12)*Range[Range$group == food & Range$start == 12, ]$c + (35-30)*Range[Range$group == food & Range$start == 30, ]$c, 0))),
                                                 ll = ifelse(tas == 12, 12*Range[Range$group == food & Range$start == 0, ]$ll,
                                                             ifelse(tas == 30, 12*Range[Range$group == food & Range$start == 0, ]$ll + (30-12)*Range[Range$group == food & Range$start == 12, ]$ll,
                                                                    ifelse(tas == 35, 12*Range[Range$group == food & Range$start == 0, ]$ll + (30-12)*Range[Range$group == food & Range$start == 12, ]$ll + (35-30)*Range[Range$group == food & Range$start == 30, ]$ll, 0))),
                                                 ul = ifelse(tas == 12, 12*Range[Range$group == food & Range$start == 0, ]$ul,
                                                             ifelse(tas == 30, 12*Range[Range$group == food & Range$start == 0, ]$ul + (30-12)*Range[Range$group == food & Range$start == 12, ]$ul,
                                                                    ifelse(tas == 35, 12*Range[Range$group == food & Range$start == 0, ]$ul + (30-12)*Range[Range$group == food & Range$start == 12, ]$ul + (35-30)*Range[Range$group == food & Range$start == 30, ]$ul, 0))) )))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Sugar added Beverages", "Plain Water",
                               "100% Juice", "Alcoholic Beverages", "Coffee and Tea",
                               "Yogurt", "Dairy Drinks and Substitutes", "Diet Beverages", "Milk"), order=T) 

p1 <- ggplot(dataset, aes(x = x, group = group)) +
  geom_line(aes(y = y, color = group, linetype = group, size = group)) +
  labs(x = "Temperature", y = "Changes in drink consumption (g / person / day)", 
       color = "Drink \nCategory", linetype = "Drink \nCategory", size = "Drink \nCategory") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(-14.5, 30),breaks = seq(-10, 30, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  theme_classic() + 
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0.5, -0.067), 
        legend.justification = c(0.5, 1),
        legend.direction = "horizontal",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        plot.margin = margin(t = 10, r = 0, b = 55, l = 10)) + 
  scale_color_manual(values = c("#e31a1c", "#1f78b4", 
                                "#FFD700", "#800080", "#6F4E37", 
                                "#f781bf", "#ec7014", "#33a02c", "#a6cee3")) +
  scale_linetype_manual(values = c("solid", "solid",
                                   "dotdash", "dotdash", "dotted",
                                   "dashed", "dashed", "dotted","twodash")) +
  scale_size_manual(values = c(0.7, 0.7, 
                               0.7, 0.7, 0.5, 0.5,
                               0.5, 0.5, 0.5, 0.5)) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed",  linewidth = 0.5, alpha = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.3) +
  guides(color = guide_legend(nrow = 4)) 

# Male Head Education
Education <- readRDS("REVIEW coefficient R focused drink consumption covert monthly Male_Head_Education.RDS")

Education$category <- Education$variable

Education <- spread(Education, coef, est)

Education$start <- ifelse(Education$range == "0-12", 0,
                          ifelse(Education$range == "12-30", 12,
                                 ifelse(Education$range == "30+", 30, NA)))

dataset <- data.frame()

for (race in unique(Education$group)) {
  for (tas in c(0, 12, 30, 35)) {
    for (food in unique(Education$category)) {
      dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race, category = food, 
                                                   y = ifelse(tas == 12, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$c,
                                                              ifelse(tas == 30, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$c,
                                                                     ifelse(tas == 35, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$c + (35-30)*Education[Education$category == food & Education$group == race & Education$start == 30, ]$c, 0))),
                                                   ll = ifelse(tas == 12, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ll,
                                                               ifelse(tas == 30, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ll + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ll,
                                                                      ifelse(tas == 35, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ll + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ll + (35-30)*Education[Education$category == food & Education$group == race & Education$start == 30, ]$ll, 0))),
                                                   ul = ifelse(tas == 12, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ul,
                                                               ifelse(tas == 30, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ul + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ul,
                                                                      ifelse(tas == 35, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ul + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ul + (35-30)*Education[Education$category == food & Education$group == race & Education$start == 30, ]$ul, 0))) )))
    }
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Grade School", "Some High School", "Graduated High School", 
                               "Some College", "Graduated College", "Post College Grad"), order=T) 

data <- dataset[dataset$category == "Sweetened Beverages", ]
p1.1 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(color = "Male Head Education") +
  xlab(NULL) + ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 0, b = 0, l = 0)) + 
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

data <- dataset[dataset$category == "Plain Water", ]
p1.2 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(color = "Male Head Education") + 
  xlab(NULL) + ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none",
        plot.margin = margin(t = 10, r = 0, b = 0, l = 0)) + 
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p1_legend <- p1.2 +
  labs(color = "Male Head Education") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6))
legend1 <- get_legend(p1_legend)
legend1 <- ggdraw() + draw_grob(as_grob(legend1))

# Female Head Education
Education <- readRDS("REVIEW coefficient R focused drink consumption covert monthly Female_Head_Education.RDS")

Education$category <- Education$variable

Education <- spread(Education, coef, est)

Education$start <- ifelse(Education$range == "0-12", 0,
                          ifelse(Education$range == "12-30", 12,
                                 ifelse(Education$range == "30+", 30, NA)))

dataset <- data.frame()

for (race in unique(Education$group)) {
  for (tas in c(0, 12, 30, 35)) {
    for (food in unique(Education$category)) {
      dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race, category = food, 
                                                   y = ifelse(tas == 12, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$c,
                                                              ifelse(tas == 30, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$c,
                                                                     ifelse(tas == 35, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$c + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$c + (35-30)*Education[Education$category == food & Education$group == race & Education$start == 30, ]$c, 0))),
                                                   ll = ifelse(tas == 12, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ll,
                                                               ifelse(tas == 30, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ll + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ll,
                                                                      ifelse(tas == 35, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ll + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ll + (35-30)*Education[Education$category == food & Education$group == race & Education$start == 30, ]$ll, 0))),
                                                   ul = ifelse(tas == 12, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ul,
                                                               ifelse(tas == 30, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ul + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ul,
                                                                      ifelse(tas == 35, 12*Education[Education$category == food & Education$group == race & Education$start == 0, ]$ul + (30-12)*Education[Education$category == food & Education$group == race & Education$start == 12, ]$ul + (35-30)*Education[Education$category == food & Education$group == race & Education$start == 30, ]$ul, 0))) )))
    }
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Grade School", "Some High School", "Graduated High School", 
                               "Some College", "Graduated College", "Post College Grad"), order=T) 

data <- dataset[dataset$category == "Sweetened Beverages", ]
p2.1 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(color = "Female Head Education") + 
  theme_classic() +
  xlab(NULL) + ylab(NULL) +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

data <- dataset[dataset$category == "Plain Water", ]
p2.2 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(color = "Female Head Education") + 
  xlab(NULL) + ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2_legend <- p2.2 +
  labs(color = "Female Head Education") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6))
legend2 <- get_legend(p2_legend)
legend2 <- ggdraw() + draw_grob(as_grob(legend2))

# Income
Income <- readRDS("REVIEW coefficient R focused drink consumption covert monthly Income.RDS")

Income$category <- Income$variable

Income <- spread(Income, coef, est)

Income$start <- ifelse(Income$range == "0-12", 0,
                       ifelse(Income$range == "12-30", 12,
                              ifelse(Income$range == "30+", 30, NA)))

dataset <- data.frame()

for (race in unique(Income$group)) {
  for (tas in c(0, 12, 30, 35)) {
    for (food in unique(Income$category)) {
      dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race, category = food, 
                                                   y = ifelse(tas == 12, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$c,
                                                              ifelse(tas == 30, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$c + (30-12)*Income[Income$category == food & Income$group == race & Income$start == 12, ]$c,
                                                                     ifelse(tas == 35, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$c + (30-12)*Income[Income$category == food & Income$group == race & Income$start == 12, ]$c + (35-30)*Income[Income$category == food & Income$group == race & Income$start == 30, ]$c, 0))),
                                                   ll = ifelse(tas == 12, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$ll,
                                                               ifelse(tas == 30, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$ll + (30-12)*Income[Income$category == food & Income$group == race & Income$start == 12, ]$ll,
                                                                      ifelse(tas == 35, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$ll + (30-12)*Income[Income$category == food & Income$group == race & Income$start == 12, ]$ll + (35-30)*Income[Income$category == food & Income$group == race & Income$start == 30, ]$ll, 0))),
                                                   ul = ifelse(tas == 12, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$ul,
                                                               ifelse(tas == 30, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$ul + (30-12)*Income[Income$category == food & Income$group == race & Income$start == 12, ]$ul,
                                                                      ifelse(tas == 35, 12*Income[Income$category == food & Income$group == race & Income$start == 0, ]$ul + (30-12)*Income[Income$category == food & Income$group == race & Income$start == 12, ]$ul + (35-30)*Income[Income$category == food & Income$group == race & Income$start == 30, ]$ul, 0))) )))
    }
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Very Low", "Low", "Medium","High", "Very High"), order=T) 

data <- dataset[dataset$category == "Sweetened Beverages", ]
p3.1 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(color = "Income") + 
  xlab(NULL) + ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

data <- dataset[dataset$category == "Plain Water", ]
p3.2 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(color = "Income") + 
  xlab(NULL) + ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p3_legend <- p3.2 +
  labs(color = "Income") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6))
legend3 <- get_legend(p3_legend)
legend3 <- ggdraw() + draw_grob(as_grob(legend3))

# Ethnic
Ethnic <- readRDS("REVIEW coefficient R focused drink consumption covert monthly Ethnic.RDS")

Ethnic$category <- Ethnic$variable

Ethnic <- spread(Ethnic, coef, est)

Ethnic$start <- ifelse(Ethnic$range == "0-12", 0,
                       ifelse(Ethnic$range == "12-30", 12,
                              ifelse(Ethnic$range == "30+", 30, NA)))

dataset <- data.frame()

for (race in unique(Ethnic$group)) {
  for (tas in c(0, 12, 30, 35)) {
    for (food in unique(Ethnic$category)) {
      dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race, category = food, 
                                                   y = ifelse(tas == 12, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$c,
                                                              ifelse(tas == 30, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$c + (30-12)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 12, ]$c,
                                                                     ifelse(tas == 35, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$c + (30-12)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 12, ]$c + (35-30)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 30, ]$c, 0))),
                                                   ll = ifelse(tas == 12, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$ll,
                                                               ifelse(tas == 30, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$ll + (30-12)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 12, ]$ll,
                                                                      ifelse(tas == 35, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$ll + (30-12)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 12, ]$ll + (35-30)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 30, ]$ll, 0))),
                                                   ul = ifelse(tas == 12, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$ul,
                                                               ifelse(tas == 30, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$ul + (30-12)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 12, ]$ul,
                                                                      ifelse(tas == 35, 12*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 0, ]$ul + (30-12)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 12, ]$ul + (35-30)*Ethnic[Ethnic$category == food & Ethnic$group == race & Ethnic$start == 30, ]$ul, 0))) )))
    }
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("White/Caucasian", "Other", "Black/African American", "Hispanic", "Asian" ), order=T) 

data <- dataset[dataset$category == "Sweetened Beverages", ]
p4.1 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Temperature", color = "Ethnic") + 
  ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

data <- dataset[dataset$category == "Plain Water", ]
p4.2 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Temperature", color = "Ethnic") + 
  ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p4_legend <- p4.2 +
  labs(color = "Ethnic") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6))
legend4 <- get_legend(p4_legend)
legend4 <- ggdraw() + draw_grob(as_grob(legend4))

# Male Work Environment
Environment <- readRDS("REVIEW coefficient R focused drink consumption covert monthly Male Environment.RDS")

Environment$category <- Environment$variable

Environment <- spread(Environment, coef, est)

Environment$start <- ifelse(Environment$range == "0-12", 0,
                            ifelse(Environment$range == "12-30", 12,
                                   ifelse(Environment$range == "30+", 30, NA)))

dataset <- data.frame()

for (race in unique(Environment$group)) {
  for (tas in c(0, 12, 30, 35)) {
    for (food in unique(Environment$category)) {
      dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race, category = food, 
                                                   y = ifelse(tas == 12, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$c,
                                                              ifelse(tas == 30, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$c,
                                                                     ifelse(tas == 35, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$c + (35-30)*Environment[Environment$category == food & Environment$group == race & Environment$start == 30, ]$c, 0))),
                                                   ll = ifelse(tas == 12, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ll,
                                                               ifelse(tas == 30, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ll + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ll,
                                                                      ifelse(tas == 35, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ll + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ll + (35-30)*Environment[Environment$category == food & Environment$group == race & Environment$start == 30, ]$ll, 0))),
                                                   ul = ifelse(tas == 12, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ul,
                                                               ifelse(tas == 30, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ul + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ul,
                                                                      ifelse(tas == 35, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ul + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ul + (35-30)*Environment[Environment$category == food & Environment$group == race & Environment$start == 30, ]$ul, 0))) )))
    }
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Indoor", "Outdoor" ), order=T) 

data <- dataset[dataset$category == "Sweetened Beverages", ]
p5.1 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Temperature", color = "Male Work Environment") + 
  ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

data <- dataset[dataset$category == "Plain Water", ]
p5.2 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Temperature", color = "Male Work Environment") +
  ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p5_legend <- p5.2 +
  labs(color = "Male Work Environment") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6))
legend5 <- get_legend(p5_legend)
legend5 <- ggdraw() + draw_grob(as_grob(legend5))

# Female Work Environment
Environment <- readRDS("REVIEW coefficient R focused drink consumption covert monthly Female Environment.RDS")

Environment$category <- Environment$variable

Environment <- spread(Environment, coef, est)

Environment$start <- ifelse(Environment$range == "0-12", 0,
                            ifelse(Environment$range == "12-30", 12,
                                   ifelse(Environment$range == "30+", 30, NA)))

dataset <- data.frame()

for (race in unique(Environment$group)) {
  for (tas in c(0, 12, 30, 35)) {
    for (food in unique(Environment$category)) {
      dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race, category = food, 
                                                   y = ifelse(tas == 12, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$c,
                                                              ifelse(tas == 30, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$c,
                                                                     ifelse(tas == 35, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$c + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$c + (35-30)*Environment[Environment$category == food & Environment$group == race & Environment$start == 30, ]$c, 0))),
                                                   ll = ifelse(tas == 12, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ll,
                                                               ifelse(tas == 30, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ll + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ll,
                                                                      ifelse(tas == 35, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ll + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ll + (35-30)*Environment[Environment$category == food & Environment$group == race & Environment$start == 30, ]$ll, 0))),
                                                   ul = ifelse(tas == 12, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ul,
                                                               ifelse(tas == 30, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ul + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ul,
                                                                      ifelse(tas == 35, 12*Environment[Environment$category == food & Environment$group == race & Environment$start == 0, ]$ul + (30-12)*Environment[Environment$category == food & Environment$group == race & Environment$start == 12, ]$ul + (35-30)*Environment[Environment$category == food & Environment$group == race & Environment$start == 30, ]$ul, 0))) )))
    }
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Indoor", "Outdoor" ), order=T) 

data <- dataset[dataset$category == "Sweetened Beverages", ]
p6.1 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Temperature", color = "Female Work Environment") + 
  ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

data <- dataset[dataset$category == "Plain Water", ]
p6.2 <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 0.5) +
  labs(x = "Temperature", color = "Female Work Environment") + 
  ylab(NULL) +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = "none") + 
  scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
  scale_y_continuous(limits = c(-17.03, 55.1),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p6_legend <- p6.2 +
  labs(color = "Female Work Environment") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.direction = "vertical",
        legend.background = element_rect(color = "black"),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 6))
legend6 <- get_legend(p6_legend)
legend6 <- ggdraw() + draw_grob(as_grob(legend6))

legend <- plot_grid(legend1, legend2, legend3, legend4, legend5, legend6,
                    nrow = 2, ncol = 3, align = "vh", axis = "l")
y <- ggdraw() + draw_label("", fontface='plain', size = 6, angle = 90)
legend <- plot_grid(y, legend, ncol=2, rel_widths=c(0.03, 1))

title1 <- ggdraw() + draw_label("Sugar added Beverages", fontface='bold', size = 7)
plot1 <- plot_grid(p1.1, p2.1, p3.1, p4.1, p5.1, p6.1, 
                   labels = c("b.1", "b.2", "b.3", "b.4", "b.5", "b.6"), label_size = 8, nrow = 2, ncol = 3, align = "vh", axis = "lrbt")
plot1 <- plot_grid(title1, plot1, ncol=1, rel_heights=c(0.1, 1))
y1 <- ggdraw() + draw_label("Changes in drink consumption (g / person / day)", fontface='plain', size = 6, angle = 90)
plot1 <- plot_grid(y1, plot1, ncol=2, rel_widths=c(0.03, 1))

title2 <- ggdraw() + draw_label("Plain Water", fontface='bold', size = 7)
plot2 <- plot_grid(p1.2, p2.2, p3.2, p4.2, p5.2, p6.2, 
                   labels = c("c.1", "c.2", "c.3", "c.4", "c.5", "c.6"), label_size = 8, nrow = 2, ncol = 3, align = "vh", axis = "lrbt")
plot2 <- plot_grid(title2, plot2, ncol=1, rel_heights=c(0.1, 1))
y2 <- ggdraw() + draw_label("Changes in drink consumption (g / person / day)", fontface='plain', size = 6, angle = 90)
plot2 <- plot_grid(y2, plot2, ncol=2, rel_widths=c(0.03, 1))

p2 <- plot_grid(plot1, plot2, legend, ncol = 1, align = "v", rel_heights=c(1, 1, 0.55)) +
  theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5))
plot_grid(p1, p2, labels = c("a", ""),  label_size = 8, ncol = 2, rel_widths=c(1, 1))

dev.off()