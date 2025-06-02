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

#### Figure 1 ####
# Line Plot
pdf("Figure 1.pdf",width=180/25.4,height=120/25.4)

dataset<-readRDS("REVIEW coefficient R added sugar monthly.RDS")

interval<-data.frame(id=c(1:19),
                     range=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                             "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"))

dataset<-merge(dataset, interval, by="id")

dataset$range<-factor(dataset$range, 
                      levels=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                               "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"), order=T) 

dataset <- pivot_longer(dataset,
                        cols = starts_with("AS"),
                        names_to = "variable",
                        values_to = "est")

dataset <- dataset %>%
  mutate(coef = case_when(
    grepl("_ll$", variable) ~ "ll",
    grepl("_ul$", variable) ~ "ul",
    TRUE ~ "c"
  ))

dataset$variable <- ifelse(grepl("AS_intake", dataset$variable), "AS_intake",
                           ifelse(grepl("AS_convert", dataset$variable), "AS_convert",
                                  dataset$variable))

dataset <- spread(dataset, coef, est)

dataset$variable <- factor(dataset$variable, 
                           levels=c("AS_convert", "AS_intake"), order=T) 

p1 <- ggplot(dataset, aes(x = range, group = variable)) +
  geom_line(aes(y = c, color = variable, size = variable, linetype = variable)) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = variable), alpha = 0.3) +
  labs(x = "Temperature", y = "Changes in added sugar consumption (g / person / day)") +
  scale_y_continuous(limits = c(-7, 30), breaks = seq(0, 30, by = 10)) +
  scale_x_discrete(expand = c(0.03,0)) +
  theme_classic() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        text = element_text(family = "sans", size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.text = element_text(size = 6)) + 
  scale_color_manual(values = c("#6a3d9a","#e31a1c"), 
                     labels = c("Changes in added sugar consumption converted to adult male",
                                "Changes in added sugar consumption actually")) + 
  scale_fill_manual(values = c("#cab2d6","#fb9a99"), 
                    labels = c("Changes in added sugar consumption converted to adult male",
                               "Changes in added sugar consumption actually")) +
  scale_size_manual(values=c(1, 0.5), 
                    labels = c("Changes in added sugar consumption converted to adult male",
                               "Changes in added sugar consumption actually")) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("Changes in added sugar consumption converted to adult male",
                                   "Changes in added sugar consumption actually")) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.3, alpha = 0.8) +
  geom_vline(xintercept = "10-12", col = "black", linetype = "dashed", linewidth = 0.3, alpha = 0.3) +
  geom_vline(xintercept = "28-30", col = "black", linetype = "dashed", linewidth = 0.3, alpha = 0.3)

# Histogram
data <- readRDS("test_nutrition_household_zone_month.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$county <- 1

data <- data%>%
  group_by(m_TEMP_range2) %>%
  summarise(count = sum(county)) %>%
  ungroup()

data$id <- data$m_TEMP_range2
data$id <- as.numeric(data$id)

interval <- data.frame(id=c(1:19),
                       range=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                               "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"))

data <- right_join(data, interval, by="id")

data$count <- ifelse(is.na(data$count), 0, data$count)

data$range<-factor(data$range, 
                   levels=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                            "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"), order=T) 
max_value <- max(data$count/100000)

p2 <- ggplot(data, aes(x = range,  y = count/100000))+
  geom_col(fill = "#d9d9d9", alpha = 0.4) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max_value * 5)) +
  scale_x_discrete(expand = c(0.03,0))+ 
  labs(x = "", y = " ") +
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.text.y = element_text(color = "transparent"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, color = "transparent"),
        text = element_text(family = "sans", size = 6),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

# Food Category
dataset <- readRDS("REVIEW coefficient R focused sugar covert monthly.RDS")

interval <- data.frame(id=c(1:19),
                       range=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                               "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"))

dataset <- merge(dataset, interval, by="id")

dataset$range <- factor(dataset$range, 
                        levels=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                                 "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"), order=T) 

dataset <- dataset[dataset$variable == "Bakery, Oils and Sugar products" | dataset$variable == "Sweetened Beverages" | dataset$variable == "Other Desserts", ]

dataset <- spread(dataset, coef, est)

dataset$ll <- ifelse(dataset$c == 0, 0, dataset$ll)
dataset$ul <- ifelse(dataset$c == 0, 0, dataset$ul)

dataset$group <- ifelse(dataset$variable == "Sweetened Beverages", "Sugar added Beverages",
                        ifelse(dataset$variable == "Other Desserts", "Frozen desserts", dataset$variable))

dataset$group <- factor(dataset$group, 
                        levels=c("Sugar added Beverages", "Frozen desserts", "Bakery, Oils and Sugar products"), order=T) 

p3 <- ggplot(dataset, aes(x = range, group = group)) +
  geom_line(aes(y = c, color = group), linewidth = 1) +
  geom_ribbon(aes(ymin = ll, ymax = ul, fill = group), alpha = 0.45) +
  labs(x = "Temperature", y = "Changes in added sugar consumption (g / person / day)", color = "Food Category", fill = "Food Category") +
  scale_x_discrete(expand = c(0.01,0)) +
  scale_y_continuous(limits = c(-7.5, 31), breaks = seq(0, 30, by = 10)) +
  theme_classic() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.title = element_text(hjust = 0.5),
        text = element_text(family = "sans", size = 6),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.4, "cm"),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7)) + 
  scale_color_manual(values = c("#e31a1c", "#1f78b4", "#ff7f00")) +
  scale_fill_manual(values = c("#d9d9d9","#d9d9d9", "#d9d9d9")) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.3, alpha = 0.8) +
  geom_vline(xintercept = "10-12", col = "black", linetype = "dashed", linewidth = 0.3, alpha = 0.3) +
  geom_vline(xintercept = "28-30", col = "black", linetype = "dashed", linewidth = 0.3, alpha = 0.3)

plot1 <-  plot_grid(p1, p3, labels = c("a", " "), label_size = 8, nrow = 1, align = "v")
plot2 <- plot_grid(p2, p3, labels = c(" ", "b"), label_size = 8, nrow = 1, align = "v")
ggdraw(plot1) + draw_plot(plot2) 

dev.off()