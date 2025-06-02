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

#### Figure S1 ####
# get boundary with high definition of all states and counties
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)
county_polygons$fips_county <- paste(county_polygons$statefp, county_polygons$countyfp, sep = "")

# buffer
buffered_counties <- st_buffer(county_polygons, dist = 100000)
buffer <- buffered_counties[buffered_counties$namelsad == "Boise County",]

# county
data <- readRDS("test_nutrition_household_month.RDS")

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

county_record <- as.data.frame(unique(data$fips_county))
names(county_record) <- "fips_county"

county_record <- left_join(county_record, county_polygons, by = "fips_county")
county_record <- st_as_sf(county_record)

# station
list <- read_csv("list(STNID, LATITUDE, LONGITUDE).csv")
station_points <- as.data.frame(list) %>% 
  st_as_sf(coords=c("LONGITUDE","LATITUDE"), crs=4326, remove=FALSE) 

ggplot() +
  geom_sf(data = all_boundaries) +
  geom_sf(data = county_polygons, aes(fill = "County", color = "County"), lwd = 0.1) +
  geom_sf(data = county_record, aes(fill = "County with records", color = "County with records"), lwd = 0.1) +
  geom_sf(data = buffer, aes(fill = "100km Buffer of an exemplary county", color = "100km Buffer of an exemplary county"), lwd = 1.5) +
  geom_sf(data = station_points, aes(fill = "Climate Station", col = "Climate Station"), pch = 21, cex = 2) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  scale_fill_manual(values = c("County" = "white", "County with records" = "#c7e9c0", 
                               "100km Buffer of an exemplary county" = "transparent", "Climate Station" = "#006d2c"),
                    breaks = c("Climate Station", "County", "100km Buffer of an exemplary county", "County with records")) + 
  scale_color_manual(values = c("County" = "black", "County with records" = "black", 
                                "100km Buffer of an exemplary county" = "#bdbdbd", "Climate Station" = "gray20"),
                     breaks = c("Climate Station","County", "100km Buffer of an exemplary county", "County with records")) +
  labs(fill = "Legend", color = "Legend") +
  guides(fill = guide_legend(override.aes = list(pch = c(21, NA, NA, NA),
                                                 lwd = c(NA, 0.2, 1, 0.2),
                                                 cex = c(3, NA, NA, NA)))) +
  theme_void() +
  theme(legend.title = element_text(hjust = 0.5, size = 15), 
        legend.position = "bottom",
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12))

#### Figure S2 ####
Range <- readRDS("REVIEW coefficient R added sugar monthly range3.RDS")

Range$start <- ifelse(Range$range == "0-12", 0,
                      ifelse(Range$range == "12-30", 12,
                             ifelse(Range$range == "30+", 30, NA)))

dataset <- data.frame()

for (tas in c(0, 12, 30, 35)){
  dataset <- rbind(dataset, as.data.frame(list(x = tas, 
                                               y = ifelse(tas == 12, 12*Range[Range$start == 0, ]$c,
                                                          ifelse(tas == 30, 12*Range[Range$start == 0, ]$c + (30-12)*Range[Range$start == 12, ]$c,
                                                                 ifelse(tas == 35, 12*Range[Range$start == 0, ]$c + (30-12)*Range[Range$start == 12, ]$c + (35-30)*Range[Range$start == 30, ]$c, 0))),
                                               ll = ifelse(tas == 12, 12*Range[Range$start == 0, ]$ll,
                                                           ifelse(tas == 30, 12*Range[Range$start == 0, ]$ll + (30-12)*Range[Range$start == 12, ]$ll,
                                                                  ifelse(tas == 35, 12*Range[Range$start == 0, ]$ll + (30-12)*Range[Range$start == 12, ]$ll + (35-30)*Range[Range$start == 30, ]$ll, 0))),
                                               ul = ifelse(tas == 12, 12*Range[Range$start == 0, ]$ul,
                                                           ifelse(tas == 30, 12*Range[Range$start == 0, ]$ul + (30-12)*Range[Range$start == 12, ]$ul,
                                                                  ifelse(tas == 35, 12*Range[Range$start == 0, ]$ul + (30-12)*Range[Range$start == 12, ]$ul + (35-30)*Range[Range$start == 30, ]$ul, 0))) )))
}


ggplot(dataset, aes(x = x)) +
  geom_line(aes(y = y), color = "#6a3d9a", linewidth = 1.2) +
  geom_ribbon(aes(ymin = ll, ymax = ul), fill = "#cab2d6", alpha = 0.45) +
  labs(x = "Temperature", y = "Changes in added sugar consumption (g / person / day)") +
  scale_y_continuous(limits = c(-10, 30),breaks = seq(-10, 30, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  theme_classic() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12)) + 
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed",  linewidth = 0.5, alpha = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.3)

#### Figure S3 ####
dataset<-readRDS("REVIEW coefficient R price change with climate.RDS")

dataset <- dataset[dataset$coef == "c", ]

interval<-data.frame(id=c(1:19),
                     range=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                             "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"))

dataset<-merge(dataset, interval, by="id")

dataset$range<-factor(dataset$range, 
                      levels=c("<=0","0-2","2-4","4-6","6-8","8-10","10-12","12-14","14-16","16-18","18-20", 
                               "20-22","22-24","24-26","26-28","28-30","30-32","32-34",">34"), order=T) 

dataset$group <- ifelse(dataset$variable == "Sweetened Beverages", "Sugar added Beverages",
                        ifelse(dataset$variable == "Other Desserts", "Frozen desserts", dataset$variable))

dataset$group <- factor(dataset$group, 
                        levels=c("Sugar added Beverages", "Frozen desserts", 
                                 "Sugars", "Candy", 
                                 "Sweet Bakery Products", "Quick Breads and Bread Products", "Breads, Rolls, Tortillas",
                                 "Fats and Oils"), order=T) 

ggplot(dataset, aes(x = range, group = group)) +
  geom_line(aes(y = est, color = group), linewidth = 1.2) +
  labs(x = "Temperature", y = "Changes in log(price) per food category per 100g", color = "Food Category")+
  theme_classic() + 
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        text = element_text(size = 12),
        legend.position = c(0, 0.5), 
        legend.title = element_text(size = 15),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12)) +
  scale_color_manual(values = c("#e31a1c", "#1f78b4",
                                "#f6e8c3", "#cab2d6",
                                "#fb9a99", "#fdbf6f", "#b15928", 
                                "#ff7f00")) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_vline(xintercept = "10-12", col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.3) +
  geom_vline(xintercept = "28-30", col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.3) 

#### Figure S4 ####
Income <- readRDS("REVIEW coefficient R price change with climate household and income group level.RDS")
Income <- spread(Income, coef, est)

Income$category <- ifelse(Income$variable == "Sweetened Beverages", "Sugar added Beverages",
                          ifelse(Income$variable == "Other Desserts", "Frozen desserts", Income$variable))

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

dataset$category <- factor(dataset$category, 
                           levels=c("Sugar added Beverages", "Frozen desserts", 
                                    "Sugars", "Candy", 
                                    "Sweet Bakery Products", "Quick Breads and Bread Products", "Breads, Rolls, Tortillas",
                                    "Fats and Oils"), order=T)

ggplot(dataset, aes(x = x, group = category)) +
  geom_line(aes(y = y, color = category), linewidth = 1.2) +
  labs(title = "Income", x = "Temperature", y = "Changes in log(price) per food category per 100g", color = "Food Category")+
  theme_linedraw() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, size = 15),
        text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.position = "bottom",
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12),
        strip.background = element_blank(),
        strip.text = element_text(color = "black", size = 12)) + 
  scale_color_manual(values = c("#e31a1c", "#1f78b4",
                                "#f6e8c3", "#cab2d6",
                                "#fb9a99", "#fdbf6f", "#b15928", 
                                "#ff7f00")) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed",  linewidth = 0.5, alpha = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.3) +
  facet_wrap(~group)
#### Figure S5 ####
data <- as.matrix(readRDS("correlations.RDS"))

rownames(data) <- c("Male Head Education", "Female Head Education", "Income", "Ethnic", "Male Work Environment", "Female Work Environment")
colnames(data) <- c("Male Head Education", "Female Head Education", "Income", "Ethnic", "Male Work Environment", "Female Work Environment")

corrplot(data, method = c("pie"), type = c("upper"), 
         col.lim = c(0, 1),
         outline = "grey", 
         order = c("original"), 
         diag = TRUE, 
         tl.cex = 0.8,
         tl.col = 'black', 
         tl.srt = 90,
         addgrid.col= 'grey',
         addCoef.col = 'black', 
         number.cex = 0.8)

#### Figure S6 ####
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
  geom_line(linewidth = 1.2) +
  labs(x = "Temperature", y = "Changes in added sugar consumption (g / person / day)", color = "Climate Zone") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) + 
  scale_color_manual(values = c(rgb(99/255, 177/255, 191/255), rgb(103/255, 128/255, 188/255), rgb(179/255, 83/255, 19/255), rgb(139/255, 181/255, 52/255), rgb(240/255, 162/255, 51/255)))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Zone[Zone$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 2, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 1, show.legend = FALSE) +
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c(rgb(99/255, 177/255, 191/255), rgb(103/255, 128/255, 188/255), rgb(179/255, 83/255, 19/255), rgb(139/255, 181/255, 52/255), rgb(240/255, 162/255, 51/255)))+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.8) +
  theme_classic() +
  labs(title = "0-12℃")  +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(-0.8, 2.4), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(Zone[Zone$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 2, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 1, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) + 
  scale_color_manual(values = c(rgb(99/255, 177/255, 191/255), rgb(103/255, 128/255, 188/255), rgb(179/255, 83/255, 19/255), rgb(139/255, 181/255, 52/255), rgb(240/255, 162/255, 51/255)))+
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.8) +
  theme_classic() +
  labs(title = "12-30℃")  +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(-0.8, 2.4), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.3, 1, 0.1, 1, 0.5))
plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(3.5, 1))
#### Figure S7 ####
CDD<-readRDS("REVIEW CDD range 3.RDS")

CDD$group<-factor(CDD$group, 
                  levels=c("Very Low", "Low", "Medium","High", "Very High"), order=T) 

dataset <- data.frame()

CDD$start <- ifelse(CDD$range == "0-12", 0,
                    ifelse(CDD$range == "12-30", 12,
                           ifelse(CDD$range == "30+", 30, NA)))

for (race in unique(CDD$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*CDD[CDD$group == race & CDD$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*CDD[CDD$group == race & CDD$start == 0, ]$c + (30-12)*CDD[CDD$group == race & CDD$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*CDD[CDD$group == race & CDD$start == 0, ]$c + (30-12)*CDD[CDD$group == race & CDD$start == 12, ]$c + (35-30)*CDD[CDD$group == race & CDD$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Very Low", "Low", "Medium","High", "Very High"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Temperature", y = "Changes in added sugar consumption (g / person / day)", color = "Background CDD") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) + 
  scale_color_manual(values = c("#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3"))+
  scale_y_continuous(limits = c(-12, 40),breaks = seq(-10, 40, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(CDD[CDD$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 2, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 1, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12℃")  +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(-0.8, 2.4), breaks = seq(-0.8, 2.4, by = 0.8))

p3 <- ggplot(CDD[CDD$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 2, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 1, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#dadaeb", "#bcbddc", "#9e9ac8", "#807dba", "#6a51a3")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30℃")  +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(-0.8, 2.4), breaks = seq(-0.8, 2.4, by = 0.8))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.3, 1, 0.1, 1, 0.5))
plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(3.5, 1))

#### Figure S8 ####
Group <- readRDS("REVIEW SSB consumption range 3.RDS")

Group$group <- factor(Group$group, 
                      levels=c("Low", "High"), order=T) 

dataset <- data.frame()

Group$start <- ifelse(Group$range == "0-12", 0,
                      ifelse(Group$range == "12-30", 12,
                             ifelse(Group$range == "30+", 30, NA)))

for (race in unique(Group$group)) {
  for (tas in c(0, 12, 30, 35)){
    dataset <- rbind(dataset, as.data.frame(list(x = tas, group = race,
                                                 y = ifelse(tas == 12, 12*Group[Group$group == race & Group$start == 0, ]$c,
                                                            ifelse(tas == 30, 12*Group[Group$group == race & Group$start == 0, ]$c + (30-12)*Group[Group$group == race & Group$start == 12, ]$c,
                                                                   ifelse(tas == 35, 12*Group[Group$group == race & Group$start == 0, ]$c + (30-12)*Group[Group$group == race & Group$start == 12, ]$c + (35-30)*Group[Group$group == race & Group$start == 30, ]$c, 0))))))
  }
}

dataset$group<-factor(dataset$group, 
                      levels=c("Low", "High"), order=T) 

p1 <- ggplot(dataset, aes(x = x, y = y, group = group, color = group)) +
  geom_line(linewidth = 1.2) +
  labs(x = "", y = "Changes in added sugar consumption \n(g / person / day)", color = "SSB Consumption Group") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 15)) + 
  scale_color_manual(values = c("#fcbba1", "#ef3b2c"))+
  scale_y_continuous(limits = c(-12, 52.7),breaks = seq(-10, 50, by = 10)) +
  scale_x_continuous(expand = c(0.01, 0), 
                     limits = c(0, 35), breaks = seq(0, 35, by = 5),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
  geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

p2 <- ggplot(Group[Group$range == "0-12", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 2, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 1, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#fcbba1", "#ef3b2c")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "0-12℃")  +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(-0.5, 2.73), breaks = seq(-0.5, 2.5, by = 0.5))

p3 <- ggplot(Group[Group$range == "12-30", ], aes(x = group, color = group)) +
  geom_point(aes(y = c), size = 2, alpha = 0.8, show.legend = FALSE) +
  geom_linerange(aes(ymin = ll, ymax = ul), linewidth = 1, show.legend = FALSE) + 
  labs(y = "Regression Slope") +
  xlab(NULL) +
  scale_color_manual(values = c("#fcbba1", "#ef3b2c")) +
  geom_hline(yintercept = 0, col = "black", linetype = "dashed", size = 0.5) +
  theme_classic() +
  labs(title = "12-30℃")  +
  theme(plot.title = element_text(hjust = 0.5, margin = margin(b = 10)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  scale_y_continuous(limits = c(-0.5, 2.73), breaks = seq(-0.5, 2.5, by = 0.5))

p2_p3 <- plot_grid(NULL, p2, NULL, p3, NULL, ncol = 1, align = "v",rel_heights = c(0.3, 1, 0.1, 1, 0.5))
plot_grid(p1, p2_p3, nrow = 1, rel_widths = c(3.5, 1))

#### Figure S9 ####
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

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

plot1 <- list()

for (i in 1:7){
  data <- dataset[dataset$category == drinks[i], ]
  plot1[[i]] <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(x = "", y = "Changes in drink consumption \n(g / person / day)", color = "Male Head Education") + 
    theme_classic() +
    theme(axis.line = element_line(color = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) + 
    scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858"))+
    scale_y_continuous(limits = c(-17.03, 55.1), breaks = seq(-10, 50, by = 10)) +
    scale_x_continuous(expand = c(0.01, 0), 
                       limits = c(0, 35), breaks = seq(0, 35, by = 5),
                       labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
    geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)
}

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

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

plot2 <- list()

for (i in 1:7){
  data <- dataset[dataset$category == drinks[i], ]
  plot2[[i]] <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(x = "", y = "", color = "Female Head Education") + 
    theme_classic() +
    theme(axis.line = element_line(color = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) + 
    scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"))+
    scale_y_continuous(limits = c(-17.03, 55.1), breaks = seq(-10, 50, by = 10)) +
    scale_x_continuous(expand = c(0.01, 0), 
                       limits = c(0, 35), breaks = seq(0, 35, by = 5),
                       labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
    geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)
}

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

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

plot3 <- list()

for (i in 1:7){
  data <- dataset[dataset$category == drinks[i], ]
  plot3[[i]] <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(x = "", y = "", color = "Income") + 
    theme_classic() +
    theme(axis.line = element_line(color = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) + 
    scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525"))+
    scale_y_continuous(limits = c(-17.03, 55.1), breaks = seq(-10, 50, by = 10)) +
    scale_x_continuous(expand = c(0.01, 0), 
                       limits = c(0, 35), breaks = seq(0, 35, by = 5),
                       labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
    geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)
}

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

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

plot4 <- list()

for (i in 1:7){
  data <- dataset[dataset$category == drinks[i], ]
  plot4[[i]] <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Temperature", y = "Changes in drink consumption \n(g / person / day)", color = "Ethnic") + 
    theme_classic() +
    theme(axis.line = element_line(color = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) + 
    scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a"))+
    scale_y_continuous(limits = c(-17.03, 55.1), breaks = seq(-10, 50, by = 10)) +
    scale_x_continuous(expand = c(0.01, 0), 
                       limits = c(0, 35), breaks = seq(0, 35, by = 5),
                       labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
    geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)
}

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

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

plot5 <- list()

for (i in 1:7){
  data <- dataset[dataset$category == drinks[i], ]
  plot5[[i]] <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Temperature", y = "", color = "Male Work Environment") + 
    theme_classic() +
    theme(axis.line = element_line(color = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) + 
    scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
    scale_y_continuous(limits = c(-17.03, 55.1), breaks = seq(-10, 50, by = 10)) +
    scale_x_continuous(expand = c(0.01, 0), 
                       limits = c(0, 35), breaks = seq(0, 35, by = 5),
                       labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
    geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)
}

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

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

plot6 <- list()

for (i in 1:7){
  data <- dataset[dataset$category == drinks[i], ]
  plot6[[i]] <- ggplot(data, aes(x = x, y = y, group = group, color = group)) +
    geom_line(linewidth = 1.2) +
    labs(x = "Temperature", y = "", color = "Female Work Environment") + 
    theme_classic() +
    theme(axis.line = element_line(color = "black"),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
          axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          plot.title = element_text(hjust = 0.5),
          text = element_text(size = 12),
          legend.position = c(0, 1), 
          legend.justification = c(0, 1),
          legend.background = element_blank(),
          legend.key.width = unit(0.6, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 10)) + 
    scale_color_manual(values = c("#74a9cf", "#fc8d59"))+
    scale_y_continuous(limits = c(-17.03, 55.1), breaks = seq(-10, 50, by = 10)) +
    scale_x_continuous(expand = c(0.01, 0), 
                       limits = c(0, 35), breaks = seq(0, 35, by = 5),
                       labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
    geom_vline(xintercept = 12, col = "black", linetype = "dashed", size = 0.3) +
    geom_vline(xintercept = 30, col = "black", linetype = "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)
}

drinks <- c("100% Juice", "Alcoholic Beverages", "Coffee and Tea", 
            "Dairy Drinks and Substitutes", "Diet Beverages", "Milk", "Yogurt") 

title <- list()

for (i in 1:7){
  title[[i]] <- ggdraw() + draw_label(drinks[i], fontface='bold')
}

p1 <- plot_grid(plot1[[1]], plot2[[1]], plot3[[1]], plot4[[1]], plot5[[1]], plot6[[1]], 
                labels = c("a.1", "a.2", "a.3", "a.4", "a.5", "a.6"), nrow = 2, ncol = 3, align = "vh")
p1 <- plot_grid(title[[1]], p1, ncol=1, rel_heights=c(0.1, 1))

p2 <- plot_grid(plot1[[2]], plot2[[2]], plot3[[2]], plot4[[2]], plot5[[2]], plot6[[2]], 
                labels = c("b.1", "b.2", "b.3", "b.4", "b.5", "b.6"), nrow = 2, ncol = 3, align = "vh")
p2 <- plot_grid(title[[2]], p2, ncol=1, rel_heights=c(0.1, 1))

p3 <- plot_grid(plot1[[3]], plot2[[3]], plot3[[3]], plot4[[3]], plot5[[3]], plot6[[3]], 
                labels = c("c.1", "c.2", "c.3", "c.4", "c.5", "c.6"), nrow = 2, ncol = 3, align = "vh")
p3 <- plot_grid(title[[3]], p3, ncol=1, rel_heights=c(0.1, 1))

p4 <- plot_grid(plot1[[4]], plot2[[4]], plot3[[4]], plot4[[4]], plot5[[4]], plot6[[4]], 
                labels = c("d.1", "d.2", "d.3", "d.4", "d.5", "d.6"), nrow = 2, ncol = 3, align = "vh")
p4 <- plot_grid(title[[4]], p4, ncol=1, rel_heights=c(0.1, 1))

p5 <- plot_grid(plot1[[5]], plot2[[5]], plot3[[5]], plot4[[5]], plot5[[5]], plot6[[5]], 
                labels = c("e.1", "e.2", "e.3", "e.4", "e.5", "e.6"), nrow = 2, ncol = 3, align = "vh")
p5 <- plot_grid(title[[5]], p5, ncol=1, rel_heights=c(0.1, 1))

p6 <- plot_grid(plot1[[6]], plot2[[6]], plot3[[6]], plot4[[6]], plot5[[6]], plot6[[6]], 
                labels = c("f.1", "f.2", "f.3", "f.4", "f.5", "f.6"), nrow = 2, ncol = 3, align = "vh")
p6 <- plot_grid(title[[6]], p6, ncol=1, rel_heights=c(0.1, 1))

p7 <- plot_grid(plot1[[7]], plot2[[7]], plot3[[7]], plot4[[7]], plot5[[7]], plot6[[7]], 
                labels = c("g.1", "g.2", "g.3", "g.4", "g.5", "g.6"), nrow = 2, ncol = 3, align = "vh")
p7 <- plot_grid(title[[7]], p7, ncol=1, rel_heights=c(0.1, 1))


plot_grid(p1, p2, p3, p4, p5, p6, p7, ncol = 2, nrow = 4, align = "vh")

#### Figure S10 ####
CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
                 "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
                 "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
                 "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
                 "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")

total8.5 <- data.frame()

for (mod in CMIP6_model){
  mod8.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6/", mod, "_MA.RDS"))
  
  mod8.5 <- select(mod8.5, year, month, fips_county, adjusted_hurs, adjusted_pr, adjusted_sfcWind, adjusted_tas)
  names(mod8.5) <- c("year", "month", "fips_county", "hurs", "pr", "sfcWind", "tas")
  
  mod8.5 <- mod8.5[mod8.5$year == 2019 | mod8.5$year == 2095, ]
  
  mod8.5$model <- mod
  
  total8.5 <- rbind(total8.5, mod8.5)
}

total8.5$season <- ifelse(total8.5$month %in% c(3, 4, 5), "Spring",
                          ifelse(total8.5$month %in% c(6, 7, 8), "Summer",
                                 ifelse(total8.5$month %in% c(9, 10, 11), "Autumn", "Winter")))

all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)
county_polygons$fips_county <- paste(county_polygons$statefp, county_polygons$countyfp, sep = "")

total <- total8.5 %>%
  group_by(year, season, fips_county) %>%
  summarise(tas = mean(tas)) %>%
  ungroup()

total$tas <- ifelse(total$tas <=0, 0, total$tas)

tas <- right_join(total, county_polygons, by = "fips_county")
tas <- st_as_sf(tas)

tas$year <- factor(tas$year, 
                   levels=c(2019, 2095), order=T) 

tas$season <- factor(tas$season, 
                     levels=c("Spring", "Summer", "Autumn", "Winter"), order=T) 

p <- ggplot() +
  geom_sf(data = all_boundaries) +
  geom_sf(data = tas, aes(fill = tas), lwd = 0.05) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(fill = "Average temperature") +
  scale_fill_gradientn(colours = c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"), 
                       na.value = "white",
                       limits = c(0, max(tas$tas)),
                       breaks = c(0, 10, 20, 30),
                       labels = c("<=0", "10", "20", "30")) +
  theme_void() +
  theme(legend.title = element_text(hjust = 0.5, size = 20), 
        legend.position = "bottom",  
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        strip.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12)) + 
  facet_wrap(~year + season, ncol = 4)

g <- ggplotGrob(p)

strips <- g$layout[grep("strip-t", g$layout$name), ]
strips <- strips[c(5, 6, 7, 8,
                   1, 2, 3, 4),]

titles <- lapply(letters[seq_len(nrow(strips))],   
                 function(label) textGrob(label, x=0.05, y=0.8, hjust=0, vjust=1, gp=gpar(fontface="bold")))  

g <- gtable_add_grob(g, grobs = titles, 
                     t = strips$t, b = strips$b, 
                     l = strips$l, r = strips$r)
grid.newpage()
grid.draw(g)
#### Figure S11 ####
# Time series
predict_result <- readRDS("REVIEW Projection change of CMIP6-ssp4.5 adjusted nation range3.RDS")

predict_result$season <- ifelse(predict_result$month %in% c(3, 4, 5), "Spring",
                                ifelse(predict_result$month %in% c(6, 7, 8), "Summer",
                                       ifelse(predict_result$month %in% c(9, 10, 11), "Autumn", "Winter")))

season_result <- predict_result %>%
  group_by(year, season) %>%
  summarise(result= mean(result, na.rm = TRUE)) %>%
  ungroup()

season_result <- spread(season_result, season, result)

year_result <- predict_result %>%
  group_by(year, model) %>%
  mutate(result =  mean(result, na.rm = TRUE)) %>%
  ungroup()

year_result <- year_result %>%
  group_by(year) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

year_result <- year_result %>%
  group_by(year, result_min, result_max) %>%
  summarise(Annual =  mean(result, na.rm = TRUE)) %>%
  ungroup()

dataset <- inner_join(year_result, season_result, by = "year")
dataset <- pivot_longer(dataset,
                        cols = c("Annual", "Spring", "Summer", "Autumn", "Winter"),
                        names_to = "group",
                        values_to = "result")

dataset$group<-factor(dataset$group, 
                      levels=c("Annual", "Spring", "Summer", "Autumn", "Winter"), order=T) 

p1 <- ggplot(dataset, aes(x = year)) +
  geom_line(aes(y = result, group = group, color = group, size = group, linetype = group)) +
  geom_ribbon(aes(ymin = result_min, ymax = result_max),
              alpha = 0.1, color = "#d9d9d9") +
  labs(x = "Year", y = "Changes in added sugar consumption growth (g / person / day)") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 12),
        legend.position = c(0.05, 0.7), 
        legend.justification = c(0, 1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 17)) +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(2030, 2100, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.2, 5.15), breaks = seq(0, 5, by = 1)) +
  scale_color_manual(values = c("#6a3d9a", "#33a02c", "#e31a1c", "#ff7f00", "#1f78b4")) +
  scale_size_manual(values=c(1.8, 0.6, 0.6, 0.6, 0.6)) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed", "dashed")) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Histogram
CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
                 "CanESM5", "CanESM5-1", 
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
                 "FGOALS-f3-L", "FGOALS-g3",  
                 "IPSL-CM6A-LR",
                 "KACE-1-0-G",
                 "MIROC6", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")
total4.5 <- data.frame()

for (mod in CMIP6_model){
  mod4.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "_MA.RDS"))
  
  mod4.5 <- select(mod4.5, year, month, fips_county, adjusted_hurs, adjusted_pr, adjusted_sfcWind, adjusted_tas)
  names(mod4.5) <- c("year", "month", "fips_county", "hurs", "pr", "sfcWind", "tas")
  
  mod4.5 <- mod4.5[mod4.5$year == 2019 | mod4.5$year == 2095, ]
  
  mod4.5$model <- mod
  
  total4.5 <- rbind(total4.5, mod4.5)
}

household <- readRDS("Predict household.RDS")

total <- inner_join(total4.5, household, by = "fips_county", multiple = "all")

total$year <- as.factor(total$year)

p2 <- ggplot(total, aes(x = tas, fill = year)) +
  geom_histogram(position = "identity", binwidth = 1, alpha = 0.6) +
  labs(x = "Temperature", y = "Number of Households") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.position = c(0, 1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(0.2, "cm"),
        legend.text = element_text(size = 12))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40), breaks = seq(0, 40, by = 10),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 570000), breaks = seq(0, 500000, by = 100000)) +
  scale_fill_manual(values = c("#d9d9d9", "#fcbba1"))

plot1 <- ggdraw(p1) +
  draw_plot(p2, x = 0.1, y = 0.7, width = 0.3, height = 0.3)

# Map
# get boundary with high definition of all states and counties
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)
county_polygons$fips_county <- paste(county_polygons$statefp, county_polygons$countyfp, sep = "")

predict_result_4.5 <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted county range3.RDS")

county_result_4.5 <- predict_result_4.5 %>%
  group_by(fips_county) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

county_result_4.5 <- right_join(county_result_4.5, county_polygons, by = "fips_county")
county_result_4.5 <- st_as_sf(county_result_4.5)

predict_result_8.5 <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted county range3.RDS")

county_result_8.5 <- predict_result_8.5 %>%
  group_by(fips_county) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

county_result_8.5 <- right_join(county_result_8.5, county_polygons, by = "fips_county")
county_result_8.5 <- st_as_sf(county_result_8.5)

p3 <- ggplot() +
  geom_sf(data = all_boundaries) +
  geom_sf(data = county_result_4.5, aes(fill = result), lwd = 0.05) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(fill = "Changes in added sugar consumption growth \n (g / person / day)") +
  scale_fill_gradientn(colours = c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"), 
                       na.value = "white",
                       limits = c(0, max(county_result_4.5$result, county_result_8.5$result, na.rm = TRUE))) +
  theme_void() +
  theme(legend.title = element_text(hjust = 0.5, size = 15),
        legend.text = element_text(size = 14),
        legend.position = "bottom")

# Heterogeneity test
# Male Education
Male_Head_Education<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Male_Head_Education.RDS")

Male_Head_Education$Male_Head_Education <- ifelse(Male_Head_Education$Male_Head_Education == 1, "Grade School",
                                                  ifelse(Male_Head_Education$Male_Head_Education == 2, "Some High School",
                                                         ifelse(Male_Head_Education$Male_Head_Education == 3, "Graduated High School",
                                                                ifelse(Male_Head_Education$Male_Head_Education == 4, "Some College",
                                                                       ifelse(Male_Head_Education$Male_Head_Education == 5, "Graduated College", "Post College Grad")))))

Male_Head_Education <- Male_Head_Education %>%
  group_by(Male_Head_Education, model) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

# describe(Male_Head_Education[Male_Head_Education$Male_Head_Education == "Grade School",]$result)

# describe(Male_Head_Education[Male_Head_Education$Male_Head_Education == "Some High School",]$result)

# describe(Male_Head_Education[Male_Head_Education$Male_Head_Education == "Graduated High School",]$result)

# describe(Male_Head_Education[Male_Head_Education$Male_Head_Education == "Some College",]$result)

# describe(Male_Head_Education[Male_Head_Education$Male_Head_Education == "Graduated College",]$result)

# describe(Male_Head_Education[Male_Head_Education$Male_Head_Education == "Post College Grad",]$result)

Male_Head_Education <- Male_Head_Education %>%
  group_by(Male_Head_Education) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Male_Head_Education <- Male_Head_Education %>%
  group_by(Male_Head_Education, result_min, result_max) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Male_Head_Education$Male_Head_Education<-factor(Male_Head_Education$Male_Head_Education,
                                                levels=c("Grade School", "Some High School", "Graduated High School",
                                                         "Some College", "Graduated College", "Post College Grad"), order=T)

names(Male_Head_Education) <- c("category", "result_min", "result_max", "result")

p4 <- ggplot(Male_Head_Education, aes(x = category, y = result, color = category)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 1) +
  labs(y = "Changes in added sugar consumption growth \n(g / person / day)", color = "Male Head Education") + 
  xlab(NULL) +
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        legend.position = c(0, 0.99), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.07, size = 20))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Female Education
Female_Head_Education<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Female_Head_Education.RDS")

Female_Head_Education$Female_Head_Education <- ifelse(Female_Head_Education$Female_Head_Education == 1, "Grade School",
                                                      ifelse(Female_Head_Education$Female_Head_Education == 2, "Some High School",
                                                             ifelse(Female_Head_Education$Female_Head_Education == 3, "Graduated High School",
                                                                    ifelse(Female_Head_Education$Female_Head_Education == 4, "Some College",
                                                                           ifelse(Female_Head_Education$Female_Head_Education == 5, "Graduated College", "Post College Grad")))))

Female_Head_Education <- Female_Head_Education %>%
  group_by(Female_Head_Education, model) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

#describe(Female_Head_Education[Female_Head_Education$Female_Head_Education == "Grade School",]$result)

#describe(Female_Head_Education[Female_Head_Education$Female_Head_Education == "Some High School",]$result)

#describe(Female_Head_Education[Female_Head_Education$Female_Head_Education == "Graduated High School",]$result)

#describe(Female_Head_Education[Female_Head_Education$Female_Head_Education == "Some College",]$result)

#describe(Female_Head_Education[Female_Head_Education$Female_Head_Education == "Graduated College",]$result)

#describe(Female_Head_Education[Female_Head_Education$Female_Head_Education == "Post College Grad",]$result)


Female_Head_Education <- Female_Head_Education %>%
  group_by(Female_Head_Education) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Female_Head_Education <- Female_Head_Education %>%
  group_by(Female_Head_Education, result_min, result_max) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Female_Head_Education$Female_Head_Education<-factor(Female_Head_Education$Female_Head_Education,
                                                    levels=c("Grade School", "Some High School", "Graduated High School",
                                                             "Some College", "Graduated College", "Post College Grad"), order=T)

names(Female_Head_Education) <- c("category", "result_min", "result_max", "result")

p5 <- ggplot(Female_Head_Education, aes(x = category, y = result, color = category)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 1) +
  labs(y = "", color = "Female Head Education") + 
  xlab(NULL) +
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        legend.position = c(0, 0.99), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.07, size = 20))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Income
Income<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Income.RDS")

Income <- Income %>%
  group_by(Income_group, model) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Income <- Income %>%
  group_by(Income_group) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Income <- Income %>%
  group_by(Income_group, result_min, result_max) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Income$Income_group<-factor(Income$Income_group, 
                            levels=c("Very Low", "Low", "Medium","High", "Very High"), order=T) 

names(Income) <- c("category", "result_min", "result_max", "result")

p6 <- ggplot(Income, aes(x = category, y = result, color = category)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 1) +
  labs(y = "Changes in added sugar consumption growth \n(g / person / day)", color = "Income") + 
  xlab(NULL) +
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        legend.position = c(0, 0.99), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.07, size = 20))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Ethnic
Ethnic<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Ethnic.RDS")

Ethnic$Ethnic <- ifelse(Ethnic$Ethnic == 1, "White/Caucasian",
                        ifelse(Ethnic$Ethnic == 2, "Black/African American",
                               ifelse(Ethnic$Ethnic == 3, "Asian",
                                      ifelse(Ethnic$Ethnic == 4, "Other", "Hispanic"))))

Ethnic <- Ethnic %>%
  group_by(Ethnic, model) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Ethnic <- Ethnic %>%
  group_by(Ethnic) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Ethnic <- Ethnic %>%
  group_by(Ethnic, result_min, result_max) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Ethnic$Ethnic<-factor(Ethnic$Ethnic, 
                      levels=c("White/Caucasian", "Other", "Black/African American", "Hispanic", "Asian" ), order=T) 

names(Ethnic) <- c("category", "result_min", "result_max", "result")

p7 <- ggplot(Ethnic, aes(x = category, y = result, color = category)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 1) +
  labs(y = "", color = "Ethnic") + 
  xlab(NULL) +
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        legend.position = c(0, 0.99), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.06, size = 20))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Male Work Environment
Environment <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Male Environment.RDS")

Environment <- Environment %>%
  group_by(Environment, model) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Environment <- Environment %>%
  group_by(Environment) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Environment <- Environment %>%
  group_by(Environment, result_min, result_max) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Environment$Environment<-factor(Environment$Environment, 
                                levels=c("Indoor", "Outdoor"), order=T) 

names(Environment) <- c("category", "result_min", "result_max", "result")

p8 <- ggplot(Environment, aes(x = category, y = result, color = category)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 1) +
  labs(y = "Changes in added sugar consumption growth \n(g / person / day)", color = "Male Work Environment") + 
  xlab(NULL) +
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        legend.position = c(0, 0.99), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.07, size = 20))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Female Work Environment
Environment <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Female Environment.RDS")

Environment <- Environment %>%
  group_by(Environment, model) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Environment <- Environment %>%
  group_by(Environment) %>%
  mutate(result_min =  min(result, na.rm = TRUE)) %>%
  mutate(result_max =  max(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Environment <- Environment %>%
  group_by(Environment, result_min, result_max) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

Environment$Environment<-factor(Environment$Environment, 
                                levels=c("Indoor", "Outdoor"), order=T) 

names(Environment) <- c("category", "result_min", "result_max", "result")

p9 <- ggplot(Environment, aes(x = category, y = result, color = category)) +
  geom_point(size = 2) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 1) +
  labs(y = "", color = "Female Work Environment") + 
  xlab(NULL) +
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        text = element_text(size = 12),
        legend.position = c(0, 0.99), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.8, "cm"),
        legend.text = element_text(size = 14)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.07, size = 20))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)



plot3 <- plot_grid(p4, p5, p6, p7, p8, p9, 
                   labels = c("c.1", "c.2", "c.3", "c.4", "c.5", "c.6"), nrow = 3, ncol = 2, align = "vh")
p1_p2 <- plot_grid(plot1, p3, labels = c("a", "b"), ncol = 1, align = "v")
plot_grid(p1_p2, plot3, nrow = 1, align = "h", rel_widths = c(1, 1.5))
