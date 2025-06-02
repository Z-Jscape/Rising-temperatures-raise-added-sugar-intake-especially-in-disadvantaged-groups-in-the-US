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

#### Figure 4 ####
pdf("Figure 4.pdf",width=180/25.4,height=130/25.4)

# Time series
predict_result <- readRDS("REVIEW Projection change of CMIP6-ssp8.5 adjusted nation range3.RDS")

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

dataset <- dataset %>%
  mutate(result_min = ifelse(group == "Annual", result_min, 0),
         result_max = ifelse(group == "Annual", result_max, 0))

dataset$group<-factor(dataset$group, 
                      levels=c("Annual", "Spring", "Summer", "Autumn", "Winter"), order=T) 

p1 <- ggplot(dataset, aes(x = year)) +
  geom_line(aes(y = result, group = group, color = group, size = group, linetype = group)) +
  geom_ribbon(aes(ymin = result_min, ymax = result_max, fill = group), alpha = 0.45) +
  labs(x = "Year", y = "Changes in added sugar consumption growth \n(g / person / day)") + 
  theme_classic() +
  theme(axis.line = element_line(color = "black"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 6),
        legend.position = c(0.01, 0.62), 
        legend.justification = c(0, 1),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.25, "cm"),
        legend.text = element_text(size = 6)) +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(2030, 2100, 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-0.2, 5.15), breaks = seq(0, 5, by = 1)) +
  scale_color_manual(values = c("#6a3d9a", "#33a02c", "#e31a1c", "#ff7f00", "#1f78b4")) +
  scale_fill_manual(values = c("#d9d9d9", "white", "white","white","white")) +
  scale_size_manual(values=c(0.9, 0.3, 0.3, 0.3, 0.3)) +
  scale_linetype_manual(values = c("solid", "dashed", "dashed", "dashed", "dashed")) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Histogram
CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
                 "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
                 "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
                 "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
                 "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")

total8.5 <- data.frame()

for (mod in CMIP6_model){
  mod8.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "_MA.RDS"))
  
  mod8.5 <- select(mod8.5, year, month, fips_county, adjusted_hurs, adjusted_pr, adjusted_sfcWind, adjusted_tas)
  names(mod8.5) <- c("year", "month", "fips_county", "hurs", "pr", "sfcWind", "tas")
  
  mod8.5 <- mod8.5[mod8.5$year == 2019 | mod8.5$year == 2095, ]
  
  mod8.5$model <- mod
  
  total8.5 <- rbind(total8.5, mod8.5)
}

household <- readRDS("Predict household.RDS")

total <- inner_join(total8.5, household, by = "fips_county", multiple = "all")

total$year <- as.factor(total$year)

p2 <- ggplot(total, aes(x = tas, fill = year)) +
  geom_histogram(position = "identity", binwidth = 1, alpha = 0.4) +
  labs(x = "Temperature", y = "Number of Households") +
  theme_classic() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        axis.title.y = element_text(margin = margin(t = 0, r = 1, b = 0, l = 0)),
        text = element_text(size = 5),
        legend.position = c(-0.05, 1.1), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key.width = unit(0.05, "cm"),
        legend.key.height = unit(0.1, "cm"),
        legend.text = element_text(size = 5))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 40), breaks = seq(0, 40, by = 10),
                     labels = function(x) ifelse(x <= 0, "<=0", as.character(x))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 570000), breaks = seq(0, 500000, by = 100000)) +
  scale_fill_manual(values = c("#d9d9d9", "#fcbba1"))

plot1 <- ggdraw(p1) +
  draw_plot(p2, x = 0.2, y = 0.6, width = 0.4, height = 0.4)

# Map
# get boundary with high definition of all states and counties
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)
county_polygons$fips_county <- paste(county_polygons$statefp, county_polygons$countyfp, sep = "")

predict_result <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted county range3.RDS")

county_result <- predict_result %>%
  group_by(fips_county) %>%
  summarise(result =  mean(result, na.rm = TRUE)) %>%
  ungroup() %>%
  distinct()

county_result <- right_join(county_result, county_polygons, by = "fips_county")
county_result <- st_as_sf(county_result)

p3 <- ggplot() +
  geom_sf(data = all_boundaries) +
  geom_sf(data = county_result, aes(fill = result), lwd = NA) +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  labs(fill = "Changes in added sugar consumption growth \n (g / person / day)") +
  scale_fill_gradientn(colours = c("#fff7ec", "#fee8c8", "#fdd49e", "#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000"), 
                       na.value = "white",
                       limits = c(0, max(county_result$result, na.rm = TRUE))) +
  theme_void() +
  theme(legend.title = element_text(hjust = 0.5, size = 6),
        legend.text = element_text(size = 5),
        legend.position = "bottom",
        legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.3, "cm"),
        plot.margin = margin(t = 0, r = 0, b = 0, l = 10))

# Heterogeneity test
# Male Education
Male_Head_Education<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Male_Head_Education.RDS")

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
  geom_point(size = 0.8) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 0.5) +
  labs(color = "Male Head Education") + 
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c("#a6bddb", "#74a9cf", "#3690c0", "#0570b0", "#045a8d", "#023858")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 6),
        legend.position = c(0, 1.05), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 5)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.05, size = 7))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Female Education
Female_Head_Education<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Female_Head_Education.RDS")

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
  geom_point(size = 0.8) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 0.5) +
  labs(color = "Female Head Education") + 
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c("#fdbb84", "#fc8d59", "#ef6548", "#d7301f", "#b30000", "#7f0000")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 6),
        legend.position = c(0, 1.05), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 5)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.05, size = 7))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Income
Income<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Income.RDS")

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
  geom_point(size = 0.8) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 0.5) +
  labs(color = "Income") + 
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c("#d9d9d9", "#bdbdbd", "#969696", "#737373", "#525252", "#252525")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 6),
        legend.position = c(0, 1.05), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.05, size = 7))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Ethnic
Ethnic<-readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Ethnic.RDS")

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
  geom_point(size = 0.8) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 0.5) +
  labs(color = "Ethnic") + 
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c("#f768a1", "#ff7f00", "#33a02c", "#b15928", "#6a3d9a")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 6),
        legend.position = c(0, 1.05), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.05, size = 7))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Male Work Environment
Environment <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Male Environment.RDS")

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
  geom_point(size = 0.8) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 0.5) +
  labs(color = "Male Work Environment") + 
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 6),
        legend.position = c(0, 1.05), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.05, size = 7))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

# Female Work Environment
Environment <- readRDS("REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Female Environment.RDS")

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
  geom_point(size = 0.8) +
  geom_linerange(aes(ymin = result_min, ymax = result_max), linewidth = 0.5) +
  labs(color = "Female Work Environment") + 
  xlab(NULL) + ylab(NULL) +
  scale_color_manual(values = c("#74a9cf", "#fc8d59")) +
  theme_classic() +  
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(size = 6),
        legend.position = c(0, 1.05), 
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key.width = unit(0.1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        legend.text = element_text(size = 6)) +
  scale_y_continuous(limits = c(-2.5, 10.5), breaks = seq(-2.5, 10, by = 2.5)) +
  guides(color = guide_legend(ncol = 2, title.theme = element_text(hjust = 0.05, size = 7))) +
  geom_hline(aes(yintercept = 0), col = "black", linetype = "dashed", linewidth = 0.5, alpha = 0.8)

plot3 <- plot_grid(p4, p5, p6, p7, p8, p9, 
                   labels = c("c.1", "c.2", "c.3", "c.4", "c.5", "c.6"), label_size = 8, nrow = 3, ncol = 2, align = "vh")
y <- ggdraw() + draw_label("Changes in added sugar consumption growth (g / person / day)", fontface='plain', size = 6, angle = 90)
plot3 <- plot_grid(y, plot3, ncol=2, rel_widths=c(0.03, 1))

p1_p2 <- plot_grid(plot1, p3, labels = c("a", "b"), label_size = 8, ncol = 1, align = "v")
plot_grid(p1_p2, plot3, nrow = 1, align = "h", rel_widths = c(1, 1.5))

dev.off()