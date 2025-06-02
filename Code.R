setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption")

library(vctrs)
library(readxl)
library(tidyverse)
library(haven)
library(reshape2)
library(data.table)
library(plm)
library(fixest)
library(broom)
library(dplyr)
library(modelsummary)
library(grid)
library(vcd)
library(car)

#### 1. products, nutrient and household data ####
# convert data to .RDS
for (i in 2004:2019) {
  setwd(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i, "/Annual_Files/"))
  
  # income, educational level, household size, etc.
  panelists <- read.delim(paste0("panelists_", i, ".tsv"))
  names(panelists)[names(panelists) == "household_cd"] <- "household_code"
  saveRDS(panelists, file = paste0("panelists_", i, ".RDS"))
  
  # product name, quantity, unit, price, promotion, etc.
  purchases <- read.delim(paste0("purchases_", i, ".tsv"))
  saveRDS(purchases, file = paste0("purchases_", i, ".RDS"))
  
  # date, store type, etc.
  trips <- read.delim(paste0("trips_", i, ".tsv"))
  saveRDS(trips, file = paste0("trips_", i, ".RDS"))
  
  # extra attributes for UPC codes
  products_extra <- read.delim(paste0("products_extra_", i, ".tsv"))
  saveRDS(products_extra, file = paste0("products_extra_", i, ".RDS"))
}

# products list
# detailed product information for each UPC code, such as UPC description, brand description, multi pack and size.
products <- read_tsv("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Master_Files/nielsen_extracts/HMS/Master_Files/Latest/products.tsv")
saveRDS(products, file = "products.RDS")

products <- readRDS("products.RDS")

# drop no-food products
data <- products
data <- data[!(data$product_module_code > 125 & data$product_module_code < 147), ]
data <- data[!(data$product_module_code > 153 & data$product_module_code < 400), ]
data <- data[!(data$product_group_code > 5500 & !is.na(data$product_group_code)), ]
data <- data[!(data$product_group_code > 4500 & data$product_group_code < 5000), ]
data <- data[!(data$product_group_code == 501 | data$product_group_code == 508), ]

saveRDS(data, file = "food product.RDS")

# "FNDDS2017-2018.xlsx" - nutrition information of FNDDS food
nutrition_fact <- read_excel("FNDDS2017-2018.xlsx", sheet = "all nutrition fact")

nutrition_fact <- nutrition_fact[, c("CODE", "DESCRIPTION", 
                                     "ADD_SUGARS", "Sugars, total")]

saveRDS(nutrition_fact, file = "nutrition fact.RDS")

# match Products to FNDDS food
product_hierarchy <- read_excel("Product_Hierarchy_01.2021.xlsx", sheet = "match")

names(product_hierarchy)[5] <- "CODE"
names(product_hierarchy)[6] <- "DESCRIPTION"

nutrition_fact <- readRDS("nutrition fact.RDS")
food_nutrition_fact <- inner_join(product_hierarchy, nutrition_fact, by = c("CODE", "DESCRIPTION"))

saveRDS(food_nutrition_fact, file = "food nutrition fact.RDS")

# get more information of Products
food_nutrition_fact <- readRDS("food nutrition fact.RDS")

food_product <- readRDS("food product.RDS")
food_product <- subset(food_product, select = c(upc, upc_ver_uc, product_module_code, multi, size1_amount, size1_units))

food_product_nutrition_fact <- inner_join(food_product, food_nutrition_fact, 
                                          by = c("product_module_code"))

# change units
## CF Cubic Foot
## CT Count
## LI Liter
## FT Foot
## YD Yard
## EXP Expired
## PO Pound
## OZ Ounce
## QT Quart
## OZ Ounce
## SQ FT Square Feet

food_product_nutrition_fact$weight_gram <- with(food_product_nutrition_fact, 
                                                ifelse(size1_units == "OZ", multi * size1_amount * 28.3495,
                                                       ifelse(size1_units == "LI", multi * size1_amount * 1000,
                                                              ifelse(size1_units == "ML", multi * size1_amount,
                                                                     ifelse(size1_units == "PO", multi * size1_amount * 453.592,
                                                                            ifelse(size1_units == "QT", multi * size1_amount * 946.352946,
                                                                                   ifelse(size1_units == "CT", multi *size1_amount * 224, NA)))))))

saveRDS(food_product_nutrition_fact, file = "food product nutrition fact.RDS")

# rematch FNDDS and FPID food to WWEIA category (need to distinct powdered beverage and liquid beverage)
FNDDS_WWEIA <- read_excel("FNDDS to WWEIA.xlsx")

FPID_WWEIA <- read_excel("REVIEW FPID to WWEIA.xlsx")
FPID_WWEIA <- FPID_WWEIA[!duplicated(FPID_WWEIA[, c("CODE", "DESCRIPTION")]), ]

FNDDS_FPID_WWEIA <- rbind(FNDDS_WWEIA, FPID_WWEIA)

WWEIA <- read_excel("REVIEW WWEIA category.xlsx")

category <- inner_join(FNDDS_FPID_WWEIA, WWEIA, by="WWEIA Category number")

saveRDS(category, file = "REVIEW Category.RDS")

# sum sugar of each trip 
category <- readRDS("Category.RDS")

for (i in 2004:2019) {
  purchases <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/purchases_", i, ".RDS"))
  
  food_product_nutrition_fact <- readRDS("food product nutrition fact.RDS")
  food_product_nutrition_fact$upc <- as.numeric(food_product_nutrition_fact$upc)
  
  food_product_nutrition_fact <- inner_join(food_product_nutrition_fact, category, by = c("CODE", "DESCRIPTION"))
  
  nutrition_trip <- inner_join(purchases, food_product_nutrition_fact, by = c("upc", "upc_ver_uc"))
  
  nutrition_trip$weight <- nutrition_trip$weight_gram * nutrition_trip$quantity
  
  # weight of 1tsp ADD_SUARS equivalent 4.2g
  # unit of "Sugars, total" is gram
  nutrition_trip$Free_Sugars <- ifelse(nutrition_trip$`WWEIA Category` == "100% Juice" | nutrition_trip$`WWEIA Category` == "Sugars", 
                                       nutrition_trip$`Sugars, total`, nutrition_trip$ADD_SUGARS * 4.2)
  nutrition_trip$ADD_SUGARS <- nutrition_trip$ADD_SUGARS * 4.2
  
  nutrition_vars <- c("Free_Sugars", "ADD_SUGARS", "Sugars, total")
  
  # calculate sugar(per 100g products) in products of ever trip
  for (var in nutrition_vars) {
    nutrition_trip <- nutrition_trip %>% 
      group_by(trip_code_uc) %>%
      mutate(!!paste0("t_", var) := sum(!!sym(var) * weight / 100, na.rm = TRUE))
  }
  
  nutrition_trip <- nutrition_trip %>% 
    select(trip_code_uc, starts_with("t_")) %>%
    distinct()
  
  saveRDS(nutrition_trip, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/nutrition_trip_", i, ".RDS"))
}

# get Added-Sugar consumed by each household per month
# convert household members to adult male
household_purchase_nutrition_month <- NULL

for (i in 2004:2019) {
  nutrition_trip <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/nutrition_trip_", i, ".RDS"))
  
  trips <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/trips_", i, ".RDS"))
  
  test <- inner_join(nutrition_trip, trips, by = "trip_code_uc")
  
  test$year <- substr(test$purchase_date, 1, 4)
  test$month <- substr(test$purchase_date, 6, 7)
  test$year <- as.numeric(test$year)
  test$month <- as.numeric(test$month)
  
  # calculate by "household" "month" and "year", not "trip"
  variable <- grep("^t_", names(test), value = TRUE)
  for (var in variable) {
    test <- test %>%
      group_by(household_code, year, month) %>%
      mutate(!!paste0("m", var) := sum(!!sym(var), na.rm = TRUE)) %>%
      ungroup()
  }
  
  test <- test %>%
    select(household_code, year, month, starts_with("mt")) %>%
    distinct()
  
  household_purchase_nutrition_month <- rbind(household_purchase_nutrition_month, test)
}

household_purchase_nutrition_month <- household_purchase_nutrition_month %>%
  group_by(household_code, year, month) %>%
  summarise(mt_ADD_SUGARS = sum(mt_ADD_SUGARS, na.rm = TRUE))

saveRDS(household_purchase_nutrition_month, file = "household purchase information.RDS")

household_information <- NULL

for (i in 2004:2019) {
  data <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/panelists_", i, ".RDS"))
  names(data)[1] <- "household_code"
  
  data$male_head_id <- ifelse(is.na(data$Male_Head_Birth), 0, 1)
  data$female_head_id <- ifelse(is.na(data$Female_Head_Birth), 0, 1)
  data$member_1_id <- ifelse(is.na(data$Member_1_Birth), 0, 1)
  data$member_2_id <- ifelse(is.na(data$Member_2_Birth), 0, 1)
  data$member_3_id <- ifelse(is.na(data$Member_3_Birth), 0, 1)
  data$member_4_id <- ifelse(is.na(data$Member_4_Birth), 0, 1)
  data$member_5_id <- ifelse(is.na(data$Member_5_Birth), 0, 1)
  data$member_6_id <- ifelse(is.na(data$Member_6_Birth), 0, 1)
  data$member_7_id <- ifelse(is.na(data$Member_7_Birth), 0, 1)
  data$number <- rowSums(data[, c("male_head_id", "female_head_id", "member_1_id", "member_2_id", "member_3_id", "member_4_id", "member_5_id", "member_6_id", "member_7_id")], na.rm = TRUE)
  
  # check whether the household size is accurate
  data$differ <- data$Household_Size - data$number
  new_data <- data[data$differ == 0, ]
  
  new_data$member1_age <- ifelse(is.na(new_data$Male_Head_Birth), NA, new_data$Panel_Year - new_data$Male_Head_Birth)
  new_data$member1_sex <- ifelse(is.na(new_data$Male_Head_Birth), NA, "male")
  new_data$member1_enerage_MJ <- ifelse(is.na(new_data$member1_age), 0,
                                        ifelse(new_data$member1_age >= 18 & new_data$member1_age <= 29, 11.2,
                                               ifelse(new_data$member1_age >= 30 & new_data$member1_age <= 39, 10.8,
                                                      ifelse(new_data$member1_age >= 40 & new_data$member1_age <= 49, 10.7,
                                                             ifelse(new_data$member1_age >= 50 & new_data$member1_age <= 59, 10.5,
                                                                    ifelse(new_data$member1_age >= 60 & new_data$member1_age <= 69, 9.6,
                                                                           ifelse(new_data$member1_age >= 70, 9.5, NA)))))))
  
  new_data$member2_age <- ifelse(is.na(new_data$Female_Head_Birth), NA, new_data$Panel_Year - new_data$Female_Head_Birth)
  new_data$member2_sex <- ifelse(is.na(new_data$Female_Head_Birth), NA, "female")
  new_data$member2_enerage_MJ <- ifelse(is.na(new_data$member2_age), 0,
                                        ifelse(new_data$member2_age >= 18 & new_data$member2_age <= 29, 9.0,
                                               ifelse(new_data$member2_age >= 30 & new_data$member2_age <= 39, 8.7,
                                                      ifelse(new_data$member2_age >= 40 & new_data$member2_age <= 49, 8.6,
                                                             ifelse(new_data$member2_age >= 50 & new_data$member2_age <= 59, 8.5,
                                                                    ifelse(new_data$member2_age >= 60 & new_data$member2_age <= 69, 7.8,
                                                                           ifelse(new_data$member2_age >= 70, 7.7, NA)))))))
  for (i in 1:7) {
    new_data[[paste0("member", i+2, "_age")]] <- ifelse(is.na(new_data[[paste0("Member_", i, "_Birth")]]), NA, new_data[["Panel_Year"]] - new_data[[paste0("Member_", i, "_Birth")]])
    new_data[[paste0("member", i+2, "_sex")]] <- ifelse(new_data[[paste0("Member_", i, "_Relationship_Sex")]] %in% c(1, 3, 5), "male",
                                                        ifelse(new_data[[paste0("Member_", i, "_Relationship_Sex")]] %in% c(2, 4, 6), "female", NA))
    new_data[[paste0("member", i+2, "_enerage_MJ")]] <- ifelse(is.na(new_data[[paste0("member", i+2, "_sex")]]), 0,
                                                               ifelse(new_data[[paste0("member", i+2, "_sex")]] == "male",
                                                                      ifelse(new_data[[paste0("member", i+2, "_age")]] <= 17,
                                                                             c(2.7, 3.3, 4.3, 4.9, 6.0, 6.4, 6.7, 7.2, 7.6, 8.1, 8.1, 8.5, 9.1, 9.8, 10.5, 11.3, 11.9, 12.3)[new_data[[paste0("member", i+2, "_age")]] + 1],
                                                                             ifelse(new_data[[paste0("member", i+2, "_age")]] >= 18 & new_data[[paste0("member", i+2, "_age")]] <= 29, 11.2,
                                                                                    ifelse(new_data[[paste0("member", i+2, "_age")]] >= 30 & new_data[[paste0("member", i+2, "_age")]] <= 39, 10.8,
                                                                                           ifelse(new_data[[paste0("member", i+2, "_age")]] >= 40 & new_data[[paste0("member", i+2, "_age")]] <= 49, 10.7,
                                                                                                  ifelse(new_data[[paste0("member", i+2, "_age")]] >= 50 & new_data[[paste0("member", i+2, "_age")]] <= 59, 10.5,
                                                                                                         ifelse(new_data[[paste0("member", i+2, "_age")]] >= 60 & new_data[[paste0("member", i+2, "_age")]] <= 69, 9.6,
                                                                                                                ifelse(new_data[[paste0("member", i+2, "_age")]] >= 70, 9.5, NA))))))),
                                                                      ifelse(new_data[[paste0("member", i+2, "_sex")]] == "female",
                                                                             ifelse(new_data[[paste0("member", i+2, "_age")]] <= 17,
                                                                                    c(2.4, 3.0, 4.0, 4.6, 5.6, 5.9, 6.3, 6.7, 7.1, 7.5, 7.6, 8.0, 8.4, 8.8, 9.1, 9.3, 9.5, 9.5)[new_data[[paste0("member", i+2, "_age")]] + 1],
                                                                                    ifelse(new_data[[paste0("member", i+2, "_age")]] >= 18 & new_data[[paste0("member", i+2, "_age")]] <= 29, 9.0,
                                                                                           ifelse(new_data[[paste0("member", i+2, "_age")]] >= 30 & new_data[[paste0("member", i+2, "_age")]] <= 39, 8.7,
                                                                                                  ifelse(new_data[[paste0("member", i+2, "_age")]] >= 40 & new_data[[paste0("member", i+2, "_age")]] <= 49, 8.6,
                                                                                                         ifelse(new_data[[paste0("member", i+2, "_age")]] >= 50 & new_data[[paste0("member", i+2, "_age")]] <= 59, 8.5,
                                                                                                                ifelse(new_data[[paste0("member", i+2, "_age")]] >= 60 & new_data[[paste0("member", i+2, "_age")]] <= 69, 7.8,
                                                                                                                       ifelse(new_data[[paste0("member", i+2, "_age")]] >= 70, 7.7, NA))))))), NA)))
  }
  
  # covert household member to adult male
  for (i in 1:9){
    new_data[[paste0("member", i, "_convert")]] <- new_data[[paste0("member", i, "_enerage_MJ")]] / 11.2
  }
  new_data$Household_size_convert <- rowSums(new_data[, grep("_convert", colnames(new_data))])
  
  new_data <- new_data[!is.na(new_data$Fips_State_Cd), ]
  new_data$Fips_State_Cd <- sprintf("%02.0f", new_data$Fips_State_Cd)
  
  new_data <- new_data[!is.na(new_data$Fips_County_Cd), ]
  new_data$Fips_County_Cd <- sprintf("%03.0f", new_data$Fips_County_Cd)
  
  new_data$fips_county <- paste0(new_data$Fips_State_Cd, new_data$Fips_County_Cd)
  
  new_data <- new_data %>%
    select(household_code, Panel_Year, Projection_Factor,
           Household_Income, Household_Size, Age_And_Presence_Of_Children,
           Male_Head_Age, Female_Head_Age, Male_Head_Education, Female_Head_Education, 
           Male_Head_Occupation, Female_Head_Occupation,
           Marital_Status, Race, Hispanic_Origin, Household_size_convert, fips_county) %>%
    rename(year = Panel_Year)
  
  household_information <- rbind(household_information, new_data)
}

saveRDS(household_information, file = "household information.RDS")

household_information <- readRDS("household information.RDS")
household_purchase_nutrition_month <- readRDS("household purchase information.RDS")

household_purchase_nutrition_month <- inner_join(household_purchase_nutrition_month, household_information, by = c("household_code", "year"))
saveRDS(household_purchase_nutrition_month, file = "household purchase nutrition month.RDS")


#### 2.Meteorological Data ####
## get GSOD data 
library(GSODR)

US_climate <- NULL
for (i in 1994:2019) {
  data <- get_GSOD(years = i, country = "United States")
  US_climate <- rbind(US_climate, data)
}

saveRDS(US_climate, "/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/US_climate.RDS")

## spatial join
library(here)
library(sf)
library(viridis)
library(USAboundaries)
library(rnaturalearth)
library(ggrepel)
library(readr)

# GSOD station
US_climate <- readRDS("US_climate.RDS")
list <- US_climate %>%
  select(STNID, LATITUDE, LONGITUDE) %>%
  distinct()

station_points <- as.data.frame(list) %>% 
  st_as_sf(coords=c("LONGITUDE","LATITUDE"), crs=4326, remove=FALSE) 

# get boundary with high definition of all states and counties
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)

# create 100km buffer of county
buffered_counties <- st_buffer(county_polygons, dist = 100000)

# spatial join station and buffer
station_buffers <- st_join(station_points, buffered_counties)
station_buffers$fips_county <- paste(station_buffers$statefp, station_buffers$countyfp, sep = "")

unique_data <- distinct(station_buffers, STNID, fips_county)

saveRDS(unique_data, "climate station to county homescan 100km.RDS")

## get GSOD-county data 
US_climate <- readRDS("US_climate.RDS")

county_data <- readRDS("climate station to county homescan 100km.RDS")

climate_county_data <- inner_join(US_climate, county_data, by = "STNID", multiple = "all")

variables <- c("TEMP", "DEWP", "VISIB", "WDSP", "PRCP", "RH")

for (var in variables) {
  climate_county_data <- climate_county_data %>%
    group_by(YEAR, MONTH, DAY, fips_county) %>%
    mutate(!!paste0(var, "_county") := mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

climate_county_data <- climate_county_data %>%
  select(YEAR, MONTH, DAY, fips_county, ends_with("_county")) %>%
  distinct()

climate_county_data$fips_county <- formatC(climate_county_data$fips_county, width = 5, format = "d", flag = "0")

climate_county_data$date <- as.Date(with(climate_county_data, paste(MONTH, DAY, YEAR, sep = "/")), format = "%m/%d/%Y")

saveRDS(climate_county_data, "climate county homescan.RDS")

# calculate month-average climate data
climate_county_data <- readRDS("climate county homescan.RDS")

names(climate_county_data)[names(climate_county_data) == "YEAR"] <- "year"
names(climate_county_data)[names(climate_county_data) == "MONTH"] <- "month"

variables <- c("TEMP_county", "DEWP_county", "VISIB_county", "WDSP_county", "PRCP_county", "RH_county")

for (var in variables) {
  climate_county_data <- climate_county_data %>%
    group_by(year, month, fips_county) %>%
    mutate(!!paste0("m_", var) := mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

climate_county_data$count <- 1
climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(month_day_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

month_day_31 <- c(1, 3, 5, 7, 8, 10, 12)
month_day_30 <- c(4, 6, 9, 11)
year_2_29 <- c(1996, 2000, 2004, 2008, 2012, 2016)

climate_county_data$month_day <- ifelse(climate_county_data$month %in% month_day_31, 31, 
                                        ifelse(climate_county_data$month %in% month_day_30, 30,
                                               ifelse(climate_county_data$year %in% year_2_29, 29, 28))) 

climate_county_data$HDD65 <- pmax(65 - climate_county_data$TEMP_county * 1.8 - 32, 0)
climate_county_data$CDD65 <- pmax(climate_county_data$TEMP_county * 1.8 + 32 - 65, 0)

climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(m_HDD65 = sum(HDD65 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_CDD65 = sum(CDD65 / month_day_count * month_day, na.rm = TRUE)) %>%
  ungroup()

breaks <- c(-Inf, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, Inf)
climate_county_data$m_TEMP_range2 <- cut(climate_county_data$m_TEMP_county, breaks = breaks, labels = 1:19)

climate_county_data$m_RH_county2 <- climate_county_data$m_RH_county^2

climate_county_data <- climate_county_data %>%
  select(year, month, month_day, fips_county, starts_with("m_")) %>%
  distinct()

saveRDS(climate_county_data, "climate county homescan month.RDS")


#### 3. main regression model ####
# regression month nutrition
household_purchase_nutrition_month <- readRDS("household purchase nutrition month.RDS")

# household_purchase_nutrition_month <- household_purchase_nutrition_month[!(household_purchase_nutrition_month$year %in% c(2003, 2020)), ]

climate_county_month <- readRDS("climate county homescan month.RDS")

test <- inner_join(household_purchase_nutrition_month, climate_county_month, by = c("fips_county", "year", "month"))

# sugar of per person per day
test$mpc_ADD_SUGARS <- test$mt_ADD_SUGARS / test$month_day / test$Household_Size
test$mpc_ADD_SUGARS_convert <- test$mt_ADD_SUGARS / test$month_day / test$Household_size_convert

saveRDS(test, "test_nutrition_household_month.RDS")

# climate zone
# climate zone data from https://www.energy.gov/sites/prod/files/2015/10/f27/ba_climate_region_guide_7.3.pdf
# combined by https://atlas.eia.gov/datasets/eia::climate-zones-doe-building-america-program/about
test <- readRDS("test_nutrition_household_month.RDS")

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

test <- inner_join(test, zone, by = "fips_county")

saveRDS(test, "test_nutrition_household_zone_month.RDS")

# Regression
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

data$m_TEMP_range2 <- factor(data$m_TEMP_range2)
data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

# model1 - "mpc_ADD_SUGARS"
model1 <- feols(mpc_ADD_SUGARS ~ 
                  m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                  Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                  Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code, 
                data = data, 
                weights = ~Projection_Factor, 
                vcov = ~fips_county, 
                fixef.rm = "single")
AS_intake <- coef(model1)
AS_intake <- as.matrix(AS_intake)
AS_intake_confint <- confint(model1, level = 0.95)
AS_intake_confint <- as.matrix(AS_intake_confint)

# model2 - "mpc_ADD_SUGARS_convert"
model2 <- feols(mpc_ADD_SUGARS_convert ~ 
                  m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                  Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                  Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,  
                data = data, 
                weights = ~Projection_Factor, 
                vcov = ~fips_county, 
                fixef.rm = "single")
AS_convert <- coef(model2)
AS_convert <- as.matrix(AS_convert)
AS_convert_confint <- confint(model2, level = 0.95)
AS_convert_confint <- as.matrix(AS_convert_confint)

models <- list("mpc_ADD_SUGARS_convert" = model2, "mpc_ADD_SUGARS" = model1)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-main model.docx")

coef_matrix <- cbind(AS_intake, AS_intake_confint, 
                     AS_convert, AS_convert_confint)

new_names <- c("AS_intake_c", "AS_intake_ll", "AS_intake_ul",
               "AS_convert_c", "AS_convert_ll", "AS_convert_ul")
colnames(coef_matrix) <- new_names

myDataframe <- as.data.frame(coef_matrix)
myDataframe <- rbind(data.frame(AS_intake_c = 0, AS_intake_ll = 0, AS_intake_ul = 0, 
                                AS_convert_c = 0, AS_convert_ll = 0, AS_convert_ul = 0), myDataframe)

rownames(myDataframe)[1] <- "m_TEMP_range2"
myDataframe <- myDataframe %>% mutate(id = row_number())
myDataframe <- myDataframe %>% filter(id <= 19)

saveRDS(myDataframe, "REVIEW coefficient R added sugar monthly.RDS")


#### 4. regression of food category - sugar ####
# get "WWEIA category" and "added sugar" of each product
food_nutrition_fact <- readRDS("food nutrition fact.RDS")

category <- readRDS("REVIEW Category.RDS")

food_product_nutrition_focused <- inner_join(food_nutrition_fact, category, by = c("CODE", "DESCRIPTION"))

food_product_nutrition_focused <- select(food_product_nutrition_focused, "product_module_code", "product_group_code", "ADD_SUGARS", "WWEIA Category number", "WWEIA Category")

food_product <- readRDS("food product.RDS")
food_product <- subset(food_product, select = c(upc, upc_ver_uc, product_module_code, multi, size1_amount, size1_units))

food_product_nutrition_focused <- inner_join(food_product, food_product_nutrition_focused, by = "product_module_code")

# change units to "g"
food_product_nutrition_focused$weight_gram <- with(food_product_nutrition_focused, 
                                                   ifelse(size1_units == "OZ", multi * size1_amount * 28.3495,
                                                          ifelse(size1_units == "LI", multi * size1_amount * 1000,
                                                                 ifelse(size1_units == "ML", multi * size1_amount,
                                                                        ifelse(size1_units == "PO", multi * size1_amount * 453.592,
                                                                               ifelse(size1_units == "QT", multi * size1_amount * 946.352946,
                                                                                      ifelse(size1_units == "CT", multi *size1_amount * 224, NA)))))))

saveRDS(food_product_nutrition_focused, file = "REVIEW food product nutrition focused.RDS")

# get added sugar of each category 
household_purchase_food_month <- data.frame()

for(i in 2004:2019) {
  purchases <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/purchases_", i, ".RDS"))
  
  food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
  food_product_focused$upc <- as.numeric(food_product_focused$upc)
  
  test <- inner_join(purchases, food_product_focused, by = c("upc", "upc_ver_uc"))
  
  trips <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/trips_", i, ".RDS"))
  
  test <- inner_join(test, trips, by = "trip_code_uc")
  
  test$year <- substr(test$purchase_date, 1, 4)
  test$month <- substr(test$purchase_date, 6, 7)
  test$year <- as.numeric(test$year)
  test$month <- as.numeric(test$month)
  
  test$ADD_SUGARS <- test$ADD_SUGARS * 4.2
  
  test <- test %>%
    group_by(household_code, year, month, `WWEIA Category`) %>%
    summarise(weight = sum(weight_gram * quantity / 100 * ADD_SUGARS, na.rm = TRUE)) %>%
    ungroup()
  
  household_purchase_food_month <- rbind(household_purchase_food_month, test)
}

household_purchase_food_month <- household_purchase_food_month %>%
  group_by(household_code, year, month, `WWEIA Category`) %>%
  summarise(weight = sum(weight, na.rm = TRUE))

household_purchase_food_month <- household_purchase_food_month %>% 
  spread(key = `WWEIA Category`, value = weight, fill = 0)

household_information <- readRDS("household information.RDS")

household_purchase_food_month <- inner_join(household_purchase_food_month, household_information, by = c("household_code", "year"))

saveRDS(household_purchase_food_month, file = "REVIEW purchase category focused sugar monthly.RDS")

# added sugar regression data
data <- readRDS("REVIEW purchase category focused sugar monthly.RDS")

data$`Bakery, Oils and Sugar products` <- data$Sugars + data$Candy + 
  data$`Sweet Bakery Products` + data$`Quick Breads and Bread Products`+ data$`Breads, Rolls, Tortillas` +
  data$`Fats and Oils`

data <- data %>% filter(year != 2003 & year != 2020)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

climate_data <- readRDS("climate county homescan month.RDS")

merged_data <- inner_join(data, climate_data, by=c("fips_county", "year", "month"))

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- c(unique(food_product_focused$`WWEIA Category`), "Bakery, Oils and Sugar products")
for (var in Category_values) {
  merged_data <- merged_data %>%
    mutate(!!paste0("mpc_", var) := !!sym(var) / month_day / Household_size_convert)
}

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

merged_data <- inner_join(merged_data, zone, by = "fips_county")

merged_data$Zone <- ifelse(merged_data$`BA Climate Zone` == "Hot-Humid", 1, 
                           ifelse(merged_data$`BA Climate Zone` == "Hot-Dry" | merged_data$`BA Climate Zone` == "Mixed-Dry", 2,
                                  ifelse(merged_data$`BA Climate Zone` == "Mixed-Humid", 3,
                                         ifelse(merged_data$`BA Climate Zone` == "Marine", 4, 5))))

merged_data$Ethnic <- ifelse(merged_data$Hispanic_Origin == 1, 5, merged_data$Race)

merged_data$m_TEMP_range2 <- factor(merged_data$m_TEMP_range2)
merged_data$Ethnic <- factor(merged_data$Ethnic)
merged_data$Marital_Status <- factor(merged_data$Marital_Status)
merged_data$Age_And_Presence_Of_Children <- factor(merged_data$Age_And_Presence_Of_Children)
merged_data$Zone <- factor(merged_data$Zone)

saveRDS(merged_data, "REVIEW sugar regression of food category.RDS")

##### 4.1 added sugar regression #####
merged_data <- readRDS("REVIEW sugar regression of food category.RDS")

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- c(unique(food_product_focused$`WWEIA Category`), "Bakery, Oils and Sugar products")

models <- list()
myDataframe <- NULL
for (i in 1:42) {
  merged_data_copy <- merged_data 
  
  merged_data_copy[, 124] <- merged_data_copy[, 71 + i] 
  colnames(merged_data_copy)[124] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model <- feols(dependent_variable ~ 
                     m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                     Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                     Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                     Ethnic + Marital_Status | 
                     year^Zone + month^Zone + household_code,  
                   data = merged_data_copy, 
                   weights = ~Projection_Factor,
                   vcov = ~fips_county, 
                   fixef.rm = "single")
    
    coef_matrix <- as.matrix(coef(model))  
    colnames(coef_matrix) <- paste0(Category_values[i], "_c")
    confint_matrix <- as.matrix(confint(model, levle = 0.95))
    colnames(confint_matrix) <- c(paste0(Category_values[i], "_ll"), paste0(Category_values[i], "_ul"))
    
    model_information <- as.data.frame(cbind(coef_matrix, confint_matrix))
    
    new_row <- data.frame(matrix(ncol = ncol(model_information), nrow = 1))
    colnames(new_row) <- colnames(model_information)
    new_row[1, 1] <- 0
    new_row[1, 2:3] <- NA
    
    model_information <- rbind(new_row, model_information)
    rownames(model_information)[1] <- "m_TEMP_range2"
    
    model_information <- model_information %>% mutate(id = row_number())
    model_information <- model_information %>% filter(id <= 19)
    
    model_information_long <- pivot_longer(model_information,
                                           cols = starts_with(Category_values[i]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[i]
    
    myDataframe <- rbind(myDataframe, model_information_long) 
    
    if (i %in% c(15, 22, 42)) {
      model_name <- switch(as.character(i),
                           "15" = "Sweetened Beverages",
                           "22" = "Other Desserts",
                           "42" = "Bakery, Oils and Sugar products")
      models[[model_name]] <- model
    }
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused sugar covert monthly.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-food groups.docx")


#### 5. regression of food category - drink consumption ####
# get consumption of each category 
household_purchase_food_month <- data.frame()

for(i in 2004:2019) {
  purchases <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/purchases_", i, ".RDS"))
  
  food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
  food_product_focused$upc <- as.numeric(food_product_focused$upc)
  
  test <- inner_join(purchases, food_product_focused, by = c("upc", "upc_ver_uc"))
  
  trips <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/trips_", i, ".RDS"))
  
  test <- inner_join(test, trips, by = "trip_code_uc")
  
  test$year <- substr(test$purchase_date, 1, 4)
  test$month <- substr(test$purchase_date, 6, 7)
  test$year <- as.numeric(test$year)
  test$month <- as.numeric(test$month)
  
  test <- test %>%
    group_by(household_code, year, month, `WWEIA Category`) %>%
    summarise(weight = sum(weight_gram * quantity, na.rm = TRUE)) %>%
    ungroup()
  
  household_purchase_food_month <- rbind(household_purchase_food_month, test)
}

household_purchase_food_month <- household_purchase_food_month %>%
  group_by(household_code, year, month, `WWEIA Category`) %>%
  summarise(weight = sum(weight, na.rm = TRUE))

household_purchase_food_month <- household_purchase_food_month %>% 
  spread(key = `WWEIA Category`, value = weight, fill = 0)

household_information <- readRDS("household information.RDS")

household_purchase_food_month <- inner_join(household_purchase_food_month, household_information, by = c("household_code", "year"))

saveRDS(household_purchase_food_month, file = "REVIEW purchase category focused monthly.RDS")

# regression data
data <- readRDS("REVIEW purchase category focused monthly.RDS")

data <- data %>% filter(year != 2003 & year != 2020)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

climate_data <- readRDS("climate county homescan month.RDS")

merged_data <- inner_join(data, climate_data, by=c("fips_county", "year", "month"))

merged_data$m_TEMP_county <- ifelse(merged_data$m_TEMP_county <=0, 0, merged_data$m_TEMP_county)

merged_data$m_TEMP_county_0_12 <- ifelse(merged_data$m_TEMP_county <= 12, merged_data$m_TEMP_county, 12)
merged_data$m_TEMP_county_12_30 <- ifelse(merged_data$m_TEMP_county > 12 & merged_data$m_TEMP_county <= 30, merged_data$m_TEMP_county - 12, 
                                          ifelse(merged_data$m_TEMP_county > 30, 30 - 12, 0))
merged_data$m_TEMP_county_30_up <- ifelse(merged_data$m_TEMP_county > 30, merged_data$m_TEMP_county - 30, 0)

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)
for (var in Category_values) {
  merged_data <- merged_data %>%
    mutate(!!paste0("mpc_", var) := !!sym(var) / month_day / Household_size_convert)
}

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

merged_data <- inner_join(merged_data, zone, by = "fips_county")

merged_data$Zone <- ifelse(merged_data$`BA Climate Zone` == "Hot-Humid", 1, 
                           ifelse(merged_data$`BA Climate Zone` == "Hot-Dry" | merged_data$`BA Climate Zone` == "Mixed-Dry", 2,
                                  ifelse(merged_data$`BA Climate Zone` == "Mixed-Humid", 3,
                                         ifelse(merged_data$`BA Climate Zone` == "Marine", 4, 5))))

merged_data$Ethnic <- ifelse(merged_data$Hispanic_Origin == 1, 5, data$Race)

merged_data$Ethnic <- factor(merged_data$Ethnic)
merged_data$Marital_Status <- factor(merged_data$Marital_Status)
merged_data$Age_And_Presence_Of_Children <- factor(merged_data$Age_And_Presence_Of_Children)
merged_data$Zone <- factor(merged_data$Zone)

saveRDS(merged_data, "REVIEW consumption regression of drink category.RDS")

##### 5.1 drink consumption regression - spline model #####
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (i in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 125] <- merged_data_copy[, 73 + i] 
  colnames(merged_data_copy)[125] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model <- feols(dependent_variable ~ 
                     m_TEMP_county_0_12 + m_TEMP_county_12_30 + m_TEMP_county_30_up + 
                     m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                     Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                     Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                     Ethnic + Marital_Status | 
                     year^Zone + month^Zone + household_code, 
                   data = merged_data_copy, 
                   weights = ~Projection_Factor,
                   vcov = ~fips_county, 
                   fixef.rm = "single")
    
    coef_matrix <- as.matrix(coef(model))  
    confint_matrix <- as.matrix(confint(model, levle = 0.95))
    
    model_information <- as.data.frame(cbind(coef_matrix, confint_matrix))
    
    model_information <- as.matrix(model_information[grep("m_TEMP_county", rownames(model_information)), ])
    
    model_information <- as.data.frame(cbind(model_information, rownames(model_information)))
    rownames(model_information) <- NULL
    names(model_information) <- c("c", "ll", "ul", "rownames")
    
    model_information$range <- "0-12"
    model_information$range[grep("m_TEMP_county_12_30", model_information$rownames)] <- "12-30"
    model_information$range[grep("m_TEMP_county_30_up", model_information$rownames)] <- "30+"
    model_information <- model_information[, -4]
    
    colnames(model_information) <- c(paste0(Category_values[i], "_c"), 
                                     paste0(Category_values[i], "_ll"), paste0(Category_values[i], "_ul"), "range")
    
    model_information_long <- pivot_longer(model_information,
                                           cols = starts_with(Category_values[i]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[i]
    
    myDataframe <- rbind(myDataframe, model_information_long) 
    
    model_name <- switch(as.character(i),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused covert monthly Range3.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups.docx")


##### 5.2 drink consumption regression - heterogeneity test #####
###### 5.2.1 Ethnic ######
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (m in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 125] <- merged_data_copy[, 73 + m] 
  colnames(merged_data_copy)[125] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model1 <- feols(dependent_variable ~
                      m_TEMP_county_0_12 * Ethnic + m_TEMP_county_12_30 * Ethnic  + m_TEMP_county_30_up + 
                      m_WDSP_county * Ethnic + m_PRCP_county * Ethnic + m_RH_county * Ethnic + m_RH_county2 * Ethnic +
                      Household_Income + Household_Size + Age_And_Presence_Of_Children +
                      Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Marital_Status | 
                      year^Zone + month^Zone + household_code,
                    data = merged_data_copy,
                    weights = ~Projection_Factor,
                    vcov = ~fips_county,
                    fixef.rm = "single")
    
    Ethnic <- as.matrix(coef(model1))
    Ethnic <- as.matrix(Ethnic[grep("m_TEMP_county", rownames(Ethnic)), ])
    
    Ethnic <- as.data.frame(cbind(Ethnic, rownames(Ethnic)))
    rownames(Ethnic) <- NULL
    names(Ethnic) <- c("c", "rownames")
    
    Ethnic$c <- as.numeric(Ethnic$c)
    
    Ethnic$group <- "White/Caucasian"
    Ethnic$group[grep("Ethnic2", Ethnic$rownames)] <- "Black/African American"
    Ethnic$group[grep("Ethnic3", Ethnic$rownames)] <- "Asian"
    Ethnic$group[grep("Ethnic4", Ethnic$rownames)] <- "Other"
    Ethnic$group[grep("Ethnic5", Ethnic$rownames)] <- "Hispanic"
    
    Ethnic$range <- "0-12"
    Ethnic$range[grep("m_TEMP_county_12_30", Ethnic$rownames)] <- "12-30"
    Ethnic$range[grep("m_TEMP_county_30_up", Ethnic$rownames)] <- "30+"
    
    Ethnic <- Ethnic[, -2]
    
    Ethnic_vcov <- as.matrix(vcov(model1))
    Ethnic_vcov <- as.matrix(Ethnic_vcov[grepl("m_TEMP_county", rownames(Ethnic_vcov)), ])
    Ethnic_vcov <- as.matrix(Ethnic_vcov[ , grepl("m_TEMP_county", colnames(Ethnic_vcov))])
    
    Ethnic$ll <- 0
    Ethnic$ul <- 0
    
    for (tas in unique(Ethnic$range)){
      j <- as.numeric(rownames(Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == tas, ]))
      
      for (race in unique(Ethnic$group)){
        
        if (race != "White/Caucasian" ) {
          Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c + Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == tas, ]$c
          
          i <- as.numeric(rownames(Ethnic[Ethnic$group == race & Ethnic$range == tas, ]))
          
          Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ll = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c - 1.96 * sqrt(Ethnic_vcov[i, i] + Ethnic_vcov[j, j] + 2*Ethnic_vcov[i, j])
          Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ul = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c + 1.96 * sqrt(Ethnic_vcov[i, i] + Ethnic_vcov[j, j] + 2*Ethnic_vcov[i, j])
        }
        
        else{
          Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ll = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c - 1.96 * sqrt(Ethnic_vcov[j, j])
          Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ul = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c + 1.96 * sqrt(Ethnic_vcov[j, j])
        }
      }
    }
    
    for (race in unique(Ethnic$group)){
      
      if (race != "White/Caucasian" ) {
        Ethnic <- rbind(Ethnic, as.data.frame(list(c = Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == "30+", ]$c, 
                                                   group = race, range = "30+", 
                                                   ll = Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == "30+", ]$ll, 
                                                   ul = Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == "30+", ]$ul)))
      }
      
    }
    
    colnames(Ethnic) <- c(paste0(Category_values[m], "_c"), "group", "range", 
                          paste0(Category_values[m], "_ll"), paste0(Category_values[m], "_ul"))
    
    model_information_long <- pivot_longer(Ethnic,
                                           cols = starts_with(Category_values[m]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[m]
    
    myDataframe <- rbind(myDataframe, model_information_long)
    
    model_name <- switch(as.character(m),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model1
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused drink consumption covert monthly Ethnic.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups-Ethnic.docx")

###### 5.2.2 Male Education ######
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")
merged_data$Male_Head_Education <- factor(merged_data$Male_Head_Education)

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (m in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 125] <- merged_data_copy[, 73 + m] 
  colnames(merged_data_copy)[125] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model2 <- feols(dependent_variable ~
                      m_TEMP_county_0_12 * Male_Head_Education + m_TEMP_county_12_30 * Male_Head_Education  + m_TEMP_county_30_up + 
                      m_WDSP_county * Male_Head_Education + m_PRCP_county * Male_Head_Education + m_RH_county * Male_Head_Education + m_RH_county2 * Male_Head_Education +
                      Household_Income + Household_Size + Age_And_Presence_Of_Children +
                      Male_Head_Age + Female_Head_Age + Female_Head_Education + Ethnic + Marital_Status | 
                      year^Zone + month^Zone + household_code,
                    data = merged_data_copy,
                    weights = ~Projection_Factor,
                    vcov = ~fips_county,
                    fixef.rm = "single")
    
    Education <- as.matrix(coef(model2))
    Education <- as.matrix(Education[grep("m_TEMP_county", rownames(Education)), ])
    
    Education <- as.data.frame(cbind(Education, rownames(Education)))
    rownames(Education) <- NULL
    names(Education) <- c("c", "rownames")
    
    Education$c <- as.numeric(Education$c)
    
    Education$group <- "Grade School"
    Education$group[grep("Male_Head_Education2", Education$rownames)] <- "Some High School"
    Education$group[grep("Male_Head_Education3", Education$rownames)] <- "Graduated High School"
    Education$group[grep("Male_Head_Education4", Education$rownames)] <- "Some College"
    Education$group[grep("Male_Head_Education5", Education$rownames)] <- "Graduated College"
    Education$group[grep("Male_Head_Education6", Education$rownames)] <- "Post College Grad"
    
    Education$range <- "0-12"
    Education$range[grep("m_TEMP_county_12_30", Education$rownames)] <- "12-30"
    Education$range[grep("m_TEMP_county_30_up", Education$rownames)] <- "30+"
    
    Education <- Education[, -2]
    
    Education_vcov <- as.matrix(vcov(model2))
    Education_vcov <- as.matrix(Education_vcov[grepl("m_TEMP_county", rownames(Education_vcov)), ])
    Education_vcov <- as.matrix(Education_vcov[ , grepl("m_TEMP_county", colnames(Education_vcov))])
    
    Education$ll <- 0
    Education$ul <- 0
    
    for (tas in unique(Education$range)){
      j <- as.numeric(rownames(Education[Education$group == "Grade School" & Education$range == tas, ]))
      
      for (race in unique(Education$group)){
        
        if (race != "Grade School" ) {
          Education[Education$group == race & Education$range == tas, ]$c = Education[Education$group == race & Education$range == tas, ]$c + Education[Education$group == "Grade School" & Education$range == tas, ]$c
          
          i <- as.numeric(rownames(Education[Education$group == race & Education$range == tas, ]))
          
          Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
          Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
        }
        
        else{
          Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[j, j])
          Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[j, j])
        }
      }
    }
    
    
    for (race in unique(Education$group)){
      if (race != "Grade School"  ) {
        Education <- rbind(Education, as.data.frame(list(c = Education[Education$group == "Grade School" & Education$range == "30+", ]$c, 
                                                         group = race, range = "30+", 
                                                         ll = Education[Education$group == "Grade School" & Education$range == "30+", ]$ll, 
                                                         ul = Education[Education$group == "Grade School" & Education$range == "30+", ]$ul)))
      }
    }
    
    colnames(Education) <- c(paste0(Category_values[m], "_c"), "group", "range", 
                             paste0(Category_values[m], "_ll"), paste0(Category_values[m], "_ul"))
    
    model_information_long <- pivot_longer(Education,
                                           cols = starts_with(Category_values[m]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[m]
    
    myDataframe <- rbind(myDataframe, model_information_long) 
    
    model_name <- switch(as.character(m),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model2
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused drink consumption covert monthly Male_Head_Education.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups-Male Education.docx")

###### 5.2.3 Female Education ######
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")
merged_data$Female_Head_Education <- factor(merged_data$Female_Head_Education)

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (m in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 125] <- merged_data_copy[, 73 + m] 
  colnames(merged_data_copy)[125] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model3 <- feols(dependent_variable ~
                      m_TEMP_county_0_12 * Female_Head_Education + m_TEMP_county_12_30 * Female_Head_Education  + m_TEMP_county_30_up + 
                      m_WDSP_county * Female_Head_Education + m_PRCP_county * Female_Head_Education + m_RH_county * Female_Head_Education + m_RH_county2 * Female_Head_Education +
                      Household_Income + Household_Size + Age_And_Presence_Of_Children +
                      Male_Head_Age + Female_Head_Age + Male_Head_Education + Ethnic + Marital_Status | 
                      year^Zone + month^Zone + household_code,
                    data = merged_data_copy,
                    weights = ~Projection_Factor,
                    vcov = ~fips_county,
                    fixef.rm = "single")
    
    Education <- as.matrix(coef(model3))
    Education <- as.matrix(Education[grep("m_TEMP_county", rownames(Education)), ])
    
    Education <- as.data.frame(cbind(Education, rownames(Education)))
    rownames(Education) <- NULL
    names(Education) <- c("c", "rownames")
    
    Education$c <- as.numeric(Education$c)
    
    Education$group <- "Grade School"
    Education$group[grep("Female_Head_Education2", Education$rownames)] <- "Some High School"
    Education$group[grep("Female_Head_Education3", Education$rownames)] <- "Graduated High School"
    Education$group[grep("Female_Head_Education4", Education$rownames)] <- "Some College"
    Education$group[grep("Female_Head_Education5", Education$rownames)] <- "Graduated College"
    Education$group[grep("Female_Head_Education6", Education$rownames)] <- "Post College Grad"
    
    Education$range <- "0-12"
    Education$range[grep("m_TEMP_county_12_30", Education$rownames)] <- "12-30"
    Education$range[grep("m_TEMP_county_30_up", Education$rownames)] <- "30+"
    
    Education <- Education[, -2]
    
    Education_vcov <- as.matrix(vcov(model3))
    Education_vcov <- as.matrix(Education_vcov[grepl("m_TEMP_county", rownames(Education_vcov)), ])
    Education_vcov <- as.matrix(Education_vcov[ , grepl("m_TEMP_county", colnames(Education_vcov))])
    
    Education$ll <- 0
    Education$ul <- 0
    
    for (tas in unique(Education$range)){
      j <- as.numeric(rownames(Education[Education$group == "Grade School" & Education$range == tas, ]))
      
      for (race in unique(Education$group)){
        
        if (race != "Grade School" ) {
          Education[Education$group == race & Education$range == tas, ]$c = Education[Education$group == race & Education$range == tas, ]$c + Education[Education$group == "Grade School" & Education$range == tas, ]$c
          
          i <- as.numeric(rownames(Education[Education$group == race & Education$range == tas, ]))
          
          Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
          Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
        }
        
        else{
          Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[j, j])
          Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[j, j])
        }
      }
    }
    
    
    for (race in unique(Education$group)){
      if (race != "Grade School"  ) {
        Education <- rbind(Education, as.data.frame(list(c = Education[Education$group == "Grade School" & Education$range == "30+", ]$c, 
                                                         group = race, range = "30+", 
                                                         ll = Education[Education$group == "Grade School" & Education$range == "30+", ]$ll, 
                                                         ul = Education[Education$group == "Grade School" & Education$range == "30+", ]$ul)))
      }
    }
    
    colnames(Education) <- c(paste0(Category_values[m], "_c"), "group", "range", 
                             paste0(Category_values[m], "_ll"), paste0(Category_values[m], "_ul"))
    
    model_information_long <- pivot_longer(Education,
                                           cols = starts_with(Category_values[m]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[m]
    
    myDataframe <- rbind(myDataframe, model_information_long) 
    
    model_name <- switch(as.character(m),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model3
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused drink consumption covert monthly Female_Head_Education.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups-Female Education.docx")

###### 5.2.4 Income #####
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income

merged_data$Income_group <- ifelse(merged_data$Household_Income <= 24999, "Very Low",
                                   ifelse(25000 <= merged_data$Household_Income & merged_data$Household_Income <= 49999, "Low", 
                                          ifelse(50000 <= merged_data$Household_Income & merged_data$Household_Income <= 69999, "Medium", 
                                                 ifelse(70000 <= merged_data$Household_Income & merged_data$Household_Income <= 99999, "High", "Very High"))))

merged_data$Income_group <- factor(merged_data$Income_group)
merged_data$Income_group <- relevel(merged_data$Income_group, ref = "Very Low")

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (m in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 126] <- merged_data_copy[, 73 + m] 
  colnames(merged_data_copy)[126] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model4 <- feols(dependent_variable ~
                      m_TEMP_county_0_12 * Income_group + m_TEMP_county_12_30 * Income_group  + m_TEMP_county_30_up + 
                      m_WDSP_county * Income_group + m_PRCP_county * Income_group + m_RH_county * Income_group + m_RH_county2 * Income_group +
                      Household_Size + Age_And_Presence_Of_Children +
                      Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                      year^Zone + month^Zone + household_code,
                    data = merged_data_copy,
                    weights = ~Projection_Factor,
                    vcov = ~fips_county,
                    fixef.rm = "single")
    
    Income <- as.matrix(coef(model4))
    Income <- as.matrix(Income[grep("m_TEMP_county", rownames(Income)), ])
    
    Income <- as.data.frame(cbind(Income, rownames(Income)))
    rownames(Income) <- NULL
    names(Income) <- c("c", "rownames")
    
    Income$c <- as.numeric(Income$c)
    
    Income$group <- "Very Low"
    Income$group[grep("Income_groupLow", Income$rownames)] <- "Low"
    Income$group[grep("Income_groupMedium", Income$rownames)] <- "Medium"
    Income$group[grep("Income_groupHigh", Income$rownames)] <- "High"
    Income$group[grep("Income_groupVery High", Income$rownames)] <- "Very High"
    
    Income$range <- "0-12"
    Income$range[grep("m_TEMP_county_12_30", Income$rownames)] <- "12-30"
    Income$range[grep("m_TEMP_county_30_up", Income$rownames)] <- "30+"
    
    Income <- Income[, -2]
    
    Income_vcov <- as.matrix(vcov(model4))
    Income_vcov <- as.matrix(Income_vcov[grepl("m_TEMP_county", rownames(Income_vcov)), ])
    Income_vcov <- as.matrix(Income_vcov[ , grepl("m_TEMP_county", colnames(Income_vcov))])
    
    Income$ll <- 0
    Income$ul <- 0
    
    for (tas in unique(Income$range)){
      j <- as.numeric(rownames(Income[Income$group == "Very Low" & Income$range == tas, ]))
      
      for (race in unique(Income$group)){
        
        if (race != "Very Low") {
          Income[Income$group == race & Income$range == tas, ]$c = Income[Income$group == race & Income$range == tas, ]$c + Income[Income$group == "Very Low" & Income$range == tas, ]$c
          
          i <- as.numeric(rownames(Income[Income$group == race & Income$range == tas, ]))
          
          Income[Income$group == race & Income$range == tas, ]$ll = Income[Income$group == race & Income$range == tas, ]$c - 1.96 * sqrt(Income_vcov[i, i] + Income_vcov[j, j] + 2*Income_vcov[i, j])
          Income[Income$group == race & Income$range == tas, ]$ul = Income[Income$group == race & Income$range == tas, ]$c + 1.96 * sqrt(Income_vcov[i, i] + Income_vcov[j, j] + 2*Income_vcov[i, j])
        }
        
        else{
          Income[Income$group == race & Income$range == tas, ]$ll = Income[Income$group == race & Income$range == tas, ]$c - 1.96 * sqrt(Income_vcov[j, j])
          Income[Income$group == race & Income$range == tas, ]$ul = Income[Income$group == race & Income$range == tas, ]$c + 1.96 * sqrt(Income_vcov[j, j])
        }
      }
    }
    
    for (race in unique(Income$group)){
      if (race != "Very Low"  ) {
        Income <- rbind(Income, as.data.frame(list(c = Income[Income$group == "Very Low" & Income$range == "30+", ]$c, 
                                                   group = race, range = "30+", 
                                                   ll = Income[Income$group == "Very Low" & Income$range == "30+", ]$ll, 
                                                   ul = Income[Income$group == "Very Low" & Income$range == "30+", ]$ul)))
      }
    }
    
    colnames(Income) <- c(paste0(Category_values[m], "_c"), "group", "range", 
                          paste0(Category_values[m], "_ll"), paste0(Category_values[m], "_ul"))
    
    model_information_long <- pivot_longer(Income,
                                           cols = starts_with(Category_values[m]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[m]
    
    myDataframe <- rbind(myDataframe, model_information_long) 
    
    model_name <- switch(as.character(m),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model4
    
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused drink consumption covert monthly Income.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups-Income.docx")

###### 5.2.5 Male Work Environment ######
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")

merged_data$Environment <- ifelse(merged_data$Male_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
merged_data$Environment <- factor(merged_data$Environment)
merged_data$Environment <- relevel(merged_data$Environment, ref = "Indoor")

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (m in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 126] <- merged_data_copy[, 73 + m] 
  colnames(merged_data_copy)[126] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model5 <- feols(dependent_variable ~
                      m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up +
                      m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                      Household_Income + Household_Size + Age_And_Presence_Of_Children +
                      Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                      year^Zone + month^Zone + household_code,
                    data = merged_data_copy,
                    weights = ~Projection_Factor,
                    vcov = ~fips_county,
                    fixef.rm = "single")
    
    Environment <- as.matrix(coef(model5))
    Environment <- as.matrix(Environment[grep("m_TEMP_county", rownames(Environment)), ])
    
    Environment <- as.data.frame(cbind(Environment, rownames(Environment)))
    rownames(Environment) <- NULL
    names(Environment) <- c("c", "rownames")
    
    Environment$c <- as.numeric(Environment$c)
    
    Environment$group <- "Indoor"
    Environment$group[grep("EnvironmentOutdoor", Environment$rownames)] <- "Outdoor"
    
    Environment$range <- "0-12"
    Environment$range[grep("m_TEMP_county_12_30", Environment$rownames)] <- "12-30"
    Environment$range[grep("m_TEMP_county_30_up", Environment$rownames)] <- "30+"
    
    Environment <- Environment[, -2]
    
    Environment_vcov <- as.matrix(vcov(model5))
    Environment_vcov <- as.matrix(Environment_vcov[grepl("m_TEMP_county", rownames(Environment_vcov)), ])
    Environment_vcov <- as.matrix(Environment_vcov[ , grepl("m_TEMP_county", colnames(Environment_vcov))])
    
    Environment$ll <- 0
    Environment$ul <- 0
    
    for (tas in unique(Environment$range)){
      j <- as.numeric(rownames(Environment[Environment$group == "Indoor" & Environment$range == tas, ]))
      
      for (race in unique(Environment$group)){
        
        if (race != "Indoor") {
          Environment[Environment$group == race & Environment$range == tas, ]$c = Environment[Environment$group == race & Environment$range == tas, ]$c + Environment[Environment$group == "Indoor" & Environment$range == tas, ]$c
          
          i <- as.numeric(rownames(Environment[Environment$group == race & Environment$range == tas, ]))
          
          Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
          Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
        }
        
        else{
          Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[j, j])
          Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[j, j])
        }
      }
    }
    
    for (race in unique(Environment$group)){
      
      if (race != "Indoor"  ) {
        Environment <- rbind(Environment, as.data.frame(list(c = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$c,
                                                             group = race, range = "30+",
                                                             ll = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ll,
                                                             ul = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ul)))
      }
    }
    
    colnames(Environment) <- c(paste0(Category_values[m], "_c"), "group", "range",
                               paste0(Category_values[m], "_ll"), paste0(Category_values[m], "_ul"))
    
    model_information_long <- pivot_longer(Environment,
                                           cols = starts_with(Category_values[m]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[m]
    
    myDataframe <- rbind(myDataframe, model_information_long)
    
    model_name <- switch(as.character(m),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model5
    
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused drink consumption covert monthly Male Environment.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups-Male Environment.docx")

###### 5.2.6 Female Work Environment ######
merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")

merged_data$Environment <- ifelse(merged_data$Female_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
merged_data$Environment <- factor(merged_data$Environment)
merged_data$Environment <- relevel(merged_data$Environment, ref = "Indoor")

merged_data$Income <- ifelse(merged_data$Household_Income == 3, 2500, 
                             ifelse(merged_data$Household_Income == 4, 6500, 
                                    ifelse(merged_data$Household_Income == 6, 9000, 
                                           ifelse(merged_data$Household_Income == 8, 11000, 
                                                  ifelse(merged_data$Household_Income == 10, 13500, 
                                                         ifelse(merged_data$Household_Income == 11, 17500, 
                                                                ifelse(merged_data$Household_Income == 13, 22500, 
                                                                       ifelse(merged_data$Household_Income == 15, 27500, 
                                                                              ifelse(merged_data$Household_Income == 16, 32500, 
                                                                                     ifelse(merged_data$Household_Income == 17, 37500, 
                                                                                            ifelse(merged_data$Household_Income == 18, 42500, 
                                                                                                   ifelse(merged_data$Household_Income == 19, 47500, 
                                                                                                          ifelse(merged_data$Household_Income == 21, 55000, 
                                                                                                                 ifelse(merged_data$Household_Income == 23, 65000, 
                                                                                                                        ifelse(merged_data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

merged_data <- merged_data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

merged_data$Household_Income <- merged_data$Income/10000

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
Category_values <- unique(food_product_focused$`WWEIA Category`)

models <- list()
myDataframe <- NULL
for (m in c(3, 10, 15, 23, 27, 30, 35, 37, 41)) {
  merged_data_copy <- merged_data
  
  merged_data_copy[, 126] <- merged_data_copy[, 73 + m] 
  colnames(merged_data_copy)[126] <- "dependent_variable"
  
  if (!all(merged_data_copy$dependent_variable == 0)) {
    model6 <- feols(dependent_variable ~
                      m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                      m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                      Household_Income + Household_Size + Age_And_Presence_Of_Children +
                      Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                      year^Zone + month^Zone + household_code,
                    data = merged_data_copy,
                    weights = ~Projection_Factor,
                    vcov = ~fips_county,
                    fixef.rm = "single")
    
    Environment <- as.matrix(coef(model6))
    Environment <- as.matrix(Environment[grep("m_TEMP_county", rownames(Environment)), ])
    
    Environment <- as.data.frame(cbind(Environment, rownames(Environment)))
    rownames(Environment) <- NULL
    names(Environment) <- c("c", "rownames")
    
    Environment$c <- as.numeric(Environment$c)
    
    Environment$group <- "Indoor"
    Environment$group[grep("EnvironmentOutdoor", Environment$rownames)] <- "Outdoor"
    
    Environment$range <- "0-12"
    Environment$range[grep("m_TEMP_county_12_30", Environment$rownames)] <- "12-30"
    Environment$range[grep("m_TEMP_county_30_up", Environment$rownames)] <- "30+"
    
    Environment <- Environment[, -2]
    
    Environment_vcov <- as.matrix(vcov(model6))
    Environment_vcov <- as.matrix(Environment_vcov[grepl("m_TEMP_county", rownames(Environment_vcov)), ])
    Environment_vcov <- as.matrix(Environment_vcov[ , grepl("m_TEMP_county", colnames(Environment_vcov))])
    
    Environment$ll <- 0
    Environment$ul <- 0
    
    for (tas in unique(Environment$range)){
      j <- as.numeric(rownames(Environment[Environment$group == "Indoor" & Environment$range == tas, ]))
      
      for (race in unique(Environment$group)){
        
        if (race != "Indoor") {
          Environment[Environment$group == race & Environment$range == tas, ]$c = Environment[Environment$group == race & Environment$range == tas, ]$c + Environment[Environment$group == "Indoor" & Environment$range == tas, ]$c
          
          i <- as.numeric(rownames(Environment[Environment$group == race & Environment$range == tas, ]))
          
          Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
          Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
        }
        
        else{
          Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[j, j])
          Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[j, j])
        }
      }
    }
    
    for (race in unique(Environment$group)){
      
      if (race != "Indoor"  ) {
        Environment <- rbind(Environment, as.data.frame(list(c = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$c, 
                                                             group = race, range = "30+", 
                                                             ll = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ll, 
                                                             ul = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ul)))
      }
    }
    
    colnames(Environment) <- c(paste0(Category_values[m], "_c"), "group", "range", 
                               paste0(Category_values[m], "_ll"), paste0(Category_values[m], "_ul"))
    
    model_information_long <- pivot_longer(Environment,
                                           cols = starts_with(Category_values[m]),
                                           names_to = "variable",
                                           values_to = "est")
    
    model_information_long <- model_information_long %>%
      mutate(coef = case_when(
        grepl("_ll$", variable) ~ "ll",
        grepl("_ul$", variable) ~ "ul",
        TRUE ~ "c"
      ))
    
    model_information_long$variable <- Category_values[m]
    
    myDataframe <- rbind(myDataframe, model_information_long) 
    
    model_name <- switch(as.character(m),
                         "3" = "mpc_Coffee and Tea",
                         "10" = "mpc_100% Juice",
                         "15" = "mpc_Sweetened Beverages",
                         "23" = "mpc_Plain Water",
                         "27" = "mpc_Milk",
                         "30" = "mpc_Alcoholic Beverages",
                         "35" = "mpc_Diet Beverages",
                         "37" = "mpc_Yogurt",
                         "41" = "mpc_Dairy Drinks and Substitutes")
    models[[model_name]] <- model6
    
  }
}

saveRDS(myDataframe, "REVIEW coefficient R focused drink consumption covert monthly Female Environment.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-drink groups-Female Environment.docx")



#### 6. heterogeneity test ####
# correlation
data <- readRDS("test_nutrition_household_month.RDS")

data <- data[data$year == 2019, ]

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Income_group <- ifelse(3 <= data$Household_Income & data$Household_Income <= 13, "Very Low",
                            ifelse(15 <= data$Household_Income & data$Household_Income <= 16, "Low", 
                                   ifelse(17 <= data$Household_Income & data$Household_Income <= 19, "Medium", 
                                          ifelse(21 <= data$Household_Income & data$Household_Income <= 23, "High", "Very High"))))

data$Male_Environment <- ifelse(data$Male_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Female_Environment <- ifelse(data$Female_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")

variables <- c("Male_Head_Education", "Female_Head_Education", "Income_group", "Ethnic", "Male_Environment", "Female_Environment")

cramer_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables))
rownames(cramer_matrix) <- variables
colnames(cramer_matrix) <- variables

for (i in seq_along(variables)) {
  for (j in seq_along(variables)) {
    tab <- xtabs(~ get(variables[i]) + get(variables[j]), data = data)
    assoc <- assocstats(tab)
    cramer_matrix[i, j] <- assoc$cramer
  }
}

cramer <- as.data.frame(cramer_matrix)
saveRDS(cramer, "correlations.RDS")

##### 6.1 Ethnic #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model1 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Ethnic + m_TEMP_county_12_30 * Ethnic  + m_TEMP_county_30_up + 
                  m_WDSP_county * Ethnic + m_PRCP_county * Ethnic + m_RH_county * Ethnic + m_RH_county2 * Ethnic +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Ethnic <- as.matrix(coef(model1))
Ethnic <- as.matrix(Ethnic[grep("m_TEMP_county", rownames(Ethnic)), ])

Ethnic <- as.data.frame(cbind(Ethnic, rownames(Ethnic)))
rownames(Ethnic) <- NULL
names(Ethnic) <- c("c", "rownames")

Ethnic$c <- as.numeric(Ethnic$c)

Ethnic$group <- "White/Caucasian"
Ethnic$group[grep("Ethnic2", Ethnic$rownames)] <- "Black/African American"
Ethnic$group[grep("Ethnic3", Ethnic$rownames)] <- "Asian"
Ethnic$group[grep("Ethnic4", Ethnic$rownames)] <- "Other"
Ethnic$group[grep("Ethnic5", Ethnic$rownames)] <- "Hispanic"

Ethnic$range <- "0-12"
Ethnic$range[grep("m_TEMP_county_12_30", Ethnic$rownames)] <- "12-30"
Ethnic$range[grep("m_TEMP_county_30_up", Ethnic$rownames)] <- "30+"

Ethnic <- Ethnic[, -2]

Ethnic_vcov <- as.matrix(vcov(model1))
Ethnic_vcov <- as.matrix(Ethnic_vcov[grepl("m_TEMP_county", rownames(Ethnic_vcov)), ])
Ethnic_vcov <- as.matrix(Ethnic_vcov[ , grepl("m_TEMP_county", colnames(Ethnic_vcov))])

Ethnic$ll <- 0
Ethnic$ul <- 0

for (tas in unique(Ethnic$range)){
  j <- as.numeric(rownames(Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == tas, ]))
  
  for (race in unique(Ethnic$group)){
    
    if (race != "White/Caucasian" ) {
      Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c + Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == tas, ]$c
      
      i <- as.numeric(rownames(Ethnic[Ethnic$group == race & Ethnic$range == tas, ]))
      
      Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ll = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c - 1.96 * sqrt(Ethnic_vcov[i, i] + Ethnic_vcov[j, j] + 2*Ethnic_vcov[i, j])
      Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ul = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c + 1.96 * sqrt(Ethnic_vcov[i, i] + Ethnic_vcov[j, j] + 2*Ethnic_vcov[i, j])
    }
    
    else{
      Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ll = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c - 1.96 * sqrt(Ethnic_vcov[j, j])
      Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$ul = Ethnic[Ethnic$group == race & Ethnic$range == tas, ]$c + 1.96 * sqrt(Ethnic_vcov[j, j])
    }
  }
}

for (race in unique(Ethnic$group)){
  
  if (race != "White/Caucasian" ) {
    Ethnic <- rbind(Ethnic, as.data.frame(list(c = Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == "30+", ]$c, 
                                               group = race, range = "30+", 
                                               ll = Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == "30+", ]$ll, 
                                               ul = Ethnic[Ethnic$group == "White/Caucasian" & Ethnic$range == "30+", ]$ul)))
  }
  
}

saveRDS(Ethnic, file = "REVIEW Ethnic range 3.RDS")

##### 6.2 Male Education #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Male_Head_Education <- factor(data$Male_Head_Education)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model2 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Male_Head_Education + m_TEMP_county_12_30 * Male_Head_Education  + m_TEMP_county_30_up + 
                  m_WDSP_county * Male_Head_Education + m_PRCP_county * Male_Head_Education + m_RH_county * Male_Head_Education + m_RH_county2 * Male_Head_Education +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Female_Head_Education + Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Education <- as.matrix(coef(model2))
Education <- as.matrix(Education[grep("m_TEMP_county", rownames(Education)), ])

Education <- as.data.frame(cbind(Education, rownames(Education)))
rownames(Education) <- NULL
names(Education) <- c("c", "rownames")

Education$c <- as.numeric(Education$c)

Education$group <- "Grade School"
Education$group[grep("Male_Head_Education2", Education$rownames)] <- "Some High School"
Education$group[grep("Male_Head_Education3", Education$rownames)] <- "Graduated High School"
Education$group[grep("Male_Head_Education4", Education$rownames)] <- "Some College"
Education$group[grep("Male_Head_Education5", Education$rownames)] <- "Graduated College"
Education$group[grep("Male_Head_Education6", Education$rownames)] <- "Post College Grad"

Education$range <- "0-12"
Education$range[grep("m_TEMP_county_12_30", Education$rownames)] <- "12-30"
Education$range[grep("m_TEMP_county_30_up", Education$rownames)] <- "30+"

Education <- Education[, -2]

Education_vcov <- as.matrix(vcov(model2))
Education_vcov <- as.matrix(Education_vcov[grepl("m_TEMP_county", rownames(Education_vcov)), ])
Education_vcov <- as.matrix(Education_vcov[ , grepl("m_TEMP_county", colnames(Education_vcov))])

Education$ll <- 0
Education$ul <- 0

for (tas in unique(Education$range)){
  j <- as.numeric(rownames(Education[Education$group == "Grade School" & Education$range == tas, ]))
  
  for (race in unique(Education$group)){
    
    if (race != "Grade School" ) {
      Education[Education$group == race & Education$range == tas, ]$c = Education[Education$group == race & Education$range == tas, ]$c + Education[Education$group == "Grade School" & Education$range == tas, ]$c
      
      i <- as.numeric(rownames(Education[Education$group == race & Education$range == tas, ]))
      
      Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
      Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
    }
    
    else{
      Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[j, j])
      Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[j, j])
    }
  }
}

for (race in unique(Education$group)){
  
  if (race != "Grade School"  ) {
    Education <- rbind(Education, as.data.frame(list(c = Education[Education$group == "Grade School" & Education$range == "30+", ]$c, 
                                                     group = race, range = "30+", 
                                                     ll = Education[Education$group == "Grade School" & Education$range == "30+", ]$ll, 
                                                     ul = Education[Education$group == "Grade School" & Education$range == "30+", ]$ul)))
  }
  
}

saveRDS(Education, file = "REVIEW Male Education range 3.RDS")

##### 6.3 Female Education #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Female_Head_Education <- factor(data$Female_Head_Education)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model3 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Female_Head_Education + m_TEMP_county_12_30 * Female_Head_Education  + m_TEMP_county_30_up + 
                  m_WDSP_county * Female_Head_Education + m_PRCP_county * Female_Head_Education + m_RH_county * Female_Head_Education + m_RH_county2 * Female_Head_Education +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Education <- as.matrix(coef(model3))
Education <- as.matrix(Education[grep("m_TEMP_county", rownames(Education)), ])

Education <- as.data.frame(cbind(Education, rownames(Education)))
rownames(Education) <- NULL
names(Education) <- c("c", "rownames")

Education$c <- as.numeric(Education$c)

Education$group <- "Grade School"
Education$group[grep("Female_Head_Education2", Education$rownames)] <- "Some High School"
Education$group[grep("Female_Head_Education3", Education$rownames)] <- "Graduated High School"
Education$group[grep("Female_Head_Education4", Education$rownames)] <- "Some College"
Education$group[grep("Female_Head_Education5", Education$rownames)] <- "Graduated College"
Education$group[grep("Female_Head_Education6", Education$rownames)] <- "Post College Grad"

Education$range <- "0-12"
Education$range[grep("m_TEMP_county_12_30", Education$rownames)] <- "12-30"
Education$range[grep("m_TEMP_county_30_up", Education$rownames)] <- "30+"

Education <- Education[, -2]

Education_vcov <- as.matrix(vcov(model3))
Education_vcov <- as.matrix(Education_vcov[grepl("m_TEMP_county", rownames(Education_vcov)), ])
Education_vcov <- as.matrix(Education_vcov[ , grepl("m_TEMP_county", colnames(Education_vcov))])

Education$ll <- 0
Education$ul <- 0

for (tas in unique(Education$range)){
  j <- as.numeric(rownames(Education[Education$group == "Grade School" & Education$range == tas, ]))
  
  for (race in unique(Education$group)){
    
    if (race != "Grade School" ) {
      Education[Education$group == race & Education$range == tas, ]$c = Education[Education$group == race & Education$range == tas, ]$c + Education[Education$group == "Grade School" & Education$range == tas, ]$c
      
      i <- as.numeric(rownames(Education[Education$group == race & Education$range == tas, ]))
      
      Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
      Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[i, i] + Education_vcov[j, j] + 2*Education_vcov[i, j])
    }
    
    else{
      Education[Education$group == race & Education$range == tas, ]$ll = Education[Education$group == race & Education$range == tas, ]$c - 1.96 * sqrt(Education_vcov[j, j])
      Education[Education$group == race & Education$range == tas, ]$ul = Education[Education$group == race & Education$range == tas, ]$c + 1.96 * sqrt(Education_vcov[j, j])
    }
  }
}

for (race in unique(Education$group)){
  
  if (race != "Grade School"  ) {
    Education <- rbind(Education, as.data.frame(list(c = Education[Education$group == "Grade School" & Education$range == "30+", ]$c, 
                                                     group = race, range = "30+", 
                                                     ll = Education[Education$group == "Grade School" & Education$range == "30+", ]$ll, 
                                                     ul = Education[Education$group == "Grade School" & Education$range == "30+", ]$ul)))
  }
  
}

saveRDS(Education, file = "REVIEW Female Education range 3.RDS")

##### 6.4 Income #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income

data$Income_group <- ifelse(data$Household_Income <= 24999, "Very Low",
                            ifelse(25000 <= data$Household_Income & data$Household_Income <= 49999, "Low", 
                                   ifelse(50000 <= data$Household_Income & data$Household_Income <= 69999, "Medium", 
                                          ifelse(70000 <= data$Household_Income & data$Household_Income <= 99999, "High", "Very High"))))
data$Income_group <- factor(data$Income_group)
data$Income_group <- relevel(data$Income_group, ref = "Very Low")

model4 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Income_group + m_TEMP_county_12_30 * Income_group  + m_TEMP_county_30_up + 
                  m_WDSP_county * Income_group + m_PRCP_county * Income_group + m_RH_county * Income_group + m_RH_county2 * Income_group +
                  Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Income <- as.matrix(coef(model4))
Income <- as.matrix(Income[grep("m_TEMP_county", rownames(Income)), ])

Income <- as.data.frame(cbind(Income, rownames(Income)))
rownames(Income) <- NULL
names(Income) <- c("c", "rownames")

Income$c <- as.numeric(Income$c)

Income$group <- "Very Low"
Income$group[grep("Income_groupLow", Income$rownames)] <- "Low"
Income$group[grep("Income_groupMedium", Income$rownames)] <- "Medium"
Income$group[grep("Income_groupHigh", Income$rownames)] <- "High"
Income$group[grep("Income_groupVery High", Income$rownames)] <- "Very High"

Income$range <- "0-12"
Income$range[grep("m_TEMP_county_12_30", Income$rownames)] <- "12-30"
Income$range[grep("m_TEMP_county_30_up", Income$rownames)] <- "30+"

Income <- Income[, -2]

Income_vcov <- as.matrix(vcov(model4))
Income_vcov <- as.matrix(Income_vcov[grepl("m_TEMP_county", rownames(Income_vcov)), ])
Income_vcov <- as.matrix(Income_vcov[ , grepl("m_TEMP_county", colnames(Income_vcov))])

Income$ll <- 0
Income$ul <- 0

for (tas in unique(Income$range)){
  j <- as.numeric(rownames(Income[Income$group == "Very Low" & Income$range == tas, ]))
  
  for (race in unique(Income$group)){
    
    if (race != "Very Low") {
      Income[Income$group == race & Income$range == tas, ]$c = Income[Income$group == race & Income$range == tas, ]$c + Income[Income$group == "Very Low" & Income$range == tas, ]$c
      
      i <- as.numeric(rownames(Income[Income$group == race & Income$range == tas, ]))
      
      Income[Income$group == race & Income$range == tas, ]$ll = Income[Income$group == race & Income$range == tas, ]$c - 1.96 * sqrt(Income_vcov[i, i] + Income_vcov[j, j] + 2*Income_vcov[i, j])
      Income[Income$group == race & Income$range == tas, ]$ul = Income[Income$group == race & Income$range == tas, ]$c + 1.96 * sqrt(Income_vcov[i, i] + Income_vcov[j, j] + 2*Income_vcov[i, j])
    }
    
    else{
      Income[Income$group == race & Income$range == tas, ]$ll = Income[Income$group == race & Income$range == tas, ]$c - 1.96 * sqrt(Income_vcov[j, j])
      Income[Income$group == race & Income$range == tas, ]$ul = Income[Income$group == race & Income$range == tas, ]$c + 1.96 * sqrt(Income_vcov[j, j])
    }
  }
}

for (race in unique(Income$group)){
  
  if (race != "Very Low"  ) {
    Income <- rbind(Income, as.data.frame(list(c = Income[Income$group == "Very Low" & Income$range == "30+", ]$c, 
                                               group = race, range = "30+", 
                                               ll = Income[Income$group == "Very Low" & Income$range == "30+", ]$ll, 
                                               ul = Income[Income$group == "Very Low" & Income$range == "30+", ]$ul)))
  }
  
}

saveRDS(Income, file = "REVIEW Income range 3.RDS")

models <- list("Male head Education" = model2, "Female head Education" = model3,
               "Income" = model4, "Ethnic" = model1)
modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-heterogeneity test.docx")

##### 6.5 Male Work Environment #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Environment <- ifelse(data$Male_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Environment <- factor(data$Environment)
data$Environment <- relevel(data$Environment, ref = "Indoor")

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model5 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                  m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Environment <- as.matrix(coef(model5))
Environment <- as.matrix(Environment[grep("m_TEMP_county", rownames(Environment)), ])

Environment <- as.data.frame(cbind(Environment, rownames(Environment)))
rownames(Environment) <- NULL
names(Environment) <- c("c", "rownames")

Environment$c <- as.numeric(Environment$c)

Environment$group <- "Indoor"
Environment$group[grep("EnvironmentOutdoor", Environment$rownames)] <- "Outdoor"

Environment$range <- "0-12"
Environment$range[grep("m_TEMP_county_12_30", Environment$rownames)] <- "12-30"
Environment$range[grep("m_TEMP_county_30_up", Environment$rownames)] <- "30+"

Environment <- Environment[, -2]

Environment_vcov <- as.matrix(vcov(model5))
Environment_vcov <- as.matrix(Environment_vcov[grepl("m_TEMP_county", rownames(Environment_vcov)), ])
Environment_vcov <- as.matrix(Environment_vcov[ , grepl("m_TEMP_county", colnames(Environment_vcov))])

Environment$ll <- 0
Environment$ul <- 0

for (tas in unique(Environment$range)){
  j <- as.numeric(rownames(Environment[Environment$group == "Indoor" & Environment$range == tas, ]))
  
  for (race in unique(Environment$group)){
    
    if (race != "Indoor") {
      Environment[Environment$group == race & Environment$range == tas, ]$c = Environment[Environment$group == race & Environment$range == tas, ]$c + Environment[Environment$group == "Indoor" & Environment$range == tas, ]$c
      
      i <- as.numeric(rownames(Environment[Environment$group == race & Environment$range == tas, ]))
      
      Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
      Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
    }
    
    else{
      Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[j, j])
      Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[j, j])
    }
  }
}

for (race in unique(Environment$group)){
  
  if (race != "Indoor"  ) {
    Environment <- rbind(Environment, as.data.frame(list(c = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$c, 
                                                         group = race, range = "30+", 
                                                         ll = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ll, 
                                                         ul = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ul)))
  }
  
}

saveRDS(Environment, file = "REVIEW Male Environment range 3.RDS")

##### 6.6 Female Work Environment #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Environment <- ifelse(data$Female_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Environment <- factor(data$Environment)
data$Environment <- relevel(data$Environment, ref = "Indoor")

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model6 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                  m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Environment <- as.matrix(coef(model6))
Environment <- as.matrix(Environment[grep("m_TEMP_county", rownames(Environment)), ])

Environment <- as.data.frame(cbind(Environment, rownames(Environment)))
rownames(Environment) <- NULL
names(Environment) <- c("c", "rownames")

Environment$c <- as.numeric(Environment$c)

Environment$group <- "Indoor"
Environment$group[grep("EnvironmentOutdoor", Environment$rownames)] <- "Outdoor"

Environment$range <- "0-12"
Environment$range[grep("m_TEMP_county_12_30", Environment$rownames)] <- "12-30"
Environment$range[grep("m_TEMP_county_30_up", Environment$rownames)] <- "30+"

Environment <- Environment[, -2]

Environment_vcov <- as.matrix(vcov(model6))
Environment_vcov <- as.matrix(Environment_vcov[grepl("m_TEMP_county", rownames(Environment_vcov)), ])
Environment_vcov <- as.matrix(Environment_vcov[ , grepl("m_TEMP_county", colnames(Environment_vcov))])

Environment$ll <- 0
Environment$ul <- 0

for (tas in unique(Environment$range)){
  j <- as.numeric(rownames(Environment[Environment$group == "Indoor" & Environment$range == tas, ]))
  
  for (race in unique(Environment$group)){
    
    if (race != "Indoor") {
      Environment[Environment$group == race & Environment$range == tas, ]$c = Environment[Environment$group == race & Environment$range == tas, ]$c + Environment[Environment$group == "Indoor" & Environment$range == tas, ]$c
      
      i <- as.numeric(rownames(Environment[Environment$group == race & Environment$range == tas, ]))
      
      Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
      Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[i, i] + Environment_vcov[j, j] + 2*Environment_vcov[i, j])
    }
    
    else{
      Environment[Environment$group == race & Environment$range == tas, ]$ll = Environment[Environment$group == race & Environment$range == tas, ]$c - 1.96 * sqrt(Environment_vcov[j, j])
      Environment[Environment$group == race & Environment$range == tas, ]$ul = Environment[Environment$group == race & Environment$range == tas, ]$c + 1.96 * sqrt(Environment_vcov[j, j])
    }
  }
}

for (race in unique(Environment$group)){
  
  if (race != "Indoor"  ) {
    Environment <- rbind(Environment, as.data.frame(list(c = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$c, 
                                                         group = race, range = "30+", 
                                                         ll = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ll, 
                                                         ul = Environment[Environment$group == "Indoor" & Environment$range == "30+", ]$ul)))
  }
  
}

saveRDS(Environment, file = "REVIEW Female Environment range 3.RDS")

models <- list("Male Head Work Environment" = model5, "Female Head Work Environment" = model6)
modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-heterogeneity test2.docx")

##### 6.7 Climate Zone #####
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

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)

data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model7 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Zone + m_TEMP_county_12_30 * Zone  + m_TEMP_county_30_up + 
                  m_WDSP_county * Zone + m_PRCP_county * Zone + m_RH_county * Zone + m_RH_county2 * Zone +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status | 
                  year + month + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single")

Zone <- as.matrix(coef(model7))
Zone <- as.matrix(Zone[grep("m_TEMP_county", rownames(Zone)), ])

Zone <- as.data.frame(cbind(Zone, rownames(Zone)))
rownames(Zone) <- NULL
names(Zone) <- c("c", "rownames")

Zone$c <- as.numeric(Zone$c)

Zone$group <- "Hot-Humid"
Zone$group[grep("Zone2", Zone$rownames)] <- "Hot-Dry/Mixed-Dry"
Zone$group[grep("Zone3", Zone$rownames)] <- "Mixed-Humid"
Zone$group[grep("Zone4", Zone$rownames)] <- "Marine"
Zone$group[grep("Zone5", Zone$rownames)] <- "Cold/Very Cold"

Zone$range <- "0-12"
Zone$range[grep("m_TEMP_county_12_30", Zone$rownames)] <- "12-30"
Zone$range[grep("m_TEMP_county_30_up", Zone$rownames)] <- "30+"

Zone <- Zone[, -2]

Zone_vcov <- as.matrix(vcov(model7))
Zone_vcov <- as.matrix(Zone_vcov[grepl("m_TEMP_county", rownames(Zone_vcov)), ])
Zone_vcov <- as.matrix(Zone_vcov[ , grepl("m_TEMP_county", colnames(Zone_vcov))])

Zone$ll <- 0
Zone$ul <- 0

for (tas in unique(Zone$range)){
  j <- as.numeric(rownames(Zone[Zone$group == "Hot-Humid" & Zone$range == tas, ]))
  
  for (race in unique(Zone$group)){
    
    if (race != "Hot-Humid" ) {
      Zone[Zone$group == race & Zone$range == tas, ]$c = Zone[Zone$group == race & Zone$range == tas, ]$c + Zone[Zone$group == "Hot-Humid" & Zone$range == tas, ]$c
      
      i <- as.numeric(rownames(Zone[Zone$group == race & Zone$range == tas, ]))
      
      Zone[Zone$group == race & Zone$range == tas, ]$ll = Zone[Zone$group == race & Zone$range == tas, ]$c - 1.96 * sqrt(Zone_vcov[i, i] + Zone_vcov[j, j] + 2*Zone_vcov[i, j])
      Zone[Zone$group == race & Zone$range == tas, ]$ul = Zone[Zone$group == race & Zone$range == tas, ]$c + 1.96 * sqrt(Zone_vcov[i, i] + Zone_vcov[j, j] + 2*Zone_vcov[i, j])
    }
    
    else{
      Zone[Zone$group == race & Zone$range == tas, ]$ll = Zone[Zone$group == race & Zone$range == tas, ]$c - 1.96 * sqrt(Zone_vcov[j, j])
      Zone[Zone$group == race & Zone$range == tas, ]$ul = Zone[Zone$group == race & Zone$range == tas, ]$c + 1.96 * sqrt(Zone_vcov[j, j])
    }
  }
}

for (race in unique(Zone$group)){
  
  if (race != "Hot-Humid"  ) {
    Zone <- rbind(Zone, as.data.frame(list(c = Zone[Zone$group == "Hot-Humid" & Zone$range == "30+", ]$c, 
                                           group = race, range = "30+", 
                                           ll = Zone[Zone$group == "Hot-Humid" & Zone$range == "30+", ]$ll, 
                                           ul = Zone[Zone$group == "Hot-Humid" & Zone$range == "30+", ]$ul)))
  }
  
}

saveRDS(Zone, file = "REVIEW Climate Zone range 3.RDS")

models <- list("Climate Zone" = model7)
modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-heterogeneity test3.docx")

##### 6.8 background CDD #####
climate_county_month <- readRDS("climate county homescan month.RDS")
climate_county_month <- climate_county_month[climate_county_month$year <= 2003, ]

climate_county_month <- climate_county_month %>%
  group_by(year, fips_county) %>%
  mutate(y_HDD = sum(m_HDD65, na.rm = TRUE)) %>%
  mutate(y_CDD = sum(m_CDD65, na.rm = TRUE)) %>%
  ungroup()

climate_county_month <- climate_county_month %>%
  group_by(fips_county) %>%
  mutate(HDD_1994_2003 = mean(y_HDD, na.rm = TRUE)) %>%
  mutate(CDD_1994_2003 = mean(y_CDD, na.rm = TRUE)) %>%
  ungroup()

climate_county_month <- climate_county_month %>%
  select(fips_county, HDD_1994_2003, CDD_1994_2003) %>%
  distinct()

# regression
data <- readRDS("test_nutrition_household_zone_month.RDS")

data <- inner_join(data, climate_county_month, by = "fips_county")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$CDD_group <- ifelse(0 <= data$CDD_1994_2003 & data$CDD_1994_2003 < 500, "Very Low",
                         ifelse(500 <= data$CDD_1994_2003 & data$CDD_1994_2003 < 1000, "Low", 
                                ifelse(1000 <= data$CDD_1994_2003 & data$CDD_1994_2003 < 1500, "Medium", 
                                       ifelse(1500 <= data$CDD_1994_2003 & data$CDD_1994_2003 < 2000, "High", "Very High"))))

data$CDD_group <- factor(data$CDD_group)
data$CDD_group <- relevel(data$CDD_group, ref = "Very Low")

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~
                 m_TEMP_county_0_12 * CDD_group + m_TEMP_county_12_30 * CDD_group  + m_TEMP_county_30_up + 
                 m_WDSP_county * CDD_group + m_PRCP_county * CDD_group + m_RH_county * CDD_group + m_RH_county2 * CDD_group +
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                 year^Zone + month^Zone + household_code,
               data = data,
               weights = ~Projection_Factor,
               vcov = ~fips_county,
               fixef.rm = "single")

summary(model)

CDD <- as.matrix(coef(model))
CDD <- as.matrix(CDD[grep("m_TEMP_county", rownames(CDD)), ])

CDD <- as.data.frame(cbind(CDD, rownames(CDD)))
rownames(CDD) <- NULL
names(CDD) <- c("c", "rownames")

CDD$c <- as.numeric(CDD$c)

CDD$group <- "Very Low"
CDD$group[grep("CDD_groupLow", CDD$rownames)] <- "Low"
CDD$group[grep("CDD_groupMedium", CDD$rownames)] <- "Medium"
CDD$group[grep("CDD_groupHigh", CDD$rownames)] <- "High"
CDD$group[grep("CDD_groupVery High", CDD$rownames)] <- "Very High"

CDD$range <- "0-12"
CDD$range[grep("m_TEMP_county_12_30", CDD$rownames)] <- "12-30"
CDD$range[grep("m_TEMP_county_30_up", CDD$rownames)] <- "30+"

CDD <- CDD[, -2]

CDD_vcov <- as.matrix(vcov(model))
CDD_vcov <- as.matrix(CDD_vcov[grepl("m_TEMP_county", rownames(CDD_vcov)), ])
CDD_vcov <- as.matrix(CDD_vcov[ , grepl("m_TEMP_county", colnames(CDD_vcov))])

CDD$ll <- 0
CDD$ul <- 0

for (tas in unique(CDD$range)){
  j <- as.numeric(rownames(CDD[CDD$group == "Very Low" & CDD$range == tas, ]))
  
  for (race in unique(CDD$group)){
    
    if (race != "Very Low") {
      CDD[CDD$group == race & CDD$range == tas, ]$c = CDD[CDD$group == race & CDD$range == tas, ]$c + CDD[CDD$group == "Very Low" & CDD$range == tas, ]$c
      
      i <- as.numeric(rownames(CDD[CDD$group == race & CDD$range == tas, ]))
      
      CDD[CDD$group == race & CDD$range == tas, ]$ll = CDD[CDD$group == race & CDD$range == tas, ]$c - 1.96 * sqrt(CDD_vcov[i, i] + CDD_vcov[j, j] + 2*CDD_vcov[i, j])
      CDD[CDD$group == race & CDD$range == tas, ]$ul = CDD[CDD$group == race & CDD$range == tas, ]$c + 1.96 * sqrt(CDD_vcov[i, i] + CDD_vcov[j, j] + 2*CDD_vcov[i, j])
    }
    
    else{
      CDD[CDD$group == race & CDD$range == tas, ]$ll = CDD[CDD$group == race & CDD$range == tas, ]$c - 1.96 * sqrt(CDD_vcov[j, j])
      CDD[CDD$group == race & CDD$range == tas, ]$ul = CDD[CDD$group == race & CDD$range == tas, ]$c + 1.96 * sqrt(CDD_vcov[j, j])
    }
  }
}

for (race in unique(CDD$group)){
  
  if (race != "Very Low"  ) {
    CDD <- rbind(CDD, as.data.frame(list(c = CDD[CDD$group == "Very Low" & CDD$range == "30+", ]$c, 
                                         group = race, range = "30+", 
                                         ll = CDD[CDD$group == "Very Low" & CDD$range == "30+", ]$ll, 
                                         ul = CDD[CDD$group == "Very Low" & CDD$range == "30+", ]$ul)))
  }
  
}

saveRDS(CDD, file = "REVIEW CDD range 3.RDS")

models <- list("CDD" = model)
modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-heterogeneity test background CDD.docx")

##### 6.9 overall added-sugar consumption of SSB #####
# merged_data <- readRDS("REVIEW consumption regression of drink category.RDS")

merged_data <- readRDS("REVIEW sugar regression of food category.RDS")

merged_data <- merged_data %>%
  group_by(household_code) %>%
  summarise(sugar = mean(`mpc_Sweetened Beverages`, na.rm = TRUE))%>%
  ungroup()

merged_data$group <- ifelse(merged_data$sugar < 45, "Low", "High")

# regression
data <- readRDS("test_nutrition_household_zone_month.RDS")

data <- inner_join(data, merged_data, by = "household_code")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$group <- factor(data$group)
data$group <- relevel(data$group, ref = "Low")

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~
                 m_TEMP_county_0_12 * group + m_TEMP_county_12_30 * group  + m_TEMP_county_30_up + 
                 m_WDSP_county * group + m_PRCP_county * group + m_RH_county * group + m_RH_county2 * group +
                 Household_Income + Household_Size + Age_And_Presence_Of_Children +
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                 year^Zone + month^Zone + household_code,
               data = data,
               weights = ~Projection_Factor,
               vcov = ~fips_county,
               fixef.rm = "single")

group <- as.matrix(coef(model))
group <- as.matrix(group[grep("m_TEMP_county", rownames(group)), ])

group <- as.data.frame(cbind(group, rownames(group)))
rownames(group) <- NULL
names(group) <- c("c", "rownames")

group$c <- as.numeric(group$c)

group$group <- "Low"
group$group[grep("groupHigh", group$rownames)] <- "High"

group$range <- "0-12"
group$range[grep("m_TEMP_county_12_30", group$rownames)] <- "12-30"
group$range[grep("m_TEMP_county_30_up", group$rownames)] <- "30+"

group <- group[, -2]

group_vcov <- as.matrix(vcov(model))
group_vcov <- as.matrix(group_vcov[grepl("m_TEMP_county", rownames(group_vcov)), ])
group_vcov <- as.matrix(group_vcov[ , grepl("m_TEMP_county", colnames(group_vcov))])

group$ll <- 0
group$ul <- 0

for (tas in unique(group$range)){
  j <- as.numeric(rownames(group[group$group == "Low" & group$range == tas, ]))
  
  for (race in unique(group$group)){
    
    if (race != "Low") {
      group[group$group == race & group$range == tas, ]$c = group[group$group == race & group$range == tas, ]$c + group[group$group == "Low" & group$range == tas, ]$c
      
      i <- as.numeric(rownames(group[group$group == race & group$range == tas, ]))
      
      group[group$group == race & group$range == tas, ]$ll = group[group$group == race & group$range == tas, ]$c - 1.96 * sqrt(group_vcov[i, i] + group_vcov[j, j] + 2*group_vcov[i, j])
      group[group$group == race & group$range == tas, ]$ul = group[group$group == race & group$range == tas, ]$c + 1.96 * sqrt(group_vcov[i, i] + group_vcov[j, j] + 2*group_vcov[i, j])
    }
    
    else{
      group[group$group == race & group$range == tas, ]$ll = group[group$group == race & group$range == tas, ]$c - 1.96 * sqrt(group_vcov[j, j])
      group[group$group == race & group$range == tas, ]$ul = group[group$group == race & group$range == tas, ]$c + 1.96 * sqrt(group_vcov[j, j])
    }
  }
}

for (race in unique(group$group)){
  
  if (race != "Low"  ) {
    group <- rbind(group, as.data.frame(list(c = group[group$group == "Low" & group$range == "30+", ]$c, 
                                             group = race, range = "30+", 
                                             ll = group[group$group == "Low" & group$range == "30+", ]$ll, 
                                             ul = group[group$group == "Low" & group$range == "30+", ]$ul)))
  }
  
}

saveRDS(group, file = "REVIEW SSB consumption range 3.RDS")

models <- list("SSB Consumption Group" = model)
modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-heterogeneity test SSB consumption.docx")


#### 7. temperature by the monthly peak highs####
##### 7.1 0-12，12-30，30+℃ #####
# climate data
climate_county_data <- readRDS("climate county homescan.RDS")

names(climate_county_data)[names(climate_county_data) == "YEAR"] <- "year"
names(climate_county_data)[names(climate_county_data) == "MONTH"] <- "month"

variables <- c("TEMP_county", "DEWP_county", "VISIB_county", "WDSP_county", "PRCP_county", "RH_county")

for (var in variables) {
  climate_county_data <- climate_county_data %>%
    group_by(year, month, fips_county) %>%
    mutate(!!paste0("m_", var) := mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

# climate_county_data$temp_group <- ifelse(climate_county_data$TEMP_county <= 0, 0, 
#                                          ifelse(climate_county_data$TEMP_county > 0 & climate_county_data$TEMP_county <= 12, 1, 
#                                                 ifelse(climate_county_data$TEMP_county > 12 & climate_county_data$TEMP_county <= 30, 2, 3)))

breaks <- c(-Inf, 0, 12, 30, Inf)
labels <- 0:3  

climate_county_data$temp_group <- cut(
  climate_county_data$TEMP_county,
  breaks = breaks,
  labels = labels,
  right = TRUE  
)

climate_county_data$temp_count_0 <- ifelse(climate_county_data$temp_group == 0, 1, 0)
climate_county_data$temp_count_1 <- ifelse(climate_county_data$temp_group == 1, 1, 0)
climate_county_data$temp_count_2 <- ifelse(climate_county_data$temp_group == 2, 1, 0)
climate_county_data$temp_count_3 <- ifelse(climate_county_data$temp_group == 3, 1, 0)

climate_county_data$count <- 1
climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(month_day_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

month_day_31 <- c(1, 3, 5, 7, 8, 10, 12)
month_day_30 <- c(4, 6, 9, 11)
year_2_29 <- c(1996, 2000, 2004, 2008, 2012, 2016)

climate_county_data$month_day <- ifelse(climate_county_data$month %in% month_day_31, 31, 
                                        ifelse(climate_county_data$month %in% month_day_30, 30,
                                               ifelse(climate_county_data$year %in% year_2_29, 29, 28))) 

climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(m_temp_count_0 = sum(temp_count_0 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_1 = sum(temp_count_1 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_2 = sum(temp_count_2 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_3 = sum(temp_count_3 / month_day_count * month_day, na.rm = TRUE)) %>%
  ungroup()

climate_county_data$m_RH_county2 <- climate_county_data$m_RH_county^2

climate_county_data <- climate_county_data %>%
  select(year, month, month_day, fips_county, starts_with("m_")) %>%
  distinct()

saveRDS(climate_county_data, "climate county homescan month peak highs.RDS")

# regression month nutrition
household_purchase_nutrition_month <- readRDS("household purchase nutrition month.RDS")

# household_purchase_nutrition_month <- household_purchase_nutrition_month[!(household_purchase_nutrition_month$year %in% c(2003, 2020)), ]

climate_county_month <- readRDS("climate county homescan month peak highs.RDS")

test <- inner_join(household_purchase_nutrition_month, climate_county_month, by = c("fips_county", "year", "month"))

# sugar of per person per day
test$mpc_ADD_SUGARS <- test$mt_ADD_SUGARS / test$month_day / test$Household_Size
test$mpc_ADD_SUGARS_convert <- test$mt_ADD_SUGARS / test$month_day / test$Household_size_convert

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

test <- inner_join(test, zone, by = "fips_county")

saveRDS(test, "test_nutrition_household_zone_month_peaks.RDS")

# regression
data <- readRDS("test_nutrition_household_zone_month_peaks.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_temp_count_1 + m_temp_count_2 + m_temp_count_3 + 
                 m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Zone + month^Zone + household_code,  
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

models <- list("mpc_ADD_SUGARS_convert" = model)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-peak highs.docx")
##### 7.2 5℃ #####
# climate data
climate_county_data <- readRDS("climate county homescan.RDS")

names(climate_county_data)[names(climate_county_data) == "YEAR"] <- "year"
names(climate_county_data)[names(climate_county_data) == "MONTH"] <- "month"

variables <- c("TEMP_county", "DEWP_county", "VISIB_county", "WDSP_county", "PRCP_county", "RH_county")

for (var in variables) {
  climate_county_data <- climate_county_data %>%
    group_by(year, month, fips_county) %>%
    mutate(!!paste0("m_", var) := mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

# climate_county_data$temp_group <- ifelse(climate_county_data$TEMP_county <= 0, 0, 
#                                          ifelse(climate_county_data$TEMP_county > 0 & climate_county_data$TEMP_county <= 12, 1, 
#                                                 ifelse(climate_county_data$TEMP_county > 12 & climate_county_data$TEMP_county <= 30, 2, 3)))

breaks <- c(-Inf, 0, 5, 10, 15, 20, 25, 30, Inf)
labels <- 0:7  

climate_county_data$temp_group <- cut(
  climate_county_data$TEMP_county,
  breaks = breaks,
  labels = labels,
  right = TRUE  
)

climate_county_data$temp_count_0 <- ifelse(climate_county_data$temp_group == 0, 1, 0)
climate_county_data$temp_count_1 <- ifelse(climate_county_data$temp_group == 1, 1, 0)
climate_county_data$temp_count_2 <- ifelse(climate_county_data$temp_group == 2, 1, 0)
climate_county_data$temp_count_3 <- ifelse(climate_county_data$temp_group == 3, 1, 0)
climate_county_data$temp_count_4 <- ifelse(climate_county_data$temp_group == 4, 1, 0)
climate_county_data$temp_count_5 <- ifelse(climate_county_data$temp_group == 5, 1, 0)
climate_county_data$temp_count_6 <- ifelse(climate_county_data$temp_group == 6, 1, 0)
climate_county_data$temp_count_7 <- ifelse(climate_county_data$temp_group == 7, 1, 0)

climate_county_data$count <- 1
climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(month_day_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

month_day_31 <- c(1, 3, 5, 7, 8, 10, 12)
month_day_30 <- c(4, 6, 9, 11)
year_2_29 <- c(1996, 2000, 2004, 2008, 2012, 2016)

climate_county_data$month_day <- ifelse(climate_county_data$month %in% month_day_31, 31, 
                                        ifelse(climate_county_data$month %in% month_day_30, 30,
                                               ifelse(climate_county_data$year %in% year_2_29, 29, 28))) 

climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(m_temp_count_0 = sum(temp_count_0 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_1 = sum(temp_count_1 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_2 = sum(temp_count_2 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_3 = sum(temp_count_3 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_4 = sum(temp_count_4 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_5 = sum(temp_count_5 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_6 = sum(temp_count_6 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_7 = sum(temp_count_7 / month_day_count * month_day, na.rm = TRUE)) %>%
  ungroup()

climate_county_data$m_RH_county2 <- climate_county_data$m_RH_county^2

climate_county_data <- climate_county_data %>%
  select(year, month, month_day, fips_county, starts_with("m_")) %>%
  distinct()

saveRDS(climate_county_data, "climate county homescan month peak highs 5℃.RDS")

# regression month nutrition
household_purchase_nutrition_month <- readRDS("household purchase nutrition month.RDS")

# household_purchase_nutrition_month <- household_purchase_nutrition_month[!(household_purchase_nutrition_month$year %in% c(2003, 2020)), ]

climate_county_month <- readRDS("climate county homescan month peak highs 5℃.RDS")

test <- inner_join(household_purchase_nutrition_month, climate_county_month, by = c("fips_county", "year", "month"))

# sugar of per person per day
test$mpc_ADD_SUGARS <- test$mt_ADD_SUGARS / test$month_day / test$Household_Size
test$mpc_ADD_SUGARS_convert <- test$mt_ADD_SUGARS / test$month_day / test$Household_size_convert

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

test <- inner_join(test, zone, by = "fips_county")

saveRDS(test, "test_nutrition_household_zone_month_peaks 5℃.RDS")

# regression
data <- readRDS("test_nutrition_household_zone_month_peaks 5℃.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_temp_count_1 + m_temp_count_2 + m_temp_count_3 + m_temp_count_4 + m_temp_count_5 + m_temp_count_6 + m_temp_count_7 +
                 m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Zone + month^Zone + household_code,  
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

# model <- lm(mpc_ADD_SUGARS_convert ~ 
#               m_temp_count_1 + m_temp_count_2 + m_temp_count_3 + m_temp_count_4 + m_temp_count_5 + m_temp_count_6 + m_temp_count_7 +
#               m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
#               Household_Income + Household_Size + Age_And_Presence_Of_Children + 
#               Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
#               Ethnic + Marital_Status,
#             data = data)
# vif_df <- vif(model)
# vif_df

models <- list("mpc_ADD_SUGARS_convert" = model)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-peak highs 5℃.docx")

##### 7.3 2℃ #####
# climate data
climate_county_data <- readRDS("climate county homescan.RDS")

names(climate_county_data)[names(climate_county_data) == "YEAR"] <- "year"
names(climate_county_data)[names(climate_county_data) == "MONTH"] <- "month"

variables <- c("TEMP_county", "DEWP_county", "VISIB_county", "WDSP_county", "PRCP_county", "RH_county")

for (var in variables) {
  climate_county_data <- climate_county_data %>%
    group_by(year, month, fips_county) %>%
    mutate(!!paste0("m_", var) := mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

# climate_county_data$temp_group <- ifelse(climate_county_data$TEMP_county <= 0, 0, 
#                                          ifelse(climate_county_data$TEMP_county > 0 & climate_county_data$TEMP_county <= 12, 1, 
#                                                 ifelse(climate_county_data$TEMP_county > 12 & climate_county_data$TEMP_county <= 30, 2, 3)))

breaks <- c(-Inf, 0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, Inf)
labels <- 0:16  

climate_county_data$temp_group <- cut(
  climate_county_data$TEMP_county,
  breaks = breaks,
  labels = labels,
  right = TRUE  
)

climate_county_data$temp_count_0 <- ifelse(climate_county_data$temp_group == 0, 1, 0)
climate_county_data$temp_count_1 <- ifelse(climate_county_data$temp_group == 1, 1, 0)
climate_county_data$temp_count_2 <- ifelse(climate_county_data$temp_group == 2, 1, 0)
climate_county_data$temp_count_3 <- ifelse(climate_county_data$temp_group == 3, 1, 0)
climate_county_data$temp_count_4 <- ifelse(climate_county_data$temp_group == 4, 1, 0)
climate_county_data$temp_count_5 <- ifelse(climate_county_data$temp_group == 5, 1, 0)
climate_county_data$temp_count_6 <- ifelse(climate_county_data$temp_group == 6, 1, 0)
climate_county_data$temp_count_7 <- ifelse(climate_county_data$temp_group == 7, 1, 0)
climate_county_data$temp_count_8 <- ifelse(climate_county_data$temp_group == 8, 1, 0)
climate_county_data$temp_count_9 <- ifelse(climate_county_data$temp_group == 9, 1, 0)
climate_county_data$temp_count_10 <- ifelse(climate_county_data$temp_group == 10, 1, 0)
climate_county_data$temp_count_11 <- ifelse(climate_county_data$temp_group == 11, 1, 0)
climate_county_data$temp_count_12 <- ifelse(climate_county_data$temp_group == 12, 1, 0)
climate_county_data$temp_count_13 <- ifelse(climate_county_data$temp_group == 13, 1, 0)
climate_county_data$temp_count_14 <- ifelse(climate_county_data$temp_group == 14, 1, 0)
climate_county_data$temp_count_15 <- ifelse(climate_county_data$temp_group == 15, 1, 0)
climate_county_data$temp_count_16 <- ifelse(climate_county_data$temp_group == 16, 1, 0)

climate_county_data$count <- 1
climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(month_day_count = sum(count, na.rm = TRUE)) %>%
  ungroup()

month_day_31 <- c(1, 3, 5, 7, 8, 10, 12)
month_day_30 <- c(4, 6, 9, 11)
year_2_29 <- c(1996, 2000, 2004, 2008, 2012, 2016)

climate_county_data$month_day <- ifelse(climate_county_data$month %in% month_day_31, 31, 
                                        ifelse(climate_county_data$month %in% month_day_30, 30,
                                               ifelse(climate_county_data$year %in% year_2_29, 29, 28))) 

climate_county_data <- climate_county_data %>%
  group_by(year, month, fips_county) %>%
  mutate(m_temp_count_0 = sum(temp_count_0 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_1 = sum(temp_count_1 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_2 = sum(temp_count_2 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_3 = sum(temp_count_3 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_4 = sum(temp_count_4 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_5 = sum(temp_count_5 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_6 = sum(temp_count_6 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_7 = sum(temp_count_7 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_8 = sum(temp_count_8 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_9 = sum(temp_count_9 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_10 = sum(temp_count_10 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_11 = sum(temp_count_11 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_12 = sum(temp_count_12 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_13 = sum(temp_count_13 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_14 = sum(temp_count_14 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_15 = sum(temp_count_15 / month_day_count * month_day, na.rm = TRUE)) %>%
  mutate(m_temp_count_16 = sum(temp_count_16 / month_day_count * month_day, na.rm = TRUE)) %>%
  ungroup()

climate_county_data$m_RH_county2 <- climate_county_data$m_RH_county^2

climate_county_data <- climate_county_data %>%
  select(year, month, month_day, fips_county, starts_with("m_")) %>%
  distinct()

saveRDS(climate_county_data, "climate county homescan month peak highs 2℃.RDS")

# regression month nutrition
household_purchase_nutrition_month <- readRDS("household purchase nutrition month.RDS")

# household_purchase_nutrition_month <- household_purchase_nutrition_month[!(household_purchase_nutrition_month$year %in% c(2003, 2020)), ]

climate_county_month <- readRDS("climate county homescan month peak highs 2℃.RDS")

test <- inner_join(household_purchase_nutrition_month, climate_county_month, by = c("fips_county", "year", "month"))

# sugar of per person per day
test$mpc_ADD_SUGARS <- test$mt_ADD_SUGARS / test$month_day / test$Household_Size
test$mpc_ADD_SUGARS_convert <- test$mt_ADD_SUGARS / test$month_day / test$Household_size_convert

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

test <- inner_join(test, zone, by = "fips_county")

saveRDS(test, "test_nutrition_household_zone_month_peaks 2℃.RDS")

# regression
data <- readRDS("test_nutrition_household_zone_month_peaks 2℃.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_temp_count_1 + m_temp_count_2 + m_temp_count_3 + m_temp_count_4 + m_temp_count_5 + m_temp_count_6 + m_temp_count_7 + m_temp_count_8 + 
                 m_temp_count_9 + m_temp_count_10 + m_temp_count_11 + m_temp_count_12 + m_temp_count_13 + m_temp_count_14 + m_temp_count_15 + m_temp_count_16 +
                 m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Zone + month^Zone + household_code,  
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

models <- list("mpc_ADD_SUGARS_convert" = model)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-peak highs 2℃.docx")


#### 8. Price ####
food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
food_product_focused$upc <- as.numeric(food_product_focused$upc)

household_information <- readRDS("household information.RDS")

county_food_price_month <- data.frame()

for(i in 2004:2019) {
  purchases <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/purchases_", i, ".RDS"))
  
  test <- inner_join(purchases, food_product_focused, by = c("upc", "upc_ver_uc"))
  
  trips <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/Nielson_ConsumerPanel/Consumer_Panel_Data_", i, "/nielsen_extracts/HMS/", i,"/Annual_Files/trips_", i, ".RDS"))
  
  test <- inner_join(test, trips, by = "trip_code_uc")
  
  test$year <- as.numeric(substr(test$purchase_date, 1, 4))
  test$month <- as.numeric(substr(test$purchase_date, 6, 7))
  
  test <- test %>%
    select(trip_code_uc, upc, total_price_paid, coupon_value, `WWEIA Category`, household_code, year, month, weight_gram, quantity)
  
  test$weight = test$weight_gram * test$quantity
  
  test <- inner_join(test, household_information, by = c("household_code", "year")) 
  
  # price per 100 grams of each product
  test$price <- (test$total_price_paid - test$coupon_value) / test$weight * 100
  
  county_food_price_month <- rbind(county_food_price_month, test)
}

county_food_price_month <- county_food_price_month[county_food_price_month$year <=2019, ]

saveRDS(county_food_price_month, file = "REVIEW food focused price totally.RDS")

##### 8.1 remove the top 5% outliers and using inflation #####
data <- readRDS("REVIEW food focused price totally.RDS")
county_food_price_month <- data

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
food_product_focused$upc <- as.numeric(food_product_focused$upc)

Category <- unique(food_product_focused$`WWEIA Category`)

for (category in Category) {
  top_5_percent <- quantile(data[data$`WWEIA Category` == category,]$price, probs = 0.95, na.rm = TRUE)
  
  county_food_price_month <- county_food_price_month %>%
    filter(!(price > top_5_percent & `WWEIA Category` == category))
}

# There may be so much of some products that a direct average would have a large impact on the average price of that food group
# average price per 100 grams of each product
test <- county_food_price_month %>%  
  group_by(fips_county, year, month, upc, `WWEIA Category`) %>%  
  summarise(m_price = mean(price, na.rm = TRUE)) %>%  
  ungroup()  

# average price per 100 grams of each category
test <- test %>%  
  group_by(fips_county, year, month, `WWEIA Category`) %>%  
  summarise(mc_price = mean(m_price, na.rm = TRUE)) %>%  
  ungroup()  

test <- test %>% 
  spread(key = `WWEIA Category`, value = mc_price, fill = NA)

for (var_name in Category) {
  if (!exists(var_name, test)) {
    test[[var_name]] <- NA
  }
}

saveRDS(test, file = "REVIEW food focused price monthly top 5% removed.RDS")

# regression data - remove the top 5% outliers
county_food_price_month <- readRDS("REVIEW food focused price monthly top 5% removed.RDS")

county_food_price_month <- county_food_price_month %>%
  complete(year, month, fips_county, fill = list(values = NA))

Category <- names(county_food_price_month)[!(names(county_food_price_month) %in% c("fips_county", "year", "month"))]

# replace missing using county-year and nation-month price
for (food in Category) {
  county_food_price_month <- county_food_price_month %>%  
    group_by(fips_county, year) %>%  
    mutate(!!paste0("county_average_", food) := mean(!!sym(food), na.rm = TRUE)) %>%
    ungroup()
}

for (food in Category) {
  county_food_price_month <- county_food_price_month %>%  
    group_by(year, month) %>%  
    mutate(!!paste0("nation_average_", food) := mean(!!sym(food), na.rm = TRUE)) %>%
    ungroup()
}

for (food in Category) {
  col_name <- paste0("county_average_", food)
  county_food_price_month <- county_food_price_month %>%  
    mutate(!!sym(food) := ifelse(is.na(!!sym(food)), !!sym(col_name), !!sym(food)))
}

for (food in Category) {
  col_name <- paste0("nation_average_", food)
  county_food_price_month <- county_food_price_month %>%  
    mutate(!!sym(food) := ifelse(is.na(!!sym(food)), !!sym(col_name), !!sym(food)))
}

county_food_price_month <- county_food_price_month %>%
  select(-starts_with("county_average_"), -starts_with("nation_average_")) %>%
  distinct()

data <- readRDS("test_nutrition_household_zone_month.RDS")

data <- inner_join(county_food_price_month, data, by = c("fips_county", "year", "month"), multiple = "all")
data <- data %>% filter(year != 2003 & year != 2020)

saveRDS(data, file = "REVIEW household food price top 5% removed.RDS")

# price regression
data <- readRDS("REVIEW household food price top 5% removed.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

cols_to_modify <- c("100% Juice", "Alcoholic Beverages", "Breads, Rolls, Tortillas", "Candy", 
                    "Cheese", "Coffee and Tea", "Condiments and Sauces", "Cooked Cereals", 
                    "Cooked Grains", "Crackers", "Cured Meats/Poultry", "Dairy Drinks and Substitutes", "Diet Beverages", 
                    "Eggs", "Fats and Oils", "Fruits", "Meats", "Milk", "Mixed Dishes - Soups", "Mixed Dishes – Bean/Vegetable-based", 
                    "Mixed Dishes – Grain-based", "Mixed Dishes – Meat, Poultry, Seafood", "Mixed Dishes – Mexican", "Mixed Dishes – Pizza", 
                    "Mixed Dishes – Sandwiches (single code)", "Other", "Other Desserts", "Plain Water", "Plant-based Protein Foods", 
                    "Poultry", "Quick Breads and Bread Products", "Ready-to-Eat Cereals", "Savory Snacks", "Seafood", 
                    "Snack/Meal Bars", "Sugars", "Sweet Bakery Products", "Sweetened Beverages", 
                    "Vegetables, excluding Potatoes", "White Potatoes", "Yogurt")

# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(all_of(cols_to_modify), ~ . / CPI_map[as.character(year)]))

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$m_TEMP_range2 <- factor(data$m_TEMP_range2)
data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 +
                 `100% Juice` + `Alcoholic Beverages` + `Breads, Rolls, Tortillas` + `Candy` + 
                 `Cheese` + `Coffee and Tea` + `Condiments and Sauces` + `Cooked Cereals` + 
                 `Cooked Grains` + `Crackers` + `Cured Meats/Poultry` + `Dairy Drinks and Substitutes` + 
                 `Diet Beverages` + `Eggs` + `Fats and Oils` + `Fruits` + `Meats` + `Milk` + 
                 `Mixed Dishes - Soups` + `Mixed Dishes – Bean/Vegetable-based` + `Mixed Dishes – Grain-based` + 
                 `Mixed Dishes – Meat, Poultry, Seafood` + `Mixed Dishes – Mexican` + `Mixed Dishes – Pizza` + 
                 `Mixed Dishes – Sandwiches (single code)` + `Other` + `Other Desserts` + `Plain Water` + 
                 `Plant-based Protein Foods` + `Poultry` + `Quick Breads and Bread Products` + `Ready-to-Eat Cereals` + 
                 `Savory Snacks` + `Seafood` + `Snack/Meal Bars` + `Sugars` + `Sweet Bakery Products` + 
                 `Sweetened Beverages` + `Vegetables, excluding Potatoes` + `White Potatoes` + `Yogurt`+
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Zone + month^Zone + household_code, 
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

modelsummary(model, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-price model top 5%.docx")

# VIF
model <- lm(mpc_ADD_SUGARS_convert ~ 
              m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 +
              `100% Juice` + `Alcoholic Beverages` + `Breads, Rolls, Tortillas` + `Candy` + 
              `Cheese` + `Coffee and Tea` + `Condiments and Sauces` + `Cooked Cereals` + 
              `Cooked Grains` + `Crackers` + `Cured Meats/Poultry` + `Dairy Drinks and Substitutes` + 
              `Diet Beverages` + `Eggs` + `Fats and Oils` + `Fruits` + `Meats` + `Milk` + 
              `Mixed Dishes - Soups` + `Mixed Dishes – Bean/Vegetable-based` + `Mixed Dishes – Grain-based` + 
              `Mixed Dishes – Meat, Poultry, Seafood` + `Mixed Dishes – Mexican` + `Mixed Dishes – Pizza` + 
              `Mixed Dishes – Sandwiches (single code)` + `Other` + `Other Desserts` + `Plain Water` + 
              `Plant-based Protein Foods` + `Poultry` + `Quick Breads and Bread Products` + `Ready-to-Eat Cereals` + 
              `Savory Snacks` + `Seafood` + `Snack/Meal Bars` + `Sugars` + `Sweet Bakery Products` + 
              `Sweetened Beverages` + `Vegetables, excluding Potatoes` + `White Potatoes` + `Yogurt`+
              Household_Income + Household_Size + Age_And_Presence_Of_Children + 
              Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
              Ethnic + Marital_Status,
            data = data)
vif_df <- vif(model)

write.csv(vif_df, "VIF_Results.csv", row.names = FALSE)


target_vars  <- c("100% Juice", "Alcoholic Beverages", "Breads, Rolls, Tortillas", "Candy", 
                  "Cheese", "Coffee and Tea", "Condiments and Sauces", "Cooked Cereals", 
                  "Cooked Grains", "Crackers", "Cured Meats/Poultry", "Dairy Drinks and Substitutes", "Diet Beverages", 
                  "Eggs", "Fats and Oils", "Fruits", "Meats", "Milk", "Mixed Dishes - Soups", "Mixed Dishes – Bean/Vegetable-based", 
                  "Mixed Dishes – Grain-based", "Mixed Dishes – Meat, Poultry, Seafood", "Mixed Dishes – Mexican", "Mixed Dishes – Pizza", 
                  "Mixed Dishes – Sandwiches (single code)", "Other", "Other Desserts", "Plain Water", "Plant-based Protein Foods", 
                  "Poultry", "Quick Breads and Bread Products", "Ready-to-Eat Cereals", "Savory Snacks", "Seafood", 
                  "Snack/Meal Bars", "Sugars", "Sweet Bakery Products", "Sweetened Beverages", 
                  "Vegetables, excluding Potatoes", "White Potatoes", "Yogurt")

vif_results <- tibble(variable = character(), vif = numeric())

for (var in target_vars) {
  data_copy <- data
  
  data_copy[, 84] <- data_copy[[var]]
  colnames(data_copy)[84] <- "dependent_variable"
  
  other_vars <- setdiff(target_vars, var) %>% 
    map_chr(~ sprintf("`%s`", .x)) %>%
    paste(collapse = " + ")
  
  formula <- as.formula(paste(
    "dependent_variable", "~",
    other_vars, 
    "+ m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2",
    "+ Household_Income + Household_Size + Age_And_Presence_Of_Children",
    "+ Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education",
    "+ Ethnic + Marital_Status | year^Zone + month^Zone + household_code"
  ))
  
  tryCatch({
    model_aux <- feols(formula, 
                       data = data_copy, 
                       weights = ~Projection_Factor,               
                       vcov = ~fips_county, 
                       fixef.rm = "single")
    r2 <- r2(model_aux, "r2")
    vif <- 1 / (1 - r2)
    
    vif_results <- vif_results %>% add_row(variable = var, vif = vif)
  }, error = function(e) {
    message("Error calculating VIF for ", var, ": ", e$message)
    vif_results <<- vif_results %>% add_row(variable = var, vif = NA_real_)
  })
}

saveRDS(vif_results, file = "vif_results.RDS")

library(writexl)
write_xlsx(vif_results, path = "vif_results.xlsx")


##### 8.2 remove the top and bottom 5% outliers and using inflation #####
data <- readRDS("REVIEW food focused price totally.RDS")
county_food_price_month <- data

food_product_focused <- readRDS("REVIEW food product nutrition focused.RDS")
food_product_focused$upc <- as.numeric(food_product_focused$upc)

Category <- unique(food_product_focused$`WWEIA Category`)

for (category in Category) {
  top_5_percent <- quantile(data[data$`WWEIA Category` == category,]$price, probs = 0.95, na.rm = TRUE)
  
  bottom_5_percent <- quantile(data[data$`WWEIA Category` == category,]$price, probs = 0.05, na.rm = TRUE)
  
  county_food_price_month <- county_food_price_month %>%
    filter(!(price > top_5_percent & `WWEIA Category` == category)) %>%
    filter(!(price < bottom_5_percent & `WWEIA Category` == category))
}

# There may be so much of some products that a direct average would have a large impact on the average price of that food group
# average price per 100 grams of each product
test <- county_food_price_month %>%  
  group_by(fips_county, year, month, upc, `WWEIA Category`) %>%  
  summarise(m_price = mean(price, na.rm = TRUE)) %>%  
  ungroup()  

# average price per 100 grams of each category
test <- test %>%  
  group_by(fips_county, year, month, `WWEIA Category`) %>%  
  summarise(mc_price = mean(m_price, na.rm = TRUE)) %>%  
  ungroup()  

test <- test %>% 
  spread(key = `WWEIA Category`, value = mc_price, fill = NA)

for (var_name in Category) {
  if (!exists(var_name, test)) {
    test[[var_name]] <- NA
  }
}

saveRDS(test, file = "REVIEW food focused price monthly top-bottom 5% removed.RDS")

# regression data - remove the top and bottom 5% outliers
county_food_price_month <- readRDS("REVIEW food focused price monthly top-bottom 5% removed.RDS")

county_food_price_month <- county_food_price_month %>%
  complete(year, month, fips_county, fill = list(values = NA))

Category <- names(county_food_price_month)[!(names(county_food_price_month) %in% c("fips_county", "year", "month"))]

# replace missing using county-year and nation-month price
for (food in Category) {
  county_food_price_month <- county_food_price_month %>%  
    group_by(fips_county, year) %>%  
    mutate(!!paste0("county_average_", food) := mean(!!sym(food), na.rm = TRUE)) %>%
    ungroup()
}

for (food in Category) {
  county_food_price_month <- county_food_price_month %>%  
    group_by(year, month) %>%  
    mutate(!!paste0("nation_average_", food) := mean(!!sym(food), na.rm = TRUE)) %>%
    ungroup()
}

for (food in Category) {
  col_name <- paste0("county_average_", food)
  county_food_price_month <- county_food_price_month %>%  
    mutate(!!sym(food) := ifelse(is.na(!!sym(food)), !!sym(col_name), !!sym(food)))
}

for (food in Category) {
  col_name <- paste0("nation_average_", food)
  county_food_price_month <- county_food_price_month %>%  
    mutate(!!sym(food) := ifelse(is.na(!!sym(food)), !!sym(col_name), !!sym(food)))
}

county_food_price_month <- county_food_price_month %>%
  select(-starts_with("county_average_"), -starts_with("nation_average_")) %>%
  distinct()

data <- readRDS("test_nutrition_household_zone_month.RDS")

data <- inner_join(county_food_price_month, data, by = c("fips_county", "year", "month"), multiple = "all")
data <- data %>% filter(year != 2003 & year != 2020)

saveRDS(data, file = "REVIEW household food price top-bottom 5% removed.RDS")

# price regression
data <- readRDS("REVIEW household food price top-bottom 5% removed.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

cols_to_modify <- c("100% Juice", "Alcoholic Beverages", "Breads, Rolls, Tortillas", "Candy", 
                    "Cheese", "Coffee and Tea", "Condiments and Sauces", "Cooked Cereals", 
                    "Cooked Grains", "Crackers", "Cured Meats/Poultry", "Dairy Drinks and Substitutes", "Diet Beverages", 
                    "Eggs", "Fats and Oils", "Fruits", "Meats", "Milk", "Mixed Dishes - Soups", "Mixed Dishes – Bean/Vegetable-based", 
                    "Mixed Dishes – Grain-based", "Mixed Dishes – Meat, Poultry, Seafood", "Mixed Dishes – Mexican", "Mixed Dishes – Pizza", 
                    "Mixed Dishes – Sandwiches (single code)", "Other", "Other Desserts", "Plain Water", "Plant-based Protein Foods", 
                    "Poultry", "Quick Breads and Bread Products", "Ready-to-Eat Cereals", "Savory Snacks", "Seafood", 
                    "Snack/Meal Bars", "Sugars", "Sweet Bakery Products", "Sweetened Beverages",
                    "Vegetables, excluding Potatoes", "White Potatoes", "Yogurt")

# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(all_of(cols_to_modify), ~ . / CPI_map[as.character(year)]))

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$m_TEMP_range2 <- factor(data$m_TEMP_range2)
data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 +
                 `100% Juice` + `Alcoholic Beverages` + `Breads, Rolls, Tortillas` + `Candy` + 
                 `Cheese` + `Coffee and Tea` + `Condiments and Sauces` + `Cooked Cereals` + 
                 `Cooked Grains` + `Crackers` + `Cured Meats/Poultry` + `Dairy Drinks and Substitutes` + 
                 `Diet Beverages` + `Eggs` + `Fats and Oils` + `Fruits` + `Meats` + `Milk` + 
                 `Mixed Dishes - Soups` + `Mixed Dishes – Bean/Vegetable-based` + `Mixed Dishes – Grain-based` + 
                 `Mixed Dishes – Meat, Poultry, Seafood` + `Mixed Dishes – Mexican` + `Mixed Dishes – Pizza` + 
                 `Mixed Dishes – Sandwiches (single code)` + `Other` + `Other Desserts` + `Plain Water` + 
                 `Plant-based Protein Foods` + `Poultry` + `Quick Breads and Bread Products` + `Ready-to-Eat Cereals` + 
                 `Savory Snacks` + `Seafood` + `Snack/Meal Bars` + `Sugars` + `Sweet Bakery Products` + 
                 `Sweetened Beverages` + `Vegetables, excluding Potatoes` + `White Potatoes` + `Yogurt`+
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Zone + month^Zone + household_code, 
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

modelsummary(model, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-price model top-bottom 5%.docx")


##### 8.3 how the prices change with temperature - county level #####
# regression data - remove the top 5% outliers
county_food_price_month <- readRDS("REVIEW food focused price monthly top 5% removed.RDS")

county_food_price_month <- county_food_price_month %>%
  complete(year, month, fips_county, fill = list(values = NA))

Category <- names(county_food_price_month)[!(names(county_food_price_month) %in% c("fips_county", "year", "month"))]

# replace missing using county-year and nation-month price
for (food in Category) {
  county_food_price_month <- county_food_price_month %>%  
    group_by(fips_county, year) %>%  
    mutate(!!paste0("county_average_", food) := mean(!!sym(food), na.rm = TRUE)) %>%
    ungroup()
}

for (food in Category) {
  county_food_price_month <- county_food_price_month %>%  
    group_by(year, month) %>%  
    mutate(!!paste0("nation_average_", food) := mean(!!sym(food), na.rm = TRUE)) %>%
    ungroup()
}

for (food in Category) {
  col_name <- paste0("county_average_", food)
  county_food_price_month <- county_food_price_month %>%  
    mutate(!!sym(food) := ifelse(is.na(!!sym(food)), !!sym(col_name), !!sym(food)))
}

for (food in Category) {
  col_name <- paste0("nation_average_", food)
  county_food_price_month <- county_food_price_month %>%  
    mutate(!!sym(food) := ifelse(is.na(!!sym(food)), !!sym(col_name), !!sym(food)))
}

county_food_price_month <- county_food_price_month %>%
  select(-starts_with("county_average_"), -starts_with("nation_average_")) %>%
  distinct()

climate_county_month <- readRDS("climate county homescan month.RDS")

data <- inner_join(county_food_price_month, climate_county_month, by = c("fips_county", "year", "month"))
data <- data %>% filter(year != 2003 & year != 2020)

zone <- read_csv("climate_zones.csv")
zone$fips_county <- paste0(zone$`State FIPS`, zone$`County FIPS`)

data <- inner_join(data, zone, by = "fips_county")

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

cols_to_modify <- c("100% Juice", "Alcoholic Beverages", "Breads, Rolls, Tortillas", "Candy", 
                    "Cheese", "Coffee and Tea", "Condiments and Sauces", "Cooked Cereals", 
                    "Cooked Grains", "Crackers", "Cured Meats/Poultry", "Dairy Drinks and Substitutes", "Diet Beverages", 
                    "Eggs", "Fats and Oils", "Fruits", "Meats", "Milk", "Mixed Dishes - Soups", "Mixed Dishes – Bean/Vegetable-based", 
                    "Mixed Dishes – Grain-based", "Mixed Dishes – Meat, Poultry, Seafood", "Mixed Dishes – Mexican", "Mixed Dishes – Pizza", 
                    "Mixed Dishes – Sandwiches (single code)", "Other", "Other Desserts", "Plain Water", "Plant-based Protein Foods", 
                    "Poultry", "Quick Breads and Bread Products", "Ready-to-Eat Cereals", "Savory Snacks", "Seafood", 
                    "Snack/Meal Bars", "Sugars", "Sweet Bakery Products", "Sweetened Beverages",
                    "Vegetables, excluding Potatoes", "White Potatoes", "Yogurt")

# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(all_of(cols_to_modify), ~ . / CPI_map[as.character(year)]))

data$Zone <- factor(data$Zone)

saveRDS(data, file = "REVIEW county food price top 5% removed.RDS")

# price regression with climate data
data <- readRDS("REVIEW county food price top 5% removed.RDS")

Category <- c("Sweetened Beverages", "Other Desserts", 
              "Sugars", "Candy", "Sweet Bakery Products", "Quick Breads and Bread Products", "Breads, Rolls, Tortillas", "Fats and Oils")

models <- list()
myDataframe <- NULL
for (i in c(41, 30, 39, 7, 40, 34, 6, 18)) {
  data_copy <- data 
  
  data_copy[, 64] <- log(data[, i])
  colnames(data_copy)[64] <- "dependent_variable"
  
  model <- feols(dependent_variable ~ 
                   m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 | 
                   year^Zone + month^Zone,  
                 data = data_copy, 
                 vcov = ~fips_county, 
                 fixef.rm = "single")
  
  coef_matrix <- as.matrix(coef(model))  
  colnames(coef_matrix) <- paste0(colnames(data_copy)[i], "_c")
  confint_matrix <- as.matrix(confint(model, levle = 0.95))
  colnames(confint_matrix) <- c(paste0(colnames(data_copy)[i], "_ll"), paste0(colnames(data_copy)[i], "_ul"))
  
  model_information <- as.data.frame(cbind(coef_matrix, confint_matrix))
  
  new_row <- data.frame(matrix(ncol = ncol(model_information), nrow = 1))
  colnames(new_row) <- colnames(model_information)
  new_row[1, 1] <- 0
  new_row[1, 2:3] <- NA
  
  model_information <- rbind(new_row, model_information)
  rownames(model_information)[1] <- "m_TEMP_range2"
  
  model_information <- model_information %>% mutate(id = row_number())
  model_information <- model_information %>% filter(id <= 19)
  
  model_information_long <- pivot_longer(model_information,
                                         cols = starts_with(colnames(data_copy)[i]),
                                         names_to = "variable",
                                         values_to = "est")
  
  model_information_long <- model_information_long %>%
    mutate(coef = case_when(
      grepl("_ll$", variable) ~ "ll",
      grepl("_ul$", variable) ~ "ul",
      TRUE ~ "c"
    ))
  
  model_information_long$variable <- colnames(data_copy)[i]
  
  myDataframe <- rbind(myDataframe, model_information_long) 
  
  if (i %in% c(41, 30, 39, 7, 40, 34, 6, 18)) {
    model_name <- switch(as.character(i),
                         "41" = "Sweetened Beverages", 
                         "30" = "Other Desserts",
                         "39" = "Sugars", 
                         "7" = "Candy", 
                         "40" = "Sweet Bakery Products", 
                         "34" = "Quick Breads and Bread Products", 
                         "6" = "Breads, Rolls, Tortillas", 
                         "18" = "Fats and Oils")
    models[[model_name]] <- model
  }
}

saveRDS(myDataframe, "REVIEW coefficient R price change with climate.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-price change with climate.docx")

##### 8.4 how the prices change with temperature - household and income group level ####
Category <- c("Sweetened Beverages", "Other Desserts", 
              "Sugars", "Candy", "Sweet Bakery Products", "Quick Breads and Bread Products", "Breads, Rolls, Tortillas", "Fats and Oils")

data <- readRDS("REVIEW food focused price totally.RDS")
data <- data[data$`WWEIA Category` %in% Category, ]

county_food_price_month <- data

for (category in Category) {
  top_5_percent <- quantile(data[data$`WWEIA Category` == category,]$price, probs = 0.95, na.rm = TRUE)
  
  county_food_price_month <- county_food_price_month %>%
    filter(!(price > top_5_percent & `WWEIA Category` == category))
}

# There may be so much of some products that a direct average would have a large impact on the average price of that food group
# average price per 100 grams of each product
test <- county_food_price_month %>%  
  group_by(household_code, year, month, upc, `WWEIA Category`) %>%  
  summarise(m_price = mean(price, na.rm = TRUE)) %>%  
  ungroup()  

# average price per 100 grams of each category
test <- test %>%  
  group_by(household_code, year, month, `WWEIA Category`) %>%  
  summarise(mc_price = mean(m_price, na.rm = TRUE)) %>%  
  ungroup()  

test <- test %>% 
  spread(key = `WWEIA Category`, value = mc_price, fill = NA)

for (var_name in Category) {
  if (!exists(var_name, test)) {
    test[[var_name]] <- NA
  }
}

data <- readRDS("test_nutrition_household_zone_month.RDS")
data <- inner_join(test, data, by = c("household_code", "year", "month"))

data <- data %>% filter(year != 2003 & year != 2020)

saveRDS(data, file = "REVIEW household level food price top 5% removed.RDS")

# price regression with climate data
data <- readRDS("REVIEW household level food price top 5% removed.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

cols_to_modify <- c("Sweetened Beverages", "Other Desserts", 
                    "Sugars", "Candy", "Sweet Bakery Products", "Quick Breads and Bread Products", "Breads, Rolls, Tortillas", "Fats and Oils")

# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(all_of(cols_to_modify), ~ . / CPI_map[as.character(year)]))

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income

data$Income_group <- ifelse(data$Household_Income <= 24999, "Very Low",
                            ifelse(25000 <= data$Household_Income & data$Household_Income <= 49999, "Low", 
                                   ifelse(50000 <= data$Household_Income & data$Household_Income <= 69999, "Medium", 
                                          ifelse(70000 <= data$Household_Income & data$Household_Income <= 99999, "High", "Very High"))))
data$Income_group <- factor(data$Income_group)
data$Income_group <- relevel(data$Income_group, ref = "Very Low")

models <- list()
myDataframe <- NULL
for (m in 4:11) {
  data_copy <- data 
  
  data_copy[, 55] <- log(data[, m]) 
  colnames(data_copy)[55] <- "dependent_variable"
  
  model <- feols(dependent_variable ~
                   m_TEMP_county_0_12 * Income_group + m_TEMP_county_12_30 * Income_group  + m_TEMP_county_30_up + 
                   m_WDSP_county * Income_group + m_PRCP_county * Income_group + m_RH_county * Income_group + m_RH_county2 * Income_group +
                   Household_Size + Age_And_Presence_Of_Children +
                   Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                   year^Zone + month^Zone + household_code,
                 data = data_copy,
                 weights = ~Projection_Factor,
                 vcov = ~fips_county,
                 fixef.rm = "single")
  
  Income <- as.matrix(coef(model))
  Income <- as.matrix(Income[grep("m_TEMP_county", rownames(Income)), ])
  
  Income <- as.data.frame(cbind(Income, rownames(Income)))
  rownames(Income) <- NULL
  names(Income) <- c("c", "rownames")
  
  Income$c <- as.numeric(Income$c)
  
  Income$group <- "Very Low"
  Income$group[grep("Income_groupLow", Income$rownames)] <- "Low"
  Income$group[grep("Income_groupMedium", Income$rownames)] <- "Medium"
  Income$group[grep("Income_groupHigh", Income$rownames)] <- "High"
  Income$group[grep("Income_groupVery High", Income$rownames)] <- "Very High"
  
  Income$range <- "0-12"
  Income$range[grep("m_TEMP_county_12_30", Income$rownames)] <- "12-30"
  Income$range[grep("m_TEMP_county_30_up", Income$rownames)] <- "30+"
  
  Income <- Income[, -2]
  
  Income_vcov <- as.matrix(vcov(model))
  Income_vcov <- as.matrix(Income_vcov[grepl("m_TEMP_county", rownames(Income_vcov)), ])
  Income_vcov <- as.matrix(Income_vcov[ , grepl("m_TEMP_county", colnames(Income_vcov))])
  
  Income$ll <- 0
  Income$ul <- 0
  
  for (tas in unique(Income$range)){
    j <- as.numeric(rownames(Income[Income$group == "Very Low" & Income$range == tas, ]))
    
    for (race in unique(Income$group)){
      
      if (race != "Very Low") {
        Income[Income$group == race & Income$range == tas, ]$c = Income[Income$group == race & Income$range == tas, ]$c + Income[Income$group == "Very Low" & Income$range == tas, ]$c
        
        i <- as.numeric(rownames(Income[Income$group == race & Income$range == tas, ]))
        
        Income[Income$group == race & Income$range == tas, ]$ll = Income[Income$group == race & Income$range == tas, ]$c - 1.96 * sqrt(Income_vcov[i, i] + Income_vcov[j, j] + 2*Income_vcov[i, j])
        Income[Income$group == race & Income$range == tas, ]$ul = Income[Income$group == race & Income$range == tas, ]$c + 1.96 * sqrt(Income_vcov[i, i] + Income_vcov[j, j] + 2*Income_vcov[i, j])
      }
      
      else{
        Income[Income$group == race & Income$range == tas, ]$ll = Income[Income$group == race & Income$range == tas, ]$c - 1.96 * sqrt(Income_vcov[j, j])
        Income[Income$group == race & Income$range == tas, ]$ul = Income[Income$group == race & Income$range == tas, ]$c + 1.96 * sqrt(Income_vcov[j, j])
      }
    }
  }
  
  for (race in unique(Income$group)){
    if (race != "Very Low"  ) {
      Income <- rbind(Income, as.data.frame(list(c = Income[Income$group == "Very Low" & Income$range == "30+", ]$c, 
                                                 group = race, range = "30+", 
                                                 ll = Income[Income$group == "Very Low" & Income$range == "30+", ]$ll, 
                                                 ul = Income[Income$group == "Very Low" & Income$range == "30+", ]$ul)))
    }
  }
  
  colnames(Income) <- c(paste0(colnames(data[, m]), "_c"), "group", "range", 
                        paste0(colnames(data[, m]), "_ll"), paste0(colnames(data[, m]), "_ul"))
  
  model_information_long <- pivot_longer(Income,
                                         cols = starts_with(colnames(data[, m])),
                                         names_to = "variable",
                                         values_to = "est")
  
  model_information_long <- model_information_long %>%
    mutate(coef = case_when(
      grepl("_ll$", variable) ~ "ll",
      grepl("_ul$", variable) ~ "ul",
      TRUE ~ "c"
    ))
  
  model_information_long$variable <- colnames(data[, m])
  
  myDataframe <- rbind(myDataframe, model_information_long) 
  
  model_name <- switch(as.character(m),
                       "4" = "Breads, Rolls, Tortillas",
                       "5" = "Candy",
                       "6" = "Fats and Oils",
                       "7" = "Other Desserts",
                       "8" = "Quick Breads and Bread Products",
                       "9" = "Sugars",
                       "10" = "Sweet Bakery Products",
                       "11" = "Sweetened Beverages")
  models[[model_name]] <- model
  
}

saveRDS(myDataframe, "REVIEW coefficient R price change with climate household and income group level.RDS")

modelsummary(models, stars = c('***' = .01, '**' = .05, '*' = .1),
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-price change with climate household and income group level.docx")


#### 9. HDD and CDD regression ####
data <- readRDS("test_nutrition_household_zone_month.RDS")

data$Ethnic <- ifelse(data$Hispanic_Origin == 1, 5, data$Race)

data$m_HDD65_10 <- data$m_HDD65 / 10
data$m_CDD65_10 <- data$m_CDD65 / 10

data <- data[data$Male_Head_Education != 0, ]
data <- data[data$Female_Head_Education != 0, ]

data <- data[data$Male_Head_Age != 0, ]
data <- data[data$Female_Head_Age != 0, ]

data$Zone <- ifelse(data$`BA Climate Zone` == "Hot-Humid", 1, 
                    ifelse(data$`BA Climate Zone` == "Hot-Dry" | data$`BA Climate Zone` == "Mixed-Dry", 2,
                           ifelse(data$`BA Climate Zone` == "Mixed-Humid", 3,
                                  ifelse(data$`BA Climate Zone` == "Marine", 4, 5))))

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

# model1 - "mpc_ADD_SUGARS"
model1 <- feols(mpc_ADD_SUGARS ~ 
                  m_HDD65_10 + m_CDD65_10 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                  Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                  Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code, 
                data = data, 
                weights = ~Projection_Factor, 
                vcov = ~fips_county, 
                fixef.rm = "single")
AS_intake <- coef(model1)
AS_intake <- as.matrix(AS_intake)
AS_intake_confint <- confint(model1, level = 0.95)
AS_intake_confint <- as.matrix(AS_intake_confint)

model2 <- feols(mpc_ADD_SUGARS_convert ~ 
                  m_HDD65_10 + m_CDD65_10 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                  Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + 
                  Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code, 
                data = data, 
                weights = ~Projection_Factor, 
                vcov = ~fips_county, 
                fixef.rm = "single")
AS_convert <- coef(model2)
AS_convert <- as.matrix(AS_convert)
AS_convert_confint <- confint(model2, level = 0.95)
AS_convert_confint <- as.matrix(AS_convert_confint)

models <- list("mpc_ADD_SUGARS_convert" = model2, "mpc_ADD_SUGARS" = model1)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-HDD&CDD model.docx")

#### 10. interactions of education with month-of-the-year and year fixed effects ####
# male education
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

data$m_TEMP_range2 <- factor(data$m_TEMP_range2)
data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Male_Head_Education <- factor(data$Male_Head_Education)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Female_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Male_Head_Education + month^Male_Head_Education + household_code,  
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

models <- list("mpc_ADD_SUGARS_convert" = model)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-main model male education interaction.docx")

# female education
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

data$m_TEMP_range2 <- factor(data$m_TEMP_range2)
data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Female_Head_Education <- factor(data$Female_Head_Education)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~ 
                 m_TEMP_range2 + m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 + 
                 Household_Income + Household_Size + Age_And_Presence_Of_Children + 
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + 
                 Ethnic + Marital_Status | 
                 year^Female_Head_Education + month^Female_Head_Education + household_code,  
               data = data, 
               weights = ~Projection_Factor, 
               vcov = ~fips_county, 
               fixef.rm = "single")

models <- list("mpc_ADD_SUGARS_convert" = model)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-main model female education interaction.docx")


#### 11. Projection ####
# extract CMIP6 data
library(GSODR)

library(CFtime)
library(ncdf4)
library(tidyverse)
library(dplyr)
library(data.table)

library(here)
library(sf)
library(dplyr)
library(viridis)
library(USAboundaries)
library(rnaturalearth)
library(ggrepel)
library(cowplot)
library(readr)
library(zoo)

# GSOD 2015-2023
setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption")

GSOD_data <- NULL
for (i in 2015:2023) {
  data <- get_GSOD(years = i, country = "United States")
  GSOD_data <- rbind(GSOD_data, data)
}

saveRDS(GSOD_data, "/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/GSOD climate 2015-2023.RDS")

GSOD_data <- readRDS("GSOD climate 2015-2023.RDS")

county_data <- readRDS("climate station to county homescan 100km.RDS")

# merge by STNID
climate_county_data <- inner_join(GSOD_data, county_data, by = "STNID", multiple = "all")

variables <- c("TEMP", "WDSP", "PRCP", "RH")

for (var in variables) {
  climate_county_data <- climate_county_data %>%
    group_by(YEAR, MONTH, fips_county) %>%
    mutate(!!paste0(var, "_county") := mean(!!sym(var), na.rm = TRUE)) %>%
    ungroup()
}

climate_county_data <- climate_county_data %>%
  select(YEAR, MONTH, fips_county, ends_with("_county")) %>%
  distinct()

climate_county_data$fips_county <- formatC(climate_county_data$fips_county, width = 5, format = "d", flag = "0")

names(climate_county_data) <- c("year", "month", "fips_county", "TEMP_county", "WDSP_county", "PRCP_county", "RH_county")

saveRDS(climate_county_data, "GSOD county data.RDS")

# SSP5-8.5
# CMIP6 SSP5-8.5 2015-2023
setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585")

variable <- c("hurs", "pr", "sfcWind", "tas")

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
           "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
           "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
           "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
           "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

for (var in variable) {
  for (mod in model) {
    file_name <- paste0(var, "_Amon_", mod, "_ssp585_r1i1p1f1")
    
    chw <- list.files(pattern = file_name, full.names = FALSE)
    
    var_mod <- data.frame()
    
    for (n in 1:length(chw)) {
      ncfname <- chw[n]
      dname <- var
      
      ncin <- nc_open(ncfname)
      
      # get longitude and latitude
      lon <- ncvar_get(ncin, "lon")
      nlon <- dim(lon)
      lat <- ncvar_get(ncin, "lat")
      nlat <- dim(lat)
      
      # get time
      time <- ncvar_get(ncin, "time")
      tunits <- ncatt_get(ncin, "time", "units")
      nt <- dim(time)
      
      # get var
      var_array <- ncvar_get(ncin, dname)
      fillvalue <- ncatt_get(ncin, dname, "_FillValue")
      
      # decode time
      cf <- CFtime(ncin$dim$time$units, ncin$dim$time$calendar, ncin$dim$time$vals)
      timestamps <- CFtimestamp(cf)
      
      # replace netCDF fill values with NA's
      var_array[var_array==fillvalue$value] <- NA
      
      # reshape the array into vector
      var_vec_long <- as.vector(var_array)
      var_mat <- matrix(var_vec_long, nrow=nlon*nlat, ncol=nt)
      
      # create dataframe
      # matrix (nlon*nlat rows by 2 cols) of lons and lats
      lonlat <- as.matrix(expand.grid(lon,lat))
      var_df <- data.frame(cbind(lonlat,var_mat))
      
      names(var_df) <- c("longitude", "latitude", timestamps)
      
      var_df_long <- var_df %>%
        pivot_longer(cols = -c("longitude", "latitude"), 
                     names_to = "time", 
                     values_to = var)
      
      if (var == "pr") {
        var_df_long$pr <- var_df_long$pr * 86400
      }
      
      if (var == "tas") {
        var_df_long$tas <- var_df_long$tas - 273.15
      }
      
      var_df_long$year <- substring(var_df_long$time, 1, 4)  
      var_df_long$month <- substring(var_df_long$time, 6, 7) 
      
      var_df_long <- var_df_long[, !names(var_df_long) %in% "time"]
      
      var_mod  <- rbind(var_mod, var_df_long[2015 <= var_df_long$year & var_df_long$year <= 2023,])
    }
    
    output_dir <- paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod)
    dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
    
    saveRDS(var_mod, paste0(output_dir,"/", var, "2015-2023.RDS"))
  }
}

all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)

# create 100km buffer of county
buffered_counties <- st_buffer(county_polygons, dist = 100000)

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
           "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
           "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
           "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
           "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

variable <- c("hurs", "pr", "sfcWind", "tas")

for (mod in model) {
  mod_var <- NULL 
  
  for (var in variable){
    
    data <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "/", var, "2015-2023.RDS"))
    
    data$longitude <-  ifelse(data$longitude > 180, data$longitude - 360, data$longitude)
    
    station_points <- as.data.frame(data) %>% 
      st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE) 
    
    # spatial join station and buffer
    station_county <- st_join(station_points, buffered_counties, left = F)
    station_county$fips_county <- paste(station_county$statefp, station_county$countyfp, sep = "")
    
    station_county <- as.data.frame(station_county)
    
    unique_data <- station_county[, c("year", "month", "fips_county", var)]
    
    unique_data <- unique_data %>%  
      group_by(year, month, fips_county) %>%  
      summarise(!!var := mean(!!sym(var), na.rm = TRUE)) %>%  
      ungroup()
    
    if (is.null(mod_var)) {
      mod_var <- unique_data
    } else {
      mod_var <- inner_join(mod_var, unique_data, by = c("year", "month", "fips_county"))
    }
  }
  saveRDS(mod_var, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "2015-2023.RDS"))
}

# CMIP6 SSP5-8.5 2015-2100
# GSOD 2015-2023
setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585")

GSOD <- readRDS("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/GSOD county data.RDS")
names(GSOD) <- c("year", "month", "fips_county", "tas_GSOD", "sfcWind_GSOD", "pr_GSOD", "hurs_GSOD")

variable <- c("hurs", "pr", "sfcWind", "tas")

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
           "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
           "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
           "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
           "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

for (var in variable) {
  for (mod in model) {
    # CMIP6 2015-2023
    CMIP6 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "2015-2023.RDS"))
    
    CMIP6$year <- as.numeric(CMIP6$year)
    CMIP6$month <- as.numeric(CMIP6$month)
    
    GSOD_CMIP6 <- inner_join(GSOD, CMIP6, by = c("year", "month", "fips_county"))
    
    for (var1 in variable) {
      GSOD_CMIP6 <- GSOD_CMIP6 %>%
        group_by(month) %>%
        mutate(!!paste0(var1, "_GSOD") := mean(!!sym(paste0(var, "_GSOD")), na.rm = TRUE)) %>%
        mutate(!!paste0(var1, "_CMIP6") := mean(!!sym(var1), na.rm = TRUE)) %>%  
        ungroup() 
    }
    
    GSOD_CMIP6 <- GSOD_CMIP6 %>%
      select(month, hurs_GSOD, pr_GSOD, sfcWind_GSOD, tas_GSOD, hurs_CMIP6, pr_CMIP6, sfcWind_CMIP6, tas_CMIP6) %>%
      distinct()
    
    # CMIP6 2015-2100
    file_name <- paste0(var, "_Amon_", mod, "_ssp585_r1i1p1f1")
    
    chw <- list.files(pattern = file_name, full.names = FALSE)
    
    var_mod <- data.frame()
    
    for (n in 1:length(chw)) {
      ncfname <- chw[n]
      dname <- var
      
      ncin <- nc_open(ncfname)
      
      # get longitude and latitude
      lon <- ncvar_get(ncin, "lon")
      nlon <- dim(lon)
      lat <- ncvar_get(ncin, "lat")
      nlat <- dim(lat)
      
      # get time
      time <- ncvar_get(ncin, "time")
      tunits <- ncatt_get(ncin, "time", "units")
      nt <- dim(time)
      
      # get var
      var_array <- ncvar_get(ncin, dname)
      fillvalue <- ncatt_get(ncin, dname, "_FillValue")
      
      # decode time
      cf <- CFtime(ncin$dim$time$units, ncin$dim$time$calendar, ncin$dim$time$vals)
      timestamps <- CFtimestamp(cf)
      
      # replace netCDF fill values with NA's
      var_array[var_array==fillvalue$value] <- NA
      
      # reshape the array into vector
      var_vec_long <- as.vector(var_array)
      var_mat <- matrix(var_vec_long, nrow=nlon*nlat, ncol=nt)
      
      # create dataframe
      # matrix (nlon*nlat rows by 2 cols) of lons and lats
      lonlat <- as.matrix(expand.grid(lon,lat))
      var_df <- data.frame(cbind(lonlat,var_mat))
      
      names(var_df) <- c("longitude", "latitude", timestamps)
      
      var_df_long <- var_df %>%
        pivot_longer(cols = -c("longitude", "latitude"), 
                     names_to = "time", 
                     values_to = var)
      
      if (var == "pr") {
        var_df_long$pr <- var_df_long$pr * 86400
      }
      
      if (var == "tas") {
        var_df_long$tas <- var_df_long$tas - 273.15
      }
      
      var_df_long$year <- as.numeric(substring(var_df_long$time, 1, 4)) 
      var_df_long$month <- as.numeric(substring(var_df_long$time, 6, 7)) 
      
      var_df_long <- var_df_long[, !names(var_df_long) %in% "time"]
      
      # Data pretreatment
      setDT(var_df_long)
      setDT(GSOD_CMIP6)
      
      CMIP6_col <- paste0(var, "_CMIP6")
      GSOD_col <- paste0(var, "_GSOD")
      
      adjusted_var <- paste0("adjusted_", var)
      
      var_df_long <- var_df_long[GSOD_CMIP6, on = "month", (CMIP6_col) := get(paste0(var, "_CMIP6"))]
      var_df_long <- var_df_long[GSOD_CMIP6, on = "month", (GSOD_col) := get(paste0(var, "_GSOD"))]
      
      var_df_long[, (adjusted_var) := get(var) - get(CMIP6_col) + get(GSOD_col)]
      
      var_df_long[, c(CMIP6_col, GSOD_col) := NULL]
      
      var_mod  <- rbind(var_mod, var_df_long)
    }
    
    output_dir <- paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod)
    dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
    
    saveRDS(var_mod, paste0(output_dir,"/", var, ".RDS"))
  }
}

# spatial join 
# get boundary with high definition of all states and counties
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)

# create 100km buffer of county
buffered_counties <- st_buffer(county_polygons, dist = 100000)

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
           "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
           "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
           "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
           "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

variable <- c("hurs", "pr", "sfcWind", "tas")

for (mod in model) {
  mod_var <- NULL 
  
  for (var in variable){
    data <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "/", var, ".RDS"))
    
    data$longitude <-  ifelse(data$longitude > 180, data$longitude - 360, data$longitude)
    
    station_points <- as.data.frame(data) %>% 
      st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE) 
    
    # spatial join station and buffer
    station_county <- st_join(station_points, buffered_counties, left = F)
    station_county$fips_county <- paste(station_county$statefp, station_county$countyfp, sep = "")
    
    station_county <- as.data.frame(station_county)
    
    unique_data <- station_county[, c("year", "month", "fips_county", paste0("adjusted_", var))]
    
    unique_data <- unique_data %>%  
      group_by(year, month, fips_county) %>%  
      summarise(!!paste0("adjusted_", var) := mean(!!sym(paste0("adjusted_", var)), na.rm = TRUE)) %>%  
      ungroup()
    
    if (is.null(mod_var)) {
      mod_var <- unique_data
    } else {
      mod_var <- inner_join(mod_var, unique_data, by = c("year", "month", "fips_county"))
    }
  }
  
  saveRDS(mod_var, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, ".RDS"))
}

# Moving average 
CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
                 "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
                 "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
                 "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
                 "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")

for (mod in CMIP6_model){
  mod_result <- data.frame()
  
  mod8.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, ".RDS"))
  
  mod8.5_wide <- pivot_wider(mod8.5, 
                             id_cols = c("year", "month"), 
                             names_from = "fips_county", 
                             values_from = c("adjusted_hurs", "adjusted_pr", "adjusted_tas", "adjusted_sfcWind"))
  
  for (i in 1:12) {
    meteor_data <- mod8.5_wide[mod8.5_wide$month == i, ]
    
    cma_result <- as.data.frame(lapply(meteor_data[, 3:ncol(meteor_data)], 
                                       function(x) rollmean(x, 9, fill = NA, align = "center")))
    
    meteor_data[, 3:ncol(meteor_data)] <- cma_result
    
    meteor_data <- meteor_data[meteor_data$year >= 2019 & meteor_data$year <= 2096, ]
    
    mod_result <- rbind(mod_result, meteor_data)
  }
  
  long_data <- pivot_longer(mod_result,
                            cols = -c(year, month), 
                            names_to = "fips_county")
  
  long_data$variable <- gsub("^(adjusted_[a-zA-Z]+)_.*", "\\1", long_data$fips_county)
  long_data$fips_county <- gsub("^adjusted_[a-zA-Z]+_(.*)", "\\1", long_data$fips_county)
  
  long_data <- pivot_wider(long_data, 
                           id_cols = c("year", "month", "fips_county"), 
                           names_from = "variable", 
                           values_from = "value")
  
  saveRDS(long_data, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "_MA.RDS"))
}

# SSP2-4.5
# CMIP6 SSP2-4.5 2015-2023
setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245")

variable <- c("hurs", "pr", "sfcWind", "tas")

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
           "CanESM5", "CanESM5-1", 
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
           "FGOALS-f3-L", "FGOALS-g3",  
           "IPSL-CM6A-LR",
           "KACE-1-0-G",
           "MIROC6", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

for (var in variable) {
  for (mod in model) {
    file_name <- paste0(var, "_Amon_", mod, "_ssp245_r1i1p1f1")
    
    chw <- list.files(pattern = file_name, full.names = FALSE)
    
    var_mod <- data.frame()
    
    for (n in 1:length(chw)) {
      ncfname <- chw[n]
      dname <- var
      
      ncin <- nc_open(ncfname)
      
      # get longitude and latitude
      lon <- ncvar_get(ncin, "lon")
      nlon <- dim(lon)
      lat <- ncvar_get(ncin, "lat")
      nlat <- dim(lat)
      
      # get time
      time <- ncvar_get(ncin, "time")
      tunits <- ncatt_get(ncin, "time", "units")
      nt <- dim(time)
      
      # get var
      var_array <- ncvar_get(ncin, dname)
      fillvalue <- ncatt_get(ncin, dname, "_FillValue")
      
      # decode time
      cf <- CFtime(ncin$dim$time$units, ncin$dim$time$calendar, ncin$dim$time$vals)
      timestamps <- CFtimestamp(cf)
      
      # replace netCDF fill values with NA's
      var_array[var_array==fillvalue$value] <- NA
      
      # reshape the array into vector
      var_vec_long <- as.vector(var_array)
      var_mat <- matrix(var_vec_long, nrow=nlon*nlat, ncol=nt)
      
      # create dataframe
      # matrix (nlon*nlat rows by 2 cols) of lons and lats
      lonlat <- as.matrix(expand.grid(lon,lat))
      var_df <- data.frame(cbind(lonlat,var_mat))
      
      names(var_df) <- c("longitude", "latitude", timestamps)
      
      var_df_long <- var_df %>%
        pivot_longer(cols = -c("longitude", "latitude"), 
                     names_to = "time", 
                     values_to = var)
      
      if (var == "pr") {
        var_df_long$pr <- var_df_long$pr * 86400
      }
      
      if (var == "tas") {
        var_df_long$tas <- var_df_long$tas - 273.15
      }
      
      var_df_long$year <- substring(var_df_long$time, 1, 4)  
      var_df_long$month <- substring(var_df_long$time, 6, 7) 
      
      var_df_long <- var_df_long[, !names(var_df_long) %in% "time"]
      
      var_mod  <- rbind(var_mod, var_df_long[2015 <= var_df_long$year & var_df_long$year <= 2023,])
    }
    
    output_dir <- paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod)
    dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
    
    saveRDS(var_mod, paste0(output_dir,"/", var, "2015-2023.RDS"))
  }
}

all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)

# create 100km buffer of county
buffered_counties <- st_buffer(county_polygons, dist = 100000)

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
           "CanESM5", "CanESM5-1", 
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
           "FGOALS-f3-L", "FGOALS-g3",  
           "IPSL-CM6A-LR",
           "KACE-1-0-G",
           "MIROC6", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

variable <- c("hurs", "pr", "sfcWind", "tas")

for (mod in model) {
  mod_var <- NULL 
  
  for (var in variable){
    
    data <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "/", var, "2015-2023.RDS"))
    
    data$longitude <-  ifelse(data$longitude > 180, data$longitude - 360, data$longitude)
    
    station_points <- as.data.frame(data) %>% 
      st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE) 
    
    # spatial join station and buffer
    station_county <- st_join(station_points, buffered_counties, left = F)
    station_county$fips_county <- paste(station_county$statefp, station_county$countyfp, sep = "")
    
    station_county <- as.data.frame(station_county)
    
    unique_data <- station_county[, c("year", "month", "fips_county", var)]
    
    unique_data <- unique_data %>%  
      group_by(year, month, fips_county) %>%  
      summarise(!!var := mean(!!sym(var), na.rm = TRUE)) %>%  
      ungroup()
    
    if (is.null(mod_var)) {
      mod_var <- unique_data
    } else {
      mod_var <- inner_join(mod_var, unique_data, by = c("year", "month", "fips_county"))
    }
  }
  saveRDS(mod_var, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "2015-2023.RDS"))
}

# CMIP6 SSP2-4.5 2015-2100
# GSOD 2015-2023
setwd("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245")

GSOD <- readRDS("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/GSOD county data.RDS")
names(GSOD) <- c("year", "month", "fips_county", "tas_GSOD", "sfcWind_GSOD", "pr_GSOD", "hurs_GSOD")

variable <- c("hurs", "pr", "sfcWind", "tas")

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
           "CanESM5", "CanESM5-1", 
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
           "FGOALS-f3-L", "FGOALS-g3",  
           "IPSL-CM6A-LR",
           "KACE-1-0-G",
           "MIROC6", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

for (var in variable) {
  for (mod in model) {
    # CMIP6 2015-2023
    CMIP6 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "2015-2023.RDS"))
    
    CMIP6$year <- as.numeric(CMIP6$year)
    CMIP6$month <- as.numeric(CMIP6$month)
    
    GSOD_CMIP6 <- inner_join(GSOD, CMIP6, by = c("year", "month", "fips_county"))
    
    for (var1 in variable) {
      GSOD_CMIP6 <- GSOD_CMIP6 %>%
        group_by(month) %>%
        mutate(!!paste0(var1, "_GSOD") := mean(!!sym(paste0(var, "_GSOD")), na.rm = TRUE)) %>%
        mutate(!!paste0(var1, "_CMIP6") := mean(!!sym(var1), na.rm = TRUE)) %>%  
        ungroup() 
    }
    
    GSOD_CMIP6 <- GSOD_CMIP6 %>%
      select(month, hurs_GSOD, pr_GSOD, sfcWind_GSOD, tas_GSOD, hurs_CMIP6, pr_CMIP6, sfcWind_CMIP6, tas_CMIP6) %>%
      distinct()
    
    # CMIP6 2015-2100
    file_name <- paste0(var, "_Amon_", mod, "_ssp245_r1i1p1f1")
    
    chw <- list.files(pattern = file_name, full.names = FALSE)
    
    var_mod <- data.frame()
    
    for (n in 1:length(chw)) {
      ncfname <- chw[n]
      dname <- var
      
      ncin <- nc_open(ncfname)
      
      # get longitude and latitude
      lon <- ncvar_get(ncin, "lon")
      nlon <- dim(lon)
      lat <- ncvar_get(ncin, "lat")
      nlat <- dim(lat)
      
      # get time
      time <- ncvar_get(ncin, "time")
      tunits <- ncatt_get(ncin, "time", "units")
      nt <- dim(time)
      
      # get var
      var_array <- ncvar_get(ncin, dname)
      fillvalue <- ncatt_get(ncin, dname, "_FillValue")
      
      # decode time
      cf <- CFtime(ncin$dim$time$units, ncin$dim$time$calendar, ncin$dim$time$vals)
      timestamps <- CFtimestamp(cf)
      
      # replace netCDF fill values with NA's
      var_array[var_array==fillvalue$value] <- NA
      
      # reshape the array into vector
      var_vec_long <- as.vector(var_array)
      var_mat <- matrix(var_vec_long, nrow=nlon*nlat, ncol=nt)
      
      # create dataframe
      # matrix (nlon*nlat rows by 2 cols) of lons and lats
      lonlat <- as.matrix(expand.grid(lon,lat))
      var_df <- data.frame(cbind(lonlat,var_mat))
      
      names(var_df) <- c("longitude", "latitude", timestamps)
      
      var_df_long <- var_df %>%
        pivot_longer(cols = -c("longitude", "latitude"), 
                     names_to = "time", 
                     values_to = var)
      
      if (var == "pr") {
        var_df_long$pr <- var_df_long$pr * 86400
      }
      
      if (var == "tas") {
        var_df_long$tas <- var_df_long$tas - 273.15
      }
      
      var_df_long$year <- as.numeric(substring(var_df_long$time, 1, 4)) 
      var_df_long$month <- as.numeric(substring(var_df_long$time, 6, 7)) 
      
      var_df_long <- var_df_long[, !names(var_df_long) %in% "time"]
      
      # Data pretreatment
      setDT(var_df_long)
      setDT(GSOD_CMIP6)
      
      CMIP6_col <- paste0(var, "_CMIP6")
      GSOD_col <- paste0(var, "_GSOD")
      
      adjusted_var <- paste0("adjusted_", var)
      
      var_df_long <- var_df_long[GSOD_CMIP6, on = "month", (CMIP6_col) := get(paste0(var, "_CMIP6"))]
      var_df_long <- var_df_long[GSOD_CMIP6, on = "month", (GSOD_col) := get(paste0(var, "_GSOD"))]
      
      var_df_long[, (adjusted_var) := get(var) - get(CMIP6_col) + get(GSOD_col)]
      
      var_df_long[, c(CMIP6_col, GSOD_col) := NULL]
      
      var_mod  <- rbind(var_mod, var_df_long)
    }
    
    output_dir <- paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod)
    dir.create(output_dir, recursive = TRUE, showWarnings = TRUE)
    
    saveRDS(var_mod, paste0(output_dir,"/", var, ".RDS"))
  }
}

# spatial join 
# get boundary with high definition of all states and counties
all_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")

us <- ne_states(country = "united states of america", returnclass = "sf")
all_boundaries <- us_states(resolution = "high", states = all_states)

county_polygons <- us_counties(resolution = "high", states = all_states)

# create 100km buffer of county
buffered_counties <- st_buffer(county_polygons, dist = 100000)

model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
           "CanESM5", "CanESM5-1", 
           "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
           "FGOALS-f3-L", "FGOALS-g3",  
           "IPSL-CM6A-LR",
           "KACE-1-0-G",
           "MIROC6", "MRI-ESM2-0",
           "NorESM2-LM", "NorESM2-MM")

variable <- c("hurs", "pr", "sfcWind", "tas")

for (mod in model) {
  mod_var <- NULL 
  
  for (var in variable){
    data <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "/", var, ".RDS"))
    
    data$longitude <-  ifelse(data$longitude > 180, data$longitude - 360, data$longitude)
    
    station_points <- as.data.frame(data) %>% 
      st_as_sf(coords=c("longitude","latitude"), crs=4326, remove=FALSE) 
    
    # spatial join station and buffer
    station_county <- st_join(station_points, buffered_counties, left = F)
    station_county$fips_county <- paste(station_county$statefp, station_county$countyfp, sep = "")
    
    station_county <- as.data.frame(station_county)
    
    unique_data <- station_county[, c("year", "month", "fips_county", paste0("adjusted_", var))]
    
    unique_data <- unique_data %>%  
      group_by(year, month, fips_county) %>%  
      summarise(!!paste0("adjusted_", var) := mean(!!sym(paste0("adjusted_", var)), na.rm = TRUE)) %>%  
      ungroup()
    
    if (is.null(mod_var)) {
      mod_var <- unique_data
    } else {
      mod_var <- inner_join(mod_var, unique_data, by = c("year", "month", "fips_county"))
    }
  }
  
  saveRDS(mod_var, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, ".RDS"))
}

# Moving average 
CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
                 "CanESM5", "CanESM5-1", 
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
                 "FGOALS-f3-L", "FGOALS-g3",  
                 "IPSL-CM6A-LR",
                 "KACE-1-0-G",
                 "MIROC6", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")

for (mod in CMIP6_model){
  mod_result <- data.frame()
  
  mod4.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, ".RDS"))
  
  mod4.5_wide <- pivot_wider(mod4.5, 
                             id_cols = c("year", "month"), 
                             names_from = "fips_county", 
                             values_from = c("adjusted_hurs", "adjusted_pr", "adjusted_tas", "adjusted_sfcWind"))
  
  for (i in 1:12) {
    meteor_data <- mod4.5_wide[mod4.5_wide$month == i, ]
    
    cma_result <- as.data.frame(lapply(meteor_data[, 3:ncol(meteor_data)], 
                                       function(x) rollmean(x, 9, fill = NA, align = "center")))
    
    meteor_data[, 3:ncol(meteor_data)] <- cma_result
    
    meteor_data <- meteor_data[meteor_data$year >= 2019 & meteor_data$year <= 2096, ]
    
    mod_result <- rbind(mod_result, meteor_data)
  }
  
  long_data <- pivot_longer(mod_result,
                            cols = -c(year, month), 
                            names_to = "fips_county")
  
  long_data$variable <- gsub("^(adjusted_[a-zA-Z]+)_.*", "\\1", long_data$fips_county)
  long_data$fips_county <- gsub("^adjusted_[a-zA-Z]+_(.*)", "\\1", long_data$fips_county)
  
  long_data <- pivot_wider(long_data, 
                           id_cols = c("year", "month", "fips_county"), 
                           names_from = "variable", 
                           values_from = "value")
  
  saveRDS(long_data, paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "_MA.RDS"))
}


##### 11.1 SSP5-8.5 #####
###### 11.1.1 using 3-range regression  ######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~
                 m_TEMP_county_0_12 + m_TEMP_county_12_30 + m_TEMP_county_30_up + 
                 m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 +
                 Household_Income + Household_Size + Age_And_Presence_Of_Children +
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                 year^Zone + month^Zone + household_code,
               data = data,
               weights = ~Projection_Factor,
               vcov = ~fips_county,
               fixef.rm = "single",
               combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", "AWI-CM-1-1-MR",
                 "CanESM5", "CanESM5-1", "CAS-ESM2-0", "CESM2-WACCM", "CMCC-CM2-SR5", "CMCC-ESM2",
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",  
                 "FGOALS-f3-L", "FGOALS-g3", "FIO-ESM-2-0", 
                 "INM-CM4-8", "INM-CM5-0", "IPSL-CM6A-LR",
                 "MIROC6", "MPI-ESM1-2-HR", "MPI-ESM1-2-LR", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")

total8.5_county <- data.frame()
total8.5_nation <- data.frame()

for (mod in CMIP6_model){
  mod8.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp585/", mod, "_MA.RDS"))
  
  mod8.5 <- select(mod8.5, year, month, fips_county, adjusted_hurs, adjusted_pr, adjusted_sfcWind, adjusted_tas)
  names(mod8.5) <- c("year", "month", "fips_county", "hurs", "pr", "sfcWind", "tas")
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_data <- mod8.5
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "fips_county", "Projection_Factor", "predict_result")]    
  
  # NOTICE: change of household in same county is same too, so we don't need to use weight to calculate the county-month average 
  mod_predict_county <- mod_data %>%
    group_by(year, month, fips_county) %>%
    summarise(result = mean(predict_result)) %>%
    ungroup()
  
  # use weights to calculate nation-month average
  mod_predict_nation <- mod_data %>%
    group_by(year, month) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict_county$model <- mod
  mod_predict_nation$model <- mod
  
  total8.5_county <- rbind(total8.5_county, mod_predict_county)
  total8.5_nation <- rbind(total8.5_nation, mod_predict_nation)
}

saveRDS(total8.5_county, "REVIEW Projection of CMIP6-ssp8.5 adjusted county range3.RDS")
saveRDS(total8.5_nation, "REVIEW Projection of CMIP6-ssp8.5 adjusted nation range3.RDS")

# calculate change relative to 2019
# nation-month average 
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted nation range3.RDS")

result2019 <- total8.5[total8.5$year == 2019, ]
result2030_2095 <- total8.5[total8.5$year >= 2030 & total8.5$year <= 2096, ]

change_result <- inner_join(result2030_2095, result2019, by = c("month", "model"))

change_result$result <- change_result$result.x - change_result$result.y

change <- change_result[, c("year.x", "month", "result", "model")]
names(change)[1] <- "year"

saveRDS(change, "REVIEW Projection change of CMIP6-ssp8.5 adjusted nation range3.RDS")

# county-month average
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted county range3.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "fips_county", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "fips_county", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted county range3.RDS")

###### 11.1.2 Heterogeneity Projection #####
####### 11.1.2.1 Ethnic #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model1 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Ethnic + m_TEMP_county_12_30 * Ethnic  + m_TEMP_county_30_up + 
                  m_WDSP_county * Ethnic + m_PRCP_county * Ethnic + m_RH_county * Ethnic + m_RH_county2 * Ethnic +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model1)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod8.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county ^ 2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model1, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Ethnic", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate Ethnic-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Ethnic) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total8.5 <- rbind(total8.5, mod_predict)
}

saveRDS(total8.5, "REVIEW Projection of CMIP6-ssp8.5 adjusted Ethnic.RDS")

# calculate change relative to 2019
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted Ethnic.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Ethnic", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Ethnic", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Ethnic.RDS")

####### 11.1.2.2 Male Education ######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Male_Head_Education <- factor(data$Male_Head_Education)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model2 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Male_Head_Education + m_TEMP_county_12_30 * Male_Head_Education  + m_TEMP_county_30_up + 
                  m_WDSP_county * Male_Head_Education + m_PRCP_county * Male_Head_Education + m_RH_county * Male_Head_Education + m_RH_county2 * Male_Head_Education +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Female_Head_Education + Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model2)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod8.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model2, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Male_Head_Education", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Male_Head_Education) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total8.5 <- rbind(total8.5, mod_predict)
}

saveRDS(total8.5, "REVIEW Projection of CMIP6-ssp8.5 adjusted Male_Head_Education.RDS")

# calculate change relative to 2019
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted Male_Head_Education.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Male_Head_Education", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Male_Head_Education", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Male_Head_Education.RDS")

####### 11.1.2.3 Female Education ######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Female_Head_Education <- factor(data$Female_Head_Education)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model3 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Female_Head_Education + m_TEMP_county_12_30 * Female_Head_Education + m_TEMP_county_30_up + 
                  m_WDSP_county * Female_Head_Education + m_PRCP_county * Female_Head_Education + m_RH_county * Female_Head_Education + m_RH_county2 * Female_Head_Education +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model3)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod8.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model3, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Female_Head_Education", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Female_Head_Education) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total8.5 <- rbind(total8.5, mod_predict)
}

saveRDS(total8.5, "REVIEW Projection of CMIP6-ssp8.5 adjusted Female_Head_Education.RDS")

# calculate change relative to 2019
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted Female_Head_Education.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Female_Head_Education", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Female_Head_Education", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Female_Head_Education.RDS")

####### 11.1.2.4 Income #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income

data$Income_group <- ifelse(data$Household_Income <= 24999, "Very Low",
                            ifelse(25000 <= data$Household_Income & data$Household_Income <= 49999, "Low", 
                                   ifelse(50000 <= data$Household_Income & data$Household_Income <= 69999, "Medium", 
                                          ifelse(70000 <= data$Household_Income & data$Household_Income <= 99999, "High", "Very High"))))
data$Income_group <- factor(data$Income_group)
data$Income_group <- relevel(data$Income_group, ref = "Very Low")

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

model4 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Income_group + m_TEMP_county_12_30 * Income_group  + m_TEMP_county_30_up + 
                  m_WDSP_county * Income_group + m_PRCP_county * Income_group + m_RH_county*Income_group + m_RH_county2 * Income_group +
                  Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model4)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Income_group", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod8.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model4, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Income_group", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Income_group) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total8.5 <- rbind(total8.5, mod_predict)
}

saveRDS(total8.5, "REVIEW Projection of CMIP6-ssp8.5 adjusted Income.RDS")

# calculate change relative to 2019
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted Income.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Income_group", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Income_group", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Income.RDS")

####### 11.1.2.5 Male Work Environment #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Environment <- ifelse(data$Male_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Environment <- factor(data$Environment)
data$Environment <- relevel(data$Environment, ref = "Indoor")

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model5 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                  m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model5)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Environment", "Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod8.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model5, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Environment", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Environment) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total8.5 <- rbind(total8.5, mod_predict)
}

saveRDS(total8.5, "REVIEW Projection of CMIP6-ssp8.5 adjusted Male Environment.RDS")

# calculate change relative to 2019
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted Male Environment.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Environment", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Environment", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Male Environment.RDS")

####### 11.1.2.6 Female Work Environment #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Environment <- ifelse(data$Female_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Environment <- factor(data$Environment)
data$Environment <- relevel(data$Environment, ref = "Indoor")

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model6 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                  m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model6)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Environment", "Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod8.5 <- inner_join(mod8.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod8.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model6, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Environment", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Environment) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total8.5 <- rbind(total8.5, mod_predict)
}

saveRDS(total8.5, "REVIEW Projection of CMIP6-ssp8.5 adjusted Female Environment.RDS")

# calculate change relative to 2019
total8.5 <- readRDS("REVIEW Projection of CMIP6-ssp8.5 adjusted Female Environment.RDS")

predict_result <- total8.5[total8.5$year == 2019 | total8.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Environment", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Environment", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp8.5 adjusted Female Environment.RDS")


##### 11.2 SSP2-4.5 #####
###### 11.2.1 using 3-range regression ######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~
                 m_TEMP_county_0_12 + m_TEMP_county_12_30 + m_TEMP_county_30_up + 
                 m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 +
                 Household_Income + Household_Size + Age_And_Presence_Of_Children +
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                 year^Zone + month^Zone + household_code,
               data = data,
               weights = ~Projection_Factor,
               vcov = ~fips_county,
               fixef.rm = "single",
               combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

CMIP6_model <- c("ACCESS-CM2", "ACCESS-ESM1-5", 
                 "CanESM5", "CanESM5-1", 
                 "EC-Earth3", "EC-Earth3-CC", "EC-Earth3-Veg", "EC-Earth3-Veg-LR",
                 "FGOALS-f3-L", "FGOALS-g3",  
                 "IPSL-CM6A-LR",
                 "KACE-1-0-G",
                 "MIROC6", "MRI-ESM2-0",
                 "NorESM2-LM", "NorESM2-MM")

total4.5_county <- data.frame()
total4.5_nation <- data.frame()

for (mod in CMIP6_model){
  mod4.5 <- readRDS(paste0("/work/pi_pengfei_liu_uri_edu/Climate change and food consumption/CMIP6-ssp245/", mod, "_MA.RDS"))
  
  mod4.5 <- select(mod4.5, year, month, fips_county, adjusted_hurs, adjusted_pr, adjusted_sfcWind, adjusted_tas)
  names(mod4.5) <- c("year", "month", "fips_county", "hurs", "pr", "sfcWind", "tas")
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_data <- mod4.5
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "fips_county", "Projection_Factor", "predict_result")]    
  
  # NOTICE: change of household in same county is same too, since we don't need to use weight to calculate the county-month average 
  mod_predict_county <- mod_data %>%
    group_by(year, month, fips_county) %>%
    summarise(result = mean(predict_result)) %>%
    ungroup()
  
  # use weights to calculate nation-month average
  mod_predict_nation <- mod_data %>%
    group_by(year, month) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict_county$model <- mod
  mod_predict_nation$model <- mod
  
  total4.5_county <- rbind(total4.5_county, mod_predict_county)
  total4.5_nation <- rbind(total4.5_nation, mod_predict_nation)
}

saveRDS(total4.5_county, "REVIEW Projection of CMIP6-ssp4.5 adjusted county range3.RDS")
saveRDS(total4.5_nation, "REVIEW Projection of CMIP6-ssp4.5 adjusted nation range3.RDS")

# calculate change relative to 2019
# nation-month average 
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted nation range3.RDS")

result2019 <- total4.5[total4.5$year == 2019, ]
result2030_2095 <- total4.5[total4.5$year >= 2030 & total4.5$year <= 2096, ]

change_result <- inner_join(result2030_2095, result2019, by = c("month", "model"))

change_result$result <- change_result$result.x - change_result$result.y

change <- change_result[, c("year.x", "month", "result", "model")]
names(change)[1] <- "year"

saveRDS(change, "REVIEW Projection change of CMIP6-ssp4.5 adjusted nation range3.RDS")

# county-month average
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted county range3.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "fips_county", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "fips_county", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted county range3.RDS")


####### 11.2.2.1 Ethnic #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model1 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Ethnic + m_TEMP_county_12_30 * Ethnic  + m_TEMP_county_30_up + 
                  m_WDSP_county * Ethnic + m_PRCP_county * Ethnic + m_RH_county * Ethnic + m_RH_county2 * Ethnic +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model1)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod4.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county ^ 2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model1, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Ethnic", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate Ethnic-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Ethnic) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total4.5 <- rbind(total4.5, mod_predict)
}

saveRDS(total4.5, "REVIEW Projection of CMIP6-ssp4.5 adjusted Ethnic.RDS")

# calculate change relative to 2019
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted Ethnic.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Ethnic", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Ethnic", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Ethnic.RDS")

####### 11.2.2.2 Male Education ######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Male_Head_Education <- factor(data$Male_Head_Education)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model2 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Male_Head_Education + m_TEMP_county_12_30 * Male_Head_Education  + m_TEMP_county_30_up + 
                  m_WDSP_county * Male_Head_Education + m_PRCP_county * Male_Head_Education + m_RH_county * Male_Head_Education + m_RH_county2 * Male_Head_Education +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Female_Head_Education + Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model2)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod4.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model2, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Male_Head_Education", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Male_Head_Education) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total4.5 <- rbind(total4.5, mod_predict)
}

saveRDS(total4.5, "REVIEW Projection of CMIP6-ssp4.5 adjusted Male_Head_Education.RDS")

# calculate change relative to 2019
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted Male_Head_Education.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Male_Head_Education", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Male_Head_Education", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Male_Head_Education.RDS")

####### 11.2.2.3 Female Education ######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Female_Head_Education <- factor(data$Female_Head_Education)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <=0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model3 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Female_Head_Education + m_TEMP_county_12_30 * Female_Head_Education + m_TEMP_county_30_up + 
                  m_WDSP_county * Female_Head_Education + m_PRCP_county * Female_Head_Education + m_RH_county * Female_Head_Education + m_RH_county2 * Female_Head_Education +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Ethnic + Marital_Status | 
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model3)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod4.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model3, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Female_Head_Education", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Female_Head_Education) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total4.5 <- rbind(total4.5, mod_predict)
}

saveRDS(total4.5, "REVIEW Projection of CMIP6-ssp4.5 adjusted Female_Head_Education.RDS")

# calculate change relative to 2019
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted Female_Head_Education.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Female_Head_Education", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Female_Head_Education", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Female_Head_Education.RDS")

####### 11.2.2.4 Income #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income

data$Income_group <- ifelse(data$Household_Income <= 24999, "Very Low",
                            ifelse(25000 <= data$Household_Income & data$Household_Income <= 49999, "Low", 
                                   ifelse(50000 <= data$Household_Income & data$Household_Income <= 69999, "Medium", 
                                          ifelse(70000 <= data$Household_Income & data$Household_Income <= 99999, "High", "Very High"))))
data$Income_group <- factor(data$Income_group)
data$Income_group <- relevel(data$Income_group, ref = "Very Low")

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

model4 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Income_group + m_TEMP_county_12_30 * Income_group  + m_TEMP_county_30_up + 
                  m_WDSP_county * Income_group + m_PRCP_county * Income_group + m_RH_county*Income_group + m_RH_county2 * Income_group +
                  Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model4)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Income_group", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod4.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model4, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Income_group", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Income_group) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total4.5 <- rbind(total4.5, mod_predict)
}

saveRDS(total4.5, "REVIEW Projection of CMIP6-ssp4.5 adjusted Income.RDS")

# calculate change relative to 2019
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted Income.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Income_group", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Income_group", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Income.RDS")

####### 11.2.2.5 Male Work Environment #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Environment <- ifelse(data$Male_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Environment <- factor(data$Environment)
data$Environment <- relevel(data$Environment, ref = "Indoor")

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model5 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                  m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model5)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Environment", "Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod4.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model5, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Environment", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Environment) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total4.5 <- rbind(total4.5, mod_predict)
}

saveRDS(total4.5, "REVIEW Projection of CMIP6-ssp4.5 adjusted Male Environment.RDS")

# calculate change relative to 2019
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted Male Environment.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Environment", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Environment", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Male Environment.RDS")

####### 11.2.2.6 Female Work Environment #######
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$Environment <- ifelse(data$Female_Head_Occupation %in% c(1, 2, 3, 4, 8, 10, 12), "Indoor", "Outdoor")
data$Environment <- factor(data$Environment)
data$Environment <- relevel(data$Environment, ref = "Indoor")

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model6 <- feols(mpc_ADD_SUGARS_convert ~
                  m_TEMP_county_0_12 * Environment + m_TEMP_county_12_30 * Environment  + m_TEMP_county_30_up + 
                  m_WDSP_county * Environment + m_PRCP_county * Environment + m_RH_county * Environment + m_RH_county2 * Environment +
                  Household_Income + Household_Size + Age_And_Presence_Of_Children +
                  Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                  year^Zone + month^Zone + household_code,
                data = data,
                weights = ~Projection_Factor,
                vcov = ~fips_county,
                fixef.rm = "single",
                combine.quick = FALSE)

# NOTE: 17 fixed-effect singletons were removed (17 observations, breakup: 0/0/17).
Fix <- fixef(model6)

Fix_household <- as.data.frame(Fix[["household_code"]])
colnames(Fix_household) <- "household_coef"
Fix_household$household_code <- as.numeric(rownames(Fix_household))
rownames(Fix_household) <- NULL

filtered_data <- unique(data[data$year == 2019, 
                             c("Environment", "Household_Income", "Household_Size", "Age_And_Presence_Of_Children", 
                               "Male_Head_Age", "Female_Head_Age", "Male_Head_Education", "Female_Head_Education", "Ethnic", "Marital_Status",
                               "household_code", "fips_county", "Projection_Factor", "Zone")])

# every household is involved in regression
household2019 <- filtered_data[filtered_data$household_code %in% Fix_household$household_code, ]

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
  
  mod4.5 <- inner_join(mod4.5, household2019, by = "fips_county", multiple = "all")
  
  # predict using fixed effect coefficient of 2019
  mod_predict <- data.frame()
  
  mod_data <- mod4.5 
  
  mod_data$real_year <- mod_data$year
  mod_data$year <- 2019
  
  mod_data$m_RH_county <- mod_data$hurs
  mod_data$m_WDSP_county <- mod_data$sfcWind
  mod_data$m_PRCP_county <- mod_data$pr
  mod_data$m_TEMP_county <- mod_data$tas
  
  mod_data$m_RH_county2 <- mod_data$m_RH_county^2
  
  mod_data$m_TEMP_county <- ifelse(mod_data$m_TEMP_county <= 0, 0, mod_data$m_TEMP_county)
  
  mod_data$m_TEMP_county_0_12 <- ifelse(mod_data$m_TEMP_county <= 12, mod_data$m_TEMP_county, 12)
  mod_data$m_TEMP_county_12_30 <- ifelse(mod_data$m_TEMP_county > 12 & mod_data$m_TEMP_county <= 30, mod_data$m_TEMP_county - 12, 
                                         ifelse(mod_data$m_TEMP_county > 30, 30 - 12, 0))
  mod_data$m_TEMP_county_30_up <- ifelse(mod_data$m_TEMP_county > 30, mod_data$m_TEMP_county - 30, 0)
  
  predict_data <- data.frame()
  predict_data <- as.data.frame(predict(model6, mod_data))
  colnames(predict_data) <- "predict_result"
  
  mod_data <- cbind(mod_data, predict_data)    
  
  mod_data$year <- mod_data$real_year
  mod_data <- mod_data[ , c("year", "month", "household_code", "Environment", "fips_county", "Projection_Factor", "predict_result")]   
  
  # use weights to calculate county-month average
  mod_predict <- mod_data %>%
    group_by(year, month, Environment) %>%
    mutate(weights = sum(Projection_Factor, na.rm = TRUE)) %>%
    summarise(result = sum(predict_result * Projection_Factor / weights)) %>%
    ungroup()
  
  mod_predict$model <- mod
  
  total4.5 <- rbind(total4.5, mod_predict)
}

saveRDS(total4.5, "REVIEW Projection of CMIP6-ssp4.5 adjusted Female Environment.RDS")

# calculate change relative to 2019
total4.5 <- readRDS("REVIEW Projection of CMIP6-ssp4.5 adjusted Female Environment.RDS")

predict_result <- total4.5[total4.5$year == 2019 | total4.5$year == 2095, ]

predict_result <- inner_join(predict_result[predict_result$year == 2019, ], predict_result[predict_result$year == 2095, ], 
                             by = c("model", "Environment", "month"))

predict_result$result <- predict_result$result.y - predict_result$result.x

change <- predict_result[, c("model", "Environment", "month", "result")]
change$year <- 2095

saveRDS(change, "REVIEW 2095 Projection change of CMIP6-ssp4.5 adjusted Female Environment.RDS")

#### 12. spline model ####
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

data$Ethnic <- factor(data$Ethnic)
data$Marital_Status <- factor(data$Marital_Status)
data$Age_And_Presence_Of_Children <- factor(data$Age_And_Presence_Of_Children)
data$Zone <- factor(data$Zone)

data$m_TEMP_county <- ifelse(data$m_TEMP_county <= 0, 0, data$m_TEMP_county)

data$m_TEMP_county_0_12 <- ifelse(data$m_TEMP_county <= 12, data$m_TEMP_county, 12)
data$m_TEMP_county_12_30 <- ifelse(data$m_TEMP_county > 12 & data$m_TEMP_county <= 30, data$m_TEMP_county - 12, 
                                   ifelse(data$m_TEMP_county > 30, 30 - 12, 0))
data$m_TEMP_county_30_up <- ifelse(data$m_TEMP_county > 30, data$m_TEMP_county - 30, 0)

data$Income <- ifelse(data$Household_Income == 3, 2500, 
                      ifelse(data$Household_Income == 4, 6500, 
                             ifelse(data$Household_Income == 6, 9000, 
                                    ifelse(data$Household_Income == 8, 11000, 
                                           ifelse(data$Household_Income == 10, 13500, 
                                                  ifelse(data$Household_Income == 11, 17500, 
                                                         ifelse(data$Household_Income == 13, 22500, 
                                                                ifelse(data$Household_Income == 15, 27500, 
                                                                       ifelse(data$Household_Income == 16, 32500, 
                                                                              ifelse(data$Household_Income == 17, 37500, 
                                                                                     ifelse(data$Household_Income == 18, 42500, 
                                                                                            ifelse(data$Household_Income == 19, 47500, 
                                                                                                   ifelse(data$Household_Income == 21, 55000, 
                                                                                                          ifelse(data$Household_Income == 23, 65000, 
                                                                                                                 ifelse(data$Household_Income == 26, 85000, 150000)))))))))))))))
# CPI of 2004-2019
CPI <- c(0.8662167812, 0.8956053237, 0.9244970508, 0.9508699238, 0.9873747739, 
         0.9838641997, 1.0000000000, 1.0315684160, 1.0529150450, 1.0683384890, 
         1.0856693210, 1.0869572200, 1.1006700890, 1.1241155730, 1.1515730320, 
         1.1724419550)
CPI_map <- setNames(CPI, 2004:2019)

data <- data %>%
  mutate(across(Income, ~ . / CPI_map[as.character(year)]))

data$Household_Income <- data$Income/10000

model <- feols(mpc_ADD_SUGARS_convert ~
                 m_TEMP_county_0_12 + m_TEMP_county_12_30 + m_TEMP_county_30_up + 
                 m_WDSP_county + m_PRCP_county + m_RH_county + m_RH_county2 +
                 Household_Income + Household_Size + Age_And_Presence_Of_Children +
                 Male_Head_Age + Female_Head_Age + Male_Head_Education + Female_Head_Education + Ethnic + Marital_Status |
                 year^Zone + month^Zone + household_code,
               data = data,
               weights = ~Projection_Factor,
               vcov = ~fips_county,
               fixef.rm = "single",
               combine.quick = FALSE)

AS_convert <- coef(model)
AS_convert <- as.matrix(AS_convert)
AS_convert_confint <- confint(model, level = 0.95)
AS_convert_confint <- as.matrix(AS_convert_confint)

models <- list("mpc_ADD_SUGARS_convert" = model)
modelsummary(models,stars = c('***' = .01, '**' = .05, '*' = .1), 
             gof_omit = 'R2 Adj.|R2 Within|RMSE|AIC|BIC',
             output = "REVIEW table-spline model.docx")

model_information <- as.data.frame(cbind(AS_convert, AS_convert_confint))

model_information <- as.matrix(model_information[grep("m_TEMP_county", rownames(model_information)), ])

model_information <- as.data.frame(cbind(model_information, rownames(model_information)))
rownames(model_information) <- NULL
names(model_information) <- c("c", "ll", "ul", "rownames")

model_information$range <- "0-12"
model_information$range[grep("m_TEMP_county_12_30", model_information$rownames)] <- "12-30"
model_information$range[grep("m_TEMP_county_30_up", model_information$rownames)] <- "30+"
model_information <- model_information[, -4]

colnames(model_information) <- c("c",  "ll", "ul", "range")

model_information$c <- as.numeric(model_information$c)
model_information$ll <- as.numeric(model_information$ll)
model_information$ul <- as.numeric(model_information$ul)

saveRDS(model_information, "REVIEW coefficient R added sugar monthly range3.RDS")
