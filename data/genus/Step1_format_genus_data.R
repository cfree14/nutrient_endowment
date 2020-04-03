

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
inputdir <- "data/genus/raw"
outputdir <- "data/genus/processed"
tabledir <- "tables"

# The GENuS Dataverse:
# https://dataverse.harvard.edu/dataverse/GENuS

# GENuS datasets
# 1. Food composition tables for GENuS
# 2. Nutrient Totals (incl. Fortification) by Age and Sex (2011)
# 3. Edible Food by Age and Sex (2011)
# 4. Nutrient Totals by Age and Sex (2011)
# 5. Total Nutrient Supplies by Country and Year
# 6. Nutrient Supplies by Food and Country (2011)
# 7. Total Nutrient Supply including Fortification (2011)
# 8. Edible Food by Country and Year

# Read nutrient key
nutrient_key <- rio::import(file.path(tabledir, "genus_nutrient_key.xlsx")) %>% 
  setNames(c("nutrient_type", "nutrient_subtype", "nutrient", "nutrient_full", "diet_req_yn"))


################################################################################
# 1. Food composition tables for GENuS
################################################################################

# Description: Food composition tables organized by the commodities included in 
# the Global Expanded Nutrient Supply model (GENuS) dataset. Their construction 
# and use are described in the GENuS methods paper (citation below). Citations 
# for original source tables and units for each nutrient are found in accompanying "readme" file.

# Notes: none.

# Files to merge
files_merge <- list.files(file.path(inputdir, "food_composition_tables"), pattern=".csv")

# Merge data
x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  
  # Read data
  fdata <- read.csv(file.path(inputdir, "food_composition_tables", x), as.is=T, na.strings="nd") %>% 
    mutate(file=x) %>% 
    select(file, everything())

})

# Format data
data <- data_orig %>% 
  # Rename columns
  setNames(tolower(colnames(.))) %>% 
  janitor::clean_names("snake") %>% 
  rename(genus_food_id=genus_foodid, 
         genus_food_name=genus_foodname,
         fct_food_name=fct_foodname) %>% 
  # Remove unimportant columns
  select(-c(x, x_2)) %>% 
  # Add region
  mutate(region=recode(file, 
                       "FCT_GENuS_ASEANFoods_v2.csv"="Southeast Asia",
                       "FCT_GENuS_India_v2.csv"="India",
                       "FCT_GENuS_LATINFoods_v2.csv"="Latin America",
                       "FCT_GENuS_NEASIA_v2.csv"="Northeast Asia",
                       "FCT_GENuS_OldAfrica_v2.csv"="Africa (old)",
                       "FCT_GENuS_US_v2.csv"="United States",
                       "FCT_GENuS_WestAfrica_v2.csv"="Africa (West)")) %>% 
  # Arrange columns
  select(file, region, everything()) %>% 
  # Convert wide-to-long
  gather(key="nutrient", value="units_per_100g", 6:ncol(.)) %>% 
  # Format nutrient name
  mutate(nutrient=str_to_title(nutrient),
         nutrient=recode(nutrient, 
                         "B12_usda_only"="Vitamin B12 (USDA only)",
                         "B6"="Vitamin B6",
                         "Dietary_fiber"="Dietary fiber",
                         "Monounsaturated_fa"="Monounsaturated fatty acids",
                         "Omega_3_usda_only"="Omega-3 fatty acids (USDA only)",
                         "Polyunsaturated_fa"="Polyunsaturated fatty acids",
                         "Saturated_fa"="Saturated fatty acids",
                         "Vitamin_a"="Vitamin A",
                         "Vitamin_c"="Vitamin C",
                         "Water_moisture"="Water moisture")) %>% 
  # Add nutrient units
  mutate(units=recode(nutrient,
                      "Calories"="kcal",
                      "Protein"="g",
                      "Fat"="g",
                      "Carbohydrates"="g",
                      "Vitamin C"="mg",
                      "Vitamin A"="mcg RAE",
                      "Folate"="mcg",
                      "Calcium"="mg",
                      "Iron"="mg",
                      "Zinc"="mg",
                      "Potassium"="mg",
                      "Dietary fiber"="g",
                      "Copper"="mg",
                      "Sodium"="mg",
                      "Phosphorus"="mg",
                      "Thiamin"="mg",
                      "Riboflavin"="mg",
                      "Niacin"="mg",
                      "Vitamin B6"="mg",
                      "Magnesium"="mg",
                      "Saturated fatty acids"="g",
                      "Monounsaturated fatty acids"="g",
                      "Polyunsaturated fatty acids"="g")) %>% 
  # Arrange
  select(file:nutrient, units, units_per_100g, everything())

# Inspect data
str(data)
sort(unique(data$nutrient))
data %>% select(nutrient, units) %>%  unique()

# Export data
saveRDS(data, file=file.path(outputdir, "genus_food_composition_tables.Rds"))


################################################################################
# 2. Nutrient Totals (incl. Fortification) by Age and Sex (2011)
################################################################################

# Description: Total nutrients supplied by all foods for each country and age-sex 
# group for most recent year (2011). Nutrients covered are: calories, protein, 
# fat, carbohydrates, vitamin C, vitamin A, folate, calcium, iron, zinc, potassium, 
# dietary fiber, copper, sodium, phosphorus, thiamin, riboflavin, niacin, B6, 
# magnesium, saturated fatty acids, monounsaturated fatty acids, polyunsaturated fatty acids.

# Notes: Countries marked with a "*" have insufficient data to estimate nutrient 
# supplies. Each file includes a median and 95% uncertainty intervals for each year, 
# calculated from 1,000 monte carlo simulations of potential nutrients supplied 
# by the foods available in each country. Added nutrients through fortification 
# are calculated based on mandatory and voluntary fortification guidelines, and 
# estimates of foods that are industrially processed/milled to which fortificants may be added.

# Files to merge
files_merge <- list.files(file.path(inputdir, "nutrient_totals_w_fort_by_age_sex"), pattern=".csv")

# Example data
exdata <- read.csv(file.path(inputdir, "nutrient_totals_w_fort_by_age_sex", files_merge[1]), 
                   as.is=T,  na.strings="*", header=F)

# Header info
header_info <-  read.csv(file.path(inputdir, "nutrient_totals_w_fort_by_age_sex", files_merge[1]), 
                         as.is=T, nrows=3, header=F, na.strings="*") %>% 
  select(4:ncol(.)) %>% t() %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  rename(nutrient=V1, stat=V2, units=V3) %>% 
  mutate(stat=recode(stat, "High 95% UI"="hi", "Low 95% UI"="lo", "Median"="md"), 
         col_name=paste(nutrient, stat, sep="_"))

# Column names
col_names <- c("iso3", "country", "blank", header_info$col_name)

# Merge data
# x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  
  # Get sex
  sex <- strsplit(x, split="_")[[1]][4] %>% 
    gsub(".csv", "", .)
  
  # Get age range
  age_range <- strsplit(x, split="_")[[1]][3] %>% 
    gsub("age", "", .)
  
  # Read data
  fdata <- read.csv(file.path(inputdir, "nutrient_totals_w_fort_by_age_sex", x), 
                    as.is=T, skip=3, na.strings="*", col.names = col_names) %>% 
    mutate(age_range=age_range, 
           sex=sex) %>% 
    select(age_range, sex, everything())
  
})

# Format data
data <- data_orig %>% 
  # Remove blank
  select(-blank) %>% 
  # Convert wide-to-long
  gather(key="key", value="value", 5:ncol(.)) %>% 
  # Add nutrient, stat, and units column
  left_join(header_info, by=c("key"="col_name")) %>% 
  rename(units_long=units) %>% 
  mutate(units_short=gsub("/person/day", "", units_long)) %>% 
  # Rearrange
  select(-key) %>% 
  select(iso3, country, age_range, sex, nutrient, 
         units_long, units_short, stat, value, everything()) %>% 
  spread(key="stat", value="value") %>% 
  rename(value_med=md, value_lo=lo, value_hi=hi) %>% 
  select(iso3, country, age_range, sex, nutrient, 
         units_long, units_short, value_med, value_lo, value_hi, everything()) %>% 
  arrange(iso3, nutrient, age_range) %>% 
  # Format nutrient column
  mutate(nutrient=str_to_title(nutrient)) %>% 
  mutate(nutrient=recode(nutrient, 
                         "B6"="Vitamin B6",
                         "Dietaryfiber"="Dietary fiber",
                         "Monounsatfa"="Monounsaturated fatty acids",
                         "Polyunsatfa"="Polyunsaturated fatty acids",
                         "Saturatedfa"="Saturated fatty acids",
                         "Vitamina"="Vitamin A",
                         "Vitaminc"="Vitamin C"),
         nutrient_label=paste0(nutrient, " (", units_short, ")")) %>% 
  # Add nutrient type
  left_join(nutrient_key %>% select(nutrient, nutrient_type), by="nutrient") %>% 
  # Format age and sex columns
  mutate(sex=recode(sex, "bothsexes"="Children", "male"="Males", "female"="Females"),
         age_range=factor(age_range, levels=c("0-4", "5-9", "10-14", "15-19",
                                                     "20-24", "25-29", "30-34", "35-39", 
                                                     "40-44", "45-49", "50-54","55-59", 
                                                     "60-64", "65-69", "70-74", "75-79", "80+"))) %>% 
  # Arrange
  select(iso3:sex, nutrient_type, nutrient_label, everything())

# Inspect data
str(data)
sort(unique(data$nutrient))
freeR::complete(data)

# Example plot
g <- ggplot(data %>% filter(country=="Ghana"), aes(x=age_range, y=value_med, fill=sex, group=sex)) +
  facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  labs(x="Age range", y="Daily per capita supply") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export data
saveRDS(data, file=file.path(outputdir, "genus_nutrient_supplies_w_fort_by_age_sex_2011.Rds"))


################################################################################
# 3. Edible Food by Age and Sex (2011)
################################################################################

# Description: Edible food supplies across 225 individual foods (grams/person/day), 
# separated by age and sex group. (2015-12-14)

# Notes: Countries marked with a "*" have insufficient data to estimate nutrient 
# supplies. Each file includes a median and 95% uncertainty intervals for each year,
# calculated from 1,000 monte carlo simulations of potential nutrients supplied 
# by the foods available in each country.

# Files
files_merge <- list.files(file.path(inputdir, "edible_food_by_age_sex"), pattern=".csv")

# Example data
exdata <- read.csv(file.path(inputdir, "edible_food_by_age_sex", files_merge[1]), as.is=T, na.strings="*")

# Header info
header_info <-  read.csv(file.path(inputdir, "edible_food_by_age_sex", files_merge[1]), as.is=T, nrows=3, header=F, na.strings="*") %>% 
  select(4:ncol(.)) %>% t() %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  rename(food_name_clean=V1, stat=V2, units=V3) %>% 
  mutate(stat=recode(stat, "High 95% UI"="hi", "Low 95% UI"="lo", "Median"="md"),
         food_name_messy=gsub("[[:punct:]]| ", ".", food_name_clean),
         col_name=paste(food_name_messy, stat, sep="_"))
col_names <- c("iso3", "country", "blank", header_info$col_name)

# Merge data
# x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  
  # Get sex
  sex <- strsplit(x, split="_")[[1]][4] %>% 
    gsub(".csv", "", .)
  
  # Get age range
  age_range <- strsplit(x, split="_")[[1]][3] %>% 
    gsub(".csv", "", .)
  
  # Read data
  fdata <- read.csv(file.path(inputdir, "edible_food_by_age_sex", x), 
                    as.is=T, skip=3, na.strings="*", col.names=col_names) %>% 
    mutate(age_range=age_range, 
           sex=sex) %>% 
    select(iso3, country, age_range, sex, everything())
  
})

# Format data
data <- data_orig %>% 
  # Remove blank column
  select(-blank) %>% 
  # Convert wide-to-long
  gather(key="key", value="value", 5:ncol(.)) %>% 
  # Arrange
  arrange(iso3, age_range) %>% 
  # Add food, units, stat 
  left_join(header_info, c("key"="col_name")) %>% 
  rename(food=food_name_clean, units_long=units) %>% 
  select(-c(key, food_name_messy)) %>% 
  mutate(units_short=gsub("/person/day", "", units_long)) %>%  
  select(iso3, country, age_range, sex, food, units_long, units_short, stat, value, everything()) %>% 
  # Reshape
  spread(key="stat", value="value") %>% 
  rename(value_med=md, value_lo=lo, value_hi=hi) %>% 
  select(iso3:units_short, value_med, value_lo, value_hi) %>% 
  # Format age/sex
  mutate(age_range=gsub("age", "", age_range),
         sex=recode(sex, "bothsexes"="Children", "male"="Males", "female"="Females"))

# Inspect
str(data)

# Export
saveRDS(data, file=file.path(outputdir, "genus_edible_food_by_age_sex_2011.Rds"))



################################################################################
# 4. Nutrient Totals by Age and Sex (2011)
################################################################################

# Description: Total supplies of 23 individual nutrients from all foods (without 
# fortification) by country and age-sex group in 2011. Nutrients covered are: 
# calories, protein, fat, carbohydrates, vitamin C, vitamin A, folate, calcium, 
# iron, zinc, potassium, dietary fiber, copper, sodium, phosphorus, thiamin, 
# riboflavin, niacin, B6, magnesium, saturated fatty acids, monounsaturated fatty 
# acids, polyunsaturated fatty acids.

# Notes: Countries marked with a "*" have insufficient data to estimate nutrient 
# supplies. Each file includes a median and 95% uncertainty intervals for each year, 
# calculated from 1,000 monte carlo simulations of potential nutrients supplied 
# by the foods available in each country.

# Files
files_merge <- list.files(file.path(inputdir, "nutrient_totals_by_age_sex"), pattern=".csv")

# Example data
exdata <- read.csv(file.path(inputdir, "nutrient_totals_by_age_sex", files_merge[1]), 
                   as.is=T, na.strings="*")

# Header info
header_info <-  read.csv(file.path(inputdir, "nutrient_totals_by_age_sex", files_merge[1]),
                         as.is=T, nrows=3, header=F, na.strings="*") %>% 
  select(4:ncol(.)) %>% t() %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  rename(nutrient=V1, stat=V2, units=V3) %>% 
  mutate(stat=recode(stat, "High 95% UI"="hi", "Low 95% UI"="lo", "Median"="md"), 
         col_name=paste(nutrient, stat, sep="_"))

# Column names
col_names <- c("iso3", "country", "blank", header_info$col_name)

# Merge
# x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  
  # Get sex
  sex <- strsplit(x, split="_")[[1]][3] %>% 
    gsub(".csv", "", .)
    
  # Get age range
  age_range <- strsplit(x, split="_")[[1]][2] %>% 
    gsub("age", "", .)
  
  # Read data
  fdata <- read.csv(file.path(inputdir, "nutrient_totals_by_age_sex", x), as.is=T, skip=3, na.strings="*", col.names = col_names) %>% 
    mutate(age_range=age_range, 
           sex=sex) %>% 
    select(age_range, sex, everything())
  
})

# Format data
data <- data_orig %>%
  # Rearrange columns
  select(-blank) %>% 
  select(iso3, country, age_range, sex, everything()) %>% 
  arrange(iso3, age_range) %>% 
  # Convert wide-to-long
  gather(key="key", value="value", 5:ncol(.)) %>% 
  # Add nutrient and stat
  left_join(header_info, by=c("key"="col_name")) %>% 
  # Format units
  rename(units_long=units) %>% 
  mutate(units_short=gsub("/person/day", "", units_long)) %>% 
  # Format nutrients
  mutate(nutrient=str_to_title(nutrient)) %>% 
  mutate(nutrient=recode(nutrient, 
                         "B6"="Vitamin B6",
                         "Dietaryfiber"="Dietary fiber",
                         "Monounsatfa"="Monounsaturated fatty acids",
                         "Polyunsatfa"="Polyunsaturated fatty acids",
                         "Saturatedfa"="Saturated fatty acids",
                         "Vitamina"="Vitamin A",
                         "Vitaminc"="Vitamin C")) %>% 
  mutate(nutrient_label=paste0(nutrient, " (", units_short, ")")) %>% 
  # Add nutrient type
  left_join(nutrient_key %>% select(nutrient, nutrient_type), by="nutrient") %>% 
  # Format sex and age range
  mutate(sex=recode(sex, "bothsexes"="Children", "male"="Males", "female"="Females")) %>% 
  mutate(age_range=factor(age_range, levels=c("0-4", "5-9", "10-14", "15-19",
                                              "20-24", "25-29", "30-34", "35-39", 
                                              "40-44", "45-49", "50-54","55-59", 
                                              "60-64", "65-69", "70-74", "75-79", "80+"))) %>% 
  # Arrange columns
  select(-key) %>% 
  select(iso3, country, age_range, sex, nutrient_type, nutrient_label, 
         nutrient, units_long, units_short, stat, value, everything()) %>% 
  # Spread stats
  spread(key="stat", value="value") %>% 
  rename(value_med=md, value_lo=lo, value_hi=hi) %>% 
  select(iso3:units_short, value_med, value_lo, value_hi, everything())

# Inspect
str(data)
sort(unique(data$nutrient))
freeR::complete(data)

# Example plot
g <- ggplot(data %>% filter(country=="Ghana"), aes(x=age_range, y=value_med, fill=sex, group=sex)) +
  facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
  geom_bar(stat="identity", position="dodge") +
  theme_bw() +
  labs(x="Age range", y="Daily per capita supply") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export
saveRDS(data, file=file.path(outputdir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))


################################################################################
# 5. Total Nutrient Supplies by Country and Year
################################################################################

# Content: Total nutrient supplies by country (175 countries) and year (1961-2011), 
# separated by nutrient (23 nutrients). Nutrients covered are: calories, protein, 
# fat, carbohydrates, vitamin C, vitamin A, folate, calcium, iron, zinc, potassium, 
# dietary fiber, copper, sodium, phosphorus, thiamin, riboflavin, niacin, B6, 
# magnesium, saturated fatty acids, monounsaturated fatty acids, polyunsaturated 
# fatty acids. (2015-12-14)

# Files
files_merge <- list.files(file.path(inputdir, "nutrient_sum"), pattern=".csv")

# Merge
# x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  
  # Nutrient name
  nutrient <- x %>% gsub("nutrient_sum_", "", .) %>% gsub(".csv", "", .) %>% substr(., 3, nchar(.))
  
  # Nutrient units
  units <- read.csv(file.path(inputdir, "nutrient_sum", x), as.is=T, nrows=3) %>% 
    slice(2) %>% select(4:ncol(.)) %>% as.character() %>% unique()
  
  # Column names
  year_cols <- expand.grid(year=1961:2011, stat=factor(c("md", "lo", "hi"), levels=c("md", "lo", "hi"))) %>% 
    arrange(year, stat) %>% 
    mutate(year_col=paste(year, stat, sep="_")) %>% 
    pull(year_col)
  col_names <- c("iso3", "country", "blank", year_cols)
  length(col_names)
  
  # Read data
  fdata <- read.csv(file.path(inputdir, "nutrient_sum", x), as.is=T, skip=3, col.names=col_names, na.strings="*") %>% 
    select(-blank) %>% 
    gather(key="key", value="value", 3:ncol(.)) %>% 
    mutate(year=substr(key, 2, 5) %>% as.numeric(),
           stat=substr(key, 7, 9), 
           nutrient=nutrient,
           units=units) %>% 
    select(iso3, country, nutrient, units, year, stat, value) %>% 
    arrange(iso3, year)
  
  
})

# Inspect nutrients
table(data_orig$nutrient)

# Format data
data <- data_orig %>% 
  # Format nutrient
  mutate(nutrient=str_to_title(nutrient)) %>% 
  mutate(nutrient=recode(nutrient, 
                         "B6"="Vitamin B6",
                         "Dietaryfiber"="Dietary fiber",
                         "Monounsatfa"="Monounsaturated fatty acids",
                         "Polyunsatfa"="Polyunsaturated fatty acids",
                         "Saturatedfa"="Saturated fatty acids",
                         "Vitamina"="Vitamin A",
                         "Vitaminc"="Vitamin C")) %>% 
  # Add nutrient type
  left_join(nutrient_key %>% select(nutrient, nutrient_type), by="nutrient") %>% 
  # Format units
  rename(units_long=units) %>% 
  mutate(units_short=gsub("/person/day", "", units_long)) %>% 
  # Add label 
  mutate(nutrient_label=paste0(nutrient, " (", units_short, ")")) %>% 
  # Rearrange
  select(iso3, country, nutrient_type, nutrient_label, nutrient, units_long, units_short, year, everything()) %>% 
  # Reshape
  spread(key="stat", value="value") %>% 
  rename(value_med=md, value_lo=lo, value_hi=hi) %>% 
  select(iso3, country, nutrient_type, nutrient_label, nutrient, units_long, units_short, year, value_med, value_lo, value_hi, everything())

# Inspect nutrients
sort(unique(data$nutrient))
freeR::complete(data)

# Example plot
cntry <- "Ghana"
g <- ggplot(data %>% filter(country==cntry), aes(x=year, y=value_med)) +
  geom_line() +
  geom_ribbon(aes(ymin=value_lo, ymax=value_hi), alpha=0.2) +
  facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
  labs(x="", y="Daily per capita supply") +
  theme_bw()
g

# Export
saveRDS(data, file=file.path(outputdir, "genus_nutrient_supplies_by_cntry_year.Rds"))


################################################################################
# 6. Nutrient Supplies by Food and Country (2011)
################################################################################

# Description: Nutrients supplied by each of 225 foods in most recent year with 
# available data (2011), separated by each of 23 individual nutrients. Nutrients 
# covered are: calories, protein, fat, carbohydrates, vitamin C, vitamin A, folate, 
# calcium, iron, zinc, potassium, dietary fiber, copper, sodium, phosphorus, 
# thiamin, riboflavin, niacin, B6, magnesium, saturated fatty acids, monounsaturated
# fatty acids, polyunsaturated fatty acids. (2015-12-14). 

# Notes: Does not include fortification. Countries marked with a "*" have 
# insufficient data to estimate nutrient supplies. Each file includes a median 
# and 95% uncertainty intervals for each year, calculated from 1,000 monte carlo 
# simulations of potential nutrients supplied by the foods available in each country.

# Files
files_merge <- list.files(file.path(inputdir, "nutrients_by_foods"), pattern=".csv")

# Merge
# x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  
  # Get nutrient name
  nutrient <- x %>% gsub("NutrientsByFood_2011_", "", .) %>% gsub(".csv", "",.) %>% substr(., 3, nchar(.))
  
  # Get nutrient units
  units <- read.csv(file.path(inputdir, "nutrients_by_foods", x), as.is=T, nrows=3) %>% 
    slice(2) %>% select(4:ncol(.)) %>% as.character() %>% unique()
  
  # Get column names
  header_info <-  read.csv(file.path(inputdir, "nutrients_by_foods", x), as.is=T, nrows=2, header=F, na.strings="*") %>% 
    select(4:ncol(.)) %>% t() %>% 
    as.data.frame() %>% 
    mutate(col_name=paste(V1, V2, sep="-"))
  col_names <- c("iso3", "country", "blank", header_info$col_name)
  
  # Read data
  fdata <- read.csv(file.path(inputdir, "nutrients_by_foods", x), as.is=T, skip=3, na.strings="*", col.names = col_names) %>% 
    mutate(file=x,
           nutrient=nutrient,
           units_long=units) %>% 
    # Remove blank column and arrange
    select(-blank) %>% 
    select(file, nutrient, units_long, everything()) %>% 
    # Convert wide to long
    gather(key="food", value="value", 6:ncol(.)) %>% 
    # Arrange
    arrange(iso3, food)
  
})

# Build key to convert messy food column names to nice food
food_key <-  read.csv(file.path(inputdir, "nutrients_by_foods", files_merge[1]), as.is=T, nrows=1, header=F, na.strings="*") %>% 
  select(4:ncol(.)) %>% t() %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  rename(food_name_clean=V1) %>% 
  mutate(food_name_messy=gsub("[[:punct:]]| ", ".", food_name_clean))

# Format data
data <- data_orig %>% 
  # Add stat column
  mutate(stat=ifelse(grepl("Median", food),"median",
                     ifelse(grepl("High.95", food), "high", "low"))) %>% 
  # Format food column
  rename(food_name_messy=food) %>% 
  mutate(food_name_messy=gsub(".High.95..UI|.Low.95..UI|.Median", "", food_name_messy)) %>% 
  # Format nutrient
  mutate(nutrient=str_to_title(nutrient),
         nutrient=recode(nutrient, 
                         "B6"="Vitamin B6",
                         "Dietaryfiber"="Dietary fiber",
                         "Monounsatfa"="Monounsaturated fatty acids",
                         "Polyunsatfa"="Polyunsaturated fatty acids",
                         "Saturatedfa"="Saturated fatty acids",
                         "Vitamina"="Vitamin A",
                         "Vitaminc"="Vitamin C")) %>% 
  # Add nutrient type
  left_join(nutrient_key %>% select(nutrient, nutrient_type), by="nutrient") %>% 
  # Add short units
  mutate(units_short=gsub("/person/day", "", units_long)) %>% 
  # Reshape values
  spread(key="stat", value="value") %>% 
  rename(value_med=median, value_lo=low, value_hi=high) %>% 
  # Add clean food name
  left_join(food_key, by="food_name_messy") %>% 
  # Rearrange columns
  select(-c(file, food_name_messy)) %>%
  rename(food=food_name_clean) %>% 
  select(iso3, country, nutrient_type, nutrient, units_long, units_short, 
         food, value_med, value_lo, value_hi, everything())

# Inspect
sort(unique(data$nutrient))
sort(unique(data$food))
freeR::complete(data)

# Export
saveRDS(data, file=file.path(outputdir, "genus_nutrient_supplies_by_cntry_food_2011.Rds"))


################################################################################
# 7. Total Nutrient Supply including Fortification (2011)
################################################################################

# Description: Total nutrients supplied by the diet in 2011, including those 
# contributed via fortification. Includes data for 175 countries and 23 nutrients: 
# calories, protein, fat, carbohydrates, vitamin C, vitamin A, folate, calcium, 
# iron, zinc, potassium, dietary fiber, copper, sodium, phosphorus, thiamin, 
# riboflavin, niacin, B6, magnesium, saturated fatty acids, monounsaturated 
# fatty acids, polyunsaturated fatty acids. 

# Notes: Countries marked with a "*" have insufficient data to estimate nutrient 
# supplies. Includes median and 95% uncertainty intervals for each year, calculated 
# from 1,000 monte carlo simulations of potential nutrients supplied by the foods 
# available in each country. Added nutrients through fortification are calculated 
# based on mandatory and voluntary fortification guidelines, and estimates of foods 
# that are industrially processed/milled to which fortificants may be added.

# Get column names
header_info <- read.csv(file.path(inputdir, "NutrientsFortification_2011.csv"), 
                        as.is=T, na.strings="*", nrows=3, header=F) %>% 
  select(4:ncol(.)) %>% t() %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  rename(nutrient=V1, stat=V2, units=V3) %>% 
  mutate(stat=recode(stat, "High 95% UI"="hi", "Low 95% UI"="lo", "Median"="md"),
         col_name=paste(nutrient, stat, sep="_"))
col_names <- c("iso3", "country", "blank", header_info$col_name)

# Read data
data_orig <- read.csv(file.path(inputdir, "NutrientsFortification_2011.csv"), as.is=T, na.strings="*", skip=3, col.names = col_names)

# Format data
data <- data_orig %>% 
  # Remove blank
  select(-blank) %>% 
  # Wide to long
  gather(key="key", value="value", 3:ncol(.)) %>% 
  # Add nutrient and stat column
  mutate(nutrient=gsub("_md|_lo|_hi", "", key),
         stat=ifelse(grepl("_md", key), "median", 
                     ifelse(grepl("_hi", key), "high", "low"))) %>% 
  # Arrange
  select(iso3, country, nutrient, stat, value) %>% 
  spread(key="stat", value="value") %>% 
  rename(value_med=median, value_lo=low, value_hi=high) %>% 
  # Format nutrients and add units
  left_join(header_info %>% select(nutrient, units) %>% unique(), by="nutrient") %>% 
  mutate(nutrient=str_to_title(nutrient),
         nutrient=recode(nutrient, 
                         "B6"="Vitamin B6",
                         "Dietaryfiber"="Dietary fiber",
                         "Monounsatfa"="Monounsaturated fatty acids",
                         "Polyunsatfa"="Polyunsaturated fatty acids",
                         "Saturatedfa"="Saturated fatty acids",
                         "Vitamina"="Vitamin A",
                         "Vitaminc"="Vitamin C")) %>% 
  # Add nutrient type
  left_join(nutrient_key %>% select(nutrient, nutrient_type), by="nutrient") %>% 
  # Format units
  rename(units_long=units) %>% 
  mutate(units_short=gsub("/person/day", "", units_long)) %>% 
  # Arrange
  select(iso3, country, nutrient_type, nutrient, units_long, units_short, everything())

# Inspect
str(data)

# Export
saveRDS(data, file=file.path(outputdir, "genus_nutrient_supplies_incl_fortification_by_cntry_2011.Rds"))



################################################################################
# 8. Edible Food by Country and Year
################################################################################

# Description: Edible food supplies for 225 individual food types in grams per 
# person per day by country, separated by year (1961-2011).

# Notes: Countries with data marked with a "*" have insufficient data to approximate 
# their food supplies. For modern countries that were once part of another country 
# (e.g., Armenia in USSR), older values reflect those of their predecessor.

# Files
files_merge <- list.files(file.path(inputdir, "edible_food_by_year"), pattern=".csv")

# Example data
exdata <- read.csv(file.path(inputdir, "edible_food_by_year", files_merge[1]), as.is=T, na.strings="*")

# Build key to convert messy food column names to nice food
food_key <-  read.csv(file.path(inputdir, "edible_food_by_year", files_merge[1]), as.is=T, nrows=1, header=F, na.strings="*") %>% 
  select(4:ncol(.)) %>% t() %>% 
  as.data.frame(stringsAsFactors=F) %>% 
  rename(food_name_clean=V1) %>% 
  mutate(food_name_messy=gsub("[[:punct:]]| ", ".", food_name_clean))
  
# Merge
# x <- files_merge[1]
data_orig <- purrr::map_df(files_merge, function(x) {
  colnames <-  colnames(read.csv(file.path(inputdir, "edible_food_by_year", x), as.is=T))
  fdata <- read.csv(file.path(inputdir, "edible_food_by_year", x), as.is=T, skip=2, col.names = colnames, na.strings="*") %>% 
    mutate(file=x) %>% 
    select(file, everything())
})

# Format
data <- data_orig %>% 
  # Add year column
  mutate(year=file %>% gsub("edible_food_", "", .) %>% gsub(".csv", "", . ) %>% as.numeric()) %>% 
  select(file, year, everything()) %>% 
  # Rename country columns
  rename(iso3=X, country=X.1) %>% 
  # Remove file column and blank column
  select(-c(file, FOOD)) %>% 
  # Rearrange and convert wide-to-long
  select(iso3, country, year, everything()) %>% 
  gather(key="food", value="value", 4:ncol(.)) %>% 
  # Format food name
  rename(food_name_messy=food) %>% 
  left_join(food_key, by="food_name_messy") %>% 
  rename(food=food_name_clean) %>% 
  select(-food_name_messy) %>% 
  # Arrange
  select(iso3, country, year, food, value) %>% 
  arrange(iso3, food, year) %>% 
  rename(g_person_day=value)
  
# Inspect
str(data)
freeR::complete(data)

# Export
saveRDS(data, file=file.path(outputdir, "genus_edible_food_by_cntry_year.Rds"))



################################################################################
# Dataverse method
################################################################################

# Dataverse package
# devtools::install_github("iqss/dataverse-client-r")
# library(dataverse)
# Sys.setenv("DATAVERSE_SERVER" = "dataverse.harvard.edu")
# genus <- get_dataverse("GENuS")
# genus_data <- dataverse_contents(genus)
# genus_data
# d1 <- get_dataset("https://doi.org/10.7910/DVN/GNFVTT")
# f <- get_file(file="FCT_GENuS_ASEANFoods_v2.tab", dataset="https://doi.org/10.7910/DVN/GNFVTT")
# f1 <- DDIwR::convert(from=f, "R")
# dataset <- get_dataset("https://doi.org/10.7910/DVN/GNFVTT")
# data <- get_file("FCT_GENuS_ASEANFoods_v2.tab", "https://doi.org/10.7910/DVN/GNFVTT", format="RData")
