

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(countrycode)

# Directories
datadir <- "data/gnd/data"


# Format data
################################################################################


# Read food
food_orig <- readxl::read_excel(file.path(datadir, "nutrient_data_4_chris.xlsx"))

# Inspect
colnames(food_orig)
table(food_orig$products)

# Country key
country_key <- food_orig %>% 
  select(countries) %>% 
  rename(country_orig=countries) %>% 
  unique() %>% 
  mutate(iso3=countrycode(country_orig, "country.name", "iso3c"),
         country=countrycode(iso3, "iso3c", "country.name"))

# Nutrient key
nutr_key <- food_orig %>% 
  select(elements) %>% 
  rename(nutrient_orig=elements) %>% 
  unique()

# Format data
food <- food_orig %>% 
  # Rearrange
  select(countries, products, elements, country_codes, product_codes, ele_codes, everything()) %>% 
  # Rename columns
  rename(country_orig=countries, product=products, nutrient_orig=elements,
         country_code=country_codes, product_code=product_codes, nutrient_code=ele_codes) %>% 
  # Gather
  gather(key="year", value="value", 7:ncol(.)) %>% 
  # Format year column
  mutate(year=gsub("_", "", year) %>% as.numeric()) %>% 
  # Remove useless columns
  select(-c(product, country_code, product_code, nutrient_code)) %>% 
  # Arrange
  arrange(country_orig, nutrient_orig, year) %>% 
  # Add country info
  left_join(country_key, by="country_orig") %>% 
  mutate(country=ifelse(!is.na(country), country, country_orig)) %>% 
  # Arrange
  select(country, iso3, nutrient_orig, year, value)

# Export data
saveRDS(food, file=file.path(datadir, "GND_1961_2017_total_food_supply.Rds"))


# Format data
################################################################################

# Read data
seafood_orig <- readxl::read_excel(file.path(datadir, "fishandseafood.xlsx"))

# Inspect
colnames(seafood_orig)
table(seafood_orig$products)

# Country key
country_key <- seafood_orig %>% 
  select(countries) %>% 
  rename(country_orig=countries) %>% 
  unique() %>% 
  mutate(iso3=countrycode(country_orig, "country.name", "iso3c"),
         country=countrycode(iso3, "iso3c", "country.name"))

# Format data
seafood <- seafood_orig %>% 
  # Rearrange
  select(countries, products, elements, country_codes, product_codes, ele_codes, everything()) %>% 
  # Rename columns
  rename(country_orig=countries, product=products, nutrient_orig=elements,
         country_code=country_codes, product_code=product_codes, nutrient_code=ele_codes) %>% 
  # Gather
  gather(key="year", value="value", 7:ncol(.)) %>% 
  # Format year column
  mutate(year=gsub("_", "", year) %>% as.numeric()) %>% 
  # Remove useless columns
  select(-c(product, country_code, product_code, nutrient_code)) %>% 
  # Arrange
  arrange(country_orig, nutrient_orig, year) %>% 
  # Add country info
  left_join(country_key, by="country_orig") %>% 
  mutate(country=ifelse(!is.na(country), country, country_orig)) %>% 
  # Arrange
  select(country, iso3, nutrient_orig, year, value)

# Export data
saveRDS(seafood, file=file.path(datadir, "GND_1961_2017_sea_food_supply.Rds"))

