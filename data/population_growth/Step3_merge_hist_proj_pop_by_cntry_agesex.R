

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(wbstats)
library(countrycode)

# Directories
datadir <- "data/population_growth/processed"

# Read data 
data_hist_orig <- readRDS(file=file.path(datadir, "WB_1960_2019_population_size_by_country_agesex.Rds"))
data_proj_orig <- readRDS(file=file.path(datadir, "UN_WPP_2019_pop_proj_by_cntry_agesex.Rds"))


# Format data
################################################################################

# Format historical
data_hist <- data_hist_orig %>% 
  mutate(source="WB historical") %>% 
  select(source, country_use, iso3_use, year, sex, age, pop_size) %>% 
  rename(country=country_use, iso3=iso3_use, age_range=age, pop_size_50perc=pop_size)

# Format projection
data_proj <- data_proj_orig %>% 
  mutate(source="UN-WPP projections") %>% 
  select(source, country_use, iso3_use, year, sex, age_range, pop_size_05perc:pop_size_95perc) %>% 
  rename(country=country_use, iso3=iso3_use)

# Merge projections
data <- bind_rows(data_proj, data_hist) %>% 
  arrange(country, iso3, year, sex, age_range)


# Export data
################################################################################

# Save
saveRDS(data, file=file.path(datadir, "WB_UN_1960_2100_human_population_by_country_agesex.Rds"))

