
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/nutrient_content/processed"
fishdir <- "data/fish_projections/data"
plotdir <- "figures"
  
# Read FAO data
fao_orig <- read.csv(file="/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2016_FAO_capture_landings.csv", as.is=T)  

# Read nutrient data
load(file.path(datadir, "Vaitla_etal_2018_nutrient_data.Rdata"))


# Build data
################################################################################

# Calculate MT of nutrient from MT of edible meat 
# Units: g/100g, mg/100g, ug/100g
# For testing: edible_mt <- 1000; nutrient_conc <- 0.7176820; units <- "g/100g"
calc_nutrient_mt <- function(edible_mt, nutrient_conc, units){
  # Convert concentration to g/100g (which is equivalent to mt/100mt)
  if(units=="g/100g"){nutrient_g <- nutrient_conc}
  if(units=="mg/100g"){nutrient_g <- nutrient_conc / 1e3}
  if(units=="ug/100g"){nutrient_g <- nutrient_conc / 1e6}
  # Calculate MT of nutrient
  nutrient_mt <- edible_mt / 100 * nutrient_g
  return(nutrient_mt)
}

# Build data
stock_ts <- fao_orig %>% 
  # Reduce to species with nutrient data
  filter(area_type=="marine" & units=="t" & species %in% preds_wide$species) %>% 
  # Reduce columns
  select(-c(area_type, units, symbol)) %>% 
  rename(catch_mt=quantity) %>% 
  # Add proportion edible meat
  mutate(edible_mt=catch_mt * 0.87) %>% 
  # Add nutrient concentrations
  left_join(preds_wide, by="species") %>% 
  # Calculate MT of nutrients
  mutate(protein_mt=calc_nutrient_mt(edible_mt, protein_g, "g/100g"),
         fat_mt=calc_nutrient_mt(edible_mt, fat_g, "g/100g"),
         omega3_mt=calc_nutrient_mt(edible_mt, omega3_g, "g/100g"),
         omega6_mt=calc_nutrient_mt(edible_mt, omega6_g, "g/100g"), 
         iron_mt=calc_nutrient_mt(edible_mt, iron_mg, "mg/100g"),
         zinc_mt=calc_nutrient_mt(edible_mt, zinc_mg, "mg/100g"),
         vitA_mt=calc_nutrient_mt(edible_mt, vitA_ug, "ug/100g"),
         vitB12_mt=calc_nutrient_mt(edible_mt, vitB12_ug, "ug/100g"),
         vitD_mt=calc_nutrient_mt(edible_mt, vitD_ug, "ug/100g")) %>% 
  ungroup()

# Nutrients by country
country_ts <- stock_ts %>% 
  select(iso3, country, year, catch_mt, edible_mt, protein_mt:vitD_mt) %>% 
  group_by(iso3, country, year) %>% 
  summarize_all(sum, na.rm=T)

# Export data
saveRDS(country_ts, file=file.path(fishdir, "country_nutrient_time_series_historic.Rds"))
# saveRDS(stock_ts, file=file.path(fishdir, "country_stock_nutrient_time_series_historic.Rds"))



















