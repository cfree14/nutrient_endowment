
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
  
# Read MSY data
load("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/gaines_eez_msy_time_series.Rdata")

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
stock_ts <- msy_ts %>% 
  # Reduce to species with nutrient data
  filter(species %in% preds_wide$species) %>% 
  # Calculate MSY total by RCP-country-species
  group_by(rcp, sovereign_iso3, sovereign, species_id, species, comm_name, comm_name1, year) %>% 
  summarize(msy_mt=sum(msy_eez, na.rm=T)) %>% 
  # Add proportion edible meat
  mutate(edible_mt=msy_mt * 0.87) %>% 
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
  select(-c(species_id, species, comm_name, comm_name1)) %>% 
  group_by(rcp, sovereign_iso3, sovereign, year) %>% 
  summarize_all(sum, na.rm=T)

# Export data
saveRDS(country_ts, file=file.path(fishdir, "country_nutrient_time_series.Rds"))
saveRDS(stock_ts, file=file.path(fishdir, "country_stock_nutrient_time_series.Rds"))



















