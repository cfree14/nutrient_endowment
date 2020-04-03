

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
outdir <- "output"
popdir <- "data/population_growth/processed"

# Read FAO data
fao_orig <- readRDS(file.path(outdir, "FAO_1950_2017_landings_by_country_to_use.Rds"))
fao_spp_key <- read.csv(file.path(outdir, "FAO_species_nutrient_content_key.csv"), as.is=T)

# Read GENUS data
genus <- read.csv(file=file.path(outdir, "GENUS_nutrient_per_100g_by_seafood_group.csv"), as.is=T)

# Read projected human population growth
pop_growth <- readRDS(file.path(popdir, "WB_UN_1960_2100_human_population_by_country.Rds"))


# Format data
################################################################################

# Build data
fao <- fao_orig %>% 
  # Step 1. Sum by GENUS group
  group_by(country, iso3, genus_group, year) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T),
            meat_mt=sum(meat_mt, na.rm=T)) %>% 
  # Step 2. Add human population size
  left_join(pop_growth %>% select(iso3, year, pop_size_50perc), by=c("iso3", "year")) %>% 
  rename(human_pop_size=pop_size_50perc) %>% 
  # Step 3. Calculate daily per capita meat availability
  mutate(meat_g_dcap= (meat_mt*1000*1000) / 365 / human_pop_size) %>% 
  # Step 4. Calculate daily per capita nutrient availability
  left_join(genus, by=c("genus_group"="genus_food_name")) %>% 
  mutate(calcium_mg_dcap = meat_g_dcap * calcium_mg / 100,
         calories_kcal_dcap = meat_g_dcap * calories_kcal / 100,
         carbohydrates_g_dcap = meat_g_dcap * carbohydrates_g / 100,
         copper_mg_dcap = meat_g_dcap * copper_mg / 100,
         fat_g_dcap = meat_g_dcap * fat_g / 100,
         folate_mcg_dcap = meat_g_dcap * folate_mcg / 100,
         iron_mg_dcap = meat_g_dcap * iron_mg / 100,
         magnesium_mg_dcap = meat_g_dcap * magnesium_mg / 100,
         monounsaturated_fatty_acids_g_dcap = meat_g_dcap * monounsaturated_fatty_acids_g / 100,
         niacin_mg_dcap = meat_g_dcap * niacin_mg / 100,
         phosphorus_mg_dcap = meat_g_dcap * phosphorus_mg / 100,
         polyunsaturated_fatty_acids_g_dcap = meat_g_dcap * polyunsaturated_fatty_acids_g / 100,
         potassium_mg_dcap = meat_g_dcap * potassium_mg / 100,
         protein_g_dcap = meat_g_dcap * protein_g / 100,
         riboflavin_mg_dcap = meat_g_dcap * riboflavin_mg / 100,
         saturated_fatty_acids_g_dcap = meat_g_dcap * saturated_fatty_acids_g / 100,
         sodium_mg_dcap = meat_g_dcap * sodium_mg / 100,
         thiamin_mg_dcap = meat_g_dcap * thiamin_mg / 100,
         vitamin_a_mcg_rae_dcap = meat_g_dcap * vitamin_a_mcg_rae / 100,
         vitamin_b6_mg_dcap = meat_g_dcap * vitamin_b6_mg / 100,
         vitamin_c_mg_dcap = meat_g_dcap * vitamin_c_mg / 100,
         zinc_mg_dcap = meat_g_dcap * zinc_mg / 100) %>% 
  # Step 5. Final cleanup
  # Eliminate regions without population growth data
  filter(!is.na(human_pop_size)) %>% 
  ungroup()

# Calculate total daily per capita amounts
data_dcap <- fao %>% 
  select(-c(genus_group, human_pop_size)) %>% 
  group_by(iso3, country, year) %>% 
  summarize_all(.fun=sum)

# Make long
data_dcap_long <- data_dcap %>% 
  gather(key="key", value="dcap", 4:ncol(.))

# Export data
save(data, data_dcap, data_dcap_long,
     file=file.path(outdir, "FAO_catch_nutrient_hist_by_cntry.Rdata"))




