

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# Directories
genusdir <- "data/genus/processed"
usdietdir <- "data/us_dietary_guidelines"
vaitladir <- "data/vaitla_etal_2018/processed"
appdatadir <- "shiny/v2/data"

# Read US dietary guidelines
load(file.path(usdietdir, "2015_2020_US_dietary_guidelines.Rdata"))
diet_reqs <- data
diet_reqs_catg <- data_catg
rm(data, data_catg)

# Nutrient supply by country and year
nut_cntry_yr_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_cntry_year.Rds"))
nut_cntry_age_sex_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))
food_cntry_yr_orig <- readRDS(file.path(genusdir, "genus_edible_food_by_cntry_year.Rds"))
nut_cntry_food_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_cntry_food_2011.Rds"))

# Read Vaitla et al. (2018) nutrition predictions
load(file.path(vaitladir, "Vaitla_etal_2018_nutrient_data.Rdata"))

# Format Vaitla
################################################################################

# Format Vaitla predictions
vaitla_nutr_preds_long <- nutrient_preds_long %>% 
  # Proportion of maximum
  group_by(nutrient) %>% 
  mutate(pmax_fill=value_md_fill/max(value_md_fill, na.rm=T)) %>% 
  ungroup() %>% 
  # Species label
  mutate(species_label=ifelse(!is.na(comm_name), 
                              paste0(comm_name, " (", species, ")"), species))

# Format Vaitla nutrient key
vaitla_nutr_key <- nutrient_key


# Compare units
################################################################################

# GENuS units
genus_units <- nut_cntry_yr_orig %>% 
  select(nutrient, units_short) %>% 
  unique() %>% 
  rename(units_genus=units_short)

# US dietary guideline units
us_diet_units <- diet_reqs %>% 
  select(nutrient, units) %>% 
  unique() %>% 
  rename(units_dreqs=units)

# Compare units
units_check <- genus_units %>% 
  left_join(us_diet_units)

# Age groups
as.character(sort(unique(diet_reqs$age_range)))

# Build age matching key
age_range_key <- tibble(age_range_genus=levels(nut_cntry_age_sex_2011_orig$age_range)) %>% 
  mutate(age_range_usdiet=recode(age_range_genus,
                                 "0-4"="1-3",            
                                 "5-9"="4-8",                
                                 "10-14"="9-13",   
                                 "15-19"="14-18",              
                                 "20-24"="19-30",              
                                 "25-29"="19-30",              
                                 "30-34"="31-50",              
                                 "35-39"="31-50",              
                                 "40-44"="31-50",
                                 "45-49"="31-50",
                                 "50-54"="51+",              
                                 "55-59"="51+",              
                                 "60-64"="51+",              
                                 "65-69"="51+",              
                                 "70-74"="51+",              
                                 "75-79"="51+",              
                                 "80+"="51+"))


# Label nutrients as in Vaitla and not in Vaitla
################################################################################

# Vaitla et al. (2018) nutrients
sort(unique(nut_cntry_yr_orig$nutrient))
nutrients_vaitla <- c("Protein", "Fat", "Iron", "Zinc", "Vitamin A", "Polyunsaturated fatty acids")
  
# Format
nut_cntry_yr <- nut_cntry_yr_orig %>% 
  # Add Vaitla classification
  mutate(vaitla_yn=ifelse(nutrient %in% nutrients_vaitla, "yes", "no"))

# Format
nut_cntry_age_sex_2011 <- nut_cntry_age_sex_2011_orig %>% 
  # Add Vaitla classification
  mutate(vaitla_yn=ifelse(nutrient %in% nutrients_vaitla, "yes", "no")) #%>% 
  # # Add US dietary guideline
  # left_join(age_range_key, c("age_range"="age_range_genus")) %>% 
  # left_join(diet_reqs %>% 
  #             select(nutrient, age_range, sex, value), 
  #           c("nutrient", "age_range_usdiet"="age_range", "sex")) %>% 
  # # Rename
  # rename(diet_rec=value)
  


# Build data: proportion of diet from seafood
################################################################################

# All food groups
food_groups <- food_cntry_yr_orig %>% 
  select(food) %>% 
  unique()

# Marine food groups
marine_seafood_groups <- c("Crustaceans", 
                           "Molluscs; Other",
                           "Demersal Fish", 
                           "Pelagic Fish",
                           "Marine Fish; Other",
                           "Fish; Body Oil",
                           "Fish; Liver Oil")

# Other aquatic food groups
other_seafood_groups <- c("Aquatic Plants", 
                          "Aquatic Animals; Others", 
                          "Freshwater Fish")

# Build data
pdiet_seafood_cntry_yr <- food_cntry_yr_orig %>% 
  # Classify food group as seafood or not-seafood
  mutate(seafood=ifelse(food %in% marine_seafood_groups, "yes", "no")) %>% 
  # Summarize contribution of seafood vs. non-seafood
  group_by(iso3, country, year) %>% 
  summarize(total_g_person_day=sum(g_person_day, na.rm=T),
          seafood_g_person_day=sum(g_person_day[seafood=="yes"], na.rm=T)) %>% 
  mutate(prop_seafood=seafood_g_person_day/total_g_person_day)

# Example plot
cntry <- "Ghana"
g <- ggplot(pdiet_seafood_cntry_yr %>% filter(country==cntry), aes(x=year, y=prop_seafood)) +
  geom_line() +
  labs(x="", y="Proportion of diet\nfrom marine seafood") +
  theme_bw()
g  

# Build data: proportion of nutrients from seafood
################################################################################

# Build data
pnutrient_seafood_cntry_2011 <- nut_cntry_food_2011_orig %>% 
  # Classify food group as seafood or not-seafood
  mutate(seafood=ifelse(food %in% marine_seafood_groups, "yes", "no")) %>% 
  # Summarize contribution of seafood vs. non-seafood
  group_by(iso3, country, nutrient, units_long, units_short) %>% 
  summarize(total_amt_person_day=sum(value_med, na.rm=T),
            seafood_amt_person_day=sum(value_med[seafood=="yes"], na.rm=T)) %>% 
  mutate(prop_seafood=seafood_amt_person_day/total_amt_person_day)
  

# Example plot
cntry <- "Ghana"
g <- ggplot(pnutrient_seafood_cntry_2011 %>% filter(country==cntry), 
            aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
  geom_bar(stat="identity") +
  labs(x="", y="Proportion of nutrient intake\nfrom marine seafood") +
  coord_flip() +
  theme_bw()
g  

# Export data
################################################################################

# Export data
saveRDS(nut_cntry_yr, file=file.path(appdatadir, "genus_nutrient_supplies_by_cntry_year.Rds"))
saveRDS(nut_cntry_age_sex_2011, file=file.path(appdatadir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))
saveRDS(pdiet_seafood_cntry_yr, file=file.path(appdatadir, "genus_pdiet_seafood_by_cntry_year.Rds"))
saveRDS(pnutrient_seafood_cntry_2011, file=file.path(appdatadir, "genus_pnutrient_seafood_by_cntry_2011.Rds"))
save(vaitla_nutr_preds_long, vaitla_nutr_key, file=file.path(appdatadir, "vaitla_etal_2018_finfish_nutrient_data.Rdata"))


