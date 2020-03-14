

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
appdatadir <- "shiny/v2/data"

# Nutrient supply by country and year
nut_cntry_yr_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_cntry_year.Rds"))
nut_cntry_age_sex_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))
food_cntry_yr_orig <- readRDS(file.path(genusdir, "genus_edible_food_by_cntry_year.Rds"))
nut_cntry_food_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_cntry_food_2011.Rds"))


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

# TO DO
# Calculate and plot error bars

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
# Add error bars
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
saveRDS(nut_cntry_yr_orig, file=file.path(appdatadir, "genus_nutrient_supplies_by_cntry_year.Rds"))
saveRDS(nut_cntry_age_sex_2011_orig, file=file.path(appdatadir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))
saveRDS(pdiet_seafood_cntry_yr, file=file.path(appdatadir, "genus_pdiet_seafood_by_cntry_year.Rds"))
saveRDS(pnutrient_seafood_cntry_2011, file=file.path(appdatadir, "genus_pnutrient_seafood_by_cntry_2011.Rds"))


