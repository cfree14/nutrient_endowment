
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "output"
codedir <- "code/calc_nutr_deficiencies/functions"
genusdir <- "data/genus/processed"
plotdir <- "code/calc_nutr_deficiencies/figures"

# Read helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Read data
data_orig <- readRDS(file.path(datadir, "GENUS_EAR_population_data_merged.Rds"))

# Read food supply (w/ fortification) by country, age, and sex
edible_mat <- readRDS(file.path(genusdir, "genus_nutrient_supplies_w_fort_by_age_sex_2011.Rds"))

# Read food supply (w/ fortification) by country, age, and sex
food_cntry_age_sex_2011 <- readRDS(file.path(genusdir, "genus_edible_food_by_age_sex_2011.Rds"))

# Read zinc/phyate concentrations by food
zinc_phyate_conc <- read.csv(file.path(codedir, "zinc_phyate_concentrations_by_food.csv"), as.is=T)


# Perform analysis
################################################################################

# Menstruation ages
mn_ages <- c("10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54")

# Calculate proportion deficient
data <- data_orig %>% 
  # Reduce to country/age/groups with data
  filter(!is.na(supply_med) & !is.na(ear)) %>% 
  # Add menstruating/non-menstruating
  mutate(menstruation=ifelse(sex=="Women" & age %in% mn_ages, "menstruating", "non-menstruating")) %>% 
  select(iso3:age, menstruation, everything()) %>% 
  # Calculate nutrient deficiencies
  group_by(nutrient) %>% 
  mutate(pdeficient=calc_nutr_deficiency(nutrient=unique(nutrient), 
                                         supply_vec=supply_med, ear_vec=ear, 
                                         mn_vec=menstruation,
                                         cv=0.25),
         phealthy=1-pdeficient,
         ndeficient=pdeficient*pop_size,
         nhealthy=phealthy*pop_size)

# For testing
exdata <- data_orig %>% 
  filter(country=="Bangladesh" & nutrient=="Zinc")
supply_vec <- exdata$supply_med
nutrient <- "Zinc"
ear_vec <- exdata$ear



# Plot results
################################################################################


country_do <- "Madagascar"
g1 <- plot_barplot_supply_ear(data, country=country_do, plotdir=plotdir)
g2 <- plot_raster_supply_ear_perc(data, country=country_do, plotdir=plotdir)
g3 <- plot_raster_perc_deficient(data, country=country_do, plotdir=plotdir)
g4 <- plot_barplot_perc_deficient(data, country=country_do, plotdir=plotdir)
g5 <- plot_barplot_n_deficient(data, country=country_do, plotdir=plotdir)






