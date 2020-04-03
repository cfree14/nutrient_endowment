

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
outdir <- "output"
genusdir <- "data/genus/processed"

# Read Free et al. (2020) catch projections
data <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/gaines_data_for_eez_analysis.Rds")

# Read Free et al. (2020) species key
spp_key <- read.csv(file.path(outdir, "Gaines_species_nutrient_content_key.csv"), as.is=T)

# Make sure that all species in data are also in key
data_spp <- sort(unique(data$species))
data_spp[!data_spp%in%spp_key$species_orig] # character(0) if all present


# Step 1. Format FAO data
################################################################################

# Scenarios
unique(data$scenario)

# Format data
data1 <- data %>% 
  # Reduce to scenarios of interest
  filter(scenario %in% c("No Adaptation", "Full Adaptation")) %>% 
  # Reduce to years of interest
  filter(year %in% c(2012, seq(2015,2100,5)))

# Add GENUS group type
data2 <- data1 %>% 
  # Add GENUS food group
  left_join(select(spp_key, species_orig, genus_food_name), by=c("species"="species_orig")) %>% 
  # Sum outcomes by SCENARIO-RCP-COUNTRY-FOOD GROUP-YEAR
  group_by(scenario, rcp, sovereign_iso3, sovereign, country_iso3, country, genus_food_name, year) %>% 
  summarize(msy_mt=sum(msy_eez, na.rm=T), 
            catch_mt=sum(harvest_eez, na.rm=T),
            profits_usd=sum(profit_eez, na.rm=T)) %>% 
  # Format RCP scenario name
  mutate(rcp=recode(rcp, 
                    "RCP26" = "RCP 2.6",
                    "RCP45" = "RCP 4.5",
                    "RCP60" = "RCP 6.0",
                    "RCP85" = "RCP 8.5")) %>% 
  # Ungroup
  ungroup()


#  Plot example results
################################################################################

# Plot results for an example country
cntry <- "Canada"
sdata <- data2 %>% 
  filter(country==cntry)

# Plot
g <- ggplot(sdata, aes(x=year, y=catch_mt/1e3, fill=genus_food_name)) +
  facet_grid(rcp ~ scenario) +
  geom_area() +
  labs(x="Year", y="Catch (1000s mt)") +
  theme_bw()
g

# Export
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "Free_etal_catch_proj_by_scen_rcp_cntry_genus_group.Rds"))



