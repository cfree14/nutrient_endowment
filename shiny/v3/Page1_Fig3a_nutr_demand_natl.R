
# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
data <- readRDS(file.path(datadir, "1960_2100_nutrient_demand_by_country.Rds"))

# Test country
country <- "Ghana"; nutrient <- "Vitamin A"

# Plot population growth data
plot_nutr_demand <- function(data, country, nutrient){

  # Subset data
  nutr_do <- nutrient
  country_do <- country
  data_hist <- data %>% 
    filter(type=="Historical" & country==country_do & nutrient==nutr_do)
  data_proj <- data %>% 
    filter(type=="UN-WPP projections" & country==country_do  & nutrient==nutr_do)
  
  # Plot data
  g <- ggplot() +
    # Add historical
    geom_line(data=data_hist, mapping=aes(x=year, y=supply_req_mt_yr_50perc)) +
    # Add projection
    geom_ribbon(data=data_proj, mapping=aes(x=year, ymin=supply_req_mt_yr_05perc, ymax=supply_req_mt_yr_95perc), alpha=0.2, fill="red") +
    geom_line(data=data_proj, mapping=aes(x=year, y=supply_req_mt_yr_50perc), color="red") +
    # Labels
    labs(x="", y="Annual nutrient supply (Mg)\nrequired to eliminate deficiencies") +
    scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20)) +
    # Theme
    theme_bw() + 
    theme(axis.title.x = element_blank())
  g
  
}

# Test function
plot_nutr_demand(data=data, country="Canada", nutrient="Riboflavin")








