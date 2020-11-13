
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
demand_prop_n <- readRDS(file.path(datadir, "2050_2100_national_nutr_demand_filled_by_seafood.Rds"))
demand_prop_g <- readRDS(file.path(datadir, "2050_2100_global_nutr_demand_filled_by_seafood.Rds"))


# Plot data
################################################################################

# Test global: data <- demand_prop_g; level <- "global"; nutrient <- "Folate"
# Test national: data <- demand_prop_n; level <- "national"; country <- "United States"; nutrient <- "Folate"
plot_demand_prop <- function(data, level, country, nutrient){
  
  # Subset data
  nutrient_do <- nutrient
  if(level=="global"){
    sdata <- data %>% 
      filter(nutrient == nutrient_do)
  }else{
    sdata <- data %>% 
      filter(nutrient == nutrient_do)
  }
  
  # Plot data
  g <- ggplot(sdata, aes(x=scenario, y=demand_prop, fill=sector)) +
    facet_grid(~year) +
    geom_bar(stat="identity") +
    # Labels
    labs(x="", y="% of demand met by marine seafood") +
    # Legend
    scale_fill_discrete(name="") +
    # Theme
    theme_bw() +
    theme(legend.position = "bottom")
  g
  
}



# Build data
################################################################################

# If building
if(F){
  
  # Read data
  #############################################################
  
  # Read nutrient concentrations by group
  wc_nutr_data <- read.csv(file.path(datadir, "GENUS_nutrient_per_100g_by_seafood_group.csv"), as.is=T) %>% 
    # Reduce to nutrients of interest
    select("genus_food_name", "calcium_mg", "copper_mg", "folate_mcg", "iron_mg", "magnesium_mg", "niacin_mg", "phosphorus_mg",
           "riboflavin_mg", "thiamin_mg", "vitamin_a_mcg_rae", "vitamin_b6_mg", "vitamin_c_mg", "zinc_mg") %>% 
    # Reduce to codes of interest
    filter(genus_food_name %in% c("Marine Fish; Other", "Molluscs; Other")) %>% 
    # Reclassify for merging with AQ projections
    mutate(genus_food_name=recode(genus_food_name,
                                  "Marine Fish; Other"="Finfish mariculture",
                                  "Molluscs; Other"="Bivalve mariculture"))
  
  # Read demand projections
  nat_nutr_demand <- readRDS(file.path(datadir, "1960_2100_nutrient_demand_by_country.Rds")) %>% 
    # Reduce to years of interest
    filter(year %in% c(2050, 2055, 2060, 2090, 2095, 2100)) %>% 
    # Add period
    mutate(period=ifelse(year<=2060, "2051-2060", "2091-2100")) %>% 
    # Group by
    group_by(country, iso3, nutrient, period) %>% 
    summarize(supply_req_mt_yr_50perc=mean(supply_req_mt_yr_50perc, na.rm=T)) %>% 
    ungroup()
  
  # Read nutrients from capture fisheries
  wc_nutr_orig <- readRDS(file.path(datadir, "2050_2100_nutr_demand_prop_from_wc_rcp_mgmt_cntry_group.Rds"))
  
  # Read aquaculture production potential
  aq_orig <- readRDS(file.path(datadir, "national_capture_mariculture_output_merged.Rds"))
    
  # Helper functions
  #############################################################
  
  # Function to calculate mt of nutrient from mt of edible meat
  # units: mg, ug=mcg 
  # meat_mt <- 29.88111; nutr_dens <- 35.5; nutr_dens_units <- "mg"
  calc_nutr_supply_mt <- function(meat_mt, nutr_dens, nutr_dens_units){
    
    # Convert meat to grams
    meat_g <- measurements::conv_unit(meat_mt, "Mg", "g")
    
    # Calculate amount of nutrient in density units
    nutrient_q <- meat_g *  nutr_dens
    
    # Calculate amount of nutrient in metric tons
    nutrient_mt <- measurements::conv_unit(nutrient_q, nutr_dens_units, "Mg")
    
    # Return
    return(nutrient_mt)
    
  }
  
  # Build national dataset
  #############################################################
  
  # Build aquaculture RCP-country-sector-nutrient-year file
  aq_use <- aq_orig %>% 
    # Remove fisheries
    filter(sector!="Capture fisheries") %>% 
    # Remove useless columns
    select(rcp:meat_mt) %>% 
    # Add nutrient densities
    left_join(wc_nutr_data, by=c("sector"="genus_food_name")) %>% 
    # Calculate nutrient supplies
    mutate(calcium_mt=calc_nutr_supply_mt(meat_mt, calcium_mg, "mg"),
           copper_mt=calc_nutr_supply_mt(meat_mt, copper_mg, "mg"),
           folate_mt=calc_nutr_supply_mt(meat_mt, folate_mcg, "ug"),
           iron_mt=calc_nutr_supply_mt(meat_mt, iron_mg, "mg"),
           magnesium_mt=calc_nutr_supply_mt(meat_mt, magnesium_mg, "mg"),
           niacin_mt=calc_nutr_supply_mt(meat_mt, niacin_mg, "mg"),
           phosphorus_mt=calc_nutr_supply_mt(meat_mt, phosphorus_mg, "mg"),
           riboflavin_mt=calc_nutr_supply_mt(meat_mt, riboflavin_mg, "mg"),
           thiamin_mt=calc_nutr_supply_mt(meat_mt, thiamin_mg, "mg"),
           vitamin_a_mt=calc_nutr_supply_mt(meat_mt, vitamin_a_mcg_rae, "ug"),
           vitamin_b6_mt=calc_nutr_supply_mt(meat_mt, vitamin_b6_mg, "mg"),
           vitamin_c_mt=calc_nutr_supply_mt(meat_mt, vitamin_c_mg, "mg"),
           zinc_mt=calc_nutr_supply_mt(meat_mt, zinc_mg, "mg")) %>% 
    # Reduce to columns of interest
    select(rcp:period, calcium_mt:zinc_mt) %>% 
    # Reshape
    gather(key="nutrient", value="supply_mt", 8:ncol(.)) %>% 
    # Format nutrient columns
    mutate(nutrient=gsub("_mt", "", nutrient) %>% gsub("_", " ", .) %>% stringr::str_to_title()) %>% 
    # Add demand
    left_join(nat_nutr_demand %>% select(iso3, nutrient, period, supply_req_mt_yr_50perc)) %>% 
    rename(demand_mt=supply_req_mt_yr_50perc) %>% 
    mutate(demand_prop=supply_mt/demand_mt) %>% 
    # Reduce to countries with projected demands
    filter(!is.na(demand_mt)) %>% 
    # Final format
    select(rcp, scenario, country, iso3, sector, period, nutrient, supply_mt, demand_prop) %>% 
    rename(year=period) %>% 
    mutate(year=recode(year, "2051-2060"="2050", "2091-2100"="2100") %>% as.numeric())
  
  # Build capture RCP-country-sector-nutrient-year file
  wc_use <- wc_nutr_orig %>% 
    mutate(sector="Capture fisheries") %>% 
    group_by(rcp, scenario, country, iso3, sector, year, nutrient) %>% 
    summarise(supply_mt=sum(supply_mt, na.rm=T),
              demand_prop=sum(demand_prop, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(scenario=recode(scenario, "BAU"="Business-as-usual", "Reforms"="Progressive reforms"))
  
  # Merge sectors
  nat_nutr_demand_prop <- bind_rows(wc_use, aq_use)
  
  # Build global dataset
  #############################################################
  
  # Global nutrient demand
  global_nutr_demand <- nat_nutr_demand %>% 
    group_by(period, nutrient) %>% 
    summarize(demand_mt=sum(supply_req_mt_yr_50perc)) %>% 
    ungroup() %>% 
    rename(year=period) %>% 
    mutate(year=recode(year, "2051-2060"="2050", "2091-2100"="2100") %>% as.numeric())
  
  # Build global
  global_nutr_demand_prop <- nat_nutr_demand_prop  %>% 
    # Group by
    group_by(year, scenario, sector, nutrient) %>% 
    summarise(supply_mt=sum(supply_mt)) %>% 
    # Add demand
    left_join(global_nutr_demand) %>% 
    # Calculate proportion
    mutate(demand_prop=supply_mt/demand_mt)
  
  
  # Export datasets
  #############################################################
  
  # Export datasets
  saveRDS(nat_nutr_demand_prop, file=file.path(datadir, "2050_2100_national_nutr_demand_filled_by_seafood.Rds"))
  saveRDS(global_nutr_demand_prop, file=file.path(datadir, "2050_2100_global_nutr_demand_filled_by_seafood.Rds"))
  
}
