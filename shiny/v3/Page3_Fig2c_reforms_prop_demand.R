
# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
nutr_demand_prop_wc <- readRDS(file=file.path(datadir, "2050_2100_nutr_demand_prop_from_wc_rcp_mgmt_cntry_group.Rds"))



# Plotting function
####################################################################################

# Plot proportion of demand met by WC reforms
# data <- nutr_demand_prop_wc; country <- "Ghana"
plot_nutr_demand_prop_wc <- function(data, country){
  
  # Country do
  country_do <- country
  
  # Prepare data
  sdata <- data %>% 
    filter(country==country_do & scenario=="Reforms")
  
  # Plot data
  g <- ggplot(sdata, aes(x=demand_prop, y=nutrient, fill=genus)) +
    facet_grid(year ~ rcp) +
    geom_bar(stat="identity") +
    # Labels
    labs(x="Percent of nutrient demand\nmet from capture fisheries reforms", y="") +
    # Legend
    scale_fill_discrete(name="Major group") +
    # Theme
    theme_bw()
  g
  
}

# Test function
plot_nutr_demand_prop_wc(data=nutr_demand_prop_wc, country="United States")





####################################################################################
# Build dataset - KAT, YOU CAN IGNORE THIS
####################################################################################

# This is how I built that data
if(F){
  
  # Data directory
  datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"
  
  # Read demand projections
  nat_nutr_demand <- readRDS(file.path(datadir, "1960_2100_nutrient_demand_by_country.Rds"))
  
  # Read capture projections
  nat_wc_proj <- readRDS(file.path(datadir, "Free_etal_2020_capture_meat_proj_by_rcp_mgmt_genus_group.Rds")) %>% 
    ungroup() %>% 
    mutate(scenario=recode_factor(scenario, 
                                  "No Adaptation"="BAU",
                                  "Full Adaptation"="Reforms"),
           genus=recode(genus,
                        "Demersal Fish"="Fish, demersal",
                        "Marine Fish; Other"="Fish, other",
                        "Molluscs; Other"="Bivalves and gastropods",
                        "Pelagic Fish"="Fish, pelagic"))
  
  # Read capture nutrient data
  wc_nutr_data <- read.csv(file.path(datadir, "GENUS_nutrient_per_100g_by_seafood_group.csv"), as.is=T) %>% 
    # Reduce to nutrients of interest
    select("genus_food_name", "calcium_mg", "copper_mg", "folate_mcg", "iron_mg", "magnesium_mg", "niacin_mg", "phosphorus_mg",
           "riboflavin_mg", "thiamin_mg", "vitamin_a_mcg_rae", "vitamin_b6_mg", "vitamin_c_mg", "zinc_mg") %>% 
    mutate(genus_food_name=recode(genus_food_name,
                                  "Demersal Fish"="Fish, demersal",
                                  "Marine Fish; Other"="Fish, other",
                                  "Molluscs; Other"="Bivalves and gastropods",
                                  "Pelagic Fish"="Fish, pelagic"))
  
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
  
  # Build data
  data <- nat_wc_proj %>% 
    # Reduce to years of interest
    filter(year%in%c(2050,2100)) %>% 
    # Add nutrient concentrations
    left_join(wc_nutr_data, c("genus"="genus_food_name")) %>% 
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
    select(rcp:year, calcium_mt:zinc_mt) %>% 
    # Reshape
    gather(key="nutrient", value="supply_mt", 8:ncol(.)) %>% 
    # Format nutrient columns
    mutate(nutrient=gsub("_mt", "", nutrient) %>% gsub("_", " ", .) %>% stringr::str_to_title()) %>% 
    # Add demand
    left_join(nat_nutr_demand %>% select(iso3, nutrient, year, supply_req_mt_yr_50perc)) %>% 
    rename(demand_mt=supply_req_mt_yr_50perc) %>% 
    mutate(demand_prop=supply_mt/demand_mt) %>% 
    # Reduce to countries with projected demands
    filter(!is.na(demand_mt))
  
  # Inspect
  str(data)
  freeR::complete(data)
  table(data$nutrient)
  
  # Export data
  saveRDS(data, file=file.path(datadir, "2050_2100_nutr_demand_prop_from_wc_rcp_mgmt_cntry_group.Rds"))
  
  
}
