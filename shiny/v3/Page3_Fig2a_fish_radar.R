
# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
group_nutr_data <- read.csv(file.path(datadir, "GENUS_nutrient_per_100g_by_seafood_group.csv"), as.is=T)


# Plot population growth data
plot_fish_radar_static <- function(group_nutr_data){
  
  # Nutrients of interest
  nutr_do <- c("calcium_mg", "copper_mg", "folate_mcg", "iron_mg", "magnesium_mg", "niacin_mg", "phosphorus_mg",
               "riboflavin_mg", "thiamin_mg", "vitamin_a_mcg_rae", "vitamin_b6_mg", "vitamin_c_mg", "zinc_mg")
  
  # Format data
  data_plot <- group_nutr_data %>% 
    # Gather
    gather(key="nutrient", value="quantity", 2:ncol(.)) %>% 
    # Calculate as percent of maximum
    group_by(nutrient) %>% 
    mutate(quantity_prop=quantity/max(quantity)) %>% 
    ungroup() %>% 
    # Recode group
    mutate(species_group=recode(genus_food_name, 
                                "Demersal Fish"="Fish, demersal",
                                "Pelagic Fish"="Fish, pelagic",
                                "Marine Fish; Other"="Fish, other",
                                "Molluscs; Other"="Bivalves and gastropods")) %>% 
    # Reduce to nutrients of interest
    filter(nutrient %in% nutr_do) %>% 
    mutate(nutrient=recode(nutrient, 
                           "calcium_mg"="Calcium", 
                           "copper_mg"="Copper", 
                           "folate_mcg"="Folate", 
                           "iron_mg"="Iron", 
                           "magnesium_mg"="Magnesium", 
                           "niacin_mg"="Niacin", 
                           "phosphorus_mg"="Phosphorus",
                           "riboflavin_mg"="Riboflavin", 
                           "thiamin_mg"="Thiamin", 
                           "vitamin_a_mcg_rae"="Vit A", 
                           "vitamin_b6_mg"="Vit B6", 
                           "vitamin_c_mg"="Vit C", 
                           "zinc_mg"="Zinc")) %>% 
    # Reduce for plotting
    select(species_group, nutrient, quantity_prop) %>% 
    spread(key="nutrient", value="quantity_prop")
  
  # Plot radar plot
  g <- ggradar::ggradar(data_plot) + 
    theme(legend.position = "right")
  g
  
}

# Test function
plot_fish_radar_static(group_nutr_data=group_nutr_data)








