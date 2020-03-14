

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(ggplot2)
library(tidyverse)

# Directories
inputdir <- "data/nutrient_content/raw"
outputdir <- "data/nutrient_content/processed"
plotdir <- "data/nutrient_content/figures"

# Read data
load(file.path(outputdir, "Vaitla_etal_2018_nutrient_data.Rdata"))

# Radar plot (ggradar approach)
################################################################################

# Example
#################################

# # Package
# library(ggradar)
# 
# # Setup data
# mtcars_radar <- mtcars %>% 
#   as_tibble(rownames = "group") %>% 
#   mutate_at(vars(-group), rescale) %>% 
#   tail(4) %>% 
#   select(1:10)
# mtcars_radar
# 
# # Plot data
# ggradar(mtcars_radar)


# Application
#################################

# Species
spp <- c("Aaptosyax grypus", "Telestes croaticus")

# Format data
data <- preds_wide %>% 
  gather(key="nutrient", value="concentration", 2:ncol(.)) %>% 
  group_by(nutrient) %>% 
  mutate(pmax=concentration/max(concentration, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(nutrient_label=recode(nutrient, 
                               "fat_g"="Fat",
                               "iron_mg"="Iron",
                               "omega3_g"="Omega-3\nfatty acids",
                               "omega6_g"="Omega-6\nfatty acids",
                               "protein_g"="Protein",
                               "vitA_ug"="Vitamin-A",
                               "vitB12_ug"="Vitamin-B12",
                               "vitD_ug"="Vitamin-D",
                               "zinc_mg"="Zinc"))

# Subset data
sdata <- data %>% 
  filter(species%in%spp) %>% 
  select(-c(nutrient,concentration)) %>% 
  spread(key="nutrient_label", value="pmax")

# Plot data
g <- ggradar(sdata) + 
  theme(legend.position = "bottom")
g





