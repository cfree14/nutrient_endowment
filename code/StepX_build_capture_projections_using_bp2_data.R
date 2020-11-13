
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Data directory
outputdir <- "output"

# Read BP2 capture projections
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/aquacast/data/capture_projections/data/Free_etal_2020_fish_proj_by_rcp_mgmt_cntry_isscaap_scaled.Rds")

# Read ISSCAAP to GENUS key
key <- readxl::read_excel(file.path(outputdir, "isscaap_genus_key.xlsx"))


# Build data
################################################################################

# Add GENUS group
data_meat <- data_orig %>% 
  # Add GENUS group
  left_join(key) %>% 
  # Summarize by RCP, management, country, group
  group_by(rcp, scenario, country, iso3, genus, year) %>% 
  summarize(meat_mt=sum(meat_mt_scaled, na.rm=T))


# Export data
################################################################################

# Export data
saveRDS(data_meat, file.path(outputdir, "Free_etal_2020_capture_meat_proj_by_rcp_mgmt_genus_group.Rds"))
