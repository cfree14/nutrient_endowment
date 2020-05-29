
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(readxl)
library(tidyverse)

# Directories
indir <- "code/calc_nutr_deficiencies/alon_daniel_work/alon_orig"
outdir <- "code/calc_nutr_deficiencies/functions"

# Read helper functions
data_orig <- readxl::read_excel(file.path(indir, "Bangladesh_EAR_CUT.xlsx"), sheet=4, col_names =F)

# Format data
data <- data_orig %>% 
  # Reduce to important rows
  slice(c(1,36,37)) %>% 
  # Transpose
  t() %>% 
  # Convert to dataframe
  as.data.frame(stringsAsFactors=F) %>% 
  # Rename columns
  setNames(., c("food", "zinc", "phyate")) %>% 
  # Eliminate 1st three rows
  slice(4:nrow(.)) %>% 
  # Eliminate empty rows
  filter(!is.na(zinc) | !is.na(phyate)) %>% 
  # Convert to numeric
  mutate(zinc=as.numeric(zinc),
         phyate=as.numeric(phyate)) %>% 
  # Rename columns
  rename(zinc_mg_100g=zinc, phyate_mg_100g=phyate)

# Export data
write.csv(data, file=file.path(outdir, "zinc_phyate_concentrations_by_food.csv"), row.names=F)
