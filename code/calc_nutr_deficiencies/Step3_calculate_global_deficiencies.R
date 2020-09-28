
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "output"

# Read data
data_orig <- readRDS(file=file.path(datadir, "nutr_deficiencies_by_cntry_sex_age_2011.Rds"))

# Format data
################################################################################

# Summarize globally
data <- data_orig %>% 
  group_by(sex, age, menstruation, nutrient_type, nutrient, units) %>% 
  summarize(pop_size=sum(pop_size), 
            ndeficient=sum(ndeficient),
            nhealthy=sum(nhealthy))

# Export
saveRDS(data, file.path(datadir, "nutr_deficiencies_by_sex_age_2011.Rds"))
