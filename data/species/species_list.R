
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/nutrient_content/processed"
fishdir <- "data/fish_projections/data"
plotdir <- "figures"

# Read MSY data
load("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/gaines_eez_msy_time_series.Rdata")

# Read AQ species
aq_species <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/aquacast/data/species/aquaculture_species_key.csv", as.is=T)

# Build species list
################################################################################

# WC Species
wc_spp <- msy_ts_g %>% 
  select(species) %>% 
  unique()

# AQ species
aq_spp <- aq_species %>% 
  select(species) %>% 
  unique()

# Build species key
spp <- sort(unique(c(wc_spp$species, aq_spp$species)))
spp_key <- freeR::taxa(spp)

# Note that some missing
spp[!spp%in%spp_key$sciname]

# Export
write.csv(spp_key, file="data/species/species.csv", row.names=F)



