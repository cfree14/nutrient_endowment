
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "code/calc_nutr_deficiencies/alon_revised/data"
codedir <- "code/calc_nutr_deficiencies/alon_revised/functions"

# Read helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Read data
library(openxlsx)
Genus <- readWorkbook(file.path(datadir, "Bangladesh_EAR_CUT.xlsx"), sheet='GENuS', startRow = 3) 
EAR_all <- readWorkbook(file.path(datadir, "Bangladesh_EAR_CUT.xlsx"), sheet='EAR', colNames=TRUE, startRow = 1) 
population <- readWorkbook(file.path(datadir, "Bangladesh_EAR_CUT.xlsx"), sheet='population', colNames=TRUE, startRow = 1) 
mat_edible <- readWorkbook(file.path(datadir, "Bangladesh_EAR_CUT.xlsx"), sheet='edible', cols = c(seq(4,678,3)))

# Subset population data
population <- population[1:32,3]


# Calculate deficiencies
################################################################################

# Protein
################################

# Setup
Nut_protein <- Genus[1:32,7]
EAR_protein <- EAR_all[1:32,14] # median
CV <- 0.25       # assume coefficient of variance for a normal distribution
CV_protein_in <- Nut_protein*CV

# Perform calculations
deficiency_protein <- EAR_CUT(Nut_protein, CV_protein_in, EAR_protein) # fraction of nutrient deficiencies per age-sex group
deficiency_protein_averaged <- sum(deficiency_protein*population)/sum(population)*100 # in percentage


# Vitamin A
################################

# Setup
Nut_vitaminA <- Genus[1:32,19]
EAR_vitaminA <- EAR_all[1:32,15] # median
CV <- 0.25       #assume coefficient of variance for a normal distribution
CV_vitaminA_in <- Nut_vitaminA*CV

# Perform calculations
deficiency_vitaminA <- EAR_CUT(Nut_vitaminA,CV_vitaminA_in,EAR_vitaminA)
deficiency_vitaminA_averaged <- sum(deficiency_vitaminA*population)/sum(population)*100 # in percentage


# Zinc
################################

# Setup
Nut_zinc <- Genus[1:32,31]
#use the miller equation to convert zinc intake to absorbed zinc
#Load in zinc and phytate densities as well as edible mass of consumption
TAZ <- zinc_absorption(mat_edible)
EAR_zinc <- EAR_all[1:32,4] #median
CV <- 0.25         #assume coefficient of variance for a normal distribution
CV_zinc_in <- Nut_zinc*CV

# Perform calculations
deficiency_zinc <- EAR_CUT(TAZ,CV_zinc_in,EAR_zinc)
deficiency_zinc_averaged <- sum(deficiency_zinc*population)/sum(population)*100 #in percentage


# Iron
################################

# Setup
Nut_iron <- Genus[1:32,28];  #median iron intake
EAR_iron <- EAR_all[1:32,6]; #[7] iron 10% bioavailability,[6] 12%, [5] 15%
CV <- 0.25         #assume coefficient of variance for a normal distribution
CV_iron_in <- Nut_iron*CV
bioavailability <- 0.12

# Perform calculations
deficiency_iron <- EAR_prob_Iron(Nut_iron,CV_iron_in,EAR_iron,bioavailability)
deficiency_iron_averaged <- sum(deficiency_iron*population)/sum(population)*100 #in percentage



