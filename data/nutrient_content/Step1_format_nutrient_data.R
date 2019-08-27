

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
inputdir <- "data/nutrient_content/raw"
outputdir <- "data/nutrient_content/processed"

# Read data, extract names/units, reread data and apply names
data_orig <- read.csv(file.path(inputdir, "Supp Data 1.csv"), as.is=T)
names <- colnames(data_orig)
units <- data_orig[1,]
data_orig <- read.csv(file.path(inputdir, "Supp Data 1.csv"), as.is=T, skip=1)
colnames(data_orig) <- names
str(data_orig)

# Read key
key <- rio::import(file.path(inputdir, "nutrient_key_chris.xlsx")) %>% 
  setNames(tolower(colnames(.))) %>% 
  rename(units2="units 2",
         thresh="source threshold (/100g)", 
         thresh_rich="rich source threshold (/100g)")

# Key to supplementary data
# 1. Database of life history and nutrient content info for all species
# 2. Predicted total fat content
# 3. Predicted omega-three fatty acids content
# 4. Predicted omega-six fatty acids content
# 5. Predicted iron content
# 6. Predicted zinc content
# 7. Predicted vitaminA content
# 8. Predicted vitaminB12 content
# 9. Predicted vitaminD content
# 10. Predicted protein content

# Build file key
################################################################################

# Files with predicted nutrient content
pred_files <- paste0("Supp Data ", 2:10, ".csv")
nutrients <- c("Total fat", "Omega-3 fatty acids", "Omega-6 fatty acids", "Iron", "Zinc", 
               "Vitamin A", "Vitamin B12", "Vitamin D", "Protein")
file_key <- tibble(file=pred_files, nutrient=nutrients)


# Format nutrient content predictions
################################################################################

# Identify columns with no data
cols_no_data <- colnames(data_orig)[apply(data_orig, 2, function(x) sum(!is.na(x))==0)]

# Format data
data <- data_orig %>% 
  # Remove columns with no data
  select(-cols_no_data) %>% 
  # Rename columns
  rename(sciname=species_name, 
         sciname_alt=alt_name, 
         iso3=coun_iso, 
         comm_name=common_name, 
         fish_yn=fish, 
         fillet_yn=fillet, 
         bones_yn=bones,
         head_yn=head, 
         eyes_yn=eyes, 
         viscera_yn=viscera, 
         skin_yn=skin, 
         dorsal_yn=dorsal, 
         ventral_yn=ventral, 
         fin_yn=fin,
         roe_yn=roe, 
         liver_yn=liver, 
         leg_yn=leg, 
         other_part_yn=othpart, 
         young_yn=young, 
         farmed_yn=farmed, 
         habitat=habcat, 
         depth_min_m=depmin, 
         depth_max_m=depmax, 
         lmax_cm=maxlen,
         length_cath=lencat,
         trophic_level=troph,
         protein_g=protein, 
         fat_g=fat, 
         iron_mg=iron,
         zinc_mg=zinc, 
         vitamin_a_re=vita, 
         vitamin_b12_mcg=vitb12, 
         vitamin_d_mcg=vitd, 
         kcalorie=kcal, 
         fac_sat_g=fac_sat, 
         fac_mon_g=fac_mon,
         fac_pol_g=fac_pol,
         fac3_pol_g=fac3_pol,
         fac6_pol_g=fac6_pol,
         chol_mg=chol, 
         lysine_mg=lysine, 
         retinol_mcg=retinol) %>% 
  # Format columns
  mutate(sciname=gsub("_", " ", sciname),
         sciname_alt=gsub("_", " ", sciname_alt)) %>% 
  # Rearrange columnns
  select(sciname, sciname_alt, comm_name, class:subspecies, iso3, description,
         fish_yn:farmed_yn, range, habitat, depth_min_m:trophic_level, kcalorie, everything())

# Inspect data
colnames(data)
str(data)


# Format nutrient content predictions
################################################################################

# Merge predicted content files
# For testing: x <- pred_files[1]
preds_merged <- purrr::map_df(pred_files, function(x) {
  df <- read.csv(file.path(inputdir, x), as.is=T) %>% 
    mutate(file=x)
})

# Format predicted nutrient content
preds_long <- preds_merged %>% 
  # Rename columns
  rename(species=X, species_closest=closestSp, 
         value=predVal, value_lo=lcl_predInt, value_hi=ucl_predInt) %>% 
  # Format columns
  mutate(species=gsub("_", " ", species),
         species_closest=gsub("_", " ", species_closest)) %>% 
  # Add nutrient
  left_join(file_key, by="file") %>% 
  # Add units
  left_join(select(key, type, nutrient, units), by="nutrient") %>% 
  mutate(nutrient_unit=paste0(nutrient, " (", units, ")")) %>% 
  # Rearrange columns
  select(file, type, nutrient_unit, nutrient, units, everything()) 

# Reshape data
preds_wide <- preds_long %>% 
  select(nutrient, species, value) %>% 
  spread(key="nutrient", value="value") %>% 
  # Rename columns
  rename(protein_g="Protein",
         fat_g="Total fat",
         omega3_g="Omega-3 fatty acids",
         omega6_g="Omega-6 fatty acids",
         iron_mg="Iron",
         zinc_mg="Zinc", 
         vitA_ug="Vitamin A",
         vitB12_ug="Vitamin B12",
         vitD_ug="Vitamin D")

# Check completeness
# There are some missing
freeR::complete(preds_wide)


# Export
################################################################################

# Export
save(key, data, preds_long, preds_wide, file=file.path(outputdir, "Vaitla_etal_2018_nutrient_data.Rdata"))

