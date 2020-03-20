

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
inputdir <- "data/vaitla_etal_2018/raw"
outputdir <- "data/vaitla_etal_2018/processed"

# Read data, extract names/units, reread data and apply names
data_orig <- read.csv(file.path(inputdir, "Supp Data 1.csv"), as.is=T)
names <- colnames(data_orig)
units <- data_orig[1,]
data_orig <- read.csv(file.path(inputdir, "Supp Data 1.csv"), as.is=T, skip=1)
colnames(data_orig) <- names
str(data_orig)

# Read key
nutrient_key <- rio::import(file.path(inputdir, "nutrient_key_chris.xlsx")) %>% 
  setNames(tolower(colnames(.))) %>%
  mutate(units_short=gsub("/100g", "", units)) %>% 
  rename(units_long=units,
         units2="units 2",
         thresh="source threshold (/100g)", 
         thresh_rich="rich source threshold (/100g)") %>% 
  select(type, nutrient, units_short, everything())

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


# Format nutrient content data
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
  rename(species_orig=X, species_closest=closestSp, 
         value=predVal, value_lo=lcl_predInt, value_hi=ucl_predInt) %>% 
  # Format columns
  mutate(species_orig=gsub("_", " ", species_orig),
         species_closest=gsub("_", " ", species_closest)) %>% 
  # Add nutrient
  left_join(file_key, by="file") %>% 
  # Add units
  left_join(select(nutrient_key, type, nutrient, units_long), by="nutrient") %>% 
  mutate(nutrient_label=paste0(nutrient, " (", units_long, ")")) %>% 
  # Rearrange columns
  select(file, type, nutrient_label, nutrient, units_long, everything()) 

# Check species names
spp_to_check <- sort(unique(preds_long$species_orig))
spp_suggest_list <- freeR::suggest_names(spp_to_check)
spp_suggest_df <- tibble(species_orig=names(spp_suggest_list), species=NA)
for(i in 1:nrow(spp_suggest_df)){
  spp_suggest <- spp_suggest_list[i][[1]]
  spp_suggest1 <- ifelse(length(spp_suggest)==0, "", spp_suggest)
  spp_suggest_df$species[i] <- spp_suggest1
}

# Build species key
spp_key <- tibble(species_orig=spp_to_check) %>% 
  # Add suggestion
  left_join(spp_suggest_df, by="species_orig") %>% 
  # Format species name
  mutate(species=ifelse(is.na(species), species_orig, species),
         species=ifelse(species=="", species_orig, species),
         species=recode(species,
                        # "Corydoras kristinae"="", 
                        # "Cyprichromis jumbo"="", # Cyprichromis leptosoma ?
                        # "Cyprichromis zebra"="", # Cyprichromis zonatus ?
                        "Hemibarbus longibarbis"="Hemibarbus maculatus", 
                        # "Macrocephenchelys soela"="", 
                        # "Puntius parvus"="", 
                        "Sebastes marinus"="Sebastes norvegicus"))
                        #"Takifugu fasciatus"=""))

# Check names
freeR::check_names(spp_key$species)

# Get common names
comm_name_df <- freeR::fb_comm_name(species=spp_key$species)

# Add common names to species key
spp_key1 <- spp_key %>% 
  left_join(comm_name_df, by="species") %>% 
  select(-source) 

# Convert to wide and add corrected species names and common names
n_distinct(preds_long$species_orig) * n_distinct(preds_long$nutrient)
preds_md <- preds_long %>% 
  select(species_orig, nutrient, value) %>% 
  spread(key="nutrient", value="value") %>% 
  gather(key="nutrient", value="value", 2:ncol(.))
preds_lo <- preds_long %>% 
  select(species_orig, nutrient, value_lo) %>% 
  spread(key="nutrient", value="value_lo") %>% 
  gather(key="nutrient", value="value_lo", 2:ncol(.))
preds_hi <- preds_long %>% 
  select(species_orig, nutrient, value_hi) %>% 
  spread(key="nutrient", value="value_hi") %>% 
  gather(key="nutrient", value="value_hi", 2:ncol(.))

# Merge data
preds_long1 <- preds_md %>% 
  left_join(preds_lo, by = c("species_orig", "nutrient")) %>% 
  left_join(preds_hi, by = c("species_orig", "nutrient")) %>% 
  # Add corrected species name and common name
  left_join(spp_key1, by="species_orig") %>% 
  # Add nutrient units
  left_join(nutrient_key %>% select(nutrient, units_long, units_short), by="nutrient") %>% 
  # Create nutrient label
  mutate(nutrient_label=paste0(nutrient, " (", units_long, ")")) %>% 
  # Fill empty values
  rename(value_md=value) %>% 
  mutate(value_md_fill=ifelse(!is.na(value_md), value_md, 0)) %>% 
  # Add closest species
  left_join(preds_long %>% select(nutrient, species_orig, species_closest), by = c("species_orig", "nutrient")) %>% 
  # Arrange
  select(nutrient_label, nutrient, units_long, units_short,
         species_orig, species, comm_name, species_closest, value_md_fill, value_md, value_lo, value_hi, everything()) %>% 
  arrange(nutrient, species_orig)

# Inspect
str(preds_long1)
freeR::complete(preds_long1)


# Export
################################################################################

# Export
nutrient_data <- data
nutrient_preds_long <- preds_long1
save(nutrient_key, nutrient_data, nutrient_preds_long, 
     file=file.path(outputdir, "Vaitla_etal_2018_nutrient_data.Rdata"))

