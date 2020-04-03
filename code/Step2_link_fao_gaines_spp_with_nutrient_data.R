

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
outdir <- "output"
genusdir <- "data/genus/processed"

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")

# Read GENUS  data
genus_orig <- readRDS(file.path(genusdir, "genus_food_composition_tables.Rds"))


# Step 1. Format GENUS data
################################################################################

# GENUS marine seafoods
genus_seafoods <- c("Cephalopods",
                    "Crustaceans", 
                    "Molluscs; Other",
                    "Demersal Fish", 
                    "Pelagic Fish",
                    "Marine Fish; Other")

# GENUS nutrients of interest
colnames(genus_orig)
nutrients_ignore <- c("Choline", "Manganese",
                      "Omega-3 fatty acids (USDA only)", 
                      "Vitamin B12 (USDA only)",
                      "Water moisture", "Ash", "Refuse" )

# Calculate median nutrient composition
genus <- genus_orig %>% 
  # Only fish
  filter(genus_food_name %in% genus_seafoods) %>% 
  # Calculate median
  group_by(genus_food_name, nutrient, units) %>% 
  summarize(units_per_100g_median=median(units_per_100g, na.rm=T)) %>% 
  # Reduce to nutrients of interest
  filter(!nutrient %in% nutrients_ignore) %>% 
  ungroup()

# Create wide version for merging with FAO data
genus_wide <- genus %>% 
  mutate(nutr_colname=paste(nutrient, units, sep="_") %>% tolower() %>% gsub(" ", "_", .)) %>% 
  select(genus_food_name, nutr_colname, units_per_100g_median) %>% 
  spread(key=nutr_colname, value=units_per_100g_median)

# Export 
write.csv(genus_wide, file=file.path(outdir, "GENUS_nutrient_per_100g_by_seafood_group.csv"), row.names=F)
  

# Step 2. Format FAO species key
################################################################################

# Pelagic
fish_pelagic <- tibble(genus_food_name="Pelagic Fish",
                       isscaap=c("Miscellaneous pelagic fishes", 
                                 "Herrings, sardines, anchovies",
                                 "Shads", 
                                 "Tunas, bonitos, billfishes"))

# Demersal
fish_demersal <- tibble(genus_food_name="Demersal Fish",
                        isscaap=c("Miscellaneous demersal fishes", 
                                  "Carps, barbels and other cyprinids",
                                  "Cods, hakes, haddocks", 
                                  "Flounders, halibuts, soles",
                                  "Sharks, rays, chimaeras"))

# Other fish
fish_other <- tibble(genus_food_name="Marine Fish; Other",
                     isscaap=c("Marine fishes not identified",
                               "Salmons, trouts, smelts",
                               "Sturgeons, paddlefishes",
                               "Miscellaneous diadromous fishes",
                               "Miscellaneous freshwater fishes", 
                               "Miscellaneous coastal fishes",
                               "River eels",
                               "Tilapias and other cichlids"))

# Molluscs
molluscs <- tibble(genus_food_name="Molluscs; Other",
                   isscaap=c("Abalones, winkles, conchs", 
                             "Clams, cockles, arkshells",
                             "Freshwater molluscs", 
                             "Miscellaneous marine molluscs",
                             "Mussels", 
                             "Oysters",
                             "Pearls, mother-of-pearl, shells", 
                             "Scallops, pectens"))

# Crustaceans
crustaceans <- tibble(genus_food_name="Crustaceans",
                      isscaap=c("Crabs, sea-spiders",
                                "Freshwater crustaceans",
                                "King crabs, squat-lobsters",
                                "Krill, planktonic crustaceans",
                                "Lobsters, spiny-rock lobsters", 
                                "Miscellaneous marine crustaceans",
                                "Shrimps, prawns"))

# Cephalopods
cephalopods <- tibble(genus_food_name="Cephalopods",
                      isscaap=c("Squids, cuttlefishes, octopuses"))

# Other invertebrates
inverts_other <- c("Corals", 
                   "Horseshoe crabs and other arachnoids",
                   "Sea-squirts and other tunicates", 
                   "Sea-urchins and other echinoderms",
                   "Sponges", 
                   "Miscellaneous aquatic invertebrates")

# ISSCAAP-GENUS key
genus_isscaap_key <- rbind(fish_pelagic,
                           fish_demersal,
                           fish_other,
                           molluscs,
                           crustaceans,
                           cephalopods)

# FAO species key
fao_spp <- fao_orig %>% 
  # Only marine species
  filter(area_type=="marine") %>% 
  # Unique species
  select(species_code, species_orig, species, comm_name, major_group, isscaap) %>% 
  unique() %>% 
  # Only fish, crustaceans, molluscs, echinoderms
  filter(major_group %in% c("Pisces", "Crustacea", "Mollusca", "Invertebrata aquatica") & !isscaap %in% inverts_other) %>% 
  # Add GENUS group
  left_join(genus_isscaap_key, by="isscaap") %>% 
  # Add GENUS nutrient concentrations
  left_join(genus_wide, by="genus_food_name")

# Inspect
str(fao_spp)
freeR:: complete(fao_spp)


# Step 3. Format Gaines species key
################################################################################

# Load final results
gaines_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/raw/global_cc_1nation_manuscript_2019Feb12.rds")

# Build Gaines species key
gaines_spp_orig <- gaines_orig %>% 
  select(SciName) %>% 
  unique() %>% 
  rename(species_orig=SciName) %>% 
  # Add FAO species key
  left_join(fao_spp, by="species_orig")
gaines_spp_done1 <- filter(gaines_spp_orig, !is.na(species))

# Fill out missing: try 1
gaines_spp_try1 <- gaines_spp_orig %>% 
  filter(is.na(species)) %>% 
  select(species_orig) %>% 
  mutate(species=recode(species_orig,
                        "Moolgarda seheli"="Crenimugil seheli",
                        "Raja rhina"="Beringraja rhina",
                        "Trigloporus lastoviza"="Chelidonichthys lastoviza")) %>% 
  left_join(fao_spp %>% select(-species_orig), by="species")
gaines_spp_done2 <- filter(gaines_spp_try1, !is.na(genus_food_name))

# Fill out missing try 2
gaines_spp_try2 <- filter(gaines_spp_try1, is.na(genus_food_name))
gaines_spp_try2_taxa <- freeR::taxa(gaines_spp_try2$species)
gaines_spp_try2_commnames <- freeR::fb_comm_name(gaines_spp_try2_taxa$sciname)
gaines_spp_done3 <- gaines_spp_try2 %>% 
  select(species_orig, species) %>% 
  # Add class and family name
  left_join(gaines_spp_try2_taxa %>% select(class, family, sciname), by=c("species"="sciname")) %>% 
  # Add common name
  left_join(gaines_spp_try2_commnames %>% select(species, comm_name), by=c("species"="species")) %>% 
  mutate(comm_name=ifelse(species=="Fenneropenaeus penicillatus", "Redtail shrimp", comm_name)) %>% 
  # Add ISSCAAP group based on family
  mutate(isscaap=recode(family, 
                        "Ammodytidae"="Miscellaneous coastal fishes", 
                        "Berycidae"="Miscellaneous demersal fishes", 
                        "Clupeidae"="Shads", 
                        "Echinidae"="Sea-urchins and other echinoderms", 
                        "Holothuriidae"="Sea-urchins and other echinoderms", 
                        "Limulidae"="Horseshoe crabs and other arachnoids", 
                        "Penaeidae"="Shrimps, prawns", 
                        "Platycephalidae"="Miscellaneous coastal fishes", 
                        "Pleuronectidae"="Flounders, halibuts, soles",
                        "Rajidae"="Sharks, rays, chimaeras", 
                        "Scorpaenidae"="Miscellaneous demersal fishes", 
                        "Sebastidae"="Miscellaneous demersal fishes", 
                        "Sillaginidae"="Miscellaneous coastal fishes", 
                        "Sparidae"="Miscellaneous coastal fishes", 
                        "Stichopodidae"="Sea-urchins and other echinoderms")) %>% 
  # Add MAJOR GROUP based on class
  mutate(major_group=recode(class, 
                            "Actinopterygii"="Pisces", 
                            "Echinoidea"="Invertebrata aquatica", 
                            "Elasmobranchii"="Invertebrata aquatica", 
                            "Holothuroidea"="Invertebrata aquatica", 
                            "Malacostraca"="Crustacea", 
                            "Merostomata"="Invertebrata aquatica")) %>%
  # Arrange columns
  select(species_orig, species, comm_name, major_group, isscaap)

# Merge Gaines keys
gaines_spp <- rbind(gaines_spp_done1 %>% select(species_orig, species, comm_name, major_group, isscaap),
                    gaines_spp_done2 %>% select(species_orig, species, comm_name, major_group, isscaap),
                    gaines_spp_done3 %>% select(species_orig, species, comm_name, major_group, isscaap)) %>% 
  filter(!(species_orig=="Lepidonotothen squamifrons" & comm_name=="Striped-eyed rockcod")) %>% 
  # Add GENUS group
  left_join(genus_isscaap_key, by="isscaap") %>% 
  # Add GENUS nutrient concentrations
  left_join(genus_wide, by="genus_food_name")

# Inspect
str(gaines_spp)
freeR::complete(gaines_spp)

# Export keys
################################################################################

# Export
write.csv(fao_spp, file=file.path(outdir, "FAO_species_nutrient_content_key.csv"), row.names=F)
write.csv(gaines_spp, file=file.path(outdir, "Gaines_species_nutrient_content_key.csv"), row.names=F)
