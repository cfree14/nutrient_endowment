
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)
library(rnaturalearth)

# Directories
outdir <- "output"
genusdir <- "data/genus/processed"

# Read FAO data
fao_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/data/fao/capture/processed/1950_2017_fao_landings_data.Rds")

# Read GENUS  data
genus_orig <- readRDS(file.path(genusdir, "genus_food_composition_tables.Rds"))



# Build ISSCAAP to GENUS key
################################################################################

# GENUS marine seafoods
genus_seafoods <- c("Cephalopods",
                    "Crustaceans", 
                    "Molluscs; Other",
                    "Demersal Fish", 
                    "Pelagic Fish",
                    "Marine Fish; Other")

# Pelagic Fish ISSCAAPs
fish_pelagic <- tibble(genus_food_name="Pelagic Fish",
                       isscaap=c("Miscellaneous pelagic fishes", 
                                 "Herrings, sardines, anchovies",
                                 "Shads", 
                                 "Tunas, bonitos, billfishes"))

# Demersal Fish ISSCAAPs
fish_demersal <- tibble(genus_food_name="Demersal Fish",
                        isscaap=c("Miscellaneous demersal fishes", 
                                  "Carps, barbels and other cyprinids",
                                  "Cods, hakes, haddocks", 
                                  "Flounders, halibuts, soles",
                                  "Sharks, rays, chimaeras"))

# Other fish ISSCAAPs
fish_other <- tibble(genus_food_name="Marine Fish; Other",
                     isscaap=c("Marine fishes not identified",
                               "Salmons, trouts, smelts",
                               "Sturgeons, paddlefishes",
                               "Miscellaneous diadromous fishes",
                               "Miscellaneous freshwater fishes", 
                               "Miscellaneous coastal fishes",
                               "River eels",
                               "Tilapias and other cichlids"))

# Mollusc ISSCAAPs
molluscs <- tibble(genus_food_name="Molluscs; Other",
                   isscaap=c("Abalones, winkles, conchs", 
                             "Clams, cockles, arkshells",
                             "Freshwater molluscs", 
                             "Miscellaneous marine molluscs",
                             "Mussels", 
                             "Oysters",
                             "Pearls, mother-of-pearl, shells", 
                             "Scallops, pectens"))

# Crustacean ISSCAAPs
crustaceans <- tibble(genus_food_name="Crustaceans",
                      isscaap=c("Crabs, sea-spiders",
                                "Freshwater crustaceans",
                                "King crabs, squat-lobsters",
                                "Krill, planktonic crustaceans",
                                "Lobsters, spiny-rock lobsters", 
                                "Miscellaneous marine crustaceans",
                                "Shrimps, prawns"))

# Cephalopod ISSCAAPs
cephalopods <- tibble(genus_food_name="Cephalopods",
                      isscaap=c("Squids, cuttlefishes, octopuses"))

# Other invertebrate ISSCAAPs
inverts_other <- c("Corals", 
                   "Horseshoe crabs and other arachnoids",
                   "Sea-squirts and other tunicates", 
                   "Sea-urchins and other echinoderms",
                   "Sponges", 
                   "Miscellaneous aquatic invertebrates")

# ISSCAAP-GENUS key
isscaap_genus_key <- rbind(fish_pelagic,
                           fish_demersal,
                           fish_other,
                           molluscs,
                           crustaceans,
                           cephalopods)


# Format FAO data
################################################################################

# Inspect
table(fao_orig$units)
table(fao_orig$area_type)
table(fao_orig$major_group)

# Format FAO data
fao <- fao_orig %>% 
  # Marine/brackish only
  filter(area_type=="marine") %>% 
  # Catch in mt only
  filter(units=="t") %>% 
  # Remove non-fish/crustaceans/bivalves/echinoderms
  filter(major_group %in% c("Pisces", "Crustacea", "Mollusca", "Invertebrata aquatica") & isscaap %in% isscaap_genus_key$isscaap) %>% 
  # Add GENUS
  left_join(isscaap_genus_key, by="isscaap") %>% 
  # Add edible meat conversion
  select(-c(units, symbol)) %>% 
  rename(catch_mt=quantity) %>% 
  mutate(pedible=recode(major_group, 
                        "Pisces"=0.87, 
                        "Crustacea"=0.36, 
                        "Mollusca"=0.17, 
                        "Invertebrata aquatica"=0.21),
         meat_mt=catch_mt * pedible) %>% 
  # Arrange columns
  select(area_type:major_group, pedible, genus_food_name, isscaap, everything()) %>% 
  rename(genus_group=genus_food_name)

# FAO 2012 catch stats by GENUS group
fao12 <- fao %>% 
  group_by(country, iso3, year) %>% 
  summarize(catch_mt=sum(catch_mt, na.rm=T)) %>% 
  filter(year == 2012 & !is.na(catch_mt)) %>% 
  rename(iso3_orig=iso3, 
         country_orig=country) %>% 
  mutate(country=countrycode(iso3_orig, "iso3c", "country.name"),
         iso3=countrycode(country, "country.name", "iso3c")) %>% 
  filter(!is.na(country)) %>% 
  ungroup() %>% 
  select(country, iso3, year, catch_mt)

# Export
write.csv(fao12, file=file.path(outdir, "FAO_2012_catch_by_country_for_included_groups.csv"), row.names=F)
saveRDS(fao, file=file.path(outdir, "FAO_1950_2017_landings_by_country_to_use.Rds"))




