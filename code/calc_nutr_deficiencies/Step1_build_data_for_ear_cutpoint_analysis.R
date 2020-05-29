
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
eardir <- "data/ears/data"
genusdir <- "data/genus/processed"
popdir <- "data/population_growth/processed"
datadir <- "data"
outputdir <- "output"
codedir <- "code/calc_nutr_deficiencies/alon_revised/functions"

# Read helper functions
sapply(list.files(codedir, pattern=".R"), function(x) source(file.path(codedir, x)))

# Read data
# Population size: by sex/age group
# Daily nutrient supply: by age/sex group
# EAR cut points: by age/sex group
ear_orig <-  readRDS(file.path(eardir, "dietary_reference_intake_data.Rds"))
genus_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))
pop_orig <- readRDS(file.path(popdir, "WB_2011_population_size_by_country_agesex.Rds"))

# Read age key
age_key <- read.delim(file.path(datadir, "genus_ear_wb_age_sex_key.txt"), as.is=T)

# Merge data
################################################################################

# Inspect age/sex levels
######################################

# Inspect age/sex levels
ear_key <- ear_orig %>% 
  select(sex, stage, age_range) %>% 
  unique()
genus_key <- genus_orig %>% 
  select(sex, age_range) %>% 
  unique()
pop_key <- pop_orig %>% 
  select(sex, age) %>% 
  unique()

# Inspect nutrient names
# Dietary fiber (GENUS) == Total fiber (EAR)
ear_nuts <- ear_orig %>% 
  select(nutrient, units) %>% 
  unique() %>% 
  filter(!is.na(units)) %>% 
  rename(units_ear=units)
genus_nuts <- genus_orig %>% 
  select(nutrient, units_long) %>% 
  unique() %>% 
  mutate(units_genus=gsub("person/day", "d", units_long),
         units_genus=gsub("microgram RAE|microgram", "µg", units_genus)) %>% 
  select(-units_long)
nutr_key <- genus_nuts %>% 
  left_join(ear_nuts, by="nutrient") %>% 
  mutate(match=units_ear==units_genus) %>% 
  arrange(match)

# Convert the following EAR cutpoints
# Potassium: g/d to mg/d
# Copper: ug/d to mg/d
# Sodium: g/d to mg/d

# WB population data
######################################

# Format WB population data for merge
pop <- pop_orig %>% 
  # filter(country_use=="Bangladesh") %>% 
  # Reduce to important columns
  select(iso3_use, country_use, sex, age, pop_size) %>% 
  # Rename columns
  rename(iso3=iso3_use, country=country_use, sex_wb=sex, age_wb=age) %>% 
  # Add harmonized age columns
  mutate(age_wb=as.character(age_wb)) %>% 
  left_join(age_key %>% select(age_wb, age_final), by="age_wb") %>% 
  rename(age=age_final) %>% 
  # Add sex column
  mutate(sex=ifelse(age=="5-9", "combined", sex_wb)) %>% 
  # Calculate totals by harmonized sex/age category
  group_by(iso3, country, sex, age) %>% 
  summarize(pop_size=sum(pop_size)) %>% 
  ungroup()
  
# Inspect
freeR::complete(pop)


# GENUS nutrient data
######################################

# Format GENUS nutrient data for merge
genus <- genus_orig %>% 
  # filter(country_use=="Bangladesh") %>% 
  # Reduce to important columns
  select(iso3_use, country_use, sex, age_range, nutrient_type:value_hi) %>% 
  # Rename columns
  rename(iso3=iso3_use, country=country_use, sex_genus=sex, age_genus=age_range) %>% 
  # Add harmonized age columns
  mutate(age_genus=as.character(age_genus)) %>%
  left_join(age_key %>% select(age_genus, age_final), by="age_genus") %>% 
  rename(age=age_final) %>% 
  # Add sex column
  mutate(sex_genus=tolower(sex_genus), 
         sex_genus=recode(sex_genus, 
                        "males"="male", 
                        "females"="female"), 
         sex=ifelse(age=="5-9", "combined", sex_genus),
         sex=recode(sex, "children"="NA*")) %>% 
  # Format nutrient columns to match EAR columns
  mutate(units_short=recode(units_short, 
                            "microgram"="µg", 
                            "microgram RAE"="µg"),
         units_long=paste0(units_short, "/d")) %>% 
  # Arrange
  select(iso3, country, sex, age, nutrient_type, nutrient, units_long, value_med, value_lo, value_hi) %>% 
  rename(units=units_long, supply_med=value_med, supply_lo=value_lo, supply_hi=value_hi)
  
# Inspect
freeR::complete(genus)
table(genus$units)


# EAR cutpoint data
######################################

# Format EAR data for merge
ear <- ear_orig %>% 
  # Only EARs
  filter(dri_type=="Estimated Average Requirement (EAR)") %>% 
  # Reduce to important columns
  select(nutrient, units, sex, stage, age_range, value) %>% 
  # Rename columns
  rename(sex_ear=sex, age_ear=age_range, ear=value) %>% 
  # Remove pregnancy/lactating stages
  filter(!stage%in%c("Pregnancy", "Lactation")) %>% 
  select(-stage) %>% 
  # Add harmonized age columns
  mutate(age_ear=as.character(age_ear) %>% gsub(" yr", "", .),
         age_ear=recode(age_ear, 
                        "0-6 mo"="0-0.5", 
                        "6-12 mo"="0.5-1")) %>%
  left_join(age_key %>% select(age_ear, age_final), by="age_ear") %>% 
  rename(age=age_final) %>% 
  # Add sex column
  mutate(sex_ear=tolower(sex_ear),
         sex_ear=recode(sex_ear, 
                        "males"="male", 
                        "females"="female", 
                        "both"="combined"), 
         sex=ifelse(age=="5-9", "combined", sex_ear)) %>% 
  # Convert values
  # (1) Sodium: g/d to mg/d (2) Potassium: g/d to mg/d (3) Copper: ug/d to mg/d
  mutate(ear=ifelse(nutrient=="Copper", ear/1000, ear),
         ear=ifelse(nutrient %in% c("Sodium", "Potassium"), ear*1000, ear), 
         units=ifelse(nutrient %in% c("Sodium", "Potassium", "Copper"), "mg/d", units)) %>% 
  # Arrange
  select(age, sex, nutrient, units, ear)

# Check number of EARS per grou
nstats <- ear %>% 
  group_by(nutrient, sex, age) %>% 
  summarize(n=sum(!is.na(ear)))

# Inspect
freeR::complete(ear)
table(ear$units)


# Merge data
######################################

# Nutrients with EAR cutpoints
ear_nutrients <- ear %>% 
  filter(!is.na(ear)) %>% pull(nutrient) %>% unique() %>% sort()

# Merge data
data <- genus %>% 
  # Add population size data
  left_join(pop %>% select(-country), by=c("iso3", "sex", "age")) %>% 
  # Add EAR cut points
  left_join(ear, by=c("age", "sex", "nutrient", "units")) %>% 
  # Eliminate sex/groups without all data components
  filter(sex!="NA*") %>% 
  # Calculate percent under/over EAR cutpoint
  mutate(ear_perc=(supply_med-ear)/ear*100,
         ear_perc_cap=pmin(ear_perc,200)) %>% 
  # Format age/sex
  mutate(sex=recode_factor(sex, 
                           "combined"="Children",
                           "female"="Women",
                           "male"="Men")) %>% 
  mutate(age=factor(age, levels=c("5-9",
                                  "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", 
                                  "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", 
                                  "70-74", "75-79", "80+"))) %>% 
  # Reduce to only nutrients with EAR cutpoints
  filter(nutrient %in% ear_nutrients & nutrient!="Protein")
  
# Inspect
freeR::complete(data)


# Plot example
plot_raster_supply_ear_perc(data, country="Bangladesh")
plot_raster_supply_ear_perc(data, country="Madagascar")
plot_raster_supply_ear_perc(data, country="United States")
plot_raster_supply_ear_perc(data, country="Germany")
plot_raster_supply_ear_perc(data, country="South Africa")

# Export data
saveRDS(data, file.path(outputdir, "GENUS_EAR_population_data_merged.Rds"))




  