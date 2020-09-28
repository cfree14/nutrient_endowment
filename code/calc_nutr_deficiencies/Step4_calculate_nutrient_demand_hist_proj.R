
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Input directories
eardir <- "data/ears/data"
popdir <- "data/population_growth/processed"
codedir <- "code/calc_nutr_deficiencies/functions"

# Outout directories
outputdir <- "output"

# Read DRI data
dris <- readRDS(file.path(eardir, "dietary_reference_intake_data.Rds"))

# Read population data
pop_natl <- readRDS(file.path(popdir, "WB_UN_1960_2100_human_population_by_country_agesex.Rds"))

# Read age/sex key 
agesex_key <- readxl::read_excel("data/genus_ear_wb_age_sex_key.xlsx", sheet=2)

# Read code to calculate nutrient supply requirement
source(file.path(codedir, "calc_nutr_demand.R"))


# Build data
################################################################################

# Calculate nutrient demands
ears <- dris %>% 
  # Reduce to EARs
  filter(dri_type=="Estimated Average Requirement (EAR)" & !is.na(value)) %>% 
  # Calculate mean nutrient intake requirement
  mutate(intake_avg_req=sapply(value, function(x) calc_nutr_demand(ptarget=0.05, x))) %>% 
  # Format age range
  mutate(age_range=gsub(" yr", "", age_range)) %>% 
  # Format sex
  filter(sex!="Both") %>% 
  mutate(sex=recode(sex, 
                    "Males"="male", 
                    "Females"="female"))

# Add EAR age
pop <- pop_natl %>% 
  # Remove 0-4 year olds
  filter(!age_range%in%c("0-4", "5-9")) %>% 
  # Add EAR age group
  left_join(agesex_key %>% select(age_un, age_ear), by=c("age_range"="age_un")) %>% 
  # Add EAR data
  left_join(ears %>% select(sex, age_range, nutrient, units, intake_avg_req),
            by=c("age_ear"="age_range", "sex")) %>% 
  # Calculate total nutrient supply required for each group
  mutate(supply_req_05perc=intake_avg_req*pop_size_05perc,
         supply_req_50perc=intake_avg_req*pop_size_50perc,
         supply_req_95perc=intake_avg_req*pop_size_95perc)


# Calculate sum demand
pop1 <- pop %>% 
  # Summarize by country, iso, year, nutrient
  group_by(country, iso3, source, year, nutrient, units) %>% 
  summarize(supply_req_05perc=sum(supply_req_05perc),
            supply_req_50perc=sum(supply_req_50perc),
            supply_req_95perc=sum(supply_req_95perc)) %>% 
  ungroup() %>% 
  # Convert to mt (Mg)
  mutate(units_short=gsub("/d", "", units)) %>% 
  # Remove protein
  filter(nutrient!="Protein") %>% 
  # Scalar
  mutate(scalar=recode(units_short, 
                       "g"=1,
                       "mg"=1/1e3,
                       "µg"=1/1e6)) %>% 
  # Convert from native units to metric tons
  mutate(supply_req_mt_yr_05perc = supply_req_05perc*scalar/1000/1000*365,
         supply_req_mt_yr_50perc = supply_req_50perc*scalar/1000/1000*365,
         supply_req_mt_yr_95perc = supply_req_95perc*scalar/1000/1000*365)


# Plot data
################################################################################

sdata <- pop1 %>% 
  filter(country=="Ghana" & nutrient=="Vitamin A")

# Plot data 
g <- ggplot(sdata) +
  # Add projection
  geom_ribbon(data=sdata, mapping=aes(x=year, ymin=supply_req_mt_yr_05perc, ymax=supply_req_mt_yr_95perc), alpha=0.2, fill="red") +
  geom_line(data=sdata, mapping=aes(x=year, y=supply_req_mt_yr_50perc), color="red") +
  # Labels
  labs(x="", y="Mean annual nutrient supply required\nto eradicate nutrient deficiecies (Mg)") +
  # Theme
  theme_bw()
g


# Final formatting
################################################################################

# Format
data <- pop1 %>% 
  select(country, iso3, nutrient, units, units_short, source, year, 
         supply_req_mt_yr_05perc, supply_req_mt_yr_50perc, supply_req_mt_yr_95perc) %>% 
  rename(type=source) %>% 
  mutate(type=recode(type, 
                     "WB historical"="Historical",
                     "UN-WPP projectioned"="Projected"))

# Global data
data_g <- data %>% 
  select(-c(country, iso3)) %>% 
  group_by(nutrient, units, units_short, type, year) %>% 
  summarize_all(sum) %>% 
  ungroup()
  

# Export data
saveRDS(data, file.path(outputdir, "1960_2100_nutrient_demand_by_country.Rds"))
saveRDS(data_g, file.path(outputdir, "1960_2100_nutrient_demand_global.Rds"))

