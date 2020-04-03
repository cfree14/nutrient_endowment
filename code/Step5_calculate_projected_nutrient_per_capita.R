
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(countrycode)

# Directories
outdir <- "output"
popdir <- "data/population_growth/processed"

# Read Free et al. (2020) catch projections
data_orig <- readRDS(file=file.path(outdir, "Free_etal_catch_proj_by_scen_rcp_cntry_genus_group.Rds"))

# Read Free et al. (2020) species key
spp_key <- read.csv(file.path(outdir, "Gaines_species_nutrient_content_key.csv"), as.is=T)

# Read projected human population growth
pop_growth <- readRDS(file.path(popdir, "WB_UN_1960_2100_human_population_by_country.Rds"))

# Read 2012 FAO catch
fao12 <- read.csv(file.path(outdir, "FAO_2012_catch_by_country_for_included_groups.csv"), as.is=T)

# Read GENUS data
genus <- read.csv(file=file.path(outdir, "GENUS_nutrient_per_100g_by_seafood_group.csv"), as.is=T)


# Build data
################################################################################

# Country key
cntry_key <- data_orig %>% 
  ungroup() %>% 
  select(sovereign_iso3, sovereign, country_iso3, country) %>% 
  unique() %>% 
  # Remove non-countries
  filter(!sovereign_iso3 %in% c("ABNJ", "Disputed", "Joint")) %>% 
  # Rename
  rename(iso3_orig=country_iso3, country_orig=country) %>% 
  # Recode a few countries
  mutate(country_format=recode(country_orig, 
                               "Micronesia"="Federated States of Micronesia",
                               "Northern Mariana Islands and Guam"="Guam",
                               "Puerto Rico of the United States"="Puerto Rico", 
                               "Virgin Islands of the United States"="US Virgin Islands")) %>% 
  # Add ISO3
  mutate(iso3=countrycode(country_format, "country.name", "iso3c"),
         iso3_use=ifelse(!is.na(iso3), iso3, sovereign_iso3),
         country_use=countrycode(iso3_use, "iso3c", "country.name")) %>% 
  # Reduce to important columns
  select(country_orig, iso3_use, country_use)


# Build data
data <- data_orig %>% 
  ungroup() %>% 
  # Step 1. Aggregate catch to correct national jurisdictions
  select(-c(sovereign, sovereign_iso3, country_iso3)) %>% 
  filter(country %in% cntry_key$country_orig) %>%
  left_join(cntry_key, by=c("country"="country_orig")) %>% 
  select(-country) %>% 
  group_by(scenario, rcp, iso3_use, country_use, genus_food_name, year) %>% 
  summarize(msy_mt=sum(msy_mt, na.rm=T),
            catch_mt=sum(catch_mt, na.rm=T),
            profits_usd=sum(profits_usd, na.rm=T)) %>% 
  rename(iso3=iso3_use, country=country_use) %>% 
  # Step 2. Scale catch to match 2012 values
  ungroup() %>% 
  filter(!is.na(genus_food_name) & iso3 %in% fao12$iso3) %>% 
  group_by(iso3) %>% 
  mutate(catch_mt_2012_pred=sum(catch_mt[year==2012 & rcp=="RCP26" & scenario=="No Adaptation"])) %>% 
  left_join(fao12 %>% select(iso3, catch_mt) %>%  rename(catch_mt_2012_obs=catch_mt), by="iso3") %>% 
  mutate(catch_scalar=catch_mt_2012_obs/catch_mt_2012_pred,
         msy_mt_scaled=msy_mt * catch_scalar,
         catch_mt_scaled=catch_mt * catch_scalar,
         profits_usd_scaled=profits_usd * catch_scalar) %>% 
  ungroup() %>% 
  # Step 3. Convert catch to edible meat
  filter(!is.na(genus_food_name)) %>% 
  mutate(major_group=recode(genus_food_name,
                            "Cephalopods"="Cephalopods",
                            "Crustaceans"="Crustaceans",
                            "Demersal Fish"="Finfish",
                            "Marine Fish; Other"="Finfish",
                            "Molluscs; Other"="Molluscs",
                            "Pelagic Fish"="Finfish"),
         pedible=recode(major_group, 
                       "Finfish"=0.87, 
                       "Crustaceans"=0.36, 
                       "Molluscs"=0.17, 
                       "Cephalopods"=0.21), 
         meat_mt=catch_mt_scaled * pedible) %>% 
  # Step 4. Add human population size
  left_join(pop_growth %>% select(iso3, year, pop_size_50perc), by=c("iso3", "year")) %>% 
  rename(human_pop_size=pop_size_50perc) %>% 
  # Step 5. Calculate daily per capita meat availability
  mutate(meat_g_dcap= (meat_mt*1000*1000) / 365 / human_pop_size) %>% 
  # Step 6. Calculate daily per capita nutrient availability
  left_join(genus, by="genus_food_name") %>% 
  mutate(calcium_mg_dcap = meat_g_dcap * calcium_mg / 100,
         calories_kcal_dcap = meat_g_dcap * calories_kcal / 100,
         carbohydrates_g_dcap = meat_g_dcap * carbohydrates_g / 100,
         copper_mg_dcap = meat_g_dcap * copper_mg / 100,
         fat_g_dcap = meat_g_dcap * fat_g / 100,
         folate_mcg_dcap = meat_g_dcap * folate_mcg / 100,
         iron_mg_dcap = meat_g_dcap * iron_mg / 100,
         magnesium_mg_dcap = meat_g_dcap * magnesium_mg / 100,
         monounsaturated_fatty_acids_g_dcap = meat_g_dcap * monounsaturated_fatty_acids_g / 100,
         niacin_mg_dcap = meat_g_dcap * niacin_mg / 100,
         phosphorus_mg_dcap = meat_g_dcap * phosphorus_mg / 100,
         polyunsaturated_fatty_acids_g_dcap = meat_g_dcap * polyunsaturated_fatty_acids_g / 100,
         potassium_mg_dcap = meat_g_dcap * potassium_mg / 100,
         protein_g_dcap = meat_g_dcap * protein_g / 100,
         riboflavin_mg_dcap = meat_g_dcap * riboflavin_mg / 100,
         saturated_fatty_acids_g_dcap = meat_g_dcap * saturated_fatty_acids_g / 100,
         sodium_mg_dcap = meat_g_dcap * sodium_mg / 100,
         thiamin_mg_dcap = meat_g_dcap * thiamin_mg / 100,
         vitamin_a_mcg_rae_dcap = meat_g_dcap * vitamin_a_mcg_rae / 100,
         vitamin_b6_mg_dcap = meat_g_dcap * vitamin_b6_mg / 100,
         vitamin_c_mg_dcap = meat_g_dcap * vitamin_c_mg / 100,
         zinc_mg_dcap = meat_g_dcap * zinc_mg / 100) %>% 
  # Step 7. Final cleanup
  # Eliminate regions without population growth data
  # Format RCP scenarios
  filter(!is.na(human_pop_size)) %>% 
  mutate(rcp=recode(rcp, 
                    "RCP26"="RCP 2.6",
                    "RCP45"="RCP 4.5",
                    "RCP60"="RCP 6.0",
                    "RCP85"="RCP 8.5")) 

# Inspect data
freeR::complete(data)
str(data)


# Calculate total daily per capita amounts
data_dcap <- data %>% 
  select(scenario, rcp, iso3, country, year, msy_mt_scaled, catch_mt_scaled, profits_usd_scaled, meat_mt, contains("dcap")) %>% 
  group_by(scenario, rcp, iso3, country, year) %>% 
  summarize_all(.fun=sum)

# Make long
data_dcap_long <- data_dcap %>% 
  gather(key="key", value="dcap", 6:ncol(.)) %>% 
  mutate(type=ifelse(grepl('dcap', key), "Nutrient", "Outcome"))


# Export data
save(data, data_dcap, data_dcap_long,
     file=file.path(outdir, "Free_etal_catch_nutrient_proj_by_scen_rcp_cntry.Rdata"))


# Plot example
################################################################################

# Subset
cntry <- "United States"
sdata <- data_dcap_long %>% 
  filter(country==cntry & type=="Nutrient")

# Plot 
g <- ggplot(sdata, aes(x=year, y=dcap, color=scenario)) +
  geom_line() +
  facet_grid(key ~ rcp, scale="free_y") +
  theme_bw()
g




# Plot example
################################################################################

# Subset
cntry <- "United States"
sdata <- data %>% 
  filter(country==cntry) %>% 
  group_by(scenario, rcp, year) %>% 
  summarize(catch_mt=sum(catch_mt),
            meat_mt=sum(meat_mt),
            meat_g_dcap=sum(meat_g_dcap, na.rm=T))

# PLot
g <- ggplot(sdata, aes(x=year, y=meat_g_dcap, color=rcp, group=rcp)) +
  facet_wrap(~scenario) +
  geom_line() +
  labs(x="", y="Edible meat supply (g per person per day)", title=cntry) +
  theme_bw()
g














