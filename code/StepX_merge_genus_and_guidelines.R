
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
tabledir <- "tables"
genusdir <- "data/genus/processed"
usdietdir <- "data/us_dietary_guidelines"
outputdir <- "shiny/v2/data"

# Read GENuS nutrient key
nutr_key <- rio::import(file.path(tabledir, "genus_nutrient_key.xlsx")) %>% 
  setNames(c("nutrient_type", "nutrient_subtype", "nutrient", "nutrient_full", "diet_req_yn"))

# Read GENUS nutrient supply by age and sex data
nut_cntry_age_sex_2011_orig <- readRDS(file.path(genusdir, "genus_nutrient_supplies_by_age_sex_2011.Rds"))

# Read and format US dietary guidelines
load(file.path(usdietdir, "2015_2020_US_dietary_guidelines.Rdata"))
diet_reqs_orig <- data
diet_reqs_catg_orig <- data_catg
rm(data, data_catg)


# Compare units
################################################################################

# GENUS units
genus_units <- nut_cntry_age_sex_2011_orig %>% 
  select(nutrient, units_short) %>% 
  unique() %>% 
  rename(units_genus=units_short)

# US dietary guideline units
us_diet_units <- diet_reqs_orig %>% 
  select(nutrient, units) %>% 
  unique() %>% 
  rename(units_dreqs=units)

# Compare units
units_check <- genus_units %>% 
  left_join(us_diet_units, by="nutrient")

# Change US diet requirement nutrient names and units to match GENUS units
diet_reqs1 <- diet_reqs_orig %>% 
  mutate(nutrient=recode(nutrient, "Carbohydrate"="Carbohydrates")) %>% 
  # Convert copper units from micrograms (mcg, μg) to milligrams (mg) 
  mutate(units_use=ifelse(nutrient=="Copper", "mg", units), 
         value_use=ifelse(nutrient=="Copper", value/1000, value)) %>% 
  # Add sex and sex_orig columns
  rename(sex_orig=sex) %>% 
  mutate(sex=sex_orig) %>% 
  # Arrange
  select(type:sex_orig, sex, everything())

# Duplicate Children (1-3) row and make Males/Females
diet_reqs_mf <- diet_reqs1 %>% 
  filter(sex_orig!="Children")
diet_reqs_c <- diet_reqs1 %>% 
  filter(sex_orig=="Children")
diet_reqs_c_mf <- bind_rows(diet_reqs_c %>% mutate(sex="Males"),
                            diet_reqs_c %>% mutate(sex="Females"))
diet_reqs2 <- bind_rows(diet_reqs_mf, diet_reqs_c_mf) %>% 
  arrange(nutrient, sex, age_range)
  
# Age groups
as.character(sort(unique(diet_reqs2$age_range)))

# Build age matching key
age_range_key <- tibble(age_range_genus=levels(nut_cntry_age_sex_2011_orig$age_range)) %>% 
  mutate(age_range_genus=factor(age_range_genus, levels=c("0-4",
                                                    "5-9", 
                                                    "10-14", 
                                                    "15-19", 
                                                    "20-24", 
                                                    "25-29", 
                                                    "30-34", 
                                                    "35-39", 
                                                    "40-44", 
                                                    "45-49",
                                                    "50-54",
                                                    "55-59",
                                                    "60-64",
                                                    "65-69",
                                                    "70-74",
                                                    "75-79",
                                                    "80+")),
         age_range_usdiet=recode_factor(age_range_genus,
                                         "0-4"="1-3",            
                                         "5-9"="4-8",                
                                         "10-14"="9-13",   
                                         "15-19"="14-18",              
                                         "20-24"="19-30",              
                                         "25-29"="19-30",              
                                         "30-34"="31-50",              
                                         "35-39"="31-50",              
                                         "40-44"="31-50",
                                         "45-49"="31-50",
                                         "50-54"="51+",              
                                         "55-59"="51+",              
                                         "60-64"="51+",              
                                         "65-69"="51+",              
                                         "70-74"="51+",              
                                         "75-79"="51+",              
                                         "80+"="51+"))

# Format diet requirements for merge
diet_reqs3 <- diet_reqs2 %>% 
  select(nutrient, age_range, sex, units_use, value_use) %>% 
  rename(diet_req=value_use,
         diet_req_units=units_use)

# Format age/sex nutrients for merge
# Double the children groups and make male/female for each age group
nut_cntry_age_sex_2011_mf <- nut_cntry_age_sex_2011_orig %>% 
  filter(sex!="Children") %>% 
  mutate(sex_orig=sex)
nut_cntry_age_sex_2011_c <- nut_cntry_age_sex_2011_orig %>% 
  filter(sex=="Children") %>% 
  mutate(sex_orig=sex)
nut_cntry_age_sex_2011_c_mf <- bind_rows(nut_cntry_age_sex_2011_c %>% mutate(sex="Males"), 
                                         nut_cntry_age_sex_2011_c %>% mutate(sex="Females")) 
nut_cntry_age_sex_2011 <- bind_rows(nut_cntry_age_sex_2011_mf, nut_cntry_age_sex_2011_c_mf) %>% 
  arrange(country, nutrient, sex, age_range) %>% 
  select(iso3, country, sex_orig, sex, age_range, everything())


# Format data
data <- nut_cntry_age_sex_2011 %>% 
  ungroup() %>% 
  # Step 1. Add diet guideline age range to GENUS data
  mutate(age_range=factor(age_range, levels=c("0-4",
                                               "5-9", 
                                               "10-14", 
                                               "15-19", 
                                               "20-24", 
                                               "25-29", 
                                               "30-34", 
                                               "35-39", 
                                               "40-44", 
                                               "45-49",
                                               "50-54",
                                               "55-59",
                                               "60-64",
                                               "65-69",
                                               "70-74",
                                               "75-79",
                                               "80+"))) %>% 
  left_join(age_range_key, by=c("age_range"="age_range_genus")) %>% 
  # Step 2. Add diet requirements by nutrient, age range, and sex
  left_join(diet_reqs3, by=c("nutrient", "age_range_usdiet"="age_range", "sex")) %>% 
  # Step 3. Arrange columns
  select(iso3, country, sex_orig, sex, age_range, age_range_usdiet, 
         nutrient_type, nutrient_label, nutrient, units_long, units_short,
         diet_req, diet_req_units, 
         value_med, value_lo, value_hi, everything()) %>% 
  # Step 4. Calculate percent difference between median value and requirement
  mutate(value_perc_req = (value_med-diet_req) / diet_req * 100,
         value_perc_req_cap=pmin(value_perc_req, 100)) %>% 
  # Step 5. Add group label
  mutate(sex_short=recode(sex, "Children"="", "Females"="F", "Males"="M"),
         group=str_trim(paste(age_range, sex_short))) #%>% 
  # Step 6. Remove nutrients without US dietary guidelines
  # filter(nutrient %in% nutr_key$nutrient[nutr_key$diet_req_yn=="X"])

# Inspect
str(data)
freeR::complete(data)

# Export data
saveRDS(data, file.path(outputdir, "genus_nutrient_supplies_by_age_sex_2011_w_us_diet_req.Rds"))


# Example plot
################################################################################

# Example plot
cntry <- "Ghana"
sdata <- data %>% 
  filter(country==cntry) %>% 
  mutate(sex=recode(sex, "Females"="Women", "Males"="Men"),
         nutrient_type=recode(nutrient_type, 
                              "Macronutrient"="Macro-\nnutrient")) %>% 
  filter(nutrient %in% nutr_key$nutrient[nutr_key$diet_req_yn=="X"])

# Approach #1
g <- ggplot(sdata, aes(x=group, y=nutrient, fill=value_perc_req_cap)) +
  facet_grid(nutrient_type ~ ., scale="free_y", space="free_y") +
  geom_raster() +
  # Labels
  labs(x="", y="", title="2011 nutritional health by age and sex") +
  scale_fill_gradient2(name="Percent above or below\ndaily recommendation", 
                       midpoint = 0) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        axis.title = element_blank())
g

# Approach #2
g <- ggplot(sdata, aes(x=age_range, y=nutrient, fill=value_perc_req_cap)) +
  facet_grid(nutrient_type ~ sex, scale="free", space="free") +
  geom_raster() +
  # Labels
  labs(x="", y="", title="2011 nutritional health by age and sex") +
  scale_fill_gradient2(name="Percent above or below\ndaily recommendation", 
                       midpoint = 0) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom",
        axis.title = element_blank())
g




