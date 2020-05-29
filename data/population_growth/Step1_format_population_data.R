

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(wbstats)
library(countrycode)

# Directories
indir <- "data/population_growth/raw/un_wpp19"
outdir <- "data/population_growth/processed"

# WB countries
wb_cntry_df_full <- wbcountries()
wb_cntry_df <- wb_cntry_df_full %>% filter(region!="Aggregates")



# Format WB age/sex population data (past)
################################################################################

# WP population indicators
wbi <- wbindicators()
wbi_pop <- wbi %>% 
  filter(grepl("SP.POP", indicatorID))

# Identify indicator codes for population by age/sex
age_ids <- paste0("SP.POP.", c("0004", "0509", "1014", "1519", "2024",
                               "2529", "3034", "3539", "4044", "4549", "5054",
                               "5559", "6064", "6569", "7074", "7579", "80UP"))
age_sex_ids <- sort(c(paste0(age_ids, ".FE"), paste0(age_ids, ".MA")))

# 2011 population by country and age/sex
pop_2011_agesex_orig <- wb(country=wb_cntry_df$iso3c, indicator = age_sex_ids , startdate = 2011, enddate = 2011)

# Country key
wb_cntry_key <- pop_2011_agesex_orig %>% 
  # Reduce to unique countries
  select(country, iso3c, iso2c) %>% 
  unique() %>% 
  rename(country_orig=country, iso3_orig=iso3c, iso2_orig=iso2c) %>% 
  # Format ISO3/country names
  mutate(country_use=countrycode(iso3_orig, "iso3c", "country.name"),
         iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
  # Replace missing values
  mutate(country_use=ifelse(is.na(country_use), country_orig, country_use),
         iso3_use=ifelse(is.na(iso3_use), iso3_orig, iso3_use)) %>% 
  # Arrange
  select(country_orig, iso3_orig, iso2_orig, country_use, iso3_use)

# Format data
pop_2011_agesex <- pop_2011_agesex_orig %>% 
  # Rename columns
  rename(country_orig=country, iso3_orig=iso3c, iso2_orig=iso2c, year=date, pop_size=value) %>% 
  # Format year
  mutate(year=as.numeric(year)) %>% 
  # Add formatted ISO/country
  left_join(wb_cntry_key %>% select(country_orig, country_use, iso3_use), by="country_orig") %>% 
  # Add age and sex
  mutate(sex=ifelse(grepl("female", indicator), "female", "male"),
         age=gsub("Population ages |, female|, male", "", indicator)) %>% 
  # Format age
  mutate(age=recode(age, 
                    "00-04"="0-4", 
                    "05-09"="5-9",
                    "80 and above"="80+"),
         age=factor(age, levels=c("0-4", "5-9", "10-14", "15-19", "20-24",
                                  "25-29", "30-34", "35-39", "40-44", "45-49", "50-54",
                                  "55-59", "60-64", "65-69", "70-74", "75-79", "80+"))) %>% 
  # Arrange
  select(country_orig, iso3_orig, iso2_orig, country_use, iso3_use, year, 
         indicatorID, indicator, sex, age, sex,
         pop_size, everything()) %>% 
  arrange(country_use, year, age, sex)

# Export data
saveRDS(pop_2011_agesex, file=file.path(outdir, "WB_2011_population_size_by_country_agesex.Rds"))


# Format WB total population data (past)
################################################################################

# Total population
pop_hist_orig <- wb(country=wb_cntry_df$iso3c, indicator = "SP.POP.TOTL", startdate = 1950, enddate = 2019)

# Country key
wb_cntry_key <- pop_hist_orig %>% 
  # Reduce to unique countries
  select(country, iso3c, iso2c) %>% 
  unique() %>% 
  rename(country_orig=country, iso3_orig=iso3c, iso2_orig=iso2c) %>% 
  # Format ISO3/country names
  mutate(country_use=countrycode(iso3_orig, "iso3c", "country.name"),
         iso3_use=countrycode(country_use, "country.name", "iso3c")) %>% 
  # Replace missing values
  mutate(country_use=ifelse(is.na(country_use), country_orig, country_use),
         iso3_use=ifelse(is.na(iso3_use), iso3_orig, iso3_use)) %>% 
  # Arrange
  select(country_orig, iso3_orig, iso2_orig, country_use, iso3_use)


# Format data
pop_hist <- pop_hist_orig %>% 
  # Reduce to important columns
  select(country, iso3c, iso2c, date, value) %>% 
  # Rename columns
  rename(country_orig=country, iso3_orig=iso3c, iso2_orig=iso2c, year=date, pop_size=value) %>% 
  # Format year
  mutate(year=as.numeric(year)) %>% 
  # Add formatted ISO/country
  left_join(wb_cntry_key %>% select(country_orig, country_use, iso3_use), by="country_orig") %>% 
  # Arrange
  select(country_orig, iso3_orig, iso2_orig, country_use, iso3_use, year, pop_size, everything()) %>% 
  arrange(country_use, year)
  

# Plot data
g <- ggplot(pop_hist, aes(x=year, y=pop_size/1e9,  fill=iso3_use)) +
  geom_area() +
  labs(x="", y="Population size (billions)") +
  scale_x_continuous(breaks=seq(1960,2020,10), limits=c(1960,2020)) +
  theme_bw() +
  theme(legend.position = "none")
g

# Export 
saveRDS(pop_hist, file=file.path(outdir, "WB_1960_2017_population_size_by_country.Rds"))


# Format UN data (future)
################################################################################

# UN World Population Prospects 2019 probabilistic projections
# https://population.un.org/wpp/Download/Probabilistic/Population/
# Population sizes are in 1000s of people

# Merge data
#####################################

# Merge data
pop_proj_orig <- purrr::map_df(1:5, function(x) {
  
  # Read data
  pop_proj_sheet1 <- readxl::read_xlsx(file.path(indir, "UN_PPP2019_Output_PopTot.xlsx"), 
                                       col_names = c("index", "variant", "region", "notes", "region_code", "region_type", "parent_code", seq(2020, 2100, 5)),
                                       sheet=x, skip=17, na="...")
  
  # Format data
  pop_proj_sheet2 <- pop_proj_sheet1  %>% 
    gather(key="year", value="pop_size", 8:ncol(.)) %>% 
    select(-index) %>% 
    select(region_type, region, region_code, parent_code, notes, variant, year, pop_size, everything())
  
  
})

# Format data
#####################################

# Format merged data
pop_proj <- pop_proj_orig %>% 
  # Convert population size to number of people
  mutate(pop_size=pop_size*1000) %>% 
  # Spread pop estimates
  spread(key="variant", value="pop_size") %>% 
  # Rename pop estimates
  rename(pop_size_05perc="Lower 95 PI",
         pop_size_20perc="Lower 80 PI",
         pop_size_50perc="Median PI",
         pop_size_80perc="Upper 80 PI",
         pop_size_95perc="Upper 95 PI") %>% 
  # Convert year to numeric
  mutate(year=as.numeric(year)) %>% 
  # Arrange columns
  select(region_type:year, pop_size_05perc, pop_size_20perc, pop_size_50perc, pop_size_80perc, pop_size_95perc)

# Inspect
str(pop_proj)

# Divide data
#####################################

# Split into individual datasets
table(pop_proj$region_type)

pop_proj_cntry <- pop_proj %>% 
  filter(region_type=="Country/Area")

pop_proj_devgroup <- pop_proj %>% 
  filter(region_type=="Development Group")

pop_proj_incgroup <- pop_proj %>% 
  filter(region_type=="Income Group")

pop_proj_region <- pop_proj %>% 
  filter(region_type=="Region")

pop_proj_sdg_region <- pop_proj %>% 
  filter(region_type=="SDG region")

pop_proj_sdg_subregion <- pop_proj %>% 
  filter(region_type=="SDG subregion")

pop_proj_other <- pop_proj %>% 
  filter(region_type=="Special other")

pop_proj_subregion <- pop_proj %>% 
  filter(region_type=="Subregion")

pop_proj_world <- pop_proj %>% 
  filter(region_type=="World")

# Format country data
#####################################

# Format country projections
pop_proj_cntry2 <- pop_proj_cntry %>% 
  # Select columns
  select(region, year, pop_size_05perc:pop_size_95perc) %>% 
  # Rename columns
  rename(country_orig=region) %>% 
  # Add ISO3/country name
  mutate(country_use=countrycode(country_orig %>% recode("Eswatini"="Swaziland"), 
                                 "country.name", "country.name"), 
         iso3_use=countrycode(country_use, "country.name", "country.name")) %>% 
  # Arrange
  select(country_orig, country_use, iso3_use, everything()) %>% 
  arrange(country_use, year)


# Plot and export data
#####################################

# Plot example country
cntry <- "Ghana"
# cntry <- "United States"
sdata <- pop_proj_cntry2 %>% 
  filter(country_use==cntry)
g <- ggplot(sdata, aes(x=year, y=pop_size_50perc/1e6)) +
  geom_ribbon(mapping=aes(ymin=pop_size_05perc/1e6, ymax=pop_size_95perc/1e6), fill="grey80", alpha=0.3) +
  geom_line() +
  labs(x="", y="Population size (millions of people)") +
  theme_bw()
g

# Export
save(pop_proj_cntry, pop_proj_cntry2,
     pop_proj_subregion, pop_proj_region, pop_proj_world,
     pop_proj_sdg_region, pop_proj_sdg_subregion,
     pop_proj_devgroup, pop_proj_incgroup, pop_proj_other,
     file=file.path(outdir, "UN_WPP_2019_population_projections.Rdata"))


# Merge historical and future
################################################################################

# Format historical for merge
pop_hist_merge <- pop_hist %>% 
  select(country_use, iso3_use, year, pop_size) %>% 
  rename(pop_size_50perc=pop_size) %>% 
  mutate(source="World Bank historical") %>% 
  select(source, iso3_use, country_use, year, pop_size_50perc) %>% 
  rename(iso3=iso3_use, country=country_use)
  
# Format future for merge
pop_proj_merge <- pop_proj_cntry2 %>% 
  select(country_use, iso3_use, year, pop_size_05perc:pop_size_95perc) %>% 
  mutate(source="World Bank historical") %>% 
  rename(iso3=iso3_use, country=country_use) %>% 
  select(source, country, iso3, year, pop_size_05perc:pop_size_95perc)

# Merge population growth
pop_data <- bind_rows(pop_proj_merge, pop_hist_merge) %>% 
  arrange(country, iso3, year)

# Export
saveRDS(pop_data, file=file.path(outdir, "WB_UN_1960_2100_human_population_by_country.Rds"))





