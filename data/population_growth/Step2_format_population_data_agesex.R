

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


# Format UN data (future)
################################################################################

# UN World Population Prospects 2019 probabilistic projections
# https://population.un.org/wpp/Download/Probabilistic/Population/
# Population sizes are in 1000s of people

# Merge data
#####################################

# Merge data
pop_proj_orig <- purrr::map_df(1:5, function(x) {
  
  # Age columns
  ages_lo <- seq(0,100,5)
  ages_hi <- ages_lo[2:length(ages_lo)]-1
  ages <- paste(ages_lo, ages_hi, sep="-")
  ages[ages=="100-4"] <- "100+"
  
  # Read female data
  f_pop_proj_sheet <- readxl::read_xlsx(file.path(indir, "UN_PPP2019_Output_PopulationByAge_Female.xlsx"), 
                                       col_names = c("index", "variant", "region", "notes", "region_code", "region_type", "parent_code", "year", 
                                                     ages),
                                       sheet=x, skip=17, na="...") %>% 
    mutate(sex="female") %>% 
    select(sex, everything())
  
  # Read male data
  m_pop_proj_sheet <- readxl::read_xlsx(file.path(indir, "UN_PPP2019_Output_PopulationByAge_Male.xlsx"), 
                                           col_names = c("index", "variant", "region", "notes", "region_code", "region_type", "parent_code", "year", 
                                                         ages),
                                           sheet=x, skip=17, na="...") %>% 
    mutate(sex="male") %>% 
    select(sex, everything())
  
  # Format data
  pop_proj_sheet <- bind_rows(f_pop_proj_sheet, m_pop_proj_sheet) %>% 
    gather(key="age_range", value="pop_size", 10:ncol(.)) %>% 
    select(-index) %>% 
    select(region_type, region, region_code, parent_code, notes, variant, year, sex, age_range, pop_size, everything())
  
  
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
  # Order age range as factor
  mutate(age_range=factor(age_range, levels=ages)) %>% 
  # Arrange columns
  select(region_type:age_range, pop_size_05perc, pop_size_20perc, pop_size_50perc, pop_size_80perc, pop_size_95perc)

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
  select(region, year, sex, age_range, pop_size_05perc:pop_size_95perc) %>% 
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
g <- ggplot(sdata, aes(x=year, y=pop_size_50perc/1e6, fill=sex, alpha=age_range)) +
  geom_area() +
  labs(x="", y="Population size (millions of people)") +
  theme_bw()
g





# # Export
# save(pop_proj_cntry, pop_proj_cntry2,
#      pop_proj_subregion, pop_proj_region, pop_proj_world,
#      pop_proj_sdg_region, pop_proj_sdg_subregion,
#      pop_proj_devgroup, pop_proj_incgroup, pop_proj_other,
#      file=file.path(outdir, "UN_WPP_2019_population_projections.Rdata"))

