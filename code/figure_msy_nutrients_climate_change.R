
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/nutrient_content/processed"
fishdir <- "data/fish_projections/data"
plotdir <- "figures"
  
# Read MSY data
# load("/Users/cfree/Dropbox/Chris/UCSB/projects/edf_climate/cc_trade/data/gaines/gaines_eez_msy_time_series.Rdata")
# saveRDS(msy_ts_g, file=file.path(fishdir, "gaines_etal_2019_msy_species_stock_projections.Rds"))
msy_ts <- readRDS(file.path(fishdir, "gaines_etal_2019_msy_species_stock_projections.Rds"))

# Read nutrient data
load(file.path(datadir, "Vaitla_etal_2018_nutrient_data.Rdata"))


# Build data
################################################################################

# Calculate MT of nutrient from MT of edible meat 
# Units: g/100g, mg/100g, ug/100g
# For testing: edible_mt <- 1000; nutrient_conc <- 0.7176820; units <- "g/100g"
calc_nutrient_mt <- function(edible_mt, nutrient_conc, units){
  # Convert concentration to g/100g (which is equivalent to mt/100mt)
  if(units=="g/100g"){nutrient_g <- nutrient_conc}
  if(units=="mg/100g"){nutrient_g <- nutrient_conc / 1e3}
  if(units=="ug/100g"){nutrient_g <- nutrient_conc / 1e6}
  # Calculate MT of nutrient
  nutrient_mt <- edible_mt / 100 * nutrient_g
  return(nutrient_mt)
}

# FAO species with nutrient data
upsides_spp <- sort(unique(msy_ts$species))
nutrient_spp <- sort(unique(preds_wide$species))
upsides_spp_ndata <- upsides_spp[upsides_spp %in% nutrient_spp]

# Build data
data <- msy_ts %>% 
  # Format columns
  rename(comm_name=comm_name1, msy_mt=msy) %>% 
  mutate(rcp=recode(rcp, "RCP26"="RCP 2.6", "RCP45"="RCP 4.5", "RCP60"="RCP 6.0", "RCP85"="RCP 8.5"),
         edible_mt=msy_mt * 0.87) %>% 
  # Reduce to species with nutrient data
  filter(species %in% preds_wide$species) %>% 
  # Add nutrient concentrations
  left_join(preds_wide, by="species") %>% 
  # Calculate MT of nutrients
  mutate(protein_mt=calc_nutrient_mt(edible_mt, protein_g, "g/100g"),
         fat_mt=calc_nutrient_mt(edible_mt, fat_g, "g/100g"),
         omega3_mt=calc_nutrient_mt(edible_mt, omega3_g, "g/100g"),
         omega6_mt=calc_nutrient_mt(edible_mt, omega6_g, "g/100g"), 
         iron_mt=calc_nutrient_mt(edible_mt, iron_mg, "mg/100g"),
         zinc_mt=calc_nutrient_mt(edible_mt, zinc_mg, "mg/100g"),
         vitA_mt=calc_nutrient_mt(edible_mt, vitA_ug, "ug/100g"),
         vitB12_mt=calc_nutrient_mt(edible_mt, vitB12_ug, "ug/100g"),
         vitD_mt=calc_nutrient_mt(edible_mt, vitD_ug, "ug/100g")) %>% 
  # Select columns
  select(rcp:edible_mt, protein_mt:vitD_mt)


# Time series of nutrients
total_ts <- data %>% 
  select(-c(species, comm_name)) %>% 
  group_by(rcp, year) %>% 
  summarize_all(sum, na.rm=T) %>% 
  gather(key="type", value="weight_mt", 3:ncol(.))

# Subset nutrient time series
nutrient_ts <- total_ts %>% 
  filter(!type %in% c("msy_mt", "edible_mt"))


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=9),
                  plot.title=element_text(size=11),
                  legend.text = element_text(size=9),
                  legend.title =element_text(size=11),
                  axis.text.x = element_text(angle = 90, hjust = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black")) 

# Plot nutrient time series (raw)
g1 <- ggplot(nutrient_ts, aes(x=year, y=weight_mt/1e6, fill=type)) +
  geom_area() +
  facet_wrap(~ rcp, ncol=2) +
  labs(x="", y="Weight (millions of mt)") +
  scale_x_continuous(breaks=c(2012, seq(2020,2100,10))) +
  scale_fill_discrete(name="Nutrient") +
  theme_bw() + my_theme
g1

ggsave(g1, filename=file.path(plotdir, "figure_msy_nutrients_cc_ts.png"), 
       width=6.5, height=6.5, units="in", dpi=600)
  
# Plot nutrient time series (proportion)
g2 <- ggplot(nutrient_ts, aes(x=year, y=weight_mt/1e6, fill=type)) +
  geom_area(position="fill") +
  facet_wrap(~ rcp, ncol=2) +
  labs(x="", y="Weight (millions of mt)") +
  scale_x_continuous(breaks=c(2012, seq(2020,2100,10))) +
  scale_fill_discrete(name="Nutrient") +
  theme_bw() + my_theme
g2
  
ggsave(g2, filename=file.path(plotdir, "figure_msy_nutrients_prop_cc_ts.png"), 
       width=6.5, height=6.5, units="in", dpi=600)







