
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)

# Directories
datadir <- "data/nutrient_content/processed"
plotdir <- "figures"
  
# Read FAO data
# load("/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2018_FAO_aq_production.Rdata")
fao_orig <- read.csv(file="/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2016_FAO_capture_landings.csv", as.is=T)  
  
# Read data
load(file.path(datadir, "Vaitla_etal_2018_nutrient_data.Rdata"))

# Read FAO data
# load("/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2018_FAO_aq_production.Rdata")
fao_orig <- read.csv(file="/Users/cfree/Dropbox/Chris/UCSB/projects/protein_curve/ocean-protein/aquaculture/data/processed/1950_2016_FAO_capture_landings.csv", as.is=T)  



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

# Actinopterygii only
sort(unique(fao_orig$isscaap))
fin_isscaaps <- c("Carps, barbels and other cyprinids",
                  "Cods, hakes, haddocks", 
                  "Flounders, halibuts, soles",
                  "Herrings, sardines, anchovies", 
                  "Marine fishes not identified",
                  "Miscellaneous coastal fishes",
                  "Miscellaneous demersal fishes",
                  "Miscellaneous diadromous fishes", 
                  "Miscellaneous freshwater fishes", 
                  "Miscellaneous pelagic fishes",
                  "River eels",
                  "Salmons, trouts, smelts", 
                  "Shads",
                  "Sturgeons, paddlefishes", 
                  "Tilapias and other cichlids",
                  "Tunas, bonitos, billfishes")
  
# Catch of FAO finfish
# Edwards et al. (2019): 87% of finfish is edible
fao_fin <- fao_orig %>% 
  # Tons only
  filter(units=="t" & area_type=="marine" & isscaap %in% fin_isscaaps) %>% 
  # Sum by species
  group_by(species_code, comm_name, species, isscaap, order, family, year) %>% 
  summarize(tl_mt=sum(quantity, na.rm=T)) %>% 
  ungroup() %>% 
  # Add proportion edible meat
  mutate(edible_mt=tl_mt * 0.87) %>% 
  # Add nutrient concentrations
  mutate(nutrient_yn=ifelse(species %in% preds_wide$species, "yes", "no")) %>% 
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
  # Reduce to important columns
  select(species_code:edible_mt, nutrient_yn, protein_mt:vitD_mt)
  
# Summarize finfish catch
fao_fin_tot <- fao_fin %>% 
  group_by(year) %>% 
  summarize(tl_mt=sum(tl_mt, na.rm=T),
            edible_mt=sum(edible_mt, na.rm=T),
            protein_mt=sum(protein_mt, na.rm=T),
            fat_mt=sum(fat_mt, na.rm=T),
            omega3_mt=sum(omega3_mt, na.rm=T),
            omega6_mt=sum(omega6_mt, na.rm=T),
            iron_mt=sum(iron_mt, na.rm=T),
            zinc_mt=sum(zinc_mt, na.rm=T),
            vitA_mt=sum(vitA_mt, na.rm=T),
            vitB12_mt=sum(vitB12_mt, na.rm=T),
            vitD_mt=sum(vitD_mt, na.rm=T)) %>% 
  gather(key="type", value="weight_mt", 2:ncol(.)) %>% 
  mutate(type=recode(type, "tl_mt"="Landings", "edible_mt"="Edible meat",
         "protein_mt"="Protein", "fat_mt"="Fat", "omega3_mt"="Omega-3 fatty acids", "omega6_mt"="Omega-6 fatty acids", 
         "iron_mt"="Iron", "zinc_mt"="Zinc", "vitA_mt"="Vitamin A", "vitB12_mt"="Vitamin B12", "vitD_mt"="Vitamin D"))

# Add edible meat totals for evaluated species
df <- fao_fin %>% 
  filter(nutrient_yn=="yes") %>% 
  group_by(year) %>% 
  summarize(weight_mt=sum(edible_mt)) %>% 
  mutate(type="Edible meat (evaluated species)") %>% 
  ungroup()
fao_fin_tot <- rbind(fao_fin_tot, df)

# Do all FAO finfish species have nutrient data?
# No, over half are missing nutrient data -- COME BACK TO THIS LATER
fao_spp <- sort(unique(fao_fin$species))
nut_spp <- sort(unique(preds_wide$species))
fao_spp_missing <- fao_spp[!fao_spp %in% nut_spp]



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

# Totals
total_ts <- fao_fin_tot %>% 
  filter(type %in% c("Landings", "Edible meat", "Edible meat (evaluated species)")) %>% 
  mutate(type=factor(type, levels=c("Landings", "Edible meat", "Edible meat (evaluated species)")))

# Divide data
nutrient_ts <- fao_fin_tot %>% 
  filter(!type %in% c("Landings", "Edible meat", "Edible meat (evaluated species)")) %>% 
  mutate(type=factor(type, levels=rev(c("Protein", "Fat", "Omega-3 fatty acids", "Omega-6 fatty acids",
                                    "Iron", "Zinc", "Vitamin A", "Vitamin B12", "Vitamin D"))))

# Nutrient type (raw) + landings reference
g0 <- ggplot(nutrient_ts, aes(x=year, y=weight_mt/1e6)) +
  geom_area(aes(fill=type)) +
  scale_fill_discrete(name="Nutrient") +
  geom_line(total_ts, mapping=aes(x=year, y=weight_mt/1e6, color=type)) +
  scale_color_discrete(name="") +
  labs(x="", y="Weight (millions of mt)") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme_bw() + my_theme
g0

ggsave(g0, filename=file.path(plotdir, "figure_fao_wc_landings_nutrient_ts.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


# Nutrient type (raw)
g1 <- ggplot(nutrient_ts, aes(x=year, y=weight_mt/1e6)) +
  geom_area(aes(fill=type)) +
  scale_fill_discrete(name="Nutrient") +
  labs(x="", y="Weight (millions of mt)") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme_bw() + my_theme
g1

ggsave(g1, filename=file.path(plotdir, "figure_fao_wc_nutrient_ts.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

# Nutrient type (proportion)
g2 <- ggplot(nutrient_ts, aes(x=year, y=weight_mt/1e6)) +
  geom_area(position="fill", aes(fill=type)) +
  scale_fill_discrete(name="Nutrient") +
  labs(x="", y="Weight (millions of mt)") +
  scale_x_continuous(breaks=seq(1950,2020,10)) +
  theme_bw() + my_theme
g2

ggsave(g2, filename=file.path(plotdir, "figure_fao_wc_nutrient_prop_ts.png"), 
       width=6.5, height=4.5, units="in", dpi=600)
