
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggplot2)
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data/fish_projections/data"
plotdir <- "figures"

# Load data
data_orig <- readRDS(file.path(datadir, "country_nutrient_time_series.Rds"))

# Nutrients
nutrients <- c("Protein", "Fat", "Omega-3 fatty acids", "Omega-6 fatty acids", 
               "Iron", "Zinc", 
               "Vitamin A", "Vitamin B12", "Vitamin D")

# Build data
################################################################################

# Time periods
yrs1 <- 2012:2021
yrs2 <- 2091:2100

# Calculate % change between time periods
stats <- data_orig %>%
  select(-c(iron_mg:zinc_mg)) %>%
  filter(year %in% c(yrs1, yrs2)) %>% 
  gather(key="type", value="weight_mt", 5:ncol(.)) %>% 
  ungroup() %>% 
  group_by(rcp, sovereign_iso3, sovereign, type) %>% 
  summarize(val1=mean(weight_mt[year %in% yrs1]),
            val2=mean(weight_mt[year %in% yrs2])) %>%
  ungroup() %>% 
  mutate(perc=(val2-val1)/val1*100,
         rcp=recode(rcp, "RCP26"="RCP 2.6", "RCP45"="RCP 4.5", "RCP60"="RCP 6.0", "RCP85"="RCP 8.5"))

# Extract nutrient stats
nstats <- stats %>% 
  filter(!type %in% c("msy_mt", "edible_mt")) %>% 
  rename(nutrient=type, nutrient_mt1=val1, nutrient_mt2=val2, nutrient_perc=perc) %>% 
  mutate(nutrient=freeR::sentcase(gsub("_mt", "", nutrient)),
         nutrient=recode(nutrient, "Omega3"="Omega-3 fatty acids",
                         "Omega6"="Omega-6 fatty acids",
                         "Vita"="Vitamin A",
                         "Vitb12"="Vitamin B12",
                         "Vitd"="Vitamin D"),
         nutrient=factor(nutrient, levels=nutrients))

# Extract MSY stats
cstats <- stats %>% 
  ungroup() %>% 
  filter(type == "msy_mt") %>% 
  rename(msy_mt1=val1, msy_mt2=val2, msy_perc=perc) %>% 
  select(-type)

# Merge stats for plotting
stats1 <- nstats %>% 
  left_join(cstats, by=c("rcp", "sovereign_iso3", "sovereign"))


# Plot data
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=10),
                  axis.title=element_text(size=12),
                  plot.title=element_text(size=14),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Plot data
g <- ggplot(stats1, aes(x=msy_perc, y=nutrient_perc, color=rcp)) +
  geom_point() +
  facet_wrap(~ nutrient, ncol=3) +
  # Add lines
  geom_abline(slope=1, intercept=0) +
  geom_hline(yintercept=0, linetype="dotted", color="grey40") +
  geom_vline(xintercept=0, linetype="dotted", color="grey40") +
  # Add labels
  labs(x="Percent change in\nmaximum sutainable yield",
       y="Percent change in\nnutrient endowment") +
  # Add legend
  scale_color_manual(name="Emissions\nscenario", values=rev(brewer.pal(4,"RdBu"))) +
  # Add themes
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "figure_msy_perc_vs_nutrient_perc.png"), 
       width=8.5, height=6.5, units="in", dpi=600)



# Radar plot
################################################################################

sdata <- nstats %>% 
  filter(sovereign=="United States")

# Radar plots
# devtools::install_github("ricardo-bion/ggradar", dependencies = TRUE)
library(ggradar)


g <- ggradar(sdata) 



