
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
data_orig <- readRDS(file.path(datadir, "country_nutrient_time_series_historic.Rds"))

# Nutrients
nutrients <- c("Protein", "Fat", "Omega-3 fatty acids", "Omega-6 fatty acids", 
               "Iron", "Zinc", 
               "Vitamin A", "Vitamin B12", "Vitamin D")

# Build data
################################################################################

# Time periods
yrs1 <- 1950:1959
yrs2 <- 2007:2016

# Calculate % change between time periods
stats <- data_orig %>%
  filter(year %in% c(yrs1, yrs2)) %>% 
  gather(key="type", value="weight_mt", 4:ncol(.)) %>% 
  ungroup() %>% 
  group_by(iso3, country, type) %>% 
  summarize(val1=mean(weight_mt[year %in% yrs1]),
            val2=mean(weight_mt[year %in% yrs2])) %>%
  ungroup() %>% 
  mutate(perc=(val2-val1)/val1*100)

# Extract nutrient stats
nstats <- stats %>% 
  filter(!type %in% c("catch_mt", "edible_mt")) %>% 
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
  filter(type == "catch_mt") %>% 
  rename(msy_mt1=val1, msy_mt2=val2, msy_perc=perc) %>% 
  select(-type)

# Merge stats for plotting
stats1 <- nstats %>% 
  left_join(cstats, by=c("iso3", "country")) %>% 
  filter(is.finite(msy_perc) & is.finite(nutrient_perc) & msy_perc <= 200)


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
g <- ggplot(stats1, aes(x=msy_perc, y=nutrient_perc)) +
  geom_point() +
  facet_wrap(~ nutrient, ncol=3) +
  # Add lines
  geom_abline(slope=1, intercept=0) +
  geom_hline(yintercept=0, linetype="dotted", color="grey40") +
  geom_vline(xintercept=0, linetype="dotted", color="grey40") +
  # Add labels
  labs(x="Percent change in\nmaximum sutainable yield",
       y="Percent change in\nnutrient endowment") +
  # Add themes
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "figure_catch_perc_vs_nutrient_perc_historic.png"), 
       width=8.5, height=6.5, units="in", dpi=600)


