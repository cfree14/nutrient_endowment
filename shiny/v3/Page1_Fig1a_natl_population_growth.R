
# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
data <- readRDS(file.path(datadir, "WB_UN_1960_2100_human_population_by_country.Rds"))

# Example country
cntry <- "Ghana"

# Plot population growth data
plot_natl_pop_growth <- function(pop_data, cntry){
  
  # Subset data
  data_hist <- pop_data %>% 
    filter(source=="World Bank historical" & country==cntry)
  
  data_proj <- pop_data %>% 
    filter(source=="UN WPP projections" & country==cntry)
  
  # Plot data
  g <- ggplot() +
    # Add historical
    geom_line(data=data_hist, mapping=aes(x=year, y=pop_size_50perc/1e6)) +
    # Add projection
    geom_ribbon(data=data_proj, mapping=aes(x=year, ymin=pop_size_05perc/1e6, ymax=pop_size_95perc/1e6), alpha=0.2, fill="red") +
    geom_line(data=data_proj, mapping=aes(x=year, y=pop_size_50perc/1e6), color="red") +
    # Labels
    labs(x="", y="Population size\n(millions of people)") +
    scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20)) +
    # Theme
    theme_bw() + 
    theme(axis.title.x = element_blank())
  g
  
}

# Test function
plot_natl_pop_growth(pop_data=data, cntry="Canada")








