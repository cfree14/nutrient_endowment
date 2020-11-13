
# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
data <- readRDS(file.path(datadir, "Free_etal_2020_capture_meat_proj_by_rcp_mgmt_genus_group.Rds")) %>% 
  ungroup() %>% 
  mutate(scenario=recode_factor(scenario, 
                                "No Adaptation"="BAU",
                                "Full Adaptation"="Reforms"),
         genus=recode(genus,
                      "Demersal Fish"="Fish, demersal",
                      "Marine Fish; Other"="Fish, other",
                      "Molluscs; Other"="Bivalves and gastropods",
                      "Pelagic Fish"="Fish, pelagic"))


# Plot population growth data
data <- data; country <- "Ghana"
plot_capture_meat_bau_v_reform <- function(data, country){
  
  # Country do
  country_do <- country
  
  # Prepare data
  sdata <- data %>% 
    filter(country==country_do) %>% 
    filter(year==2100)
  
  # Plot data
  g <- ggplot(sdata, aes(x=scenario, y=meat_mt/1e6, fill=genus)) +
    facet_wrap(~rcp, ncol=4) +
    geom_bar(stat="identity") +
    # Labels/ legend
    labs(x="Management scenario", y="Edible meat (millions mt)") +
    scale_fill_discrete(name="Major group") +
    # Theme
    theme_bw()
  g
  
}

# Test function
plot_capture_meat_bau_v_reform(data=data, country=country)








