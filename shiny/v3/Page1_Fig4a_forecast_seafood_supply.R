
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# You need to build a file with the following
# rcp, scenario, country, iso3, year, nutrient, supply_mt, demand_mt, demand_prop

# Read projected data
data_g <- readRDS(file.path(datadir, "global_capture_mariculture_output_merged.Rds"))
data_n <- readRDS(file.path(datadir, "national_capture_mariculture_output_merged.Rds"))

# Read historical data
fao_g <- readRDS(file.path(datadir, "FAO_1950_2018_wc_aq_seafood_per_capita.Rds"))
fao_n <- readRDS(file.path(datadir, "FAO_1950_2018_wc_aq_seafood_per_capita_national.Rds"))


# Plot global production forecasts
################################################################################

# Plot global production forecast
# Global test: data_hist <- fao_g; data_proj <- data_g; level <- "global"; type <- "production"
# National test: data_hist <- fao_n; data_proj <- data_n; level <- "national"; country <- "United States"; type <- "production"
# type options: "production", "meat", "meat_kg_person"
# level options
plot_prod_proj_global <- function(data_hist, data_proj, level, country=NULL, type){
  
  # Format data
  #################################
  
  # Format historical
  data_hist <- data_hist %>% 
    mutate(sector=factor(sector, levels=c("Capture fisheries", "Bivalve mariculture", "Finfish mariculture")))
  
  # Format projections
  data_proj <- data_proj %>% 
    mutate(rcp=gsub("RCP ", "", rcp)) %>% 
    mutate(sector=factor(sector, levels=c("Capture fisheries", "Bivalve mariculture", "Finfish mariculture")))
  
  # Subset country data if national
  if(level=="national"){
    country_do <- country
    data_hist_use <- data_hist %>% 
      filter(country==country_do)
    data_proj_use <- data_proj %>% 
      filter(country==country_do)
  }else{
    data_hist_use <- data_hist
    data_proj_use <- data_proj
  }
  
  
  # Total production
  #################################
  
  if(type=="production"){
    
    # Ymax
    hist_max <- data_hist_use %>% 
      group_by(year) %>% 
      summarize(prod_mmt=sum(prod_mt)/1e6) %>% 
      pull(prod_mmt) %>% max()
    proj_max <- data_proj_use %>% 
      group_by(scenario, rcp, period) %>% 
      summarize(prod_mmt=sum(prod_mt)/1e6) %>% 
      pull(prod_mmt) %>% max()
    max_val <- max(hist_max, proj_max) * 1.02
  
    # Historical panel
    g1 <- ggplot(data_hist_use, aes(x=year, y=prod_mt/1e6, fill=sector)) +
      geom_area() +
      # Axis
      scale_y_continuous(lim=c(0, max_val )) +
      scale_x_continuous(lim=c(1950, 2020), breaks=seq(1950,2020,10)) +
      # Labels
      labs(x=" ", y="Total production (millions of mt)", title=" \nHistorical seafood production") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = c(0.2, 0.85))
    g1
    
    # BAU forecast
    data_proj_bau <- data_proj_use %>%  
      filter(scenario=="Business-as-usual" & period!="2021-2030") %>% 
      mutate(year=recode(period, "2051-2060"="2050", "2091-2100"="2100"))
    g2 <- ggplot(data_proj_bau, aes(x=rcp, y=prod_mt/1e6, fill=sector)) +
      geom_bar(stat="identity") +
      facet_wrap(~year) +
      # Axis
      scale_y_continuous(lim=c(0, max_val )) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a business-as-usual scenario") +
      geom_text(data=data_proj_bau %>% select(scenario, year) %>% unique(), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
    g2
    
    # Reform forecast
    data_proj_ref <- data_proj_use %>%  
      filter(scenario=="Progressive reforms" & period!="2021-2030") %>% 
      mutate(year=recode(period, "2051-2060"="2050", "2091-2100"="2100") %>% as.numeric())
    g3 <- ggplot(data_proj_ref, aes(x=rcp, y=prod_mt/1e6, fill=sector)) +
      geom_bar(stat="identity") +
      facet_wrap(~year) +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a progressive reform scenario") +
      geom_text(data=data_proj_ref %>% select(scenario, year) %>% unique(), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
    g3
    
    
    # Merge plots
    g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))
    g
    
  }
  
  # Edible meat
  #################################
  
  if(type=="meat"){
    
    # Ymax
    hist_max <- data_hist_use %>% 
      group_by(year) %>% 
      summarize(meat_mt=sum(meat_mt)/1e6) %>% 
      pull(meat_mt) %>% max()
    proj_max <- data_proj_use %>% 
      group_by(scenario, rcp, period) %>% 
      summarize(meat_mt=sum(meat_mt)/1e6) %>% 
      pull(meat_mt) %>% max()
    max_val <- max(hist_max, proj_max) * 1.02
    
    # Historical panel
    g1 <- ggplot(data_hist_use, aes(x=year, y=meat_mt/1e6, fill=sector)) +
      geom_area() +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      scale_x_continuous(lim=c(1950, 2020), breaks=seq(1950,2020,10)) +
      # Labels
      labs(x=" ", y="Edible meat (millions of mt)", title=" \nHistorical production") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = c(0.1, 0.85))
    g1
    
    # BAU forecast
    data_proj_bau <- data_proj_use %>%  
      filter(scenario=="Business-as-usual" & period!="2021-2030") %>% 
      mutate(year=recode(period, "2051-2060"="2050", "2091-2100"="2100"))
    g2 <- ggplot(data_proj_bau, aes(x=rcp, y=meat_mt/1e6, fill=sector)) +
      facet_wrap(~year) +
      geom_bar(stat="identity") +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a business-as-usual scenario") +
      geom_text(data=data_proj_bau %>% select(scenario, year) %>% unique(), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
    g2
    
    # Reform forecast
    data_proj_ref <- data_proj_use %>%  
      filter(scenario=="Progressive reforms" & period!="2021-2030") %>% 
      mutate(year=recode(period, "2051-2060"="2050", "2091-2100"="2100"))
    g3 <- ggplot(data_proj_ref, aes(x=rcp, y=meat_mt/1e6, fill=sector)) +
      facet_wrap(~year) +
      geom_bar(stat="identity") +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a progressive reform scenario") +
      geom_text(data=data_proj_ref %>% select(scenario, year) %>% unique(), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
    g3
    
    # Merge plots
    g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))
    g
    
  }
  
  # Edible meat per capita
  if(type=="meat_kg_person"){
    
    # Ymax
    hist_max <- data_hist_use %>% 
      group_by(year) %>% 
      summarize(meat_kg_person=sum(meat_kg_person, na.rm=T)) %>% 
      pull(meat_kg_person) %>% max()
    proj_max <- data_proj_use %>% 
      group_by(scenario, rcp, period) %>% 
      summarize(meat_kg_person=sum(meat_kg_person)) %>% 
      pull(meat_kg_person) %>% max()
    max_val <- max(hist_max, proj_max) * 1.02
    
    # Historical panel
    g1 <- ggplot(data_hist_use, aes(x=year, y=meat_kg_person, fill=sector)) +
      geom_area() +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      scale_x_continuous(lim=c(1950, 2020), breaks=seq(1950,2020,10)) +
      # Labels
      labs(x=" ", y="Edible meat per capita (kg/person/day)", title=" \nHistorical production") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = c(0.1, 0.85))
    g1
    
    # BAU forecast
    data_proj_bau <- data_proj_use %>%  
      filter(scenario=="Business-as-usual" & period!="2021-2030") %>% 
      mutate(year=recode(period, "2051-2060"="2050", "2091-2100"="2100"))
    g2 <- ggplot(data_proj_bau, aes(x=rcp, y=meat_kg_person, fill=sector)) +
      geom_bar(stat="identity") +
      facet_wrap(~year) +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a business-as-usual scenario") +
      geom_text(data=data_proj_bau %>% select(scenario, year) %>% unique(), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = "none", 
            strip.background = element_blank(),
            strip.text = element_blank())
    g2
    
    # Reform forecast
    data_proj_ref <- data_proj_use %>%  
      filter(scenario=="Progressive reforms" & period!="2021-2030") %>% 
      mutate(year=recode(period, "2051-2060"="2050", "2091-2100"="2100"))
    g3 <- ggplot(data_proj_ref, aes(x=rcp, y=meat_kg_person, fill=sector)) +
      geom_bar(stat="identity") +
      facet_wrap(~year) +
      # Axis
      scale_y_continuous(lim=c(0, max_val)) +
      # Labels
      labs(x="Climate change scenario (RCP)", y="", title="Future seafood production\nin a progressive reform scenario") +
      geom_text(data=data_proj_ref %>% select(scenario, year) %>% unique(), mapping=aes(x=2.5, y=max_val, label=year), inherit.aes = F, size=4, fontface="bold") +
      scale_fill_discrete(name="Sector") +
      # Theme
      theme_bw() +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text = element_blank())
    g3
    
    # Merge plots
    g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1, widths=c(0.4, 0.3, 0.3))
    g
    
  }
  
  # Return plot
  g

}

# Test global function
plot_prod_proj_global(data_hist=fao_g, data_proj=data_g, level="global", type="production")
plot_prod_proj_global(data_hist=fao_g, data_proj=data_g, level="global", type="meat")
plot_prod_proj_global(data_hist=fao_g, data_proj=data_g, level="global", type="meat_kg_person")

# Test national function
plot_prod_proj_global(data_hist=fao_n, data_proj=data_n, level="national", country="China", type="production")
plot_prod_proj_global(data_hist=fao_n, data_proj=data_n, level="national", country="China", type="meat")
plot_prod_proj_global(data_hist=fao_n, data_proj=data_n, level="national", country="China", type="meat_kg_person")



################################################################################
# Build national data - KAT YOU CAN IGNORE THIS
################################################################################

if(F){
  
  # Read national data
  data_n <- load("/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/blue-paper-2/data/output/processed/national_capture_mariculture_output_merged.Rdata")
  
  # Use "proportional" development scenario
  nat_wc_use <- wc_nat_use %>% 
    filter(dev_scenario=="Proportional" & period!="2021-2030") %>% 
    select(rcp, scenario, dev_scenario, country, iso3, sector, period, prod_mt, meat_mt)
  nat_faq_use <- faq_nat %>% 
    filter(dev_scenario=="Proportional" & period!="2021-2030") %>% 
    select(rcp, scenario, dev_scenario, country, iso3, sector, period, prod_mt, meat_mt)
  nat_baq_use <- baq_nat %>% 
    filter(dev_scenario=="Proportional" & period!="2021-2030") %>% 
    select(rcp, scenario, dev_scenario, country, iso3, sector, period, prod_mt, meat_mt)
  
  # Identify population size
  pop <- data1 %>% 
    select(country, iso3, period, npeople) %>% 
    unique() %>% 
    filter(iso3!="CHN") %>% 
    mutate(iso3=recode(iso3, "CHN/HKG/MAC"="CHN"),
           country=recode(country, "China, Hong Kong, Macau"="China"))
  
  # Merge scenarios
  data_n <- bind_rows(nat_wc_use, nat_baq_use, nat_faq_use) %>% 
    # Fix a few countries
    mutate(iso3=recode(iso3, "CHN/HKG/MAC"="CHN"),
           country=recode(country, "China, Hong Kong, Macau"="China")) %>% 
    # Add population size and calculate meat per capita
    left_join(pop) %>% 
    mutate(meat_kg_person=meat_mt*1000/npeople)
  
  # Export data
  saveRDS(data_n, file=file.path(datadir, "national_capture_mariculture_output_merged.Rds"))
  
}

