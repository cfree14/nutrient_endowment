
# Plot FAO historical catch data
# cntry <- "Albania"
plot_fao_catch_data <- function(fao, cntry, my_theme=my_theme1){
  
  # Subset data
  sdata <- fao %>% 
    # Subset
    filter(country_use==cntry) %>% 
    # Spread and gather to add missing years
    spread(key="year", value="prod_mt") %>% 
    gather(key="year", value="prod_mt", 7:ncol(.)) %>% 
    mutate(prod_mt=ifelse(!is.na(prod_mt), prod_mt, 0)) %>% 
    # Rename production type
    mutate(prod_type=recode_factor(prod_type, 
                                   "Landings"="Total landings", 
                                   "Edible meat"="Edible meat"),
           year=as.numeric(year)) %>% 
    ungroup()
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=prod_mt/1e3, fill=isscaap)) +
    geom_area() +
    facet_wrap(~prod_type, ncol=2) +
    # Labels
    labs(y="Production (1000s mt)") +
    scale_fill_discrete(name="Commercial group") +
    scale_x_continuous(breaks=seq(1950,2020,10), limits=c(1950,2020)) +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  g
  
}