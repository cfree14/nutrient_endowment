
# Plot FAO historical catch data
# cntry <- "Ghana"
plot_fao_catch_data <- function(fao, cntry, my_theme=my_theme1){
  
  # Subset data
  sdata <- fao %>% 
    filter(country_use==cntry) #%>% 
  # Spread then gather (makes plotting easier)
  # This ensures that every year has a value (0 if not present before)
  # spread(key="prod_type", value="prod_mt", fill=0) %>%
  # gather(key="prod_type", value="prod_mt", 7:ncol(.))
  
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