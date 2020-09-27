
# Plot raster
plot_raster_perc_deficient <- function(data, country, my_theme=my_theme1){
  
  # Country data
  country_do <- country
  sdata <- data %>% 
    filter(country==country_do)
  
  # Plot data
  g <- ggplot(sdata, aes(x=age, y=nutrient, fill=pdeficient*100)) +
    facet_grid(nutrient_type~sex, scales="free", space="free") +
    geom_raster() +
    # Legend
    scale_fill_gradientn(name="% of population\nnutrient deficient", 
                         colors=RColorBrewer::brewer.pal(9, "YlOrRd"), limits=c(0,100)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Labels
    labs(x="Age group", y="", title=country_do) +
    theme_bw() + my_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "bottom")
  print(g)
  
}