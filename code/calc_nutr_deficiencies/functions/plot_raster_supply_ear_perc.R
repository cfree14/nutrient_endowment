
# Plot raster
plot_raster_supply_ear_perc <- function(data, country, plotdir=NULL){
  
  # My theme
  my_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=9),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))
  
  # Country data
  country_do <- country
  sdata <- data %>% 
    filter(country==country_do)
  
  # Plot data
  g <- ggplot(sdata, aes(x=age, y=nutrient, fill=ear_perc_cap)) +
    facet_grid(nutrient_type~sex, scales="free", space="free") +
    geom_raster() +
    # Legend
    scale_fill_gradient2(name="Percent difference between mean supply\nand the estimated average requirement (EAR)", 
                         midpoint = 0,
                         breaks=seq(-100,200,100)) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Labels
    labs(x="Age group", y="", title=country_do) +
    theme_bw() + my_theme +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "bottom")
  print(g)
  
  # Export
  if(!is.null(plotdir)){
    outfile <- paste0("supply2ear_raster_", tolower(country) %>% gsub(" ", "_", .), ".png")
    ggsave(g, filename=file.path(plotdir, outfile), 
           width=6.5, height=4.5, units="in", dpi=600)
  }
  
}