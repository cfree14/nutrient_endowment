
# Plot raster
plot_barplot_supply_ear <- function(data, country, plotdir=NULL){
  
  # My theme
  my_theme <- theme(axis.text=element_text(size=6),
                    axis.text.x=element_text(size=4),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=9),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))
  
  # Format data for plotting
  country_do <- country
  data_plot <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Remove children (not symmetric)
    filter(sex!="Children") %>% 
    # Arrange bars
    arrange(sex, age) %>% 
    mutate(sexage=paste(sex, age, sep="-"),
           sexage=factor(sexage, levels=sexage))
  
 # Age labels for x-axis
 ages <- rep(c("10-14", "15-19", "20-24", "25-29", 
               "30-34", "35-39", "40-44", "45-49", 
               "50-54", "55-59", "60-64", "65-69", 
               "70-74", "75-79", "80+"), 2)
  
  # Plot data
  g <- ggplot(data_plot, aes(y=supply_med, x=sexage, fill=sex)) +
    # By nutrient
    facet_wrap(~nutrient, ncol=4, scales="free_y") +
    # Plot bars
    geom_bar(stat="identity", position="dodge") + 
    # Plot EARs
    geom_line(data_plot, mapping=aes(y=ear, x=sexage, group=sex), inherit.aes = F, color="black") +
    # geom_point(data_plot, mapping=aes(y=ear, x=sexage), inherit.aes = F, color="black") +
    # Labels
    labs(y="Daily per capita nutrient supply\nand estimated average requirement (EAR)", x="Age range", title=country_do) + 
    scale_x_discrete(labels=ages) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position="bottom", 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  print(g)
  
  # Export
  if(!is.null(plotdir)){
    outfile <- paste0("supply2ear_barplot_", tolower(country) %>% gsub(" ", "_", .), ".png")
    ggsave(g, filename=file.path(plotdir, outfile), 
           width=8, height=6.5, units="in", dpi=600)
  }
  
}