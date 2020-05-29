
# Plot raster
plot_barplot_perc_deficient <- function(data, country, plotdir=NULL){
  
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
  
  # Country do
  country_do <- country
  
  # Format data for plotting
  data_plot <- data %>% 
    filter(country==country_do) %>% 
    filter(sex!="Children") %>% 
    mutate(pdeficient=ifelse(sex=="Men", pdeficient*-1, pdeficient))
  
  # Plot data
  g <- ggplot(data_plot, aes(y=pdeficient, x=reorder(age, desc(age)), fill=sex)) +
    geom_bar(stat="identity") +
    facet_wrap(~nutrient, ncol=4) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    labs(y="Proportion deficient", x="Age range", title=country_do) +
    scale_y_continuous(limits=c(-1,1), 
                       breaks=seq(-1, 1, 0.5),
                       labels=c(1,0.5,0, 0.5, 1)) +
    theme_bw() + my_theme
  print(g)
  
  # Export
  if(!is.null(plotdir)){
    outfile <- paste0("pdeficient_barplot_", tolower(country) %>% gsub(" ", "_", .), ".png")
    ggsave(g, filename=file.path(plotdir, outfile), 
           width=6.5, height=5, units="in", dpi=600)
  }
  
}