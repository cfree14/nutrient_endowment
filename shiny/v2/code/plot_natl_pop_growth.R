
# Plot population growth data
plot_natl_pop_growth <- function(pop_hist, pop_proj, cntry, my_theme=my_theme1){
  
  # Subset data
  sdata1 <- pop_hist %>% 
    filter(country_use==cntry)
  sdata2 <- pop_proj %>% 
    filter(country_use==cntry)
  
  # Plot data
  g <- ggplot() +
    # Add historical
    geom_line(data=sdata1, mapping=aes(x=year, y=pop_size/1e6)) +
    # Add projection
    geom_ribbon(data=sdata2, mapping=aes(x=year, ymin=pop_size_05perc/1e6, ymax=pop_size_95perc/1e6), alpha=0.2, fill="red") +
    geom_line(data=sdata2, mapping=aes(x=year, y=pop_size_50perc/1e6), color="red") +
    # Labels
    labs(x="", y="Population size\n(millions of people)") +
    scale_x_continuous(limits=c(1960,2100), breaks=seq(1960, 2100, 20)) +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.x = element_blank())
  g
  
}