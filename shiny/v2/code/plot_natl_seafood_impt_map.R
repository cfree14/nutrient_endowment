
# Plot national importance of seafood as a protein source
# cntry <- "Ghana"
plot_natl_seafood_impt_map <- function(pdiet_seafood_cntry_yr, 
                                       pnutrient_seafood_cntry_2011, 
                                       cntry, my_theme=my_theme1){
  
  # Subset protein data
  sdata <- pnutrient_seafood_cntry_2011 %>% 
    filter(nutrient=="Protein") %>% 
    mutate(prop_seafood_cap=pmin(prop_seafood, 0.1))
  
  # Add protein data to map
  world1 <- world %>% 
    left_join(sdata, by=c("iso3_use"="iso3"))
  
  # Country ISO3
  cntry_iso3 <- pnutrient_seafood_cntry_2011 %>% 
    filter(country==cntry) %>% 
    pull(iso3) %>% unique()
  
  # Plot map
  g1 <- ggplot(world1) +
    geom_sf(mapping=aes(fill=prop_seafood_cap*100), lwd=0.1) +
    geom_sf(data=world_pts %>% filter(iso3_use==cntry_iso3), 
            color="red", size=3) +
    # geom_sf_text(data=world_pts %>% filter(iso3_use==cntry_iso3), 
    #              label=cntry, color="red") + 
    scale_fill_gradientn(name="Percent of protein\nfrom marine seafood", 
                         colors=RColorBrewer::brewer.pal(n=9, name="Blues"), 
                         na.value = "grey70",
                         limits=c(0,10),
                         breaks=seq(0,10,5), 
                         labels=c("0%", "5%", ">10%")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    theme_bw() + my_theme +
    theme(legend.position = "bottom", 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  g1
  
  # Plot p(diet) histogram
  # sdata <- pdiet_seafood_cntry_yr %>% 
  #   filter(year==2011)
  # g <- ggplot(sdata, aes(x=prop_seafood)) +
  #   geom_histogram(binwidth = 0.005) +
  #   labs(x="Proportion of diet\nfrom marine seafood", y="Number of countries") +
  #   theme_bw() + my_theme
  # g
  
  # Plot p(protein) histogram
  cntry_prop <- sdata %>%
    filter(country==cntry) %>% 
    pull(prop_seafood)
  g2 <- ggplot(sdata, aes(x=prop_seafood)) +
    geom_histogram(binwidth = 0.01) +
    geom_vline(xintercept = cntry_prop, col="red") +
    annotate(geom="text", y=25, x=cntry_prop+0.01, hjust=0, label=cntry, color="red") +
    labs(x="Percent of protein\nfrom marine seafood", y="Number of countries") +
    scale_x_continuous(labels = scales::percent_format(accuracy=1)) +
    theme_bw() + my_theme
  g2
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.75, 0.25))
  g
  
}