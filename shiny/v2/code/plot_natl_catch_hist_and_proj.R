
# cntry <- "Ghana"
plot_natl_catch_hist_and_proj <- function(fao_catch_nutr_cntry, gaines_catch_nutr_cntry, cntry, my_theme=my_theme1){
  
  # Subset historical data
  sdata1 <- fao_catch_nutr_cntry %>% 
    filter(country==cntry) %>% 
    filter(key %in% c("catch_mt", "meat_mt")) %>% 
    mutate(key=recode(key, 
                      "catch_mt"="Catch",
                      "meat_mt"="Edible meat"), 
           scenario="Historical", rcp="Historical") %>% 
    select(scenario, rcp, iso3, country, year, key, dcap)
  
  # Subset projected data
  sdata2 <- gaines_catch_nutr_cntry %>% 
    filter(country==cntry) %>% 
    filter(type=="Outcome" & key!="profits_usd_scaled" & year>=2020) %>% 
    mutate(key=recode(key, 
                     "msy_mt_scaled"="Maximum sustainable\nyield (MSY)", 
                     "catch_mt_scaled"="Catch",
                     "meat_mt"="Edible meat")) %>% 
    select(scenario, rcp, iso3, country, year, key, dcap)
  
  # Merge
  sdata <- bind_rows(sdata1, sdata2) %>% 
    # Refactor
    mutate(scenario=factor(scenario, levels=c("Historical", "No Adaptation", "Full Adaptation")),
           rcp=factor(rcp, levels=c("Historical", paste("RCP", c("2.6", "4.5", "6.0", "8.5")))),
           key=factor(key, levels=c("Maximum sustainable\nyield (MSY)", "Catch", "Edible meat")))

  # Plot data
  g <- ggplot(sdata, aes(x=year, y=dcap/1000, color=scenario)) +
    facet_grid(key ~ rcp, scale="free", space="free_x") +
    geom_line() + 
    # Labels
    labs(x="", y="Production (1000s mt)") +
    scale_color_manual(name="", values=c("black", "red", "blue")) +
    scale_x_continuous(breaks=seq(1960,2100,10)) +
    expand_limits(x=2020) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = "bottom",
          axis.title.x = element_blank(),
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
  g
  
  
}
