

# cntry <- "Ghana"
plot_natl_catch_nutr_hist_and_proj <- function(fao_catch_nutr_cntry, gaines_catch_nutr_cntry, cntry, my_theme=my_theme1){
  
  # Subset historical data
  sdata1 <- fao_catch_nutr_cntry %>% 
    filter(country==cntry & grepl("_dcap", key)) %>% 
    mutate(scenario="Historical", 
           rcp="Historical") %>% 
    mutate(key=gsub("_", " ", key) %>% 
             gsub(" dcap", "", .) %>% 
             str_to_sentence() %>% 
             gsub(" mg", " (mg)", .) %>% 
             gsub(" g", " (g)", .) %>%  
             gsub(" mcg rae", " (mcg RAE)", .) %>% 
             gsub(" mcg", " (mcg)", .) %>% 
             gsub(" kcal", " (kcal)", .),
           key=recode(key, 
                      "Vitamin a (mcg RAE)"="Vitamin A (mcg RAE)",
                      "Vitamin b6 (mg)"="Vitamin B6 (mg)",
                      "Vitamin c (mg)"="Vitamin C (mg)")) %>% 
    select(scenario, rcp, iso3, country, year, key, dcap)
  
  # Subset projected data
  sdata2 <- gaines_catch_nutr_cntry %>% 
    filter(country==cntry & year>=2020) %>% 
    filter(type=="Nutrient") %>% 
    mutate(key=gsub("_", " ", key) %>% 
             gsub(" dcap", "", .) %>% 
             str_to_sentence() %>% 
             gsub(" mg", " (mg)", .) %>% 
             gsub(" g", " (g)", .) %>%  
             gsub(" mcg rae", " (mcg RAE)", .) %>% 
             gsub(" mcg", " (mcg)", .) %>% 
             gsub(" kcal", " (kcal)", .),
           key=recode(key, 
                      "Vitamin a (mcg RAE)"="Vitamin A (mcg RAE)",
                      "Vitamin b6 (mg)"="Vitamin B6 (mg)",
                      "Vitamin c (mg)"="Vitamin C (mg)")) %>% 
    select(scenario, rcp, iso3, country, year, key, dcap)
  
  # Merge
  sdata <- bind_rows(sdata1, sdata2) %>% 
    # Refactor
    mutate(scenario=factor(scenario, levels=c("Historical", "No Adaptation", "Full Adaptation")),
           rcp=factor(rcp, levels=c("Historical", paste("RCP", c("2.6", "4.5", "6.0", "8.5"))))) %>% 
    # Reformat nutrient label
    mutate(nutr_label=recode(key,
                             "Vitamin A (mcg RAE)"="Vitamin A\n(mcg RAE)",
                             "Saturated fatty acids (g)"="Saturated\nfatty acids (g)",
                             "Monounsaturated fatty acids (g)"="Monounsaturated\nfatty acids (g)",
                             "Polyunsaturated fatty acids (g)"="Polyunsaturated\nfatty acids (g)")) #%>% 
    # Order alphabetically except Meat (g) at top
    # mutate(nutr_label=factor(nutr_label, levels=c("Meat (g)", unique(nutr_label)[unique(nutr_label)!="Meat (g)"])) )
    
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=dcap, color=scenario)) +
    facet_grid(nutr_label ~ rcp, scale="free", space="free_x") +
    geom_line() + 
    # Labels
    labs(x="", y="", title="Daily per capita supply (amount / person / day)") +
    scale_color_manual(name="", values=c("black", "red", "blue")) +
    scale_x_continuous(breaks=seq(1960,2100,10)) +
    expand_limits(x=2020) +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = "bottom",
          axis.title = element_blank(),
          legend.title = element_blank(), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0))
  g
  
  
}
