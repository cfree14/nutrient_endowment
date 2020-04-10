
# Plot national importance of seafood
# cntry <- "Albania"
plot_natl_seafood_impt <- function(pdiet_seafood_cntry_yr, pnutrient_seafood_cntry_2011, cntry, my_theme=my_theme1){
  
  # p(diet from seafood)
  ###############################
  
  # Data
  pdiet <- pdiet_seafood_cntry_yr %>% 
    filter(country==cntry) %>% 
    mutate(seafood_g_person_day=ifelse(total_g_person_day==0, NA, seafood_g_person_day))
  
  # Plot % of diet from seafood over time
  g1 <- ggplot(pdiet, aes(x=year, y=seafood_g_person_day)) +
    geom_line() +
    labs(y="Daily per capita seafood consumption\n(grams / person / day)",
         title="A. Daily per capita marine seafood consumption (g)") +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  g1
  
  # Plot % of diet from seafood over time
  g2 <- ggplot(pdiet, aes(x=year, y=prop_seafood)) +
    geom_line() +
    # Labels
    scale_y_continuous(labels = scales::percent) +
    labs(y="Percent of daily diet\nfrom marine seafood",
         title="B. Percent of daily diet from marine seafood") +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5))
  g2
  
  # p(nutrients from seafood)
  ###############################
  
  # Data
  pnutrient <- pnutrient_seafood_cntry_2011 %>% 
    filter(country==cntry)
  
  # Plot % of nutrient consumption from seafood in 2011
  g3 <- ggplot(pnutrient, aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
    geom_bar(stat="identity") +
    coord_flip() +
    # Labels
    labs(y="Percent of daily nutrient consumption\nfrom marine seafood",
         title="C. Percent of daily nutrition from marine seafood") +
    scale_y_continuous(labels = scales::percent) +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.y=element_blank())
  g3
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(data=c(1,2, 3,3), byrow=F, ncol=2), widths=c(0.45, 0.55))
  g
  
}