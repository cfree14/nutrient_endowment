
# Plot national nutrient intake by age/sex in 2011 relative to US dietary guidelines
# For testing: cntry <- "Ghana"
plot_natl_nutr_age_sex_rel_req <- function(nut_cntry_age_sex_2011_w_reqs, cntry, my_theme=my_theme1){
  
  # Subset and format data
  sdata <- nut_cntry_age_sex_2011_w_reqs %>% 
    filter(country==cntry & !is.na(diet_req)) %>% 
    mutate(sex=recode_factor(sex, "Children"="Children", "Females"="Women", "Males"="Men"),
           nutrient_type=recode(nutrient_type, 
                                "Macronutrient"="Macro\nnutrient"))
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=nutrient, fill=value_perc_req_cap)) +
    facet_grid(nutrient_type ~ sex, scale="free", space="free") +
    geom_raster() +
    # Labels
    labs(x="", y="", title="2011 nutritional health by age and sex") +
    scale_fill_gradient2(name="Percent above or below\ndaily recommendation", 
                         midpoint = 0) +
    # Theme
    theme_bw() + my_theme1 +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          legend.position = "bottom",
          axis.title = element_blank())
  g
  
}