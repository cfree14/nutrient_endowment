
# Plot national nutrient intake over time and by age/sex in 2011
# For testing: cntry <- "Ghana"; nutr <- "Calcium"
plot_natl_nutrient_supply <- function(nut_cntry_yr, nut_cntry_age_sex_2011_w_reqs, cntry, nutr, dris=dris, my_theme=my_theme1){
  
  # Subset data
  #################################
  
  # Subset data
  sdata1 <- nut_cntry_yr %>% 
    filter(country==cntry & nutrient==nutr)
  
  # Subset data
  sdata2 <- nut_cntry_age_sex_2011_w_reqs %>% 
    filter(country==cntry & nutrient==nutr) %>% 
    mutate(sex=recode_factor(sex, "Females"="Women", "Males"="Men"))
  
  # Subset DRIs
  dris_use <- dris %>% 
    filter(nutrient==nutr & dri_type=="Estimated Average Requirement (EAR)") %>% 
    arrange(sex, age_range)
  
  # Maximum value
  max1 <- max(sdata1$value_hi, na.rm=T)
  max2 <- max(sdata2$value_hi, na.rm=T)
  max_dri <- max(dris_use$value)
  max_val <- max(max1, max2, max_dri)
  
  # Extract unit
  nutr_unit <- unique(sdata1$units_short)
  nutr_label1 <- paste(unique(sdata1$nutrient_label), "consumption over time")
  nutr_label2 <- paste("2011", unique(sdata1$nutrient_label), "consumption by age and sex")
  
  # Nutrient time series
  #################################
  
  # Plot data
  g1 <- ggplot(sdata1, aes(x=year, y=value_med)) +
    geom_line() +
    geom_ribbon(aes(ymin=value_lo, ymax=value_hi), alpha=0.2) +
    # Y-axis limits
    ylim(0, max_val) +
    # Labels
    labs(x="Year", y=paste0("Daily per capita consumption\n(", nutr_unit, " / person / day)"), title=nutr_label1) +
    # Theme
    theme_bw() + my_theme
  #g1
  
  # Nutrient time series
  #################################
  
  # Approach #1: Plot data
  # g2 <- ggplot(sdata2, aes(x=age_range, y=value_med, fill=sex, group=sex)) +
  #   geom_bar(stat="identity", position="dodge") +
  #   # geom_errorbar(aes(ymin=value_lo, ymax=value_hi), 
  #   #               colour="black", width=0, position=position_dodge(.9)) +
  #   # geom_line(aes(x=age_range, y=diet_rec, color=sex, group=sex)) +
  #   # Y-axis limits
  #   ylim(0, max_val) +
  #   # Labels
  #   labs(x="Age range", y="") +
  #   scale_fill_discrete(name="Sex") +
  #   # Theme
  #   theme_bw() + my_theme +
  #   theme(legend.position = "right",
  #         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #g2
  
  # Approach #2: Plot data
  g2 <- ggplot(sdata2, aes(x=age_range, y=value_med, fill=sex)) +
    facet_grid(~sex, scale="free_x", space="free") +
    # Plot bars
    geom_bar(stat="identity", position="dodge", alpha=0.6, show.legend = F) +
    # Add error bars
    geom_errorbar(mapping=aes(x=age_range, ymin=value_lo, ymax=value_hi, color=sex),
                  width=0, lwd=1, show.legend = F) +
    # Add diet requirement
    geom_line(data=dris_use, mapping=aes(x=age_range, y=value, group=sex), inherit.aes = F, color="black") +
    geom_point(data=dris_use, mapping=aes(x=age_range, y=value, group=sex), inherit.aes = F, color="black") +
    # geom_line(aes(x=age_range, y=diet_req, group=sex, fill=NULL), color="black") +
    # geom_point(aes(x=age_range, y=diet_req, group=sex, fill=NULL), color="black") +
    # Y-axis limits
    ylim(0, max_val) +
    # Labels
    labs(x="Age range", y="", title=nutr_label2) +
    scale_fill_discrete(name="Sex") +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  g2
  
  # Merge and export
  #################################
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.45, 0.55))
  g
  
}