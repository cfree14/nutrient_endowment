
# Plot nutrient supply time series
plot_nutrient_ts <- function(nut_cntry_yr, cntry){
  
  # Subset data
  # cntry <- "Ghana"
  sdata <- nut_cntry_yr %>% 
    filter(country==cntry)
  
  # Plot data
  g <- ggplot(sdata, aes(x=year, y=value_med)) +
    geom_line() +
    geom_ribbon(aes(ymin=value_lo, ymax=value_hi), alpha=0.2) +
    facet_wrap(~nutrient_label, ncol=6, scale="free_y") +
    labs(x="", y="Daily per capita supply\n(amount / person / day)") +
    theme_bw()
  g
  
}

# Plot 2011 nutrient supply by age and sex
plot_nutrient_age_sex <- function(nut_cntry_age_sex_2011, cntry){
  
  # Subset data
  # cntry <- "Ghana"
  sdata <- nut_cntry_age_sex_2011 %>% 
    filter(country==cntry)
  
  # Plot data
  g <- ggplot(sdata, aes(x=age_range, y=value_med, fill=sex, group=sex)) +
    facet_wrap(~nutrient_label, ncol=6, scale="free_y") +
    geom_bar(stat="identity", position="dodge") +
    theme_bw() +
    labs(x="Age range", y="Daily per capita supply\n(amount / person / day)") +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  g
  
}

