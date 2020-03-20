
# Theme
################################################################################

# Theme
my_theme1 <- theme(axis.text=element_text(size=12),
                  axis.title=element_text(size=14),
                  plot.title=element_text(size=16),
                  legend.text=element_text(size=12),
                  legend.title=element_text(size=14),
                  strip.text=element_text(size=14))

# Fish nutrient page
################################################################################

# Vaitla et al. (2018) finfish nutrient radar plot
# spp <- c("Flat needlefish (Ablennes hians)"); my_theme <- my_theme1
plot_vaitla_finfish_nutr_radar <- function(vaitla_nutr_preds_long, spp, my_theme=my_theme1){
  
  # Subset data
  sdata <- vaitla_nutr_preds_long %>% 
    filter(species_label %in% spp) %>% 
    select(species_label, nutrient, pmax_fill) %>% 
    mutate(nutrient=recode(nutrient, 
                           "Omega-3 fatty acids"="Omega-3\nfatty acids",
                           "Omega-6 fatty acids"="Omega-6\nfatty acids")) %>% 
    spread(key="nutrient", value="pmax_fill")

  
  # Plot data
  g <- ggradar(sdata) + 
    theme(legend.position = "right")
  g
  
}

# Vaitla et al. (2018) finfish nutrient histogram
plot_vaitla_finfish_nutr_hist <- function(vaitla_nutr_preds_long, vaitla_nutr_key, spp, my_theme=my_theme1){
  
  # Subset species data
  sdata <- vaitla_nutr_preds_long %>% 
    filter(species_label %in% spp)
  
  # Plot
  g <- ggplot(vaitla_nutr_preds_long, aes(x=value_md)) +
    geom_histogram(fill="grey70") +
    facet_wrap(~nutrient_label, ncol=3, scales="free") +
    # Add species lines
    # geom_rect(data=sdata, aes(xmin=value_lo, xmax=value_hi, ymin=0, ymax=Inf, fill=species), alpha=0.2) +
    geom_vline(data=sdata, aes(xintercept=value_md, color=species_label), lwd=1.5) +
    # Add rich threshold (don't show b/c most species are either rich or not rich)
    # geom_vline(vaitla_nutr_key, mapping=aes(xintercept=thresh_rich), linetype="dotted") +
    # Labels
    scale_color_discrete(name="") +
    labs(x="Concentration", y="Number of species") +
    # Theme
    theme_bw() + my_theme
  g
  
  
}

# National nutrition trends and status
################################################################################

# Plot national importance of seafood
# cntry <- "Ghana"
plot_natl_seafood_impt <- function(pdiet_seafood_cntry_yr, pnutrient_seafood_cntry_2011, cntry, my_theme=my_theme1){
  
  # p(diet from seafood)
  ###############################
  
  # Data
  pdiet <- pdiet_seafood_cntry_yr %>% 
    filter(country==cntry)
  
  # Plot % of diet from seafood over time
  g1 <- ggplot(pdiet, aes(x=year, y=seafood_g_person_day)) +
    geom_line() +
    labs(y="Daily per capita seafood consumption\n(grams per person per day)") +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.x=element_blank())
  g1
  
  # Plot % of diet from seafood over time
  g2 <- ggplot(pdiet, aes(x=year, y=prop_seafood)) +
    geom_line() +
    # Labels
    scale_y_continuous(labels = scales::percent) +
    labs(y="Percentage of daily diet\nfrom marine seafood") +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.x=element_blank())
  g2
  
  # p(nutrients from seafood)
  ###############################
  
  # Data
  pnutrient <- pnutrient_seafood_cntry_2011 %>% 
    filter(country==cntry)
  
  # Plot % of nutrient intake from seafood in 2011
  g3 <- ggplot(pnutrient, aes(x=reorder(nutrient, prop_seafood), y=prop_seafood)) +
    geom_bar(stat="identity") +
    coord_flip() +
    # Labels
    labs(y="Percentage of daily nutrient intake\nfrom marine seafood") +
    scale_y_continuous(labels = scales::percent) +
    # Theme
    theme_bw() + my_theme +
    theme(axis.title.y=element_blank())
  g3
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(data=c(1,2, 3,3), byrow=F, ncol=2))
  g
  
}

# Plot national nutrient intake over time and by age/sex in 2011
plot_natl_nutrient_supply <- function(nut_cntry_yr, nut_cntry_age_sex_2011, cntry, nutr, my_theme=my_theme1){
  
  # Subset data
  #################################
  
  # Subset data
  # cntry <- "Ghana"; nutr <- "Protein"
  sdata1 <- nut_cntry_yr %>% 
    filter(country==cntry & nutrient==nutr)
  
  # Subset data
  sdata2 <- nut_cntry_age_sex_2011 %>% 
    filter(country==cntry & nutrient==nutr)
  
  # Maximum value
  max1 <- max(sdata1$value_hi, na.rm=T)
  max2 <- max(sdata2$value_med, na.rm=T)
  max_val <- max(max1, max2)
  
  # Nutrient time series
  #################################
  
  # Plot data
  g1 <- ggplot(sdata1, aes(x=year, y=value_med)) +
    geom_line() +
    geom_ribbon(aes(ymin=value_lo, ymax=value_hi), alpha=0.2) +
    # Y-axis limits
    ylim(0, max_val) +
    # Labels
    labs(x="Year", y="Daily per capita supply\n(amount / person / day)") +
    # Theme
    theme_bw() + my_theme
  #g1
  
  # Nutrient time series
  #################################
  
  # Plot data
  g2 <- ggplot(sdata2, aes(x=age_range, y=value_med, fill=sex, group=sex)) +
    geom_bar(stat="identity", position="dodge") +
    # geom_errorbar(aes(ymin=value_lo, ymax=value_hi), 
    #               colour="black", width=0, position=position_dodge(.9)) +
    # geom_line(aes(x=age_range, y=diet_rec, color=sex, group=sex)) +
    # Y-axis limits
    ylim(0, max_val) +
    # Labels
    labs(x="Age range", y="") +
    scale_fill_discrete(name="Sex") +
    # Theme
    theme_bw() + my_theme +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  #g2
  
  # Merge and export
  #################################
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, ncol=2)
  g
  
}
 

# Historical nutrition from fisheries
################################################################################


# Future nutrition from fisheries
################################################################################


# National report
################################################################################






