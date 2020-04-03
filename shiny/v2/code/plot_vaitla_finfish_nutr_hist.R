
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