
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