
# Plot bar plot
plot_barplot_n_deficient <- function(data, country, my_theme=my_theme1){
  
  # Format data for plotting
  country_do <- country
  data_plot <- data %>% 
    # Country of interest
    filter(country==country_do) %>% 
    # Remove children (not symmetric)
    filter(sex!="Children") %>% 
    # Make male values negative for plotting
    mutate(pdeficient=ifelse(sex=="Men", pdeficient*-1, pdeficient),
           ndeficient=ifelse(sex=="Men", ndeficient*-1, ndeficient),
           nhealthy=ifelse(sex=="Men", nhealthy*-1, nhealthy)) %>% 
    # Reduce columns
    select(age, sex, nutrient, ndeficient, nhealthy) %>% 
    # Gather
    gather(key="type", value="npeople", 4:5) %>% 
    # Format columns
    mutate(type=recode_factor(type,
                       "ndeficient"="Deficient",
                       "nhealthy"="Healthy"),
           sex=factor(sex, levels=c("Men", "Women")))
  
  # Plot data
  g <- ggplot(data_plot, aes(y=npeople/1e6, x=reorder(age, desc(age)), fill=sex, alpha=type)) +
    # By nutrient
    facet_wrap(~nutrient, ncol=4) +
    # Plot bars
    geom_bar(stat="identity") +
    geom_hline(yintercept = 0) +
    # Flip axis
    coord_flip() +
    # Labels
    labs(y="Millions of people", x="Age range", title=country_do) +
    # Legends
    scale_fill_discrete(name="Sex") +
    scale_alpha_manual(name="Nutritional health", values=c(0.4, 1.0)) +
    # Theme
    theme_bw() + my_theme
  print(g)
  
}