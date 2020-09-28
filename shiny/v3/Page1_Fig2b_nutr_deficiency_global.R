
# Packages
library(tidyverse)

# Data directory
datadir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/nutrition-seafood-climate/data/"

# Read data
data <- readRDS(file.path(datadir, "nutr_deficiencies_by_sex_age_2011.Rds"))

# Test country

# Plot population growth data
plot_barplot_n_deficient_global <- function(data){
  
  # Format data for plotting
  data_plot <- data %>% 
    ungroup() %>% 
    # Remove children (not symmetric)
    filter(sex!="Children") %>% 
    # Make male values negative for plotting
    mutate(ndeficient=ifelse(sex=="Men", ndeficient*-1, ndeficient),
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
    labs(y="Millions of people", x="Age range") +
    # Legends
    scale_fill_discrete(name="Sex") +
    scale_alpha_manual(name="Nutritional health", values=c(0.4, 1.0)) +
    # Theme
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  print(g)
  
}

