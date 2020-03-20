

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(rio)
library(ggplot2)
library(tidyverse)

# Directories
inputdir <- "data/vaitla_etal_2018/raw"
outputdir <- "data/vaitla_etal_2018/processed"
plotdir <- "data/vaitla_etal_2018/figures"

# Read data
load(file.path(outputdir, "Vaitla_etal_2018_nutrient_data.Rdata"))


# Plot data
################################################################################

# Format data
preds_long <- nutrient_preds_long %>% 
  mutate(nutrient_title=paste0(nutrient, "\n(", units_long, ")"))

# Add column to key
key <- nutrient_key %>% 
  mutate(nutrient_title=paste0(nutrient, "\n(", units_long, ")"))

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=11),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))

# Histogram nutrient data
g <- ggplot(preds_long, aes(x=value_md)) +
  geom_histogram(fill="grey70") +
  labs(x="Concentration", y="Number of species") +
  facet_wrap(~nutrient_title, ncol=3, scales="free") +
  geom_vline(key, mapping=aes(xintercept=thresh), linetype="dotted") +
  geom_vline(key, mapping=aes(xintercept=thresh_rich), linetype="dotted") +
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_nutrient_dists_pred.png"), 
       width=6.5, height=6.5, units="in", dpi=600)



