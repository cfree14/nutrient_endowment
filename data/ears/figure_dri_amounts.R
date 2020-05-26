
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/ears/data"
plotdir <- "data/ears/figures"

# Read data
data <- readRDS(file=file.path(outdir, "dietary_reference_intake_data.Rds"))



# Figure 1
################################################################################

# Build data
stats <- data %>% 
  group_by(nutrient_type, nutrient, dri_type) %>% 
  summarise(ngroups=sum(!is.na(value))) %>% 
  ungroup() %>% 
  mutate(dri_type=recode_factor(dri_type,
                                "Estimated Average Requirement (EAR)"="EAR",
                                "Recommended Dietary Allowance (RDA)"="RDA",
                                "Adequate Intake (AI)"="AI",
                                "Tolerable Upper Intake Level (UL)"="UL")) %>% 
  filter(ngroups>0)

# Plot data
g <- ggplot(stats, aes(x=dri_type, y=nutrient, fill=ngroups)) +
  geom_raster() +
  facet_grid(nutrient_type~., scales="free_y", space="free_y") +
  scale_fill_gradientn(name="Number of life\nstages groups", colors=RColorBrewer::brewer.pal(n=9, "Greens")[3:9]) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  labs(x="Daily recommended intake type", y="") +
  theme_bw() +
  theme(axis.text=element_text(size=6),
        axis.title=element_text(size=8),
        plot.title=element_text(size=10),
        strip.text=element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),)
g

# Export
ggsave(g, filename=file.path(plotdir, "dri_availability_by_nutrient.png"), 
       width=4, height=4.5, units="in", dpi=600)


# Figure 2
################################################################################

# Setup theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_text(size=10),
                  plot.title=element_text(size=12),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  legend.position = "bottom",
                  legend.text = element_text(size=8),
                  legend.title= element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"))


# Plot nutrient group
################################

# Nutrient
type_do <- "Macronutrient"

# Plot data
g <- ggplot(data %>% filter(nutrient_type==type_do), aes(x=age_range, y=value, color=dri_type, group=dri_type)) +
  facet_grid(nutrient~sex_stage, scales="free", space = "free_x") +
  geom_line() +
  geom_point() + 
  labs(x="", y="Daily recommended intake (DRI)", title=type_do) +
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "dris_by_macronutrient.pdf"), 
       width=8.5, height=11, units="in", dpi=600)
