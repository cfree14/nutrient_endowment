
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/ears/data/raw"
outdir <- "data/ears/data"

# Read data
data1_orig <- readxl::read_excel(file.path(indir, "DRI_summary_tables.xlsx"), sheet=1)
data2_orig <- readxl::read_excel(file.path(indir, "DRI_summary_tables.xlsx"), sheet=2)
data3_orig <- readxl::read_excel(file.path(indir, "DRI_summary_tables.xlsx"), sheet=3)
data4_orig <- readxl::read_excel(file.path(indir, "DRI_summary_tables.xlsx"), sheet=4)
data5_orig <- readxl::read_excel(file.path(indir, "DRI_summary_tables.xlsx"), sheet=5)
data6_orig <- readxl::read_excel(file.path(indir, "DRI_summary_tables.xlsx"), sheet=6)

# Format data
################################################################################

# Format data
data1 <- data1_orig %>% 
  # Remove column
  select(-pages) %>% 
  # Convert wide to long
  gather(key="nutrient_orig", value="ear", 4:ncol(.)) %>% 
  # Format nutrient and units
  mutate(nutrient=recode(nutrient_orig, 
                         "cho_g"="CHO", 
                         "copper_ug"="Copper", 
                         "folate_ug"="Folate",
                         "iodine_ug"="Iodine",
                         "iron_mg"="Iron",
                         "magnesium_mg"="Magnesium",
                         "molybdenum_ug"="Molybdenum",
                         "niacin_mg"="Niacin",
                         "phosphorus_mg"="Phosphorus",
                         "protein_g_kg"="Protein",
                         "riboflavin_mg"="Riboflavin",
                         "selenium_ug"="Selenium",
                         "thiamin_mg"="Thiamin",
                         "vitamin_a_ug"="Vitamin A",
                         "vitamin_b12_ug"="Vitamin B12", 
                         "vitamin_b6_mg"="Vitamin B6",
                         "vitamin_c_mg"="Vitamin C",
                         "vitamin_e_mg"="Vitamin E",
                         "zinc_mg"="Zinc"),
         units=recode(nutrient_orig, 
                      "cho_g"="g", 
                      "copper_ug"="μg", 
                      "folate_ug"="μg",
                      "iodine_ug"="μg",
                      "iron_mg"="mg",
                      "magnesium_mg"="mg",
                      "molybdenum_ug"="μg",
                      "niacin_mg"="mg",
                      "phosphorus_mg"="mg",
                      "protein_g_kg"="g/kg",
                      "riboflavin_mg"="mg",
                      "selenium_ug"="μg",
                      "thiamin_mg"="mg",
                      "vitamin_a_ug"="μg",
                      "vitamin_b12_ug"="μg", 
                      "vitamin_b6_mg"="mg",
                      "vitamin_c_mg"="mg",
                      "vitamin_e_mg"="mg",
                      "zinc_mg"="mg"),
         nutrient_label=paste0(nutrient, " (", units, ")")) %>% 
  # Rearrange columns
  select(-c(nutrient_orig, type)) %>% 
  select(nutrient_label, nutrient, units, sex, age_range, ear, everything()) %>% 
  # Factor ages
  mutate(age_range=factor(age_range, levels=c("7-12 mo",  "1-3 yr", "4-8 yr", 
                                              "9-13 yr" , "14-18 yr", "19-30 yr", 
                                              "31-50 yr", "51-70 yr", ">70 yr")))

# Plot data
g <- ggplot(data1, aes(x=age_range, y=ear, fill=sex)) +
  facet_wrap(~nutrient_label, ncol=4, scale="free_y") +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Age range", y="Estimated average requirement (EAR)") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
g







