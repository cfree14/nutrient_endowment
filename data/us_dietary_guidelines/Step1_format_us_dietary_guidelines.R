
# Clear
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(RColorBrewer)

# Directories
datadir <- "data/us_dietary_guidelines"

# Read data
data_orig <- rio::import(file.path(datadir, "2015_2020_US_dietary_guidelines.xlsx"), skip=1)

# Table A7-1.
# Daily Nutritional Goals for Age-Sex Groups Based on Dietary Reference Intakes 
# and Dietary Guidelines Recommendations
# https://health.gov/our-work/food-nutrition/2015-2020-dietary-guidelines/guidelines/appendix-7/

# Sources
# RDA = Recommended Dietary Allowance
# AI = Adequate Intake
# UL = Tolerable Upper Intake Level
# AMDR = Acceptable Macronutrient Distribution Range
# DGA = 2015-2020 Dietary Guidelines recommended limit
# 14 g fiber per 1,000 kcal = basis for AI for fiber

# Footnotes
# b Calcium RDA for males ages 71+ years is 1,200 mg
# c Vitamin D RDA for males and females ages 71+ years is 800 IU

# To do list
# 1) Seperate nutrient and unit
# 3) Convert source acronymn


# Format data
################################################################################

# Header info
header_info <- expand.grid(sex=c("female", "male"), 
                           age_range=c("4-8", "9-13", "14-18", "19-30", "31-50", "51+")) %>% 
  mutate(col_name=paste(sex, age_range, sep="_"))

# Column names
col_names <- c("type", "nutrient", "source", "child", header_info$col_name)

# Format data
data1 <- data_orig %>% 
  setNames(col_names) %>% 
  gather(key="key", value="value", 4:ncol(.)) %>% 
  # Add sex and age range columns
  mutate(sex=ifelse(key=="child", "child", 
                    ifelse(grepl("female", key), "female", "male")),
         age_range=gsub("female_|male_", "", key),
         age_range=recode(age_range, "child"="1-3"),
         age_range=factor(age_range, levels=c("1-3","4-8", "9-13", 
                                              "14-18", "19-30", "31-50", "51+")),
         value_type=ifelse(grepl("%", nutrient), "categorical", "continuous")) %>% 
  # Rearrange
  select(type, nutrient, source, sex, age_range, value_type, value) %>% 
  # Format values
  mutate(value=recode(value, 
                      "600c"="600",
                      "1,000b"="1000",
                      "1.1000000000000001"="1.1",
                      "19.600000000000001"="19.6",
                      "2.2000000000000002"="2.2",
                      "2.2999999999999998"="2.3",
                      "43971"="5-20",
                      "44134"="10-30",
                      "13058"="10-35")) %>% 
  # Arrange
  arrange(nutrient, sex, age_range) 

# Seperate
data <- data1 %>% 
  filter(value_type=="continuous") %>% 
  mutate(value=as.numeric(value))
data_catg <- data1 %>% 
  filter(value_type=="categorical")

# Plot data
g <- ggplot(data, aes(x=age_range, y=value, fill=sex)) +
  facet_wrap(~nutrient, ncol=4, scale="free_y") +
  geom_bar(stat="identity", position="dodge") +
  labs(x="Age range", y="Daily guideline", title="2015-20 US dietary guidelines") +
  theme_bw() +
  theme(legend.position="bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g

# Export plot
ggsave(g, filename=file.path(datadir, "2015_2020_us_dietary_guidelines.png"), 
       width=6.5, height=8.5, units="in", dpi=600)

# Export data
save(data, data_catg, file=file.path(datadir, "2015_2020_US_dietary_guidelines.Rdata"))
  
  












