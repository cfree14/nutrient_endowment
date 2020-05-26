
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
data1_orig <- readxl::read_excel(file.path(indir, "dietary_reference_intake_tables.xlsx"), sheet=1)
data2_orig <- readxl::read_excel(file.path(indir, "dietary_reference_intake_tables.xlsx"), sheet=2)
data3_orig <- readxl::read_excel(file.path(indir, "dietary_reference_intake_tables.xlsx"), sheet=3)
data4_orig <- readxl::read_excel(file.path(indir, "dietary_reference_intake_tables.xlsx"), sheet=4)
data5_orig <- readxl::read_excel(file.path(indir, "dietary_reference_intake_tables.xlsx"), sheet=5)
data6_orig <- readxl::read_excel(file.path(indir, "dietary_reference_intake_tables.xlsx"), sheet=6)

# This data is from:
# https://www.nal.usda.gov/sites/default/files/fnic_uploads/recommended_intakes_individuals.pdf

# Format data
################################################################################

# EARS
###################################

# EARs columns
colnames(data1_orig)

# EARs vitamins
vitamins <- c("Vitamin A (ug/d)",
              "Vitamin B6 (mg/d)",
              "Vitamin B12 (ug/d)",
              "Vitamin C (mg/d)", 
              "Vitamin D (ug/d)", 
              "Vitamin E (mg/d)", 
              "Folate (ug/d)",
              "Niacin (mg/d)",
              "Riboflavin (mg/d)",
              "Thiamin (mg/d)")

# EARs macronutrients
macronutrients <- c("CHO (g/d)", 
                    "Protein (g/kg/d)")

# EARs elements
elements <- c("Calcium (mg/d)", 
              "Copper (ug/d)",
              "Iodine (ug/d)", 
              "Iron (mg/d)", 
              "Magnesium (mg/d)", 
              "Molybdenum (ug/d)", 
              "Phosphorus (mg/d)", 
              "Selenium (ug/d)", 
              "Zinc (mg/d)")

# Format data
ears <- data1_orig %>% 
  # Remove blank rows
  filter(!is.na(Sex)) %>% 
  # Rename columns
  rename(sex=Sex, stage=Stage, age_range=Age) %>% 
  # Gather wide-to-long
  gather(key="nutrient", value="value", 4:ncol(.)) %>% 
  # Add DRI type
  mutate(dri_type="Estimated Average Requirement (EAR)") %>% 
  # Add nutrient type
  mutate(nutrient_type=ifelse(nutrient %in% vitamins, "Vitamin",
                              ifelse(nutrient %in% elements, "Element", "Macronutrient"))) %>% 
  # Format nutrient
  mutate(nutrient=recode(nutrient, "CHO (g/d)"="Carbohydrate (g/d)")) %>% 
  # Arrange
  select(sex:age_range, nutrient_type, nutrient, dri_type, value, everything())

# Inspect
str(ears)


# Vitamins, RDAs and AIs
###################################

# Format data
vitamin_rdas <- data2_orig %>% 
  # Remove blank rows
  filter(!is.na(Sex)) %>%
  # Rename columns
  rename(sex=Sex, stage=Stage, age_range=Age) %>% 
  # Gather wide-to-long
  gather(key="nutrient", value="value", 4:ncol(.)) %>% 
  # Extract and eliminate footnote
  mutate(footnote=gsub("[[:digit:]]|.", "", value),
          value=gsub("[^0-9.-]", "", value) %>% as.numeric()) %>% 
  # Add DRI type
  mutate(dri_type=ifelse(footnote=="*", "Adequate Intakes (AI)", "Recommended Dietary Allowances (RDA)"),
         nutrient_type="Vitamin") %>% 
  # Arrange
  select(sex:age_range, nutrient_type, nutrient, dri_type, value, footnote, everything())

# Inspect data
str(vitamin_rdas)


# Elements, RDAs and AIs
###################################

# Format data
element_rdas <- data3_orig %>% 
  # Remove blank rows
  filter(!is.na(Sex)) %>%
  # Rename columns
  rename(sex=Sex, stage=Stage, age_range=Age) %>% 
  # Gather wide-to-long
  gather(key="nutrient", value="value", 4:ncol(.)) %>% 
  # Extract and eliminate footnote
  mutate(footnote=gsub("[[:digit:]]|.", "", value),
         value=gsub("[^0-9.-]", "", value) %>% as.numeric()) %>% 
  # Add DRI type
  mutate(dri_type=ifelse(footnote=="*", "Adequate Intakes (AI)", "Recommended Dietary Allowances (RDA)"),
         nutrient_type="Element") %>% 
  # Arrange
  select(sex:age_range, nutrient_type, dri_type, value, footnote, everything())

# Inspect data
str(element_rdas)


# Vitamins, ULs
###################################

# Format data
vitamin_uls <- data5_orig %>% 
  # Remove blank rows
  filter(!is.na(Sex)) %>%
  # Rename columns
  rename(sex=Sex, stage=Stage, age_range=Age) %>% 
  # Gather wide-to-long
  gather(key="nutrient", value="value", 4:ncol(.)) %>% 
  # Format value
  mutate(footnote=gsub("[[:digit:]]|.", "", value),
         value=gsub("[^0-9.-]", "", value) %>% as.numeric()) %>%   
  # Add DRI type
  mutate(dri_type="Tolerable Upper Intake Level (UL)",
         nutrient_type="Vitamin") %>% 
  # Arrange
  select(sex:age_range, nutrient_type, dri_type, value, everything())
  
# Inspect data
str(vitamin_uls)

# Elements, ULs
###################################

# Format data
element_uls <- data6_orig %>% 
  # Remove blank rows
  filter(!is.na(Sex)) %>%
  # Rename columns
  rename(sex=Sex, stage=Stage, age_range=Age) %>% 
  # Gather wide-to-long
  gather(key="nutrient", value="value", 4:ncol(.)) %>% 
  # Format value
  mutate(footnote=gsub("[[:digit:]]|.", "", value),
         value=gsub("[^0-9.-]", "", value) %>% as.numeric()) %>%   
  # Add DRI type
  mutate(dri_type="Tolerable Upper Intake Level (UL)",
         nutrient_type="Element") %>% 
  # Arrange
  select(sex:age_range, nutrient_type, dri_type, value, everything())

# Inspect data
str(element_uls)


# Macronutrients
###################################

# Format data
macrinutr_rdas <- data4_orig %>% 
  # Remove blank rows
  filter(!is.na(Sex)) %>%
  # Rename columns
  rename(sex=Sex, stage=Stage, age_range=Age) %>% 
  # Gather wide-to-long
  gather(key="nutrient", value="value", 4:ncol(.)) %>% 
  # Extract and eliminate footnote
  mutate(footnote=gsub("[[:digit:]]|.", "", value),
         value=gsub("[^0-9.-]", "", value) %>% as.numeric()) %>% 
  # Add DRI type
  mutate(dri_type=ifelse(footnote=="*", "Adequate Intakes (AI)", "Recommended Dietary Allowances (RDA)"),
         nutrient_type="Macronutrient") %>% 
  # Arrange
  select(sex:age_range, nutrient_type, dri_type, value, footnote, everything())

# Inspect data
str(macrinutr_rdas)



# Merge data
################################################################################

# Age groups
age_groups <- c("0-6 mo", "6-12 mo",
                "1-3 yr", "4-8 yr", "9-13 yr", "14-18 yr", "19-30 yr", "31-50 yr", "51-70 yr", ">70 yr")

# Merge data
data1 <- bind_rows(ears,
                  vitamin_rdas, vitamin_uls,
                  element_rdas, element_uls,
                  macrinutr_rdas) %>% 
  # Rename column
  rename(nutrient_units=nutrient) %>% 
  # Extract units
  mutate(units=gsub(".*\\((.*)\\).*", "\\1", nutrient_units),
         units=ifelse(grepl("/", units), units, NA),
         units=recode(units, "ug/d"="µg/d")) %>% 
  # Extract nutrient
  mutate(nutrient=sub("\\(.*", "", nutrient_units) %>% trimws()) %>% 
  mutate(nutrient=recode(nutrient, "a-Linolenic acid"="α-Linolenic acid")) %>% 
  # Make ages a factor
  mutate(age_range=factor(age_range, levels=age_groups)) %>% 
  # Make sex-stage
  mutate(sex_stage=paste0(sex, " (", stage, ")"), 
         sex_stage=recode_factor(sex_stage, 
                          "Both (Infants)"="Infants",
                          "Both (Children)"="Children",
                          "Females (None)"="Women",
                          "Females (Pregnancy)"="Women (pregnant)",
                          "Females (Lactation)"="Women (lactating)",
                          "Males (None)"="Men")) %>% 
  # Arrange
  select(nutrient_type, nutrient_units, nutrient, units, 
         sex_stage, sex, stage, age_range, 
         dri_type, value, footnote, everything())

# Nutrients w/out units
# Vitamin K, Thiamin, Riboflavin, Vitamin B12, Pantothenic acid, Biotin, Carotenoids, Arsenic, Chromium, Silicon

# Inspect data
freeR::complete(data1)
table(data1$nutrient_type)
table(data1$nutrient)
table(data1$units)
table(data1$sex_stage)
table(data1$sex)
table(data1$stage)
table(data1$age_range)


# Build nutrient key
nutrient_key <- data1 %>% 
  group_by(nutrient_type, nutrient, units, dri_type) %>% 
  summarize(n=sum(!is.na(value))) %>% 
  select(-n) %>% 
  spread(key="dri_type", value="units")


# Visualize data
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

# Nutrient
nutrient <- "Iron"

# Plot data
g <- ggplot(data %>% filter(nutrient=="Iron"), aes(x=age_range, y=value, color=dri_type, group=dri_type)) +
  facet_grid(~sex_stage, scales="free_x", space = "free_x") +
  geom_line() +
  geom_point() + 
  labs(x="", y="Daily recommended intake (DRI)", main=nutrient) +
  theme_bw() + my_theme
g




