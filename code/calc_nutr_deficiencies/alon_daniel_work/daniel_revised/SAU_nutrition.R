#################Linking SAU catch with nutritional datafrom Vaitla et al and FAO##############
######Author: Daniel Viana
######Date: 04/20/20

library(gdata)
library(readr)
library(tidyverse) # for general data wrangling and plotting
library(rgeos)
library(scales)
library(gdata)
library(rfishbase)


##Load fishbase data to store taxa information for all species
fish_taxa = load_taxa()
family_info = fish_taxa %>% 
  dplyr::select(Genus, Family) %>% 
  distinct(Genus, .keep_all = TRUE)
  

###read SAU data raw files
setwd("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data")
sau = rbind(read_csv(file="SAU raw database by EEZ 1997-2014 all catch part 1.csv"), 
             read_csv(file="SAU raw database by EEZ 1997-2014 all catch part 2.csv"))


#get unique species name from SAU
unique_spp_SAU = sau %>% 
  select(scientific_name, common_name, functional_group) %>% 
  distinct(scientific_name, .keep_all = TRUE) %>% 
  rename(species = scientific_name)

unique_spp_SAU = unique_spp_SAU %>%
  mutate(Iron = "Iron",
         Zinc = "Zinc",
         "Vitamin A" = "Vitamin A",
         "Vitamin B12" = "Vitamin B-12",
         "Omega-3 fatty acids" = "Omega-3 fatty acids",
         "Omega-6 fatty acids" = "Omega-6 fatty acids",
         "Vitamin D" = "Vitamin D",
         Protein = "Protein",
         "Total fat" = "Total fat")

unique_spp_SAU = reshape2::melt(unique_spp_SAU, id.vars = c("species", "common_name", "functional_group"),
                      measure.vars = c("Iron", "Zinc", "Protein", "Vitamin A", "Vitamin D", "Vitamin B12",
                                       "Omega-3 fatty acids","Omega-6 fatty acids", "Total fat"))
unique_spp_SAU = unique_spp_SAU %>%
  rename(nutrient = variable) %>%
  select(-value)

unique_spp_SAU = unique_spp_SAU %>% 
  separate(species, c("genus_SAU", "spp"), " ", remove=FALSE)

unique_spp_SAU = left_join(unique_spp_SAU, family_info, by=c("genus_SAU" = "Genus"))

unique_spp_SAU = unique_spp_SAU %>% 
  rename(family_SAU = Family)

###read nutrition data from Vaitla et al
setwd("~/MPA_Nutrition")
load("~/MPA_Nutrition/Vaitla_etal_2018_nutrient_data.Rdata")

###Read FAO ufish 1 data
setwd("~/MPA_Nutrition")
ufish1 = read_csv(file="ufish1.csv")

#########################Clean Vaitla et al data
nutrient_preds_long = nutrient_preds_long %>% 
  separate(species, c("genus", "species_2"), " ", remove=FALSE) 

#create a column for family name
nutrient_preds_long = left_join(nutrient_preds_long, family_info, by=c("genus" = "Genus"))

nutrient_preds_long = nutrient_preds_long %>% 
  distinct(species, nutrient, .keep_all = TRUE)

#########################Clean FAO ufish data
ufish1$Vitamin_A=0
ufish1$Vitamin_D=0

for(n in 1:nrow(ufish1)){
  ufish1$Vitamin_A[n] = ifelse(is.na(ufish1$`Vitamin A-retinol activity-equivalents`[n]), ufish1$`Vitamin A-retinol equivalents`[n], ufish1$`Vitamin A-retinol activity-equivalents`[n])
  ufish1$Vitamin_D[n] = ifelse(is.na(ufish1$`Vitamin D (D2 + D3)`[n]), ufish1$`Vitamin D equivalents (D2 + D3 + 5 x OH-cholecalciferol)`[n], ufish1$`Vitamin D (D2 + D3)`[n])
}

ufish1 = ufish1 %>% 
  rename(food_name = `Food name in English`,
         species = `Species/Subspecies`,
         ASFIS_common_name = `ASFIS English name`,
         ASFIS_sci_name = `ASFIS Scientific name`, 
         Protein = `Protein, total; calculated from total nitrogen`,
         Iron = `Iron, total`,
         Zinc = Zink,
         "Vitamin B12" = `Vitamin B-12`,
         "Omega-3 fatty acids" = `Fatty acids, total n-3 polyunsaturated`,
         "Omega-6 fatty acids" = `Fatty acids, total n-6 polyunsaturated`,
         "Vitamin A" = Vitamin_A,
         "Vitamin D" = Vitamin_D) %>% 
  select(`Food item ID`, Subgroup, Habitat_wild_farmed, ISSCAAP, food_name, 
         Processing_raw_dried_processed,species, ASFIS_common_name, ASFIS_sci_name,
         Protein, Iron, Zinc, `Vitamin B12`, `Vitamin D`, `Vitamin A`, `Omega-3 fatty acids`, 
         `Omega-6 fatty acids`)

ufish=reshape2::melt(ufish1, id.vars = c("Food item ID", "Subgroup", "Habitat_wild_farmed", "ISSCAAP", "food_name", 
                                         "Processing_raw_dried_processed","species", "ASFIS_common_name", "ASFIS_sci_name"), 
                     measure.vars = c("Protein", "Iron", "Zinc", "Vitamin B12", "Vitamin D", "Vitamin A", "Omega-3 fatty acids", 
                                      "Omega-6 fatty acids"))
#chanche class of variable
ufish$value = as.numeric(ufish$value)

#Fix scientific names, add genus and family
ufish$species<- gsub("[()]","",ufish$species)
ufish$species<- sub(",.*","",ufish$species)
ufish$species<- sub("/"," ",ufish$species)
ufish$species<- sub("and"," ",ufish$species)

ufish = ufish %>% 
  separate(species, c("genus", "spp"), " ", remove=FALSE) %>% 
  rename(species_original = species) 

ufish$spp[is.na(ufish$spp)] = "spp"
ufish$spp[ufish$spp=="sp."] = "spp"
ufish$spp[ufish$spp=="spp"] = "spp."

ufish = ufish %>% 
  mutate(species = paste(genus, spp, sep=" "))

#Add family
ufish = left_join(ufish, family_info, by=c("genus" = "Genus"))


######calculate average for species scientific names, genus and family#####################

######Vaitla et al####################

#get unique species_closest
vaitla_spp_closest = nutrient_preds_long %>%
  distinct(species_closest, nutrient, .keep_all = TRUE) %>% 
  select(species_closest, nutrient, value_md_fill)

#calculate mean values for genus
vaitla_genus = nutrient_preds_long %>% 
  drop_na(genus, value_md_fill) %>% 
  group_by(genus, nutrient) %>% 
  summarize(value_md_fill = mean(value_md_fill))

#calculate mean values for family
vaitla_family = nutrient_preds_long %>%  
  drop_na(Family, value_md_fill) %>%
  group_by(nutrient, Family) %>% 
  summarize(value_md_fill = mean(value_md_fill))

#####FAO ufish 1 data##############

##aggrgate mean values by species scientific name
ufish_sciname = ufish %>% 
  drop_na(value, species) %>% 
  group_by(species, variable) %>% 
  summarize(mean(value)) %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(species, nutrient, value_md_fill)

##aggrgate mean values by genus (some entries - family)
ufish_genus = ufish %>% 
  drop_na(genus, value) %>% 
  group_by(genus, variable) %>% 
  summarize(mean(value)) %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(genus, nutrient, value_md_fill)

##agreggate by family
ufish_family = ufish %>% 
  drop_na(Family, value) %>% 
  group_by(Family, variable) %>% 
  summarize(mean(value)) %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(Family, nutrient, value_md_fill)


##########################Fill nutritional data by species scientific name from Vaitla et al
#join databases
sau_nutrition = left_join(unique_spp_SAU, nutrient_preds_long, by=c("species", "nutrient"))

sau_nutrition$value_md_fill[sau_nutrition$value_md_fill==0]=NA

#find NAs and remove lobsters and crabs
missing = sau_nutrition %>% 
  filter(is.na(value_md_fill)) %>% 
  select(-value_md, -value_lo, -value_hi)

sau_nutrition = sau_nutrition %>% 
  filter(!is.na(value_md_fill)) %>% 
  select(-value_md, -value_lo, -value_hi)

#############fill missing values with species_closest ###############
missing = missing %>% 
  dplyr::select(-value_md_fill)
missing = left_join(missing, vaitla_spp_closest, by=c("species" = "species_closest", "nutrient"="nutrient"), keep=T)
missing_closest = missing %>% 
  filter(!is.na(value_md_fill))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_closest)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)


###############Fill missing with genus #######################

#Join datasets 
missing = left_join(missing, vaitla_genus, by=c("genus_SAU" = "genus", "nutrient"="nutrient"), keep=T)
missing_genus = missing %>% 
  filter(!is.na(value_md_fill)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_genus)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)


###############Fill missing with family #######################

#Join datasets
missing = left_join(missing, vaitla_family, by=c("species" = "Family", "nutrient"="nutrient"), keep=T)
missing_family1 = missing %>% 
  filter(!is.na(value_md_fill)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_family1)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)

#Join datasets
missing = left_join(missing, vaitla_family, by=c("family_SAU" = "Family", "nutrient"="nutrient"))
missing_family2 = missing %>% 
  filter(!is.na(value_md_fill))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_family2)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)

##########################Fill missing values with FAO data ###############################################

#############fill missing values with species scientific name ###############
missing = left_join(missing, ufish_sciname, by=c("species" = "species", "nutrient"="nutrient"), keep=T)
#missing$value_md_fill[missing$value_md_fill==0]=NA
missing_ufish_sciname = missing %>% 
  filter(!is.na(value_md_fill))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_ufish_sciname)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)


###############Fill missing with genus #######################

#Join datasets 
missing = left_join(missing, ufish_genus, by=c("genus_SAU" = "genus", "nutrient"="nutrient"), keep=T)
missing_ufish_genus = missing %>% 
  filter(!is.na(value_md_fill)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_ufish_genus)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)


###############Fill missing with family #######################

#Join datasets
missing = left_join(missing, ufish_family, by=c("species" = "Family", "nutrient"="nutrient"), keep=T)
missing_ufish_family1 = missing %>% 
  filter(!is.na(value_md_fill)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_ufish_family1)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)

#Join datasets
missing = left_join(missing, ufish_family, by=c("family_SAU" = "Family", "nutrient"="nutrient"))
missing_ufish_family2 = missing %>% 
  filter(!is.na(value_md_fill))

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_ufish_family2)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)

########################## Fill missing data by SAU functional group #############################

######calculate mean values for each functional group from compiled data
unique_functional_group = sau_nutrition %>% 
  group_by(functional_group, nutrient) %>% 
  summarize(value_md_fill = mean(value_md_fill))

#Join datasets
missing = left_join(missing, unique_functional_group, by=c("functional_group", "nutrient"), keep=T)
missing_functional_group = missing %>% 
  filter(!is.na(value_md_fill)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_functional_group)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) %>% 
  dplyr::select(-value_md_fill)

########################## Fill missing data by FAO ufish1 fish_names##########

#Calculate average for shrimps
ufish_shrimps = ufish %>% 
  filter(str_detect(food_name, "Shrimp"),
         str_detect(Processing_raw_dried_processed, "r"))

ufish_shrimps = ufish_shrimps %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group="Shrimps") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(functional_group, nutrient, value_md_fill)

#Calculate average for Lobster, crab
ufish_lobster = ufish %>% 
  filter(str_detect(food_name, c("Lobster", "crab")),
         str_detect(Processing_raw_dried_processed, "r"))

ufish_lobster = ufish_lobster %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group="Lobsters, crabs") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(functional_group, nutrient, value_md_fill)

#Calculate average for Jellyfish
ufish_Jellyfish = ufish %>% 
  filter(str_detect(food_name, c("Jellyfish")),
         str_detect(Processing_raw_dried_processed, "r"))

ufish_Jellyfish = ufish_Jellyfish %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group="Jellyfish") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(nutrient, functional_group, value_md_fill)

#Calculate average for Other demersal invertebrates
ufish_invert = ufish %>% 
  filter(str_detect(food_name, c("mussel", "oyster", "clam", "abalone", "scallop")),
         str_detect(Processing_raw_dried_processed, "r"))

ufish_invert = ufish_invert %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group="Other demersal invertebrates") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(nutrient, functional_group, value_md_fill)

#Calculate average for Cephalopods
ufish_cephalopods = ufish %>%
  filter(str_detect(food_name, c("octopus", "squid")),
         str_detect(Processing_raw_dried_processed, "r"))


ufish_cephalopods = ufish_cephalopods %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group="Cephalopods") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(nutrient, functional_group, value_md_fill)

#Calculate average for sharks
ufish_sharks = ufish %>% 
  filter(Subgroup == "Finfish") %>% 
  filter(str_detect(food_name, c("shark")),
       str_detect(Processing_raw_dried_processed, "r"))

ufish_sharks = ufish_sharks %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group1="Small to medium sharks (<90 cm)",
         functional_group2="Large sharks (>=90 cm)") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(nutrient, functional_group1, functional_group2, value_md_fill)

ufish_sharks1 = ufish_sharks %>% select(-functional_group2) %>% rename(functional_group = functional_group1)
ufish_sharks2 = ufish_sharks %>% select(-functional_group1) %>% rename(functional_group = functional_group2)

ufish_sharks = rbind(ufish_sharks1, ufish_sharks2)

#Calculate average for rays
ufish_rays = ufish %>% 
  filter(Subgroup == "Finfish") %>% 
  filter(str_detect(food_name, c("ray")),
         str_detect(Processing_raw_dried_processed, "r"))

ufish_rays = ufish_rays %>% 
  drop_na(value) %>% 
  group_by(variable) %>% 
  summarize(mean(value)) %>% 
  mutate(functional_group1="Small to medium rays (<90 cm)",
         functional_group2="Large rays (>=90 cm)") %>% 
  rename(nutrient = variable,
         value_md_fill = "mean(value)") %>% 
  dplyr::select(nutrient, functional_group1, functional_group2, value_md_fill)

ufish_rays1 = ufish_rays %>% select(-functional_group2) %>% rename(functional_group = functional_group1)
ufish_rays2 = ufish_rays %>% select(-functional_group1) %>% rename(functional_group = functional_group2)

ufish_rays = rbind(ufish_rays1, ufish_rays2)

ufish_functional_group = rbind(ufish_sharks, ufish_Jellyfish, ufish_invert,
                                ufish_lobster, ufish_rays, ufish_shrimps, ufish_cephalopods)

#Join datasets
missing = left_join(missing, ufish_functional_group, by=c("functional_group", "nutrient"), keep=T)
missing_ufish_functional_group = missing %>% 
  filter(!is.na(value_md_fill)) 

##include filled values
sau_nutrition = rbind(sau_nutrition, missing_ufish_functional_group)

##Seperate remaining missing values
missing = missing %>% 
  filter(is.na(value_md_fill)) 

##Final data cleaning
sau_nutrition = sau_nutrition %>% 
  select(species, genus_SAU, spp, family_SAU, common_name, functional_group, nutrient, nutrient_label,
         units_long, units_short, value_md_fill) %>% 
  rename(genus = genus_SAU,
         family=family_SAU)

missing = missing %>% 
  select(species, genus_SAU, spp, family_SAU, common_name, functional_group, nutrient, nutrient_label,
         units_long, units_short, value_md_fill) %>% 
  rename(genus = genus_SAU,
         family=family_SAU)


#Export final dataset
write.csv(sau_nutrition, "nutrient_SAU_spp.csv", row.names=FALSE)

#Export missing species/nutrient combinations
write.csv(missing, "missing_nutrient_SAU_spp.csv", row.names=FALSE)
















########################## Fill missing data by commom name and family ##########

##Rename some common names
missing_fill_comName = missing_fill %>% mutate(common_name=recode(common_name, 
                                                                  "Parupeneid goatfishes"="Yellowfin goatfish",
                                                                  "Soft goatfishes"="Yellowfin goatfish",
                                                                  "Thryssas" = "Moustached thryssa"))
#Find the scientific names
comName = common_to_sci(missing_fill_comName$common_name)

#Merge with missing data
missing_fill_comName = left_join(missing_fill_comName, comName, by=c("common_name" = "ComName"))

#Get family names
missing_fill_comName = left_join(missing_fill_comName, family_info, by="Species")

#Get unique values
missing_fill_comName = missing_fill_comName %>% 
  distinct(species, .keep_all = TRUE)

#Merge with nutritional family estimates 
missing_fill_comName = left_join(missing_fill_comName, unique_family, by=c("Family"))

#Selct columns to bind data
missing_fill_comName = missing_fill_comName %>% 
  filter(!is.na(value_md_fill)) %>% 
  dplyr::select(-Family, -Species, -Language, -SpecCode)

#Bind data
sau_nutrition = rbind(sau_nutrition, missing_fill_comName)

#Export final dataset
write.csv(sau_nutrition, "Vaitla_etal_2018_nutrient_data_SAU_sau.csv")

