##################SAU data cleaning - assigning unidentified catch to species caught in each country
#################Author: Daniel Viana
#################Date: 4/28/2020

library(gdata)
library(readr)
library(tidyverse)
library(rgeos)
library(scales)
library(gdata)
library(rfishbase)

###read SAU data raw files
sau = rbind(read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data/SAU raw database by EEZ 1997-2014 all catch part 1.csv"), 
            read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data/SAU raw database by EEZ 1997-2014 all catch part 2.csv"))

##Filter most recent year
sau_2014 = sau %>% filter(year==2014)

####Assign a class of fish vs invertebrates
sau_2014 = sau_2014 %>% 
  mutate(general_group = if_else(functional_group=="Lobsters, crabs" |
                                 functional_group=="Shrimps" |
                                 functional_group=="Cephalopods" |
                                 functional_group=="Jellyfish" |
                                 functional_group=="Other demersal invertebrates", "inverts", "fishes"))

###Read country code data for SAU database
country_code_SAU = read_csv(file="country_code_SAU.csv") 
country_code_SAU = country_code_SAU %>%  
  rename(fishing_entity = country_name)

###Merge SAU catch data with country code
sau_2014 = left_join(sau_2014, country_code_SAU, by="fishing_entity")

####Add unique ID for general category and country code
sau_2014 = sau_2014 %>% 
  mutate(country_group_ID = paste(country_ISO, general_group, sep="_"))

####Catch of SAU unidentified catch by functional group, year and country###
sau_nei_2014 = sau_2014 %>% 
  filter(scientific_name %in% c("Miscellaneous marine crustaceans",
                                "Miscellaneous aquatic invertebrates",
                                "Miscellaneous diadromous fishes",
                                "Marine finfishes not identified",
                                "Marine fishes not identified",
                                "Marine pelagic fishes not identified")) %>% 
  group_by(country_group_ID, fishing_entity) %>% 
  summarize(tonnes = sum(tonnes))

####proportion of SAU catch by species, year and country, without the unidentified species###
sau_spp_catch_2014 = sau_2014 %>% 
  filter(!scientific_name %in% c("Miscellaneous marine crustaceans",
                                "Miscellaneous aquatic invertebrates",
                                "Miscellaneous diadromous fishes",
                                "Marine finfishes not identified",
                                "Marine fishes not identified",
                                "Marine pelagic fishes not identified")) %>% 
  group_by(scientific_name, country_group_ID, fishing_entity, country_ISO, country_code, functional_group, fishing_sector) %>% 
  summarize(tonnes = sum(tonnes))


#################################Fill unidentified catch based on species proportions#############
gen_groups = unique(sau_2014$country_group_ID)

#for(n in 1:length(years)){
for(j in 1:length(gen_groups)){
    x = sau_spp_catch_2014 %>% 
      filter(country_group_ID == gen_groups[j])
    total_tonnes = sum(x$tonnes)
    x = x %>% 
      mutate(prop_catch = tonnes/total_tonnes)
    y = sau_nei_2014 %>% filter(country_group_ID == gen_groups[j])
    y = y$tonnes[1]
    x = x %>% 
      mutate(pred_nei_catch = prop_catch*y)
    x$pred_nei_catch[is.na(x$pred_nei_catch)]=0
    x = x %>% 
      mutate(pred_total_catch = pred_nei_catch + tonnes)
    if(j==1){
      sau_pred_catch_2014 = x}else{
        sau_pred_catch_2014 = rbind(sau_pred_catch_2014, x)
      }
  }

sau_pred_catch_2014 = sau_pred_catch_2014 %>%
  mutate(genus_seafood_cat = case_when(#Cephalopods
    functional_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    functional_group=="Small demersals (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium demersals (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large demersals (>=90 cm)" ~ "Demersal Fish", 
    functional_group=="Small bathydemersals (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium bathydemersals (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large bathydemersals (>=90 cm)" ~ "Demersal Fish",
    functional_group=="Small to medium flatfishes (<90 cm)" ~ "Demersal Fish",
    functional_group=="Large flatfishes (>=90 cm)" ~ "Demersal Fish",
    #Pelagic fish
    functional_group=="Small pelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium pelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large pelagics (>=90 cm)" ~ "Pelagic Fish", 
    functional_group=="Small benthopelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium benthopelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large benthopelagics (>=90 cm)" ~ "Pelagic Fish",
    functional_group=="Small bathypelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium bathypelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large bathypelagics (>=90 cm)" ~ "Pelagic Fish",
    #Crustaceans
    functional_group=="Shrimps" ~ "Crustaceans",
    functional_group=="Lobsters, crabs" ~ "Crustaceans",
    #Marine Fish; Other
    functional_group=="Small reef assoc. fish (<30 cm)" ~ "Marine Fish; Other",
    functional_group=="Medium reef assoc. fish (30 - 89 cm)" ~ "Marine Fish; Other",
    functional_group=="Large reef assoc. fish (>=90 cm)" ~ "Marine Fish; Other", 
    functional_group=="Small to medium rays (<90 cm)" ~ "Marine Fish; Other",
    functional_group=="Large rays (>=90 cm)" ~ "Marine Fish; Other",
    functional_group=="Small to medium sharks (<90 cm)" ~ "Marine Fish; Other",
    functional_group=="Large sharks (>=90 cm)" ~ "Marine Fish; Other",
    #Moluscs; Other
    functional_group=="Other demersal invertebrates" ~ "Moluscs; Other",
    functional_group=="Jellyfish" ~ "Moluscs; Other"))

sau_pred_catch_2014 = sau_pred_catch_2014 %>%
  rename(catch_without_nei = tonnes) %>% 
  select(fishing_entity, country_ISO, country_code, scientific_name, everything())

write.csv(sau_pred_catch_2014, "C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data/SAU_catch_without_unidentified_species_2014.csv", row.names=FALSE)

###################################Analysis for all years#################################

###read SAU data raw files
sau = rbind(read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data/SAU raw database by EEZ 1997-2014 all catch part 1.csv"), 
            read_csv("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data/SAU raw database by EEZ 1997-2014 all catch part 2.csv"))

##sttore unique coutntry list
countries = unique(sau$fishing_entity)

####Assign a class of fish vs invertebrates
sau_dat = sau %>% 
  mutate(general_group = if_else(functional_group=="Lobsters, crabs" |
                                   functional_group=="Shrimps" |
                                   functional_group=="Cephalopods" |
                                   functional_group=="Jellyfish" |
                                   functional_group=="Other demersal invertebrates", "inverts", "fishes"))

###Read country code data for SAU database
country_code_SAU = read_csv(file="country_code_SAU.csv") 
country_code_SAU = country_code_SAU %>%  
  rename(fishing_entity = country_name)

###Merge SAU catch data with country code
sau_dat = left_join(sau_dat, country_code_SAU, by="fishing_entity")

####Add unique ID for general category and country code
sau_dat = sau_dat %>% 
  mutate(country_group_ID = paste(country_ISO, general_group, sep="_"))

####Catch of SAU unidentified catch by functional group, year and country###
sau_nei = sau_dat %>% 
  filter(scientific_name %in% c("Miscellaneous marine crustaceans",
                                "Miscellaneous aquatic invertebrates",
                                "Miscellaneous diadromous fishes",
                                "Marine finfishes not identified",
                                "Marine fishes not identified",
                                "Marine pelagic fishes not identified")) %>% 
  group_by(year, country_group_ID, fishing_entity) %>% 
  summarize(tonnes = sum(tonnes))

####proportion of SAU catch by species, year and country, without the unidentified species###
sau_spp_catch = sau_dat %>% 
  filter(!scientific_name %in% c("Miscellaneous marine crustaceans",
                                 "Miscellaneous aquatic invertebrates",
                                 "Miscellaneous diadromous fishes",
                                 "Marine finfishes not identified",
                                 "Marine fishes not identified",
                                 "Marine pelagic fishes not identified")) %>% 
  group_by(year, scientific_name, country_group_ID, fishing_entity, functional_group, fishing_sector) %>% 
  summarize(tonnes = sum(tonnes))


#################################Fill unidentified catch based on species proportions#############
gen_groups = unique(sau_dat$country_group_ID)
years = unique(sau_dat$year)

for(n in 1:length(years)){
  for(j in 1:length(gen_groups)){
  x = sau_spp_catch %>% 
    filter(year == years[n],
           country_group_ID == gen_groups[j])
  total_tonnes = sum(x$tonnes)
  x = x %>% 
    mutate(prop_catch = tonnes/total_tonnes)
  y = sau_nei %>% filter(year == years[n],
                         country_group_ID == gen_groups[j])
  y = y$tonnes[1]
  x = x %>% 
    mutate(pred_nei_catch = prop_catch*y)
  x$pred_nei_catch[is.na(x$pred_nei_catch)]=0
  x = x %>% 
    mutate(pred_total_catch = pred_nei_catch + tonnes)
  if(n==1 & j==1){
    sau_pred_catch = x}else{
      sau_pred_catch = rbind(sau_pred_catch, x)
    }
  }
}
sau_pred_catch = sau_pred_catch %>%
  mutate(genus_seafood_cat = case_when(#Cephalopods
    functional_group=="Cephalopods" ~ "Cephalopods",
    #Demersal fish
    functional_group=="Small demersals (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium demersals (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large demersals (>=90 cm)" ~ "Demersal Fish", 
    functional_group=="Small bathydemersals (<30 cm)" ~ "Demersal Fish",
    functional_group=="Medium bathydemersals (30 - 89 cm)" ~ "Demersal Fish",
    functional_group=="Large bathydemersals (>=90 cm)" ~ "Demersal Fish",
    functional_group=="Small to medium flatfishes (<90 cm)" ~ "Demersal Fish",
    functional_group=="Large flatfishes (>=90 cm)" ~ "Demersal Fish",
    #Pelagic fish
    functional_group=="Small pelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium pelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large pelagics (>=90 cm)" ~ "Pelagic Fish", 
    functional_group=="Small benthopelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium benthopelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large benthopelagics (>=90 cm)" ~ "Pelagic Fish",
    functional_group=="Small bathypelagics (<30 cm)" ~ "Pelagic Fish",
    functional_group=="Medium bathypelagics (30 - 89 cm)" ~ "Pelagic Fish",
    functional_group=="Large bathypelagics (>=90 cm)" ~ "Pelagic Fish",
    #Crustaceans
    functional_group=="Shrimps" ~ "Crustaceans",
    functional_group=="Lobsters, crabs" ~ "Crustaceans",
    #Marine Fish; Other
    functional_group=="Small reef assoc. fish (<30 cm)" ~ "Marine Fish; Other",
    functional_group=="Medium reef assoc. fish (30 - 89 cm)" ~ "Marine Fish; Other",
    functional_group=="Large reef assoc. fish (>=90 cm)" ~ "Marine Fish; Other", 
    functional_group=="Small to medium rays (<90 cm)" ~ "Marine Fish; Other",
    functional_group=="Large rays (>=90 cm)" ~ "Marine Fish; Other",
    functional_group=="Small to medium sharks (<90 cm)" ~ "Marine Fish; Other",
    functional_group=="Large sharks (>=90 cm)" ~ "Marine Fish; Other",
    #Moluscs; Other
    functional_group=="Other demersal invertebrates" ~ "Moluscs; Other",
    functional_group=="Jellyfish" ~ "Moluscs; Other"))

sau_pred_catch = sau_pred_catch %>% 
  select(-country_group_ID) %>% 
  rename(catch_without_nei = tonnes)

write.csv(sau_pred_catch, "C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data/SAU_catch_without_unidentified_species.csv", row.names=FALSE)

# #####Fill unidentified catch based on species within functional groups
# #####Add unidentified catch based on catch proportion for each functional group and country 
# 
# #years = unique(sau$year)
# func_groups = unique(sau$functional_group)
# 
# #for(n in 1:length(years)){
#   for(j in 1:length(countries)){
#     for(k in 1:length(func_groups)){
#       x = sau_spp_catch_2014 %>% 
#         filter(year==2014,
#                fishing_entity == countries[j],
#                functional_group == func_groups[k])
#       total_tonnes = sum(x$tonnes)
#       x = x %>% 
#         mutate(prop_catch = tonnes/total_tonnes)
#       y= data.frame(countries[j], func_groups[k])
#       colnames(y) = c("fishing_entity", "functional_group")
#       y = left_join(y, sau_nei_2014, by=c("fishing_entity", "functional_group"))
#       y = y$tonnes[1]
#       x = x %>% 
#         mutate(pred_nei_catch = prop_catch*y)
#       x$pred_nei_catch[is.na(x$pred_nei_catch)]=0
#       x = x %>% 
#         mutate(pred_total_catch = pred_nei_catch + tonnes)
#       if(j==1 & k==1){
#         sau_pred_catch = x}else{
#           sau_pred_catch = rbind(sau_pred_catch, x)
#         }
#     }
#   }
# #}
# 
# 
# ##Sanity check
# sau_2014 = sau %>% filter(year==2014)
# sau_spp_catch_2014 = sau_spp_catch %>% filter(year==2014)
# sau_nei_2014 = sau_nei %>% filter(year==2014)
# sum(sau_2014$tonnes)
# 
# sum(sau_pred_catch$pred_total_catch) - sum(sau_2014$tonnes)
# 
# sum(sau_pred_catch$pred_nei_catch) - sum(sau_nei_2014$tonnes)
# 
# sum(sau_nei_2014$tonnes)
# 
# sau_pred_nei_catch = sau_pred_catch %>% 
#   group_by(fishing_entity) %>% 
#   summarize(pred_catch = sum(pred_nei_catch))
# 
# sau_func_group = sau_nei_2014 %>% 
#   group_by(fishing_entity) %>% 
#   summarize(tonnes = sum(tonnes))
# 
# sau_nei_pred = left_join(sau_pred_nei_catch, sau_func_group, by="fishing_entity")
# sau_nei_pred = sau_nei_pred %>% mutate(diff_catch = tonnes - pred_catch)