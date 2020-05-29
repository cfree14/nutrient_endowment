############Code Linking Species Level SAU data to GENuS Seafood Categories########
#Disaggregating Industrial, Artisanal, Recreational and Subsistence
#Linking Species to broad seafood categories (demersal, pelagic, shellfish, etc.)
#Applying proportion of catch retained domestically to Seafood consumption estimates

#Author: Daniel Viana
#04/08/2020

##Load packages
library(tidyverse) # for general data wrangling and plotting
library(rgeos)
library(scales)
library(gdata)
library(countrycode)
library(wbstats)

##Load SAU data
#setwd("C:/Users/Daniel/Google Drive/MPAs and Human Nutrition/SAU data")

###Read SAU catch (Sea Around Us)
SAU = rbind(read_csv(file="SAU raw database by EEZ 1997-2014 all catch part 1.csv"), 
            read_csv(file="SAU raw database by EEZ 1997-2014 all catch part 2.csv"))

#Linking Species to broad seafood categories (demersal, pelagic, shellfish, etc.)
#Genus seafood categories:`Demersal Fish`, `Pelagic Fish`,  
#`Marine Fish; Other`, Crustaceans, Cephalopods, `Molluscs; Other`
SAU_GENuS = SAU %>%
    mutate(genus_seafood_cat = case_when(
      #Cephalopods
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
      functional_group=="Krill" ~ "Crustaceans",
      #Reef fish
      functional_group=="Small reef assoc. fish (<30 cm)" ~ "Demersal Fish",
      functional_group=="Medium reef assoc. fish (30 - 89 cm)" ~ "Demersal Fish",
      functional_group=="Large reef assoc. fish (>=90 cm)" ~ "Demersal Fish", 
      functional_group=="Small to medium rays (<90 cm)" ~ "Demersal Fish",
      #Sharks and rays
      functional_group=="Large rays (>=90 cm)" ~ "Demersal Fish",
      functional_group=="Small to medium sharks (<90 cm)" ~ "Demersal Fish",
      functional_group=="Large sharks (>=90 cm)" ~ "Demersal Fish",
      #Moluscs; Other
      functional_group=="Other demersal invertebrates" ~ "Moluscs; Other",
      functional_group=="Jellyfish" ~ "Aquatic Animals; Others"))

                    

