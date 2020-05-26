
rm(list=ls())    #delete variables
library(openxlsx)

# Load in nutrients for Bangladesh
Genus <- readWorkbook(".\\Bangladesh_EAR_CUT.xlsx", sheet='GENuS',startRow = 3) 
# Load EAR
EAR_all <- readWorkbook(".\\BAngladesh_EAR_CUT.xlsx", sheet='EAR',colNames=TRUE,startRow = 1) 
#load population per age-sex groups
population <- readWorkbook(".\\BAngladesh_EAR_CUT.xlsx", sheet='population',colNames=TRUE,startRow = 1) 
population=population[1:32,3];

#specify which nutrient to work with

#protein (although does not qualify as a good nutrient for EAR cUT)
Nut_protein=Genus[1:32,7];
EAR_protein=EAR_all[1:32,14]; #median
CV=0.25;         #assume coefficient of variance for a normal distribution
CV_protein_in=Nut_protein*CV
source("EAR_CUT.R")
deficiency_protein=EAR_CUT(Nut_protein,CV_protein_in,EAR_protein) #fraction of nutrient defficiences per age-sex group
deficiency_protein_averaged=sum(deficiency_protein*population)/sum(population)*100 #in percentage



#vitamin A
Nut_vitaminA=Genus[1:32,19];
EAR_vitaminA=EAR_all[1:32,15]; #median
CV=0.25;         #assume coefficient of variance for a normal distribution
CV_vitaminA_in=Nut_vitaminA*CV
source("EAR_CUT.R")
deficiency_vitaminA=EAR_CUT(Nut_vitaminA,CV_vitaminA_in,EAR_vitaminA)
deficiency_vitaminA_averaged=sum(deficiency_vitaminA*population)/sum(population)*100 #in percentage



#zinc
Nut_zinc=Genus[1:32,31];
#use the miller equation to convert zinc intake to absorbed zinc
#Load in zinc and phytate densities as well as edible mass of consumption
mat_edible<- readWorkbook(".\\Bangladesh_EAR_CUT.xlsx", sheet='edible',cols = c(seq(4,678,3)))
source("zinc_absorption.R");
TAZ=zinc_absorption(mat_edible)
EAR_zinc=EAR_all[1:32,4]; #median
CV=0.25;         #assume coefficient of variance for a normal distribution
CV_zinc_in=Nut_zinc*CV
source("EAR_CUT.R")
deficiency_zinc=EAR_CUT(TAZ,CV_zinc_in,EAR_zinc)
deficiency_zinc_averaged_=sum(deficiency_zinc*population)/sum(population)*100 #in percentage


#iron
Nut_iron=Genus[1:32,28];  #median iron intake
source("EAR_prob_Iron.R");
EAR_iron=EAR_all[1:32,6]; #[7] iron 10% bioavailability,[6] 12%, [5] 15%
CV=0.25;         #assume coefficient of variance for a normal distribution
CV_iron_in=Nut_iron*CV
bioavailability=0.12;
deficiency_iron=EAR_prob_Iron(Nut_iron,CV_iron_in,EAR_iron,bioavailability)
deficiency_iron_averaged_=sum(deficiency_iron*population)/sum(population)*100 #in percentage



