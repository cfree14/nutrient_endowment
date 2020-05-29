
#' Calculate proportion of a population with a nutrient deficiency
#'
#' Calculates the proportion of a population that is nutrient deficient based on the EAR cut point method.
#'
#' @param nutrient Nutrient name
#' @param supply_vec Nutrient supply for each population
#' @param ear_vec EAR cut point for each population
#' @param cv C.V. for a normal distribution describing nutrient supply
#' @return Proportion of each population that is nutrient deficient
#' @export
calc_nutr_deficiency <- function(nutrient, supply_vec, ear_vec, mn_vec=NULL, cv=0.25){
  
  # Check vectors
  if(length(supply_vec)!=length(ear_vec)){stop("Nutrient supply and EAR cut point vectors are different lengths.")}
  
  # Calculate iron deficiency
  if(nutrient=="Iron"){
    perc_deficient <- calc_nutr_deficiency_iron(nutrient, supply_vec, ear_vec, mn_vec, cv=0.25)
  }
  
  # Calculate zinc deficiency
  # if(nutrient=="Zinc"){
  #   perc_deficient <- calc_nutr_deficiency_zinc(nutrient, supply_vec, ear_vec, mn_vec, cv=0.25)
  # }
  
  # Calculate nutrient deficiency 
  if(!nutrient %in% c("Iron")){
    perc_deficient <- pnorm(q=ear_vec, mean=supply_vec, sd=cv*supply_vec)
  }
  
  # Return vector
  return(perc_deficient)
  
}