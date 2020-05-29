
#' Calculate proportion of a population with a zinc deficiency
#'
#' Calculates the proportion of a population that is zinc deficient after accounting for zinc absorption rates.
#' 
#' @param nutrient Nutrient name (a string)
#' @param supply_vec Nutrient supply for each population (numeric vector)
#' @param ear_vec EAR cut point for each population (numeric vector)
#' @param cv C.V. for a normal distribution describing nutrient supply (a number)
#' @return Proportion of each population that is nutrient deficient (numeric vector)
#' @references Miller, L.V., Krebs, N.F., Hambidge, K.M. (2007) A mathematical model of zinc absorption in humans as a function of dietary zinc. 
#' Journal of Nutrition 137(1): 135-41.
#' @references Hambidge, K.M., Miller, L.V., Westcott, J.E., Sheng, X., Krebs, N.F. (2010) Zinc bioavailability and homeostasis. 
#' The American Journal of Clinical Nutrition 91(5): 1478S-1483S.
#' @export
calc_nutr_deficiency_zinc <- function(nutrient, supply_vec, ear_vec, cv){
  
  # Miller equation parameter values
  # Miller et al. 2007: AMAX=0.130 ± 0.047, Kp=1.20 ± 0.74, Kr=0.100 ± 0.083
  # Hambidge et al. 2010: Amax = 0.091 ± 0.007; Kp =0.68 ± 0.2; Kr = 0.033 ± 0.01 
  Amax <- 0.091 
  Kp <- 0.68 
  Kr <- 0.033
  
  # Miller equation
  miller_eq <- function(zinc_supply_mmol, phyate_intake_mmol){
    taz <- 0.5 * (Amax + zinc_supply_mmol + Kr * (1 + phyate_intake_mmol / Kp) - sqrt((Amax + zinc_supply_mmol + Kr * (1 + phyate_intake_mmol / Kp))^2 - 4 * Amax * zinc_supply_mmol))
    return(taz)
  }

  # Molecular weights (g/mole = mg/millimole)
  zincM <- 65.38
  phyateM <- 660.4
  
  # Format edible food data
  zinc_supply <- edible_mat %>% 
    # Reduce to sources of zinc
    filter(nutrient %in% c("Zinc")) %>% 
    # Reduce to country of interest 
    filter(iso3_use=="BGD") %>% 
    # Convert to millimoles
    mutate(value_med=value_med/zincM,
           value_lo=value_lo/zincM,
           value_hi=value_hi/zincM,
           units_short="mmol",
           units_long="mmol/person/day",
           nutrient_label="Zinc (mmol)")
    
  country_do <- country
  food <- food_cntry_age_sex_2011 %>% 
    # Reduce to country of interest
    filter(country_use==country_do) %>% 
    # Reduce to foods with zinc/phyate
    filter(food %in% zinc_phyate_conc$food) %>% 
    # Add zinc/phyate absorption
    left_join(zinc_phyate_conc, by="food") %>% 
    # Calculate intakes in mg/day (mg/100g * g/d)
    mutate(zinc_intake_mg_d=zinc_mg_100g * value_med / 100,
           phyate_intake_mg_d=phyate_mg_100g * value_med / 100) %>% 
    # Convert intakes to millimoles/day
    mutate(zinc_intake_mmol_d=zinc_intake_mg_d / zincM,
           phyate_intake_mmol_d=phyate_intake_mg_d / phyateM) %>%
    # Sum by age/sex group
    group_by(age_range, sex) %>% 
    summarize(zinc_intake_mmol_d = sum(zinc_intake_mmol_d, na.rm=T),
              phyate_intake_mmol_d = sum(phyate_intake_mmol_d, na.rm=T)) %>% 
    # Add zinc supply from GENUS data
    left_join(zinc_supply %>% select(age_range, sex, value_med), by=c("age_range", "sex")) %>% 
    rename(zinc_supply_mmol_d=value_med) %>% 
    # Calculate zinc absorption with Miller equation
    mutate(zinc_absorbed_mmol_d = miller_eq(zinc_supply_mmol = zinc_supply_mmol_d, 
                                            phyate_intake_mmol = phyate_intake_mmol_d)) %>% 
    # Convert absorbed zinc back to mg/day
    mutate(zinc_absorbed_mg_d = zinc_absorbed_mmol_d * zincM)
  
  
  # Return
  return(TAZ)

}



