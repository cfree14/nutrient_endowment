
#' Calculate proportion of a population with an iron deficiency
#'
#' Calculates the proportion of a population that is iron deficient using the EAR cut point method for chilfren, men, and non-menstruating women and the NRC (1986) probability approach for menstruating women.
#'
#' @param nutrient Nutrient name (a string)
#' @param supply_vec Nutrient supply for each population (numeric vector)
#' @param ear_vec EAR cut point for each population (numeric vector)
#' @param cv C.V. for a normal distribution describing nutrient supply (a number)
#' @return Proportion of each population that is nutrient deficient (numeric vector)
#' @references NRC (1986) Nutrient Adequacy: Assessment Using Food Consumption Surveys. National Academies Press: Washington, DC 
#' @export
calc_nutr_deficiency_iron <- function(nutrient, supply_vec, ear_vec, mn_vec, cv=0.25){
  
  # Check vectors
  if(length(supply_vec)!=length(ear_vec)){stop("Nutrient supply and EAR cut point vectors are different lengths.")}
  
  # Calculate iron deficiency using EAR cut point
  ######################################################
  
  # Calculate iron deficiency using EAR cut point method
  perc_deficient_ear <- pnorm(q=ear_vec, mean=supply_vec, sd=cv*supply_vec)
  
  # Calculate iron deficiency using NRC (1986 method)
  ######################################################
  
  # Ironn bioavailability
  bioavailability <- 0.12
  
  # SD of iron supply
  sd_vec <- cv*supply_vec
  
  # Perform calculations that I don't understand yet...
  perc_deficient_nrc <- rep(NA, length(supply_vec))
  for(i in 1:length(perc_deficient_nrc)){
    integrand <- function(x) {(1-pnorm(log((x*bioavailability-0.87)-(-0.81)/0.84)))*(dnorm(x*bioavailability, supply_vec[i]*bioavailability,bioavailability*sd_vec[i]*supply_vec[i]))}
    j <- integrate(integrand, lower=0, upper = Inf, stop.on.error = FALSE)
    integrandnorm <- function(x){dnorm(x*bioavailability,supply_vec[i]*bioavailability,bioavailability*sd_vec[i]*supply_vec[i])}
    m <- integrate(integrandnorm, lower=0, upper = Inf, stop.on.error = FALSE)
    perc_deficient_nrc[i] <- j$value / m$value
  }
  
  # Merge and select correct estimates
  ######################################################
  
  # Merge data
  df <- tibble(supply=supply_vec,
               ear=ear_vec,
               menstruation=mn_vec,
               perc_deficient_ear=perc_deficient_ear,
               perc_deficient_nrc=perc_deficient_nrc) %>% 
    mutate(perc_deficient=ifelse(menstruation=="non-menstruating", perc_deficient_ear, perc_deficient_nrc))

  # Final data
  perc_deficient <- df$perc_deficient
  
  # Return vector
  return(perc_deficient)
  
}






EAR_prob_Iron <- function(Nut, CV_iron, EAR_iron, bioavailability) {

  
  
  
  #divide between mensturating women and all others
  def<-c(1:length(Nut))  #allocate vector def
  for (i in 1:length(Nut))
    if (i==5||i==7||i==9||i==11||i==13||i==15||1==17)  #location of sex age groups - mensturating women
    {
      #for mensturating women
      #based on 
      #Z =(log((EAR_iron[i]*bioavailability-0.87),base = exp(1))-(-0.81)/0.84);
      #integrating a normal distribution of intake with a probablistic nutrient defficiency risk model (1-normal cumulative) multiplied 
      #by a normal distribution of consumption (which is normalised)
      
      integrand <- function(x) {(1-pnorm(log((x*bioavailability-0.87)-(-0.81)/0.84)))*(dnorm(x*bioavailability,Nut[i]*bioavailability,bioavailability*CV_iron[i]*Nut[i]))}
      j=integrate(integrand, lower=0, upper = Inf, stop.on.error = FALSE)
      integrandnorm<- function(x){dnorm(x*bioavailability,Nut[i]*bioavailability,bioavailability*CV_iron[i]*Nut[i])}
      m=integrate(integrandnorm, lower=0, upper = Inf, stop.on.error = FALSE)
      def[i] <- j$value/m$value
    }
  else #for all others, use EAR cut method
  {
    Nut_iron=Nut[i]*bioavailability;
    #assume coefficient of variance for a normal distribution
    CV_iron_in=Nut_iron*CV
    def[i]=EAR_CUT(Nut_iron,CV_iron_in,EAR_iron)
  }
  end
  
  
  return(def)
}