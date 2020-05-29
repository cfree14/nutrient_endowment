
# Calculate absorbed zinc (TAZ) as a function of ingested zinc (TDZ) and ingested phyate (TDP)
# The units of the inputs and coefficients are in millimoles per day.

# Equation derived from:
# Miller et al. (2007) A mathematical model of zinc absorption in humans as a function of dietary zinc.
# Journal of Nutrition 137(1): 135-41. doi: 10.1093/jn/137.1.135.

# Updated coefficients from:
# Zinc bioavailability and homeostasis, Hambidge et al, Am J Clin Nutr 2010;91(suppl):1478S-83S.

# Function to calculate absorbed zinc
zinc_absorption <- function(mat_edible) {
  
  # replace NA with 0
  mat_edible[is.na(mat_edible)] <- 0  
  
  # Do some stuff
  edible_food <- mat_edible[3:34,]
  zinc_content <- mat_edible[35,]
  phytate_content <- mat_edible[36,]
  nn=32 # number of age-sex groups
  p <- length(zinc_content)
  TDZ <- matrix(nrow=32,ncol=p)
  TDP <- matrix(nrow=32,ncol=p)
  
  # Loop through...
  for (i in 1:p){
    f <- matrix(as.numeric(edible_food[,i])) #actual food consumed in g per person per day
    g <- as.numeric(zinc_content[i])
    l <- as.numeric(phytate_content[i])
    TDZ[,i] <- as.numeric(f*g/100) #zinc content g eaten*mg per g = mg 
    TDP[,i] <- as.numeric(f*l/100) #phytate content g eaten*mg per g = mg
    #names_food_unique(r)=names_food(i);
  }
  
  # Do some stuff
  TDZall <- apply(TDZ,1,sum);   #total ingested Zn (mg/d)
  TDPall <- apply(TDP,1,sum);   #total ingested phytate (mg/d)

  # miller_equation;  %absorbed Zn (mg/d)
  # convert units to millimole per day, which is required by the Miller equation. 
  phytateM <- 660.4   # mg/millimole because phytate has a molecular weight of 660.4 g/mol
  zincM <- 65.38      # mg/millimole because zinc has a molecular weight of 65.38 g/mol

  # Do some more stuff
  TDZ_tot <- Nut_zinc/zincM   #take the more representative Zn intake value from the GeNUS
  TDP_tot <- TDPall/phytateM
  n <- length(TDZ_tot);
  Amax <- 0.091; #+-0.007
  Kp <- 0.68; #+-0.22
  Kr <- 0.033; #+-0.01
  TAZ1 = matrix(nrow=1,ncol=n)
  
  # Loop through something
  for (j in 1:n){
    TAZ1[j]<-0.5*(Amax+TDZ_tot[j]+Kr*(1+TDP_tot[j]/Kp)-sqrt((Amax+TDZ_tot[j]+Kr*(1+TDP_tot[j]/Kp))^2-4*Amax*TDZ_tot[j]));
  }
  
  # convert back to mg/d
  TAZ <- TAZ1*zincM
  
  # Return
  return(TAZ)

}



