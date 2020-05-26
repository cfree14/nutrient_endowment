
zinc_absorption <- function(mat_edible) {
  #Miller equation for calculting absorbed zinc (TAZ) as a function of ingested
  #zinc(TDZ) and ingested phytate (TDP). Units of inputs and coefficients are
  #in millimole per day. Equation derived from A Mathematical Model of Zinc Absorption
  #in Humans As a Function of Dietary Zinc, Miller et al, 2007, J. Nutr. 137: 135-141, 2007.
  #Updated coefficiencts from Zinc bioavailability and homeostasis, Hambidge et al, Am J Clin Nutr 2010;91(suppl):1478S-83S.
mat_edible[is.na(mat_edible)] <- 0   #replace NA with 0
edible_food<-mat_edible[3:34,];
zinc_content<-mat_edible[35,];
phytate_content<-mat_edible[36,];
nn=32; #number of age-sex groups
p<-length(zinc_content);
TDZ<-matrix(nrow=32,ncol=p);
TDP<-matrix(nrow=32,ncol=p);
for (i in 1:p){
f<-matrix(as.numeric(edible_food[,i]));#actual food consumed in g per person per day
g<-as.numeric(zinc_content[i]);
l<-as.numeric(phytate_content[i]);
TDZ[,i]<-as.numeric(f*g/100); #zinc content g eaten*mg per g = mg 
TDP[,i]<-as.numeric(f*l/100);#phytate content g eaten*mg per g = mg
#names_food_unique(r)=names_food(i);
}

TDZall<-apply(TDZ,1,sum);   #total ingested Zn (mg/d)
TDPall<-apply(TDP,1,sum);   #total ingested phytate (mg/d)

#miller_equation;  %absorbed Zn (mg/d)
#convert units to millimole per day, which is required by the Miller
#equation. 
14
phytateM<-660.4; #mg/millimole because phytate has a molecular weight of 660.4 g/mol
zincM<-65.38;#mg/millimole because zinc has a molecular weight of 65.38 g/mol

TDZ_tot<-Nut_zinc/zincM;   #take the more representative Zn intake value from the GeNUS
TDP_tot<-TDPall/phytateM;
n<-length(TDZ_tot);
Amax<-0.091;#+-0.007
Kp<-0.68;#+-0.22
Kr<-0.033;#+-0.01
TAZ1=matrix(nrow=1,ncol=n);
for (j in 1:n){
TAZ1[j]<-0.5*(Amax+TDZ_tot[j]+Kr*(1+TDP_tot[j]/Kp)-sqrt((Amax+TDZ_tot[j]+Kr*(1+TDP_tot[j]/Kp))^2-4*Amax*TDZ_tot[j]));
}
TAZ<-TAZ1*zincM; #convert back to mg/d
return(TAZ)
}



