

# Function to... do some stuff
EAR_prob_Iron <- function(Nut, CV_iron, EAR_iron, bioavailability) {
#perform the EAR cut point for non menstruating women and the probablility approach to menstruating women based
#based on https://www.ncbi.nlm.nih.gov/books/NBK217522/  
 
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





