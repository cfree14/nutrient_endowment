#A program to calculate the nutrient gap based on an algorithm that integrates the distribution of intake of a group, the risk of
#inadequency based on the CDF of intake requirement and the distance to certain RDA or AI level
rm(list=ls())    #delete variables

library("openxlsx")
library("rlang")
library("ggplot2")
library("dplyr")
library("tidyverse")
library("data.table")
library("reshape2")

#-----------define working variables for demonstration
x=seq(0,50,0.01);
RDA=50;
EAR=25;sd_normal=2;
mean_intake=40
pop<-10^6; #population size

#-------------------------intake normal  
y<-dlnorm(x,meanlog =log(mean_intake),sdlog = log(sd_normal)) #lognormal
#y<-dnorm(x,mean =(mean_intake),sd = (sd_normal)) #normal
#g<-skew(y)
t<-data.table(x=x,y=y)

#-------------------------requirements
#requirement_risk<-1-plnorm(x, meanlog = log(EAR), sdlog = log(1.5));  #1 minus the cdf
requirement_risk<-1-pnorm(x, mean = (EAR), sd = (10));  #1 minus the cdf
t_req<-data.table(x=x,y=requirement_risk)

#--------------------calculate nutrient gap
integrand <- function(x) {pop*dnorm(x,mean =(mean_intake),sd = (sd_normal))*(RDA-x)*(1-pnorm(x, mean = (EAR), sd = sd_normal))}      #normal
j=integrate(integrand, lower=0, upper = RDA, stop.on.error = FALSE)


#-------------------------draw risk factor and intake distributions
p2<-ggplot(t_req,aes(x=x,y=requirement_risk))+
geom_line(color='red',data=t_req,aes(x=x,y=requirement_risk))+
geom_line(color='blue',data=t,aes(x=x,y=y/max(y)))+
  scale_y_continuous(name="risk",sec.axis = sec_axis(~ . *max(y) , name = "intake"),limits = c(0,1))
p2




#--------------------calculate nutrient gap across parameters

#intgrate for each x intake: population*probability density of intake*distance from RDA*probability of risk
sd_normal=seq(1.01,10,length.out =50);
mean_intake=seq(20,45,length.out =50)
p<-matrix(nrow=50,ncol = 50);

for (val in seq(1,50)){
for (val1 in seq(1,50)){
  #lognormal
  #integrand <- function(x) {pop*dlnorm(x,meanlog =log(mean_intake[val1]),sdlog = log(sd_normal[val]))*(RDA-x)*(1-plnorm(x, meanlog = log(EAR), sdlog = log(1.5)))}    #lognormal
  #normal
  #integrand <- function(x) {pop*dnorm(x,mean =(mean_intake[val1]),sd = (sd_normal[val]))*(RDA-x)*(1-plnorm(x, meanlog = log(EAR), sdlog = log(1.5)))}      #normal
  integrand <- function(x) {pop*dnorm(x,mean =(mean_intake[val1]),sd = (sd_normal[val]))*(RDA-x)*(1-pnorm(x, mean = (EAR), sd = sd_normal[val]))}      #normal
  
j=integrate(integrand, lower=0, upper = RDA, stop.on.error = FALSE)
p[val1,val]=j$value}}

##create colormap
pp=melt(p)
sd_normal=format(sd_normal,digits = 1,nsmall = 1)
mean_intake=format(mean_intake,digits = 1,nsmall = 1)

p3<-ggplot(pp, aes(x = Var2, y = Var1, z=value)) +
  geom_raster(aes(fill = value)) +
  scale_fill_gradient(low="grey90", high="red")+
  geom_contour(aes(colour = stat(level)),show.legend = FALSE,bins=15)+
scale_y_continuous(name='mean intake', breaks=(seq(1,50,10)),labels=as.character(mean_intake[seq(1,50,10)]))+
scale_x_continuous(name='sd', breaks=(seq(1,50,10)),labels=as.character(sd_normal[seq(1,50,10)]))+
  #annotate("text", x = 4, y = as.numeric(last(mean_intake)), label = "Some text")+  
  theme_bw()
p3
  
  
