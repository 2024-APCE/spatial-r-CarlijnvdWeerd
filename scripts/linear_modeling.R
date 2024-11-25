### Linear modeling exercise 
renv::restore()

library(read_csv)
library(tidyverse)
library(lme4)
library(ggplot2)

CS <-read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQVcphvEPyp-VCzTdRQUtE_M4KkylBLS9yfzNGSCAtC08NrOhupLAZPMeQgxd4TLIK11JPhq9cMqV5p/pub?gid=1736259690&single=true&output=csv", h=T, sep=",") |>
  na.omit(CS) |>
  as_tibble()

names(CS)

# check for outliers
hist(CS$CS)

### step 1. linear regression 
m1 <- lm(CS~annual_density, data = CS)
summary(m1)

### Visualizing regression
ggplot(CS,aes(x=annual_density, y=CS)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, se=FALSE, color="red") 

### check residuals
par(mfrow=c(2,2))
plot(m1)
hist(residuals(m1))

### step 2. linear regression - density as factor 
CS$annual_density_cat<-as.factor(CS$annual_density_cat)
m2<-lm(CS~annual_density_cat,data=CS)
summary(m2)

### step 3. linear regression - taking annual means 
CS2<- CS |>
  dplyr::group_by(annual_density) |>
  dplyr::summarize(CS_avg=mean(CS),
                   CS_sd=sd(CS),
                   n_obs=n(),
                   CS_se=CS_sd/sqrt(n_obs))

m3<-lm(CS_avg~annual_density,data=CS2)
summary(m3)

## visualisation
ggplot(CS2,aes(x=annual_density, y=CS_avg)) + 
  geom_errorbar(aes(ymin=CS_avg-CS_se, ymax=CS_avg+CS_se), width=0.1) + 
  geom_point() + geom_smooth(method=lm,
                             se=FALSE)

m1<-glmer(CS~annual_density + year + (1|plotID) + (1|femaleID), 
          data=CS, family="gaussian")
summary(m1)
## year has three different options: as a fixed effect, a factor or a random effect
## it depends on your question: change over the years, within the years or between the years. 

m1 <- glmer(CS ~ (1|femaleID), data= CS, family="gaussian")
summary(m1)
## r = 1.211 / 1.211 + 1.544 = 0.44, this is relatively high 
confint(m1)

m2 <- glmer(CS ~ annual_density + (1|plotID), data=CS, family="gaussian")
summary(m2)
confint(m2)
## r = 0.225 / 0.225 + 2.404 = 0.09

m3<-glmer(CS~annual_density + year + (1|plotID) + (1|femaleID), 
          data=CS, family="gaussian")
summary(m3)
confint(m3)
## r = 0.8980 / (0.8980 + 0.3178 + 1.4759) = 0.35
## now variance of female is lower, because female and plot are intertwined. Which effects the variance. 

install.packages("squid")
install.packages("shiny")
install.packages("markdown")
library(markdown)

library(squid)
library(shiny)
squidApp()

###############################################################################
#Load package qdapTools to be able to use the lookup function
install.packages("qdapTools")
library(qdapTools)
#Center Annual Density  per individual
ind_avgF<-aggregate(cbind(annual_density)~femaleID,CS,mean) 
# Calc avg density per fem
## Between individual effect: mean density for each female! This is how individuals differ
CS$Btw_Ind_Dens<-lookup(CS$femaleID,ind_avgF[,c("femaleID","annual_density")])
## Within individual effect: how each value differs from individual mean.
CS$Wthin_Ind_Dens<-CS$annual_density-CS$Btw_Ind_Dens 
#Model with annual_density_cen (within individual effect) and avgAnDens (between individual effect
m6<-glmer(CS~Wthin_Ind_Dens + Btw_Ind_Dens+ (1|femaleID), data= CS, family="gaussian")
summary(m6)
confint(m6)

