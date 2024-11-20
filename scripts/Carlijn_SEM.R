#### STRUCTURAL EQUATION MODELLING USING LAVAAN

# analysis of Carlijn dataset
# Paper:
# browseURL("https://drive.google.com/file/d/1aasI7uIFj7zcjuu0A3QhPDU4X-oQtcnx/view?usp=sharing")

# restore libraries

rm(list = ls()) # clear environment

library(tidyverse)
# load the lavaan library
install.packages("lavaan")
library(lavaan)


# dataset:
# browseURL("https://docs.google.com/spreadsheets/d/1O0WScvSnl9QR4SeG5FhCaP-QXVwi6-56LOtklJP4lDM/edit?gid=885630451#gid=885630451")

# read the data from the google docs link:
SEMdata<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTNO-e_26V8I3-6ftnxxUiKnHqs5Bbl0Z8-sIxey1SZqe2fz9Ll1sGozpHNTRDjCTDUyK_O6kZOmKII/pub?gid=885630451&single=true&output=csv") 
SEMdata

# standardize all variables to mean 0 and standard deviation 1
SEMdatastd <- SEMdata |>
  mutate_all(~(scale(.) %>% as.vector)) |>
  as_tibble()
SEMdatastd
# note that this does not affect the relations between the variables, only the scales  

# make a pairs panel to inspect linearity of relations and expected normality of residuals
psych::pairs.panels(SEMdata %>% select(distance2river,elevation,CorProtAr,rainfall,CEC,burnfreq,hills,soil_pH,woodybiom),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(distance2river,elevation,CorProtAr,rainfall,CEC,burnfreq,hills,soil_pH,woodybiom),
                    stars = T, ellipses = F)

ggsave("./_figures/pairs_panels.png", width = 26, height = 18, units = "cm", dpi=300) 


# analyse the model (response ~ predictors) with a multiple regression approach 
SEMdatastd<-lm(woodybiom~distance2river,elevation,CorProtAr,rainfall,CEC,burnfreq,hills,EVI,soil_pH)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Woodybiom_model1 <- 'woodybiom~burnfreq+rainfall+CEC+EVI+soil_pH
                    CorProtAr~elevation
                    burnfreq~CorProtAr+distance2river
                    CEC~rainfall+burnfreq
                    EVI~CEC+rainfall
                    soil_pH~rainfall
                    distance2river~hills'
               

Woodybiom_model1
Woodybiom_fit1<-lavaan::sem(Woodybiom_model1, data=SEMdatastd)
# show the model results
summary(Woodybiom_fit1,standardized=T,fit.measures=T,rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

Woodybiom_model2 <- 'woodybiom~burnfreq+rainfall+CEC+soil_pH+distance2river
                    CorProtAr~elevation
                    burnfreq~CorProtAr+distance2river+rainfall+woodybiom
                    rainfall~elevation
                    CEC~rainfall+burnfreq
                    soil_pH~rainfall+burnfreq
                    distance2river~hills'


Woodybiom_model2
Woodybiom_fit2<-lavaan::sem(Woodybiom_model2, data=SEMdatastd)
# show the model results
summary(Woodybiom_fit2,standardized=T,fit.measures=T,rsquare=T)

Woodybiom_model3 <- 'woodybiom~burnfreq+rainfall+CEC+soil_pH
                    CorProtAr~elevation
                    burnfreq~CorProtAr+distance2river+rainfall
                    rainfall~elevation
                    CEC~rainfall+burnfreq+elevation
                    soil_pH~rainfall+burnfreq
                    distance2river~hills+rainfall'


Woodybiom_model3
Woodybiom_fit3<-lavaan::sem(Woodybiom_model3, data=SEMdatastd)
# show the model results
summary(Woodybiom_fit3,standardized=T,fit.measures=T,rsquare=T)


Woodybiom_model4 <- 'woodybiom~burnfreq+rainfall+CEC+soil_pH
                    CorProtAr~elevation
                    burnfreq~CorProtAr+distance2river+rainfall
                    rainfall~elevation
                    CEC~burnfreq+elevation+soil_pH
                    soil_pH~rainfall+burnfreq
                    distance2river~hills+rainfall'


Woodybiom_model4
Woodybiom_fit4<-lavaan::sem(Woodybiom_model4, data=SEMdatastd)
# show the model results
summary(Woodybiom_fit4,standardized=T,fit.measures=T,rsquare=T)

Woodybiom_model5 <- 'woodybiom~rainfall+CEC+soil_pH
                    CorProtAr~elevation
                    burnfreq~CorProtAr+rainfall+distance2river
                    rainfall~elevation
                    CEC~burnfreq+elevation+soil_pH
                    soil_pH~rainfall+burnfreq
                    distance2river~hills+rainfall'


Woodybiom_model5
Woodybiom_fit5<-lavaan::sem(Woodybiom_model5, data=SEMdatastd)
# show the model results
summary(Woodybiom_fit5,standardized=T,fit.measures=T,rsquare=T)

Woodybiom_model6 <- 'woodybiom~rainfall+CEC+soil_pH
                    CorProtAr~elevation
                    burnfreq~CorProtAr+rainfall+distance2river
                    rainfall~elevation
                    CEC~burnfreq+elevation+soil_pH+woodybiom
                    soil_pH~rainfall+burnfreq+elevation
                    distance2river~hills+rainfall'


Woodybiom_model6
Woodybiom_fit6<-lavaan::sem(Woodybiom_model6, data=SEMdatastd)
# show the model results
summary(Woodybiom_fit6,standardized=T,fit.measures=T,rsquare=T)




