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
psych::pairs.panels(SEMdata %>% select(distance2river,elevation,CorProtAr,rainfall,CEC,burnfreq,hills,EVI,soil_pH,woodybiom),
                    stars = T, ellipses = F)
psych::pairs.panels(SEMdatastd %>% select(distance2river,elevation,CorProtAr,rainfall,CEC,burnfreq,hills,EVI,soil_pH,woodybiom),
                    stars = T, ellipses = F)

# analyse the model (response ~ predictors) with a multiple regression approach 
SEMdatastd<-lm(woodybiom~distance2river,elevation,CorProtAr,rainfall,CEC,burnfreq,hills,EVI,soil_pH)

# visualization of the result: 
# browseURL("https://docs.google.com/presentation/d/1Q7uXC5Wiu0G4Xsp5uszCNHKOnf1IMI9doY-13Wbay4A/edit?usp=sharing")

# Make a lavaan model as hypothesized in the Anderson et al 2007 paper and fit the model 
Woodybiom_model <- 'LF_N~BIOMASS+RES_LHU+FIRE_FRQ+NMS
               BIOMASS~FIRE_FRQ+RES_LHU
               NMS~FIRE_FRQ+RES_LHU'
Leaf_N_model
Leaf_N_fit<-lavaan::sem(Leaf_N_model, data=Anderson2007std)
# show the model results
summary(Leaf_N_fit,standardized=T,fit.measures=T,rsquare=T)

# goodness of fit (should be >0.9): CFI and TLI
# badness of fit: ( should be <0.1): RMSEA, SRMR

# Make a lavaan model for phosphorus 
Leaf_P_model <- 'LF_P~BIOMASS+RES_LHU+FIRE_FRQ+NMS
                 BIOMASS~FIRE_FRQ+RES_LHU
                 NMS~FIRE_FRQ+RES_LHU'
Leaf_P_model
Leaf_P_fit<-lavaan::sem(Leaf_P_model, data=Anderson2007std)
summary(Leaf_P_fit,standardized=T,fit.measures=T,rsquare=T)




