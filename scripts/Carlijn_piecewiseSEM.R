# Piecewise SEM
install.packages("piecewiseSEM")
library(piecewiseSEM)

# read the pointdata
pointdata_init<-read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTNO-e_26V8I3-6ftnxxUiKnHqs5Bbl0Z8-sIxey1SZqe2fz9Ll1sGozpHNTRDjCTDUyK_O6kZOmKII/pub?gid=885630451&single=true&output=csv")
pointdata <- pointdata_init |> # Remove rows with missing values
  na.omit() |>   # keep complete cases
  dplyr:: filter(woodybiom>0, woodybiom<20)   # remove 2 extreme values and avoid interpolated negative values

# note that you should not standardize your data for a PicewiseSEM as then eg logistic regression cannot be used

# Check for missing values
sum(is.na(pointdata))
colSums(is.na(pointdata))


psych::pairs.panels(pointdata,stars = T, ellipses = F)


# Define the models
# I started from this initially hypothesized causal scheme, my model 1)
# browseURL("https://docs.google.com/presentation/d/1PB8rhbswyPew-FYULsw1pIl8Jyb1FFElKPf34DZrEY8/edit?usp=sharing")

# Model 1: woody predicted by burnfreq and rainfall
model_woody <- lm(woodybiom ~ CEC +rainfall+soil_pH+elevation+burnfreq , 
                  data = pointdata)
summary(model_woody)
p1<-ggplot(data=pointdata,aes(x=rainfall,y=woodybiom))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p1
p2<-ggplot(data=pointdata,aes(x=CEC,y=woodybiom))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T) 
p2

p3<-ggplot(data=pointdata,aes(x=soil_pH,y=woodybiom))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T)
p3

p20<-ggplot(data=pointdata,aes(x=elevation,y=woodybiom))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T)
p20

p21<-ggplot(data=pointdata,aes(x=burnfreq,y=woodybiom))+
  geom_point() +
  geom_smooth(method="lm",
              #              method.args=list(family=Gamma(link="log")),
              formula= y~x,
              se=T)
p21

# Model_burnfreq: burning frequency predicted by Core Protected Areas, Distance 2 River and Rainfall
model_burnfreq_init <- glm(burnfreq ~ CorProtAr + rainfall + distance2river, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
# install.packages("MASS")
library(MASS)
model_burnfreq <- MASS::glm.nb(burnfreq ~ CorProtAr + rainfall 
                               + distance2river +elevation, 
                               data = pointdata)
summary(model_burnfreq)

p4<-ggplot(data=pointdata,aes(y=burnfreq,x=CorProtAr))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
p4

p5<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p5

p6<-ggplot(data=pointdata,aes(y=burnfreq,x=distance2river))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p6

p19<-ggplot(data=pointdata,aes(y=burnfreq,x=elevation))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p19

# model_cec: predicted by elevation, burning frequency and soil pH

model_CEC <- lm(CEC ~ elevation + burnfreq + soil_pH, 
                data = pointdata)
summary(model_CEC)

p7<-ggplot(data=pointdata,aes(y=CEC,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p7

p8<-ggplot(data=pointdata,aes(y=CEC,x=burnfreq))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p8

p9<-ggplot(data=pointdata,aes(y=CEC,x=soil_pH))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p9


# model_CorProtAra:  predicted by elevation
model_CorProtAr <-glm(CorProtAr~elevation+hills,
                      family=binomial,
                      data=pointdata)
summary(model_CorProtAr)
p10<-ggplot(data=pointdata,aes(y=CorProtAr,x=elevation))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
p10

# p22 <- ggplot(data=pointdata, aes(y=CorProtAr, x=hills)) +
  geom_jitter(width = 0.05, height = 0.1) + # Jitter to prevent overplotting
  geom_smooth(method="glm",                # Use generalized linear model
              method.args = list(family="binomial"), # Logistic regression
              formula = y ~ x,             # Specify the formula
              se = TRUE)
# p22

# model_rainfall: rainfall predicted by elevation
model_rainfall <- lm(rainfall ~ elevation+hills, 
                     data = pointdata)
summary(model_rainfall)

p11<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p11

p23<-ggplot(data=pointdata,aes(y=rainfall,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p23

# model soil pH
model_soil_pH <- glm(soil_pH ~ rainfall+burnfreq+elevation+hills,
                     family=Gamma(link = "log"), 
                    data = pointdata)

# Calculate dispersion statgaussian()# Calculate dispersion statistic
dispersion_stat2 <- summary(model_soil_pH)$deviance / summary(model_soil_pH)$df.residual
dispersion_stat2
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
# install.packages("MASS")
library(MASS)
model_soil_pH_mass <- MASS::glm.nb(soil_pH ~ rainfall + burnfreq, 
                               data = pointdata)
summary(model_soil_pH_mass)

p12<-ggplot(data=pointdata,aes(y=soil_pH,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=gaussian),  # close to glm.nb
              formula= y~x,
              se=T)
p12

p13<-ggplot(data=pointdata,aes(y=soil_pH,x=burnfreq))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
p13

p16<-ggplot(data=pointdata,aes(y=soil_pH,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p16

p17<-ggplot(data=pointdata,aes(y=soil_pH,x=hills))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p17

model_distance2river<-lm(distance2river~hills+rainfall+elevation,
                         data=pointdata)

p14<-ggplot(data=pointdata,aes(y=distance2river,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p14

p15<-ggplot(data=pointdata,aes(y=distance2river,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p15

p18<-ggplot(data=pointdata,aes(y=distance2river,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p18

# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+p8+p9+p10+p11+p12+p13+p14+p15 +
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

ggsave("./_figures/allplots.png", width = 26, height = 18, units = "cm", dpi=300) 

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_CEC,
                                 model_CorProtAr,
                                 model_rainfall,
                                 model_soil_pH,
                                 model_distance2river)

# Summarize the SEM results
summary(psem_model)



# a Significant (P<0.05) global goodness of fit means that your model does not fit well, 
# indicating potential problems like missing paths, mis-specfied relations, 
# or unaccounted-for correlations

# update the model based on your results
# significant tests of directed separation could mean a missing direct effect between the variables

# Best Practices:
# - Hypothesize Carefully:
#   Construct the initial model based on theoretical or empirical understanding.
# - Evaluate d-Separation Results:
#   Add or adjust paths based on significant independence test results.
# - Refit and Validate:
#   Rerun the model after adjustments and recheck the Fisher’s C statistic and independence claims.
# - Avoid Overfitting:
#   Add paths only when justified by theory or strong evidence, not purely to improve fit.
# Common pitfall: 
# - ignofing significant d-separation tests and failing to modify the model
# - adding too many variables and pathways, leading to overfitting and loss of parsimony

install.packages("statmod")
install.packages("tweedie")
library(statmod)
library(tweedie)

# Model 2: woody predicted by burnfreq and rainfall
model_woody2 <- glm(woodybiom ~ CEC +rainfall+soil_pH ,
                    family=gaussian(link = "log"),
                  data = pointdata)
summary(model_woody2)

model_woody2<- lm(woodybiom ~ CEC +rainfall+soil_pH, 
                 data = pointdata)



plot1<-ggplot(data=pointdata,aes(x=rainfall,y=woodybiom))+
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=inverse.gaussian(link="log")),
              formula= y~x,
              se=T)
plot1

plot2<-ggplot(data=pointdata,aes(x=CEC,y=woodybiom))+
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=inverse.gaussian(link="log")),
              formula= y~x,
              se=T)
plot2

plot3<-ggplot(data=pointdata,aes(x=soil_pH,y=woodybiom))+
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=inverse.gaussian(link="log")),
              formula= y~x,
              se=T)
plot3

# Model_burnfreq: burning frequency predicted by Core Protected Areas, Distance 2 River and Rainfall
model_burnfreq_init <- glm(burnfreq ~ CorProtAr + rainfall + distance2river, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
# install.packages("MASS")
library(MASS)
model_burnfreq2 <- MASS::glm.nb(burnfreq ~ CorProtAr + rainfall 
                               + distance2river+elevation, 
                               data = pointdata)
summary(model_burnfreq2)

plot4<-ggplot(data=pointdata,aes(y=burnfreq,x=CorProtAr))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),  # close to glm.nb
              formula= y~x,
              se=T)
plot4

plot5<-ggplot(data=pointdata,aes(y=burnfreq,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
plot5

plot6<-ggplot(data=pointdata,aes(y=burnfreq,x=distance2river))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
plot6

plot7<-ggplot(data=pointdata,aes(y=burnfreq,x=elevation))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=quasipoisson),
              formula= y~x,
              se=T)
plot7

# model_cec: predicted by elevation, burning frequency and soil pH

model_CEC2 <- lm(CEC ~ elevation + burnfreq, 
                data = pointdata)
summary(model_CEC2)

plot8<-ggplot(data=pointdata,aes(y=CEC,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
plot8

plot9<-ggplot(data=pointdata,aes(y=CEC,x=burnfreq))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
plot9


# model_CorProtAra:  predicted by elevation
model_CorProtAr2 <-glm(CorProtAr~elevation,
                      family=binomial,
                      data=pointdata)
summary(model_CorProtAr2)

plot10<-ggplot(data=pointdata,aes(y=CorProtAr,x=elevation))+
  geom_jitter(height = 0.02) +
  geom_smooth(method="glm",
              method.args=list(family=binomial),
              formula= y~x,
              se=T)
plot10

# model_rainfall: rainfall predicted by elevation
model_rainfall2 <- lm(rainfall ~ elevation, 
                     data = pointdata)
summary(model_rainfall2)

plot11<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
plot11

# model soil pH
model_soil_pH2 <- lm(soil_pH ~ rainfall+burnfreq+elevation,
                     data = pointdata)

plot12<-ggplot(data=pointdata,aes(y=soil_pH,x=rainfall))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=gaussian),  # close to glm.nb
              formula= y~x,
              se=T)
plot12

plot13<-ggplot(data=pointdata,aes(y=soil_pH,x=burnfreq))+
  geom_jitter(width = 0.05, height = 0.1) +
  geom_smooth(method="glm",
              method.args=list(family=gaussian),
              formula= y~x,
              se=T)
plot13

plot14<-ggplot(data=pointdata,aes(y=soil_pH,x=elevation))+
  geom_point() +
  geom_smooth(method="glm",
              method.args=list(family=gaussian),
              formula= y~x,
              se=T)
plot14

# model distance 2 river
model_distance2river2<-lm(distance2river~hills+rainfall,
                         data=pointdata)

plot15<-ggplot(data=pointdata,aes(y=distance2river,x=hills))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
plot15

plot16<-ggplot(data=pointdata,aes(y=distance2river,x=rainfall))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
plot16

# combine the figures
library(patchwork)
allplots2<-plot1+plot2+plot3+plot4+plot5+plot6+plot7+plot8+plot9+plot10+plot11+plot12+plot13+plot14+plot15+plot16 +
  patchwork::plot_layout(ncol=4) +
  patchwork::plot_annotation(title="Relations in model 2")
allplots2

ggsave("./_figures/allplots2.png", width = 26, height = 18, units = "cm", dpi=300) 

####### Combine all models into a single piecewise SEM
psem_model2 <- piecewiseSEM::psem(model_woody2,
                                 model_burnfreq2,
                                 model_CEC2,
                                 model_CorProtAr2,
                                 model_rainfall2,
                                 model_soil_pH2,
                                 model_distance2river2)

# Summarize the SEM results
summary(psem_model2)


# compare the two psem models
anova(psem_model, psem_model2)
#############################################################################

# Model 3: woody predicted by burnfreq and rainfall
model_woody3 <- glm(woodybiom ~ CEC +rainfall+soil_pH+elevation+burnfreq ,
                    family=inverse.gaussian(link="log"),
                    data = pointdata)
summary(model_woody3)

# Model_burnfreq: burning frequency predicted by Core Protected Areas, Distance 2 River and Rainfall
model_burnfreq_init <- glm(burnfreq ~ CorProtAr + rainfall + distance2river, 
                           family=poisson, 
                           data = pointdata)
# Calculate dispersion statistic
dispersion_stat <- summary(model_burnfreq_init)$deviance / summary(model_burnfreq_init)$df.residual
dispersion_stat
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
# install.packages("MASS")
library(MASS)
model_burnfreq3 <- MASS::glm.nb(burnfreq ~ CorProtAr + rainfall 
                                + distance2river +elevation, 
                                data = pointdata)
summary(model_burnfreq3)


# model_cec: predicted by elevation, burning frequency and soil pH

model_CEC3 <- lm(CEC ~ elevation + burnfreq + soil_pH, 
                 data = pointdata)
summary(model_CEC3)


# model_CorProtAra:  predicted by elevation
model_CorProtAr3 <-glm(CorProtAr~elevation+hills,
                       family=binomial,
                       data=pointdata)
summary(model_CorProtAr3)

# model_rainfall: rainfall predicted by elevation
model_rainfall3 <- lm(rainfall ~ elevation+hills, 
                      data = pointdata)
summary(model_rainfall3)

# model soil pH
model_soil_pH3 <- glm(soil_pH ~ rainfall+burnfreq+elevation,
                      family=Gamma(link = "inverse"), 
                      data = pointdata)

# Calculate dispersion statgaussian()# Calculate dispersion statistic
dispersion_stat2 <- summary(model_soil_pH)$deviance / summary(model_soil_pH)$df.residual
dispersion_stat2
# If 𝜙≈1 : No evidence of overdispersion → Poisson is appropriate. (mean≈variance)
# If 𝜙>1 : Overdispersion is present → Consider quasi-Poisson or negative binomial.
# If 𝜙<1 : Underdispersion (less common) → Investigate the data further.
# install.packages("MASS")
library(MASS)
#model_soil_pH_mass2 <- MASS::glm.nb(soil_pH ~ rainfall + burnfreq, 
#    data = pointdata)
# summary(model_soil_pH_mass)

# model distance 2 river
model_distance2river3<-lm(distance2river~hills+rainfall+elevation,
                          data=pointdata)

####### Combine all models into a single piecewise SEM
psem_model3 <- piecewiseSEM::psem(model_woody3,
                                  model_burnfreq3,
                                  model_CEC3,
                                  model_CorProtAr3,
                                  model_rainfall3,
                                  model_soil_pH3,
                                  model_distance2river3)

# Summarize the SEM results
summary(psem_model3)

# compare with previous models
anova(psem_model, psem_model3)
anova(psem_model2, psem_model3)

################################################################################




