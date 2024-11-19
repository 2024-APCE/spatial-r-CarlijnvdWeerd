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
model_woody <- lm(woodybiom ~ CEC +rainfall+soil_pH , 
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
model_burnfreq <- MASS::glm.nb(burnfreq ~ CorProtAr + rainfall + distance2river, 
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
model_CorProtAr <-glm(CorProtAr~elevation,
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

# model_rainfall: rainfall predicted by elevation
model_rainfall <- lm(rainfall ~ elevation, 
                     data = pointdata)
summary(model_rainfall)

p8<-ggplot(data=pointdata,aes(y=rainfall,x=elevation))+
  geom_point() +
  geom_smooth(method="lm",
              formula= y~x,
              se=T)
p8

# combine the figures
library(patchwork)
allplots<-p1+p2+p3+p4+p5+p6+p7+
  patchwork::plot_layout(ncol=3) +
  patchwork::plot_annotation(title="Relations in model 1")
allplots

####### Combine all models into a single piecewise SEM
psem_model <- piecewiseSEM::psem(model_woody,
                                 model_burnfreq,
                                 model_cec,
                                 model_CorProtAr,
                                 model_rainfall)

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

