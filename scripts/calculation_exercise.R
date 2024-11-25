##### Calculating exercise APCE 
renv::restore()

avg_traits_indi <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vTbQSYFND4l5105-jZmsEAnX2n1xHTOA80D2rRnctLmqLx5n3DwZhLai2V6uZmoHpUIC8soTQsnNI5i/pub?gid=0&single=true&output=csv") |> dplyr::filter(Sex=="1") |>
  dplyr::select(c("WingLength", "Individual", "Year"))

#########################################################################
## Estimate repeatability which is the proportion of the total variance
m1 <- lmer(WingLength ~ (1|Individual), data = avg_traits_indi)
summary(m1)

# repeatability then is 142.07 / (142.07+76.28) = 0.651

## Estimate adjusted repeatability, so when accounting for fixed effects 
m2 <- lmer(WingLength ~ (1|Individual) + (1|Year), data=avg_traits_indi)
summary(m2) 

# adjusted repeatability then is 132.01 / (27.45 + 132.01 + 67.90) = 0.610

pedigree <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRU5kaGgJrtnEk150-Z_XL_zwO6FkfgNSwG7uItcN9Zib6AzvIsgApRz8TYhUta6lL8PnRuFHpoU5Bu/pub?gid=968118397&single=true&output=csv")
pedigree <- as.data.frame(pedigree)

pedigree_Child <- dplyr::select(pedigree, c("ChildID", "BirthYear", "MumID"))

ped_wing <- left_join(pedigree_Child, avg_traits_indi, by=c("ChildID"= "Individual"))|>
  na.omit()

pedigree_Mum <- dplyr::select(pedigree, c("MumID"))
ped_mum_wing <- left_join(pedigree_Mum, avg_traits_indi, by=c("MumID"= "Individual")) |>
  na.omit()

Child_Mum_wing <- left_join(ped_wing, ped_mum_wing, by= "MumID") |>
  na.omit()

m3 <- lmer(WingLength.x ~ WingLength.y, data = Child_Mum_wing)
summary(m3)$coefficients

p1 <- ggplot(Child_Mum_wing, aes(x=WingLength.y, y=WingLength.x)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x="Mum wing length", y="Child wing length")
p1

# The slope of the regression line is 0.33, which means that for every unit 
# but this is only mum so heritability is 0.33 *2 = 0.66