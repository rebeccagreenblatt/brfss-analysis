epa <- read.csv("epa_multiple_vars.csv")

dim(epa)
head(epa)
library(dplyr)
library(car)

ozone_dat <- filter(epa, parameter_name == "Ozone")
dim(ozone_dat)

pm2.5_dat <- filter(epa, parameter_name == "PM2.5_local")
dim(pm2.5_dat)

head(ozone_dat)

brfss <- read.csv("brfss_complete_whispanc.csv")
dim(brfss)

brfss$CntyFIPS <- as.integer(gsub(",","", as.character(brfss$CntyFIPS)))

dat <- inner_join(ozone_dat, brfss, by = c("id", "CntyFIPS"))
#dat <- inner_join(pm2.5_dat, brfss, by = c("id", "CntyFIPS"))

dat$asthma <- recode(dat$ASTHMA2, recodes = "1=1; 2=0; else=NA")
dat$asthma_now <- recode(dat$ASTHNOW, recodes = "1=1; 2=0; else = NA") #where 1 will equal yes, still have it, 2 = not anymore

str(dat)

#SMOKE100 --> smoke100
table(dat$SMOKE100)
#1=yes, 2=no, 7=Don't know/Not sure, 9=Refused
dat$smoke100 <- recode(dat$SMOKE100, recodes = "1=1; 2=0; else = NA")
chisq.test(table(dat$asthma, dat$smoke100)) #p-value < 2.2e-16

#AGE --> age
hist(dat$AGE)
summary(dat$AGE)
#filter dataset to only include >=18
dat$age <- ifelse(dat$AGE >= 18, dat$AGE, NA)
summary(dat$age)
boxplot(age ~ asthma, data = dat)
summary((glm(asthma~age, data=dat)))


#EDUCA --> educa
table(dat$EDUCA)
#1=never attended school or only kindgergarten, 
#2=grades 1-8 (elementary), 3=grades 9-11 (some high school), 
#4=grade 12 or GED (high school graduate), 
#5=College 1 year to 3 years (some college or technical school)
#ASSUMPTION: 6 is completed college. Was NOT in data dictionary
#turning into binary variable indicating whether or not HS was completed 
dat$educa <- recode(dat$EDUCA, recodes = "c(1,2,3) = 0; c(4,5,6) = 1; else = NA")
table(dat$educa)
chisq.test(table(dat$asthma, dat$educa)) #p-value < 2.2e-16

#INCOME2 --> inc.lt35k
table(dat$INCOME2)
#1=less than $10,000; 2=less than $15,000; 3=less than $20,000
#4=less than $25,000; 5=less than $35,000; 6=less than $50,000; 
#7=less than $75,000; 8=$75,000 or more; 77=Don't know/Not sure, 99=Refused 
by_inc <- dat %>%
  group_by(INCOME2) %>%
  summarise(mean(ASTHMA, na.rm = TRUE), count = n())
dat$income2 <- recode(dat$INCOME2, recodes = "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; else = NA")
dat$incomecat <- ordered(dat$income2, levels = c(1,2,3,4,5,6,7,8))
boxplot(income2 ~ asthma, data = dat)
#recoding variable to indicate 1 if income < 35K 
dat$inc.lt35k <- recode(dat$INCOME2, recodes  = "c(1,2,3,4,5) = 1; c(6,7,8) = 0; else = NA")
chisq.test(table(dat$asthma, brfss.w.epa$inc.lt35k)) #p-value < 2.2e-16
cor(dat$income2, dat$asthma, use = "complete.obs")
cor(dat$inc.lt35k, dat$asthma, use = "complete.obs")

#SEX --> sex
#1 = male, 2 = female
dat$sex <- factor(dat$SEX, levels = c(1,2), labels= c("male", "female"))

#RACE2 --> race
#1=White, 2=Black or African American, 3=Asian, 
#4=Native Hawaiian or Other Pacific Islander, 5=American Indian, Alaska Native, 
#6=Other, 7=Don't know/Not sure, 9=Not asked/Missing
dat$race <- recode(dat$RACE2, recodes = "1=1; 2=2; 3=3; 4=4; 5=5; else = NA")
dat$race <- factor(dat$race, levels = c(1,2,3,4,5), 
                           labels = c("White", "Black or African American", "Asian",
                                      "Native Hawaiian or Other Pacific Islander",
                                      "American Indian, Alaska Native"))
table(dat$race, useNA = "ifany")
chisq.test(table(dat$asthma, brfss.w.ozone$race)) #p-value < 2.2e-16

##ADD##
#HISPANC2 --> hispanc
table(dat$HISPANC2)
#1=yes, 2=no, 7=Don't know/Not sure, 9=Refused
dat$hispanc <- recode(dat$HISPANC2, recodes = "1=1; 2=0; else = NA")

#BMICAT --> bmicat
table(dat$BMICAT)
#1=neither overweight nor obese (BMI <2500), 2=overweight (BMI 2500-LT3000), 
#3=obese (BMI GE300),  9=DK/Refused/Missing
dat$bmicat <- recode(dat$BMICAT, recodes = "1=1; 2=2; 3=3; else = NA")
dat$bmicat <- ordered(dat$bmicat, levels = c(1,2,3), labels = c("neither overweight nor obese",
                                                                                "overweight", "obese"))
boxplot(bmicat ~ asthma, data = dat)

str(dat)
cor(dat$median_measurement, dat$asthma, use = "complete.obs")

model_F95th <- glm(asthma ~ F95th, data = dat) 
summary(model_F95th)
boxplot(F95th ~ asthma, data = dat)
boxplot(F3rd_max ~ asthma, data = dat)
boxplot(sd ~ asthma, data = dat)
cor(dat$median_measurement, dat$asthma, use = "complete.obs")

model <- glm(asthma ~ smoke100 + age + educa + inc.lt35k + 
               sex + race + hispanc + bmicat + F95th, data = dat) 
summary(model)
odds_ratios <- exp(coef(model))

##PM2.5_local
# Call:
#   glm(formula = asthma ~ smoke100 + age + educa + inc.lt35k + sex + 
#         race + hispanc + bmicat + F95th, data = dat)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.3278  -0.1510  -0.1190  -0.0823   1.0093  
# 
# Coefficients:
#   Estimate Std. Error
# (Intercept)                                    1.891e-01  1.847e-03
# smoke100                                       2.076e-02  6.200e-04
# age                                           -1.312e-03  1.883e-05
# educa                                         -2.836e-02  1.359e-03
# inc.lt35k                                      3.146e-02  6.790e-04
# sexfemale                                      4.528e-02  6.335e-04
# raceBlack or African American                 -4.703e-03  9.807e-04
# raceAsian                                     -1.996e-02  1.724e-03
# raceNative Hawaiian or Other Pacific Islander  7.050e-03  5.251e-03
# raceAmerican Indian, Alaska Native             2.753e-02  3.130e-03
# hispanc                                       -2.065e-02  3.278e-03
# bmicat.L                                       4.796e-02  5.576e-04
# bmicat.Q                                       1.326e-02  5.265e-04
# F95th                                         -4.671e-06  8.405e-07
# t value Pr(>|t|)    
# (Intercept)                                   102.371  < 2e-16 ***
#   smoke100                                       33.487  < 2e-16 ***
#   age                                           -69.666  < 2e-16 ***
#   educa                                         -20.876  < 2e-16 ***
#   inc.lt35k                                      46.343  < 2e-16 ***
#   sexfemale                                      71.480  < 2e-16 ***
#   raceBlack or African American                  -4.796 1.62e-06 ***
#   raceAsian                                     -11.580  < 2e-16 ***
#   raceNative Hawaiian or Other Pacific Islander   1.343    0.179    
# raceAmerican Indian, Alaska Native              8.795  < 2e-16 ***
#   hispanc                                        -6.300 2.97e-10 ***
#   bmicat.L                                       86.021  < 2e-16 ***
#   bmicat.Q                                       25.181  < 2e-16 ***
#   F95th                                          -5.557 2.74e-08 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.1125628)
# 
# Null deviance: 140058  on 1222607  degrees of freedom
# Residual deviance: 137619  on 1222594  degrees of freedom
# (459043 observations deleted due to missingness)
# AIC: 799153
# 
# Number of Fisher Scoring iterations: 2
# > odds_ratios
# (Intercept) 
# 1.2081037 
# smoke100 
# 1.0209785 
# age 
# 0.9986892 
# educa 
# 0.9720378 
# inc.lt35k 
# 1.0319652 
# sexfemale 
# 1.0463244 
# raceBlack or African American 
# 0.9953078 
# raceAsian 
# 0.9802361 
# raceNative Hawaiian or Other Pacific Islander 
# 1.0070752 
# raceAmerican Indian, Alaska Native 
# 1.0279125 
# hispanc 
# 0.9795579 
# bmicat.L 
# 1.0491325 
# bmicat.Q 
# 1.0133454 
# F95th 
# 0.9999953 
 ----------------------------------------------------------
##Ozone
# Call:
#   glm(formula = asthma ~ smoke100 + age + educa + inc.lt35k + sex + 
#         race + hispanc + bmicat + F95th, data = dat)
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -0.33471  -0.15125  -0.11916  -0.08199   1.01267  
# 
# Coefficients:
#   Estimate Std. Error
# (Intercept)                                    1.915e-01  1.832e-03
# smoke100                                       2.062e-02  6.280e-04
# age                                           -1.337e-03  1.912e-05
# educa                                         -2.996e-02  1.374e-03
# inc.lt35k                                      3.161e-02  6.893e-04
# sexfemale                                      4.578e-02  6.421e-04
# raceBlack or African American                 -5.065e-03  9.939e-04
# raceAsian                                     -1.938e-02  1.808e-03
# raceNative Hawaiian or Other Pacific Islander  3.601e-03  5.600e-03
# raceAmerican Indian, Alaska Native             3.080e-02  3.226e-03
# hispanc                                       -2.185e-02  3.214e-03
# bmicat.L                                       4.793e-02  5.649e-04
# bmicat.Q                                       1.314e-02  5.332e-04
# F95th                                         -1.027e-05  1.195e-06
# t value Pr(>|t|)    
# (Intercept)                                   104.498  < 2e-16 ***
#   smoke100                                       32.839  < 2e-16 ***
#   age                                           -69.949  < 2e-16 ***
#   educa                                         -21.800  < 2e-16 ***
#   inc.lt35k                                      45.853  < 2e-16 ***
#   sexfemale                                      71.285  < 2e-16 ***
#   raceBlack or African American                  -5.096 3.46e-07 ***
#   raceAsian                                     -10.721  < 2e-16 ***
#   raceNative Hawaiian or Other Pacific Islander   0.643     0.52    
# raceAmerican Indian, Alaska Native              9.546  < 2e-16 ***
#   hispanc                                        -6.799 1.05e-11 ***
#   bmicat.L                                       84.855  < 2e-16 ***
#   bmicat.Q                                       24.639  < 2e-16 ***
#   F95th                                          -8.593  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.1126441)
# 
# Null deviance: 136706  on 1192178  degrees of freedom
# Residual deviance: 134290  on 1192165  degrees of freedom
# (446628 observations deleted due to missingness)
# AIC: 780125
# 
# Number of Fisher Scoring iterations: 2
# > odds_ratios
# (Intercept) 
# 1.2110310 
# smoke100 
# 1.0208367 
# age 
# 0.9986637 
# educa 
# 0.9704890 
# inc.lt35k 
# 1.0321098 
# sexfemale 
# 1.0468388 
# raceBlack or African American 
# 0.9949476 
# raceAsian 
# 0.9808021 
# raceNative Hawaiian or Other Pacific Islander 
# 1.0036077 
# raceAmerican Indian, Alaska Native 
# 1.0312795 
# hispanc 
# 0.9783826 
# bmicat.L 
# 1.0491008 
# bmicat.Q 
# 1.0132231 
# F95th 
# 0.9999897 

