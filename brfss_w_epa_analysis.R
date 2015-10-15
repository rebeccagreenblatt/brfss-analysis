library(dplyr)
library(car)

epa <- read.csv("epa_complete.csv")
brfss <- read.csv("brfss_complete.csv")

##turning brfss CntyFIPS column into integer and removing commas 
brfss$CntyFIPS <- as.integer(gsub(",","", as.character(brfss$CntyFIPS)))

brfss.w.epa <- inner_join(epa, brfss, by = c("id", "CntyFIPS"))

#ASTHMA2 --> asthma
brfss.w.epa$asthma <- recode(brfss.w.epa$ASTHMA2, recodes = "1=1; 2=0; else=NA")

str(brfss.w.epa)

#SMOKE100 --> smoke100
table(brfss.w.epa$SMOKE100)
#1=yes, 2=no, 7=Don't know/Not sure, 9=Refused
brfss.w.epa$smoke100 <- recode(brfss.w.epa$SMOKE100, recodes = "1=1; 2=0; else = NA")
chisq.test(table(brfss.w.epa$asthma, brfss.w.epa$smoke100)) #p-value < 2.2e-16

#AGE --> age
hist(brfss.w.epa$AGE)
summary(brfss.w.epa$AGE)
#filter dataset to only include >=18
brfss.w.epa$age <- ifelse(brfss.w.epa$AGE >= 18, brfss.w.epa$AGE, NA)
summary(brfss.w.epa$age)
boxplot(age ~ asthma, data = brfss.w.epa)
summary((glm(asthma~age, data=brfss.w.epa)))

#EDUCA --> educa
table(brfss.w.epa$EDUCA)
#1=never attended school or only kindgergarten, 
#2=grades 1-8 (elementary), 3=grades 9-11 (some high school), 
#4=grade 12 or GED (high school graduate), 
#5=College 1 year to 3 years (some college or technical school)
#ASSUMPTION: 6 is completed college. Was NOT in data dictionary
#turning into binary variable indicating whether or not HS was completed 
brfss.w.epa$educa <- recode(brfss.w.epa$EDUCA, recodes = "c(1,2,3) = 0; c(4,5,6) = 1; else = NA")
table(brfss.w.epa$educa)
chisq.test(table(brfss.w.epa$asthma, brfss.w.epa$educa)) #p-value < 2.2e-16

#INCOME2 --> inc.lt35k
table(brfss.w.epa$INCOME2)
#1=less than $10,000; 2=less than $15,000; 3=less than $20,000
#4=less than $25,000; 5=less than $35,000; 6=less than $50,000; 
#7=less than $75,000; 8=$75,000 or more; 77=Don't know/Not sure, 99=Refused 
by_inc <- brfss.w.epa %>%
  group_by(INCOME2) %>%
  summarise(mean(ASTHMA, na.rm = TRUE), count = n())
brfss.w.epa$income2 <- recode(brfss.w.epa$INCOME2, recodes = "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; else = NA")
boxplot(income2 ~ asthma, data = brfss.w.epa)
#recoding variable to indicate 1 if income < 35K 
brfss.w.epa$inc.lt35k <- recode(brfss.w.epa$INCOME2, recodes  = "c(1,2,3,4,5) = 1; c(6,7,8) = 0; else = NA")
chisq.test(table(brfss.w.epa$asthma, brfss.w.epa$inc.lt35k)) #p-value < 2.2e-16

#SEX --> sex
#1 = male, 2 = female
brfss.w.epa$sex <- factor(brfss.w.epa$SEX, levels = c(1,2), labels= c("male", "female"))

#RACE2 --> race
#1=White, 2=Black or African American, 3=Asian, 
#4=Native Hawaiian or Other Pacific Islander, 5=American Indian, Alaska Native, 
#6=Other, 7=Don't know/Not sure, 9=Not asked/Missing
brfss.w.epa$race <- recode(brfss.w.epa$RACE2, recodes = "1=1; 2=2; 3=3; 4=4; 5=5; else = NA")
brfss.w.epa$race <- factor(brfss.w.epa$race, levels = c(1,2,3,4,5), 
                             labels = c("White", "Black or African American", "Asian",
                                        "Native Hawaiian or Other Pacific Islander",
                                        "American Indian, Alaska Native"))
table(brfss.w.epa$race, useNA = "ifany")
chisq.test(table(brfss.w.epa$asthma, brfss.w.epa$race)) #p-value < 2.2e-16

##ADD##
#HISPANC2 --> hispanc

#BMICAT --> bmicat
table(brfss.w.epa$BMICAT)
#1=neither overweight nor obese (BMI <2500), 2=overweight (BMI 2500-LT3000), 
#3=obese (BMI GE300),  9=DK/Refused/Missing
brfss.w.epa$bmicat <- recode(brfss.w.epa$BMICAT, recodes = "1=1; 2=2; 3=3; else = NA")
brfss.w.epa$bmicat <- ordered(brfss.w.epa$bmicat, levels = c(1,2,3), labels = c("neither overweight nor obese",
                                                                                    "overweight", "obese"))
boxplot(bmicat ~ asthma, data = brfss.w.epa)

summary(brfss.w.epa$ozone)
summary(brfss.w.epa$PM2.5.local)

pm2.5.asthma.model <- glm(asthma ~ PM2.5.local + smoke100 + age + educa + inc.lt35k + sex + race, family = "binomial", data = brfss.w.epa)
# Call:
#   glm(formula = asthma ~ PM2.5.local + smoke100 + age + educa + 
#         inc.lt35k + sex + race, family = "binomial", data = brfss.w.epa)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.9393  -0.5662  -0.5083  -0.4426   2.4653  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                                   -1.3487720  0.0184792 -72.989  < 2e-16
# PM2.5.local                                   -0.0211425  0.0011055 -19.124  < 2e-16
# smoke100                                       0.1849604  0.0057028  32.433  < 2e-16
# age                                           -0.0110030  0.0001728 -63.683  < 2e-16
# educa                                         -0.2203527  0.0112417 -19.601  < 2e-16
# inc.lt35k                                      0.2760281  0.0060765  45.426  < 2e-16
# sexfemale                                      0.3848071  0.0059970  64.167  < 2e-16
# raceBlack or African American                  0.0593328  0.0088199   6.727 1.73e-11
# raceAsian                                     -0.3154455  0.0180078 -17.517  < 2e-16
# raceNative Hawaiian or Other Pacific Islander  0.0588282  0.0460602   1.277    0.202
# raceAmerican Indian, Alaska Native             0.2322666  0.0257551   9.018  < 2e-16
# 
# (Intercept)                                   ***
#   PM2.5.local                                   ***
#   smoke100                                      ***
#   age                                           ***
#   educa                                         ***
#   inc.lt35k                                     ***
#   sexfemale                                     ***
#   raceBlack or African American                 ***
#   raceAsian                                     ***
#   raceNative Hawaiian or Other Pacific Islander    
# raceAmerican Indian, Alaska Native            ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 879493  on 1121312  degrees of freedom
# Residual deviance: 867007  on 1121302  degrees of freedom
# (512577 observations deleted due to missingness)
# AIC: 867029
# 
# Number of Fisher Scoring iterations: 4

ozone.asthma.model <- glm(asthma ~ ozone + smoke100 + age + educa + inc.lt35k + sex + race, family = "binomial", data = brfss.w.epa)
# Call:
#   glm(formula = asthma ~ ozone + smoke100 + age + educa + inc.lt35k + 
#         sex + race, family = "binomial", data = brfss.w.epa)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.9404  -0.5666  -0.5090  -0.4427   2.4875  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                                   -1.3184735  0.0263694 -50.000   <2e-16
# ozone                                         -4.4358389  0.4537295  -9.776   <2e-16
# smoke100                                       0.1805458  0.0057672  31.306   <2e-16
# age                                           -0.0110229  0.0001748 -63.076   <2e-16
# educa                                         -0.2284879  0.0113205 -20.184   <2e-16
# inc.lt35k                                      0.2754859  0.0061574  44.741   <2e-16
# sexfemale                                      0.3881579  0.0060747  63.897   <2e-16
# raceBlack or African American                  0.0208454  0.0087278   2.388   0.0169
# raceAsian                                     -0.3244899  0.0190115 -17.068   <2e-16
# raceNative Hawaiian or Other Pacific Islander  0.0282891  0.0493391   0.573   0.5664
# raceAmerican Indian, Alaska Native             0.2636178  0.0261466  10.082   <2e-16
# 
# (Intercept)                                   ***
#   ozone                                         ***
#   smoke100                                      ***
#   age                                           ***
#   educa                                         ***
#   inc.lt35k                                     ***
#   sexfemale                                     ***
#   raceBlack or African American                 *  
#   raceAsian                                     ***
#   raceNative Hawaiian or Other Pacific Islander    
# raceAmerican Indian, Alaska Native            ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 859355  on 1094438  degrees of freedom
# Residual deviance: 847164  on 1094428  degrees of freedom
# (539451 observations deleted due to missingness)
# AIC: 847186
# 
# Number of Fisher Scoring iterations: 4


##next, use 'do you have asthma now' as the response variable?


