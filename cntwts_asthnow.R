library(survey)
library(car)
library(dplyr)

BRFSS.complete.allvars <- read.csv("brfss.complete.wcntywts.csv")
BRFSS.complete.allvars$CntyFIPS <- as.integer(gsub(",","", as.character(BRFSS.complete.allvars$CntyFIPS)))
BRFSS.complete.allvars$STSTR <- BRFSS.complete.allvars$CntyFIPS #think this is what STSTR was in Omar's file... 
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="INCOME2"] <- "INCOME"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="RACE2"] <- "RACE"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="BMICAT"] <- "BMI"

##I might be wrongly assigning CNTYFIPS to STSTR variable... don't know where STSTR came from

BRFSS.complete <- select(BRFSS.complete.allvars, id, ASTHNOW, AGE, EDUCA, INCOME, SEX, SMOKER, BMI, CNTYWT, RACE, CntyFIPS)

#--FOR ASTHNOW -- 
##need to confirm that 0 also means N/A... responses 1 and 2 roughly add up to people who responded yes to ever having asthma, so just sticking with these
BRFSS.complete$ASTHNOW <- recode(BRFSS.complete$ASTHNOW, recodes = "1='1'; 2='0'; else = NA", as.factor.result=T)
BRFSS.complete$ASTHNOW<-relevel(BRFSS.complete$ASTHNOW, ref='0')
#------------------------

#Race/ethnicity
BRFSS.complete$RACE <- recode(BRFSS.complete$RACE, recodes="1='white'; 2='black'; 3:4='asian/pacific islander'; 5='native'; 8='hispanic'; c(6,7,9,0)=NA", as.factor.result=T)
BRFSS.complete$RACE <- relevel(BRFSS.complete$RACE, ref='white')

#Income <$25K, $25-75K, >$75k
BRFSS.complete$INCOME <- recode(BRFSS.complete$INCOME, recodes="1:4='low'; 5:7='middle'; 8='high'; c(0,77,99)=NA", as.factor.result=T)
BRFSS.complete$INCOME <- relevel(BRFSS.complete$INCOME, ref='high')

#Age
BRFSS.complete$AGE <- recode(BRFSS.complete$AGE, recodes="0:9=NA", as.numeric.result=T)

#Education
BRFSS.complete$EDUCA <- recode(BRFSS.complete$EDUCA, recodes="1:2='noHighSchool'; 3:4='Some/highSchool'; 5:6='higherEducation';c(0,9)=NA", as.factor.result=T)
BRFSS.complete$EDUCA <- relevel(BRFSS.complete$EDUCA, ref='higherEducation')

#Sex
BRFSS.complete$SEX <- recode(BRFSS.complete$SEX, recodes="1='male'; 2='female'", as.factor.result=T)
BRFSS.complete$SEX <- relevel(BRFSS.complete$SEX, ref='male')

#Smoking
BRFSS.complete$SMOKER <- recode(BRFSS.complete$SMOKER, recodes="1:2='current smoker'; 3='former smoker'; 4='never smoked';9=NA", as.factor.result=T)
BRFSS.complete$SMOKER <- relevel(BRFSS.complete$SMOKER, ref='never smoked')

#BMI
BRFSS.complete$BMI <- recode(BRFSS.complete$BMI, recodes="1='neither overweight nor obese'; 2='overweight'; 3='obese';c(0,9)=NA", as.factor.result=T)
BRFSS.complete$BMI <- relevel(BRFSS.complete$BMI, ref='neither overweight nor obese')

#BRFSS.complete <- BRFSS.complete[ -c(1) ] 

BRFSS.complete <- BRFSS.complete[ which(BRFSS.complete$ASTHNOW !='NA' ), ]
BRFSS.test <- BRFSS.complete[complete.cases(BRFSS.complete), ]

# BRFSS.complete.asthnow <- BRFSS.complete[ which(BRFSS.complete$ASTHNOW !='NA' ), ]
# BRFSS.test.asthnow <- BRFSS.complete.asthnow[complete.cases(BRFSS.complete.asthnow), ]

##joining with epa
epa <- read.csv("epa_final.csv")
ozone_dat <- filter(epa, Parameter_Name == "Ozone")
pm2.5_dat <- filter(epa, Parameter_Name == "PM2.5 - Local Conditions")

dat_pm2.5 <- inner_join(pm2.5_dat, BRFSS.test, by = c("id", "CntyFIPS"))
brfss.pm2.5 <- dat_pm2.5[complete.cases(dat_pm2.5),]

dat_ozone <- inner_join(ozone_dat, BRFSS.test, by = c("id", "CntyFIPS"))
brfss.ozone <- dat_ozone[complete.cases(dat_ozone),]

options(survey.lonely.psu = "adjust")

##ASTHNOW + pm2.5 - local
des.pm2.5 <- svydesign(ids=~1, strata=~CntyFIPS, weights=~as.numeric(CNTYWT),  data=brfss.pm2.5)
model.pm2.5 <- svyglm(ASTHNOW ~ AGE+EDUCA+INCOME+RACE+SEX+SMOKER+BMI+median+sd, des.pm2.5, family=quasibinomial)

summary(model.pm2.5)
exp(coefficients(model.pm2.5))
# 
# > summary(model.pm2.5)
# 
# Call:
#   svyglm(formula = ASTHNOW ~ AGE + EDUCA + INCOME + RACE + SEX + 
#            SMOKER + BMI + median + sd, des.pm2.5, family = quasibinomial)
# 
# Survey design:
#   svydesign(ids = ~1, strata = ~CntyFIPS, weights = ~as.numeric(CNTYWT), 
#             data = brfss.pm2.5)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.4405137  0.0669980  -6.575 4.88e-11 ***
#   AGE                         0.0106240  0.0007568  14.039  < 2e-16 ***
#   EDUCAnoHighSchool           0.1096698  0.0936850   1.171 0.241752    
# EDUCASome/highSchool        0.1236069  0.0291337   4.243 2.21e-05 ***
#   INCOMElow                   0.4156302  0.0345153  12.042  < 2e-16 ***
#   INCOMEmiddle                0.1211291  0.0274836   4.407 1.05e-05 ***
#   RACEasian/pacific islander -0.3753240  0.0708074  -5.301 1.16e-07 ***
#   RACEblack                  -0.0184329  0.0367048  -0.502 0.615534    
# RACEhispanic               -0.4249085  0.0418630 -10.150  < 2e-16 ***
#   RACEnative                  0.0221174  0.1264066   0.175 0.861103    
# SEXfemale                   0.5945792  0.0238388  24.942  < 2e-16 ***
#   SMOKERcurrent smoker        0.0235573  0.0323997   0.727 0.467176    
# SMOKERformer smoker        -0.0433790  0.0269873  -1.607 0.107972    
# BMIobese                    0.3211626  0.0292133  10.994  < 2e-16 ***
#   BMIoverweight               0.1012464  0.0285512   3.546 0.000391 ***
#   median                     -0.0167379  0.0053512  -3.128 0.001761 ** 
#   sd                          0.0216145  0.0072667   2.974 0.002936 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for quasibinomial family taken to be 1.000705)
# 
# Number of Fisher Scoring iterations: 4
# 
# > exp(coefficients(model.pm2.5))
# (Intercept)                        AGE          EDUCAnoHighSchool 
# 0.6437057                  1.0106807                  1.1159095 
# EDUCASome/highSchool                  INCOMElow               INCOMEmiddle 
# 1.1315710                  1.5153254                  1.1287706 
# RACEasian/pacific islander                  RACEblack               RACEhispanic 
# 0.6870667                  0.9817360                  0.6538296 
# RACEnative                  SEXfemale       SMOKERcurrent smoker 
# 1.0223638                  1.8122681                  1.0238369 
# SMOKERformer smoker                   BMIobese              BMIoverweight 
# 0.9575484                  1.3787298                  1.1065493 
# median                         sd 
# 0.9834014                  1.0218497 

##ASTHNOW + OZONE
options(survey.lonely.psu = "adjust")
des.ozone <- svydesign(ids=~1, strata=~CntyFIPS, weights=~as.numeric(CNTYWT),  data=brfss.ozone)
model.ozone <- svyglm(ASTHNOW ~ AGE+EDUCA+INCOME+RACE+SEX+SMOKER+BMI+F95th, des.ozone, family=quasibinomial)

summary(model.ozone)
exp(coefficients(model.ozone))
# > summary(model.ozone)
# 
# Call:
#   svyglm(formula = ASTHNOW ~ AGE + EDUCA + INCOME + RACE + SEX + 
#            SMOKER + BMI + F95th, des.ozone, family = quasibinomial)
# 
# Survey design:
#   svydesign(ids = ~1, strata = ~CntyFIPS, weights = ~as.numeric(CNTYWT), 
#             data = brfss.ozone)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.7545420  0.0941188  -8.017 1.09e-15 ***
#   AGE                         0.0108163  0.0007597  14.238  < 2e-16 ***
#   EDUCAnoHighSchool           0.0991454  0.0950921   1.043 0.297124    
# EDUCASome/highSchool        0.1283190  0.0294166   4.362 1.29e-05 ***
#   INCOMElow                   0.3997143  0.0348146  11.481  < 2e-16 ***
#   INCOMEmiddle                0.1031728  0.0277029   3.724 0.000196 ***
#   RACEasian/pacific islander -0.3591269  0.0720295  -4.986 6.18e-07 ***
#   RACEblack                  -0.0376827  0.0366266  -1.029 0.303559    
# RACEhispanic               -0.4182218  0.0422150  -9.907  < 2e-16 ***
#   RACEnative                  0.0641860  0.1321953   0.486 0.627294    
# SEXfemale                   0.5954936  0.0240630  24.747  < 2e-16 ***
#   SMOKERcurrent smoker        0.0214019  0.0328730   0.651 0.515016    
# SMOKERformer smoker        -0.0387686  0.0271497  -1.428 0.153306    
# BMIobese                    0.3006913  0.0294776  10.201  < 2e-16 ***
#   BMIoverweight               0.0978268  0.0288447   3.391 0.000695 ***
#   F95th                       4.0444382  1.1704683   3.455 0.000550 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for quasibinomial family taken to be 1.00016)
# 
# Number of Fisher Scoring iterations: 4
# 
# > summary(brfss.ozone$F95th)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.03100 0.06367 0.07017 0.06973 0.07600 0.10570 
# > exp(coefficients(model.ozone))
# (Intercept)                        AGE          EDUCAnoHighSchool 
# 0.4702259                  1.0108750                  1.1042268 
# EDUCASome/highSchool                  INCOMElow               INCOMEmiddle 
# 1.1369156                  1.4913985                  1.1086830 
# RACEasian/pacific islander                  RACEblack               RACEhispanic 
# 0.6982857                  0.9630185                  0.6582162 
# RACEnative                  SEXfemale       SMOKERcurrent smoker 
# 1.0662907                  1.8139261                  1.0216326 
# SMOKERformer smoker                   BMIobese              BMIoverweight 
# 0.9619733                  1.3507923                  1.1027718 
# F95th 
# 57.0791090 
