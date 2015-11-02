library(survey)
library(car)
library(dplyr)

BRFSS.complete.allvars <- read.csv("brfss.complete.wcntywts.csv")
BRFSS.complete.allvars$CntyFIPS <- as.integer(gsub(",","", as.character(BRFSS.complete.allvars$CntyFIPS)))
BRFSS.complete.allvars$STSTR <- BRFSS.complete.allvars$CntyFIPS #think this is what STSTR was in Omar's file... 
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHMA2"] <- "ASTHMA"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="INCOME2"] <- "INCOME"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="RACE2"] <- "RACE"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="BMICAT"] <- "BMI"

#FOR SECOND TIME AROUND, NOW USING ASTHNOW INSTEAD OF ASTHMA2
# names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHMA"] <- "ASTHMA2"
# names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHNOW"] <- "ASTHMA"

##I might be wrongly assigning CNTYFIPS to STSTR variable... don't know where STSTR came from

BRFSS.complete <- select(BRFSS.complete.allvars, id, ASTHMA, AGE, EDUCA, INCOME, SEX, SMOKER, BMI, CNTYWT, RACE, CntyFIPS)

#____________________
#For this part, depends if we want to do the analysis with ASTHMA2 variable or ASTHMNOW 

#--FOR ASTHMA2 -- 
BRFSS.complete$ASTHMA<-recode(BRFSS.complete$ASTHMA, recodes="1='1'; 2='0';c(7,9)=NA", as.factor.result=T)
BRFSS.complete$ASTHMA<-relevel(BRFSS.complete$ASTHMA, ref='0')

#--FOR ASTHNOW -- skipping for now
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

BRFSS.complete <- BRFSS.complete[ which(BRFSS.complete$ASTHMA !='NA' ), ]
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

##ASTHMA2 + pm2.5 - local
des.pm2.5 <- svydesign(ids=~1, strata=~CntyFIPS, weights=~as.numeric(CNTYWT),  data=brfss.pm2.5)
model.pm2.5 <- svyglm(ASTHMA ~ AGE+EDUCA+INCOME+RACE+SEX+SMOKER+BMI+median+sd, des.pm2.5, family=quasibinomial)

summary(model.pm2.5)
exp(coefficients(model.pm2.5))

# > summary(model.pm2.5)
# 
# Call:
#   svyglm(formula = ASTHMA ~ AGE + EDUCA + INCOME + RACE + SEX + 
#            SMOKER + BMI + median + sd, des.pm2.5, family = quasibinomial)
# 
# Survey design:
#   svydesign(ids = ~1, strata = ~CntyFIPS, weights = ~as.numeric(CNTYWT), 
#             data = brfss.pm2.5)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -1.6482244  0.0321206 -51.314  < 2e-16 ***
#   AGE                        -0.0123281  0.0003681 -33.492  < 2e-16 ***
#   EDUCAnoHighSchool          -0.3746998  0.0420762  -8.905  < 2e-16 ***
#   EDUCASome/highSchool       -0.1083529  0.0136591  -7.933 2.15e-15 ***
#   INCOMElow                   0.2705789  0.0163953  16.503  < 2e-16 ***
#   INCOMEmiddle                0.0401427  0.0132775   3.023   0.0025 ** 
#   RACEasian/pacific islander -0.3142923  0.0361977  -8.683  < 2e-16 ***
#   RACEblack                   0.0070459  0.0171152   0.412   0.6806    
# RACEhispanic               -0.3379646  0.0218622 -15.459  < 2e-16 ***
#   RACEnative                  0.3513460  0.0570242   6.161 7.21e-10 ***
#   SEXfemale                   0.3498251  0.0117234  29.840  < 2e-16 ***
#   SMOKERcurrent smoker        0.1919219  0.0151794  12.644  < 2e-16 ***
#   SMOKERformer smoker         0.1881559  0.0126928  14.824  < 2e-16 ***
#   BMIobese                    0.5074072  0.0139528  36.366  < 2e-16 ***
#   BMIoverweight               0.1386809  0.0138424  10.019  < 2e-16 ***
#   median                     -0.0258239  0.0023786 -10.857  < 2e-16 ***
#   sd                          0.0157394  0.0022950   6.858 6.98e-12 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for quasibinomial family taken to be 1.001524)
# 
# Number of Fisher Scoring iterations: 5
# 
# > exp(coefficients(model.pm2.5))
# (Intercept)                        AGE          EDUCAnoHighSchool 
# 0.1923912                  0.9877476                  0.6874956 
# EDUCASome/highSchool                  INCOMElow               INCOMEmiddle 
# 0.8973109                  1.3107230                  1.0409593 
# RACEasian/pacific islander                  RACEblack               RACEhispanic 
# 0.7303055                  1.0070708                  0.7132206 
# RACEnative                  SEXfemale       SMOKERcurrent smoker 
# 1.4209789                  1.4188194                  1.2115759 
# SMOKERformer smoker                   BMIobese              BMIoverweight 
# 1.2070217                  1.6609791                  1.1487575 
# median                         sd 
# 0.9745067                  1.0158639 

##ASTHMA2 + OZONE
options(survey.lonely.psu = "adjust")
des.ozone <- svydesign(ids=~1, strata=~CntyFIPS, weights=~as.numeric(CNTYWT),  data=brfss.ozone)

model.ozone <- svyglm(ASTHMA ~ AGE+EDUCA+INCOME+RACE+SEX+SMOKER+BMI+F95th, des.ozone, family=quasibinomial)

summary(model.ozone)
exp(coefficients(model.ozone))
##all environmental variables came out negative


