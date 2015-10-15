##have to go back and integrate HISPANC2 variable

dat <- read.csv("brfss_complete_whispanc.csv")
library(dplyr)
dat <- tbl_df(dat)
str(dat)
dim(dat)
#[1] 2018191      25
table(dat$BMICAT)
library(car)

dat$bmi <- recode(dat$BMICAT, recodes = "1=1; 2=2; 3=3; 9=NA")
table(dat$bmi, useNA = "ifany")
dat$bmicat <- ordered(dat$bmi, levels = c(1,2,3), labels = c("neither overweight nor obese",
                                                                                "overweight", "obese"))
table(dat$bmicat)
#neither overweight nor obese     overweight        obese 
#0.3880912                        0.3615108         0.2503980 

table(dat$INCOME2)
#1=less than $10,000; 2=less than $15,000; 3=less than $20,000
#4=less than $25,000; 5=less than $35,000; 6=less than $50,000; 
#7=less than $75,000; 8=$75,000 or more; 77=Don't know/Not sure, 99=Refused 
dat$income2 <- recode(dat$INCOME2, recodes = "1=1; 2=2; 3=3; 4=4; 5=5; 6=6; 7=7; 8=8; else = NA")
dat$incomecat <- ordered(dat$income2, levels = c(1,2,3,4,5,6,7,8), labels = c("lt10k", "lt15k", "lt20k", "lt25k", "lt35k", "lt50k", "lt75k", "gt75"))
dat$inc.lt35k <- recode(dat$INCOME2, recodes  = "c(1,2,3,4,5) = 1; c(6,7,8) = 0; else = NA")

table(dat$RACE2)
#1=White, 2=Black or African American, 3=Asian, 
#4=Native Hawaiian or Other Pacific Islander, 5=American Indian, Alaska Native, 
#6=Other, 7=Don't know/Not sure, 9=Not asked/Missing
dat$race <- recode(dat$RACE2, recodes = "1=1; 2=2; 3=3; 4=4; 5=5; else = NA")
dat$racecat <- factor(dat$race, levels = c(1,2,3,4,5), 
                           labels = c("White", "Black or African American", "Asian",
                                      "Native Hawaiian or Other Pacific Islander",
                                      "American Indian, Alaska Native"))

table(dat$HISPANC2)
#1=yes, 2=no, 7=Don't know/Not sure, 9=Refused
dat$hispanc <- recode(dat$HISPANC2, recodes = "1=1; 2=0; else = NA")

dat$sexcat <- factor(dat$SEX, levels = c(1,2), labels= c("male", "female"))

#removing anyone below 18
dat$age <- ifelse(dat$AGE > 18, dat$AGE, NA)

table(dat$EDUCA)
#1=never attended school or only kindgergarten, 
#2=grades 1-8 (elementary), 3=grades 9-11 (some high school), 
#4=grade 12 or GED (high school graduate), 
#5=College 1 year to 3 years (some college or technical school)
#ASSUMPTION: 6 is completed college. Was NOT in data dictionary
#turning into binary variable indicating whether or not HS was completed 
dat$educat <- recode(dat$EDUCA, recodes = "c(1,2,3) = 0; c(4,5,6) = 1; else = NA")

library(MASS)
bmi_model <- polr(bmicat ~ sexcat + racecat + hispanc + as.numeric(incomecat) + age + educat, data = dat, Hess=TRUE)
summary(bmi_model)

ctable <- coef(summary(bmi_model))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable
#                                                         Value   Std. Error    t value       p value
# sexcatfemale                                     -0.464659978 3.127095e-03 -148.59160  0.000000e+00
# racecatBlack or African American                  0.600518589 5.153521e-03  116.52588  0.000000e+00
# racecatAsian                                     -0.732630500 9.226792e-03  -79.40251  0.000000e+00
# racecatNative Hawaiian or Other Pacific Islander  0.335703619 2.711213e-02   12.38205  3.268985e-35
# racecatAmerican Indian, Alaska Native             0.369468898 1.547583e-02   23.87392 5.715960e-126
# hispanc                                           0.318017234 1.633407e-02   19.46956  1.989627e-84
# as.numeric(incomecat)                            -0.052461569 8.025728e-04  -65.36674  0.000000e+00
# age                                               0.003166647 9.561206e-05   33.11975 1.544632e-240
# educat                                           -0.131459126 6.950753e-03  -18.91293  8.924978e-80
# neither overweight nor obese|overweight          -0.980462296 1.000084e-02  -98.03802  0.000000e+00
# overweight|obese                                  0.620752100 9.979410e-03   62.20329  0.000000e+00

bmi_model_35k <- polr(bmicat ~ sexcat + racecat + hispanc + inc.lt35k + age + educat, data = dat, Hess=TRUE)
summary(bmi_model_35k)
ctable_35k <- coef(summary(bmi_model_35k))
p <- pnorm(abs(ctable_35k[, "t value"]), lower.tail = FALSE) * 2
ctable_35k <- cbind(ctable_35k, "p value" = p)
ctable_35k
#                                                         Value   Std. Error    t value       p value
# sexcatfemale                                     -0.454101778 3.119052e-03 -145.58970  0.000000e+00
# racecatBlack or African American                  0.622502667 5.135722e-03  121.21036  0.000000e+00
# racecatAsian                                     -0.731490271 9.220625e-03  -79.33196  0.000000e+00
# racecatNative Hawaiian or Other Pacific Islander  0.349710771 2.711050e-02   12.89946  4.532829e-38
# racecatAmerican Indian, Alaska Native             0.392693002 1.546408e-02   25.39388 2.946343e-142
# hispanc                                           0.336363037 1.632850e-02   20.59975  2.758557e-94
# inc.lt35k                                         0.153761584 3.418547e-03   44.97864  0.000000e+00
# age                                               0.003526262 9.559348e-05   36.88810 7.170934e-298
# educat                                           -0.178954580 6.885581e-03  -25.98976 6.465405e-149
# neither overweight nor obese|overweight          -0.630256977 9.035180e-03  -69.75588  0.000000e+00
# overweight|obese                                  0.969075493 9.055904e-03  107.01035  0.000000e+00