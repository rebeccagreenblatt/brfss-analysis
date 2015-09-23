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
by_inc <- epa_and_brfss %>%
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
ozone.asthma.model <- glm(asthma ~ ozone + smoke100 + age + educa + inc.lt35k + sex + race, family = "binomial", data = brfss.w.epa)

##next, use 'do you have asthma now' as the response variable?
