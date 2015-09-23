library(dplyr)

for (i in 2002:2012){
  assign(paste0("brfss.",i), read.csv(paste("census-app/data/Cnty_BRFSS_",i,".C.csv", sep="")))
}

yearly.brfss <- list(brfss.2002, brfss.2003, brfss.2004, 
                     brfss.2005, brfss.2006, brfss.2007, 
                     brfss.2008, brfss.2009, brfss.2010, 
                     brfss.2011, brfss.2012)

for (i in yearly.brfss){
  i <- tbl_df(i)
}

#variable name changed to ASTHMA3 from ASTHMA2 in 2011, the questions are almost identical
brfss.2011$ASTHMA2 <- brfss.2011$ASTHMA3
brfss.2012$ASTHMA2 <- brfss.2012$ASTHMA3

##BMI variable had various names/various category numbers -- normalizing to 3 categories, where 9 is NA
brfss.2002$BMICAT <- brfss.2002$A_BMI2CA
brfss.2003$BMICAT <- brfss.2003$F__BMI3CAT
brfss.2004$BMICAT <- brfss.2004$F__BMI4CAT
brfss.2005$BMICAT <- brfss.2005$F__BMI4CAT
brfss.2006$BMICAT <- brfss.2006$F__BMI4CAT
brfss.2007$BMICAT <- brfss.2007$F__BMI4CAT
brfss.2008$BMICAT <- brfss.2008$F__BMI4CAT
brfss.2009$BMICAT <- brfss.2009$F__BMI4CAT
brfss.2010$BMICAT <- brfss.2010$F__BMI4CAT
brfss.2011$BMICAT <- recode(brfss.2011$F__BMI5CAT, recodes = ("c(1,2) = 1; 3=2; 4=3; else=9"))
brfss.2012$BMICAT <- recode(brfss.2012$F__BMI5CAT, recodes = ("c(1,2) = 1; 3=2; 4=3; else=9"))

variables.to.keep <- c("ASTHMA2", "GENHLTH", "SMOKE100", "AGE", "EDUCA",
                       "INCOME2", "SEX", "RACE2", "BMICAT", "CntyFIPS") 

reduced.2002 <- brfss.2002[,variables.to.keep]
reduced.2003 <- brfss.2003[,variables.to.keep]
reduced.2004 <- brfss.2004[,variables.to.keep]
reduced.2005 <- brfss.2005[,variables.to.keep]
reduced.2006 <- brfss.2006[,variables.to.keep]
reduced.2007 <- brfss.2007[,variables.to.keep]
reduced.2008 <- brfss.2008[,variables.to.keep]
reduced.2009 <- brfss.2009[,variables.to.keep]
reduced.2010 <- brfss.2010[,variables.to.keep]
reduced.2011 <- brfss.2011[,variables.to.keep]
reduced.2012 <- brfss.2012[,variables.to.keep]

##adding IDs to datasets so when we combine we keep what year the data was from
reduced.2002$id <- 2002
reduced.2003$id <- 2003
reduced.2004$id <- 2004
reduced.2005$id <- 2005
reduced.2006$id <- 2006
reduced.2007$id <- 2007
reduced.2008$id <- 2008
reduced.2009$id <- 2009
reduced.2010$id <- 2010
reduced.2011$id <- 2011
reduced.2012$id <- 2012

yearly.reduced <- list(reduced.2002, reduced.2003, reduced.2004,
                       reduced.2005, reduced.2006, reduced.2007,
                       reduced.2008, reduced.2009, reduced.2010,
                       reduced.2011, reduced.2012)

##combien all years into one data table
library(data.table)
brfss.all.years <- rbindlist(yearly.reduced)

##create new complete file 
write.csv(brfss.all.years, file = "brfss_complete.csv")
