#leaving out 2012 data until we get the fips codes

library(dplyr)
library(reshape2)

for (i in 2002:2012){
  assign(paste0("epa.",i), read.csv(paste0("EPAForBlanca/OzPM25PM10_",i,".csv")))
}

yearly.epa <- list(epa.2002, epa.2003, epa.2004, 
                     epa.2005, epa.2006, epa.2007, 
                     epa.2008, epa.2009, epa.2010, 
                     epa.2011)

for (i in yearly.epa){
  i <- tbl_df(i)
}

head(epa.2012$CntyFIPS)
#problems#
##no CntyFIPS data for epa.2012...
##parameter_code in 2010 defaults to wrong format
#different parameter codes for different years - 
#...go through and make sure each one always corresponds to the same measurements
levels(epa.2010$Parameter_Code)
levels(epa.2010$Parameter_Code) <- c("44,201", "81,102", "81,104", "85,101", 
                                     "88,101", "88,500", "88,501", "88,503")

epa.2002$id <- 2002
epa.2003$id <- 2003
epa.2004$id <- 2004
epa.2005$id <- 2005
epa.2006$id <- 2006
epa.2007$id <- 2007
epa.2008$id <- 2008
epa.2009$id <- 2009
epa.2010$id <- 2010
epa.2011$id <- 2011
epa.2012$id <- 2012

yearly.epa <- list(epa.2002, epa.2003, epa.2004, 
                   epa.2005, epa.2006, epa.2007, 
                   epa.2008, epa.2009, epa.2010, 
                   epa.2011)

library(data.table)

epa.all.years <- rbindlist(yearly.epa)

##44,201 = Ozone, ##88,101 = PM2.5 - local conditions
epa.all.years <- filter(epa.all.years, Parameter_Code %in% c("44,201", "88,101")) %>%
  group_by(id, CntyFIPS, Parameter_Code) %>%
  summarise(median_measurement = mean(F50th_Percentile))

epa.all.years.ozone.pm25 <- dcast(epa.all.years, id + CntyFIPS ~ Parameter_Code)
#Using median_measurement as value column: use value.var to override.

epa.all.years.ozone.pm25$ozone <- epa.all.years.ozone.pm25[,3]
epa.all.years.ozone.pm25$PM2.5.local <- epa.all.years.ozone.pm25[,4]

epa.complete.ozone.pm25 <- select(epa.all.years.ozone.pm25, id, CntyFIPS, ozone, PM2.5.local)
tail(epa.complete.ozone.pm25)

epa.2010.from_complete <- filter(epa.complete.ozone.pm25, id == 2010)

##map of epa data overlay with environmental data
##do you have asthma now? how many people will this limit us to? 

write.csv(epa.complete.ozone.pm25, file = "epa_complete.csv")

