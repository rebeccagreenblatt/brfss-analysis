BRFSS
========================================================
  
  ```{r}
library(survey)
library(car)

#BRFSS.complete <- read.csv("C:/Users/omansour/Desktop/AsthmaProject/Model1/BRFSS.complete.csv")

#BRFSS.complete <- read.csv("/Users/bhimes/Dropbox/Visualize_Asthma_Rates/Summer2015_Analysis/Omar/BRFSS.complete.csv")

BRFSS.complete.allvars <- read.csv("brfss.complete.wcntywts.csv")
BRFSS.complete.allvars$CntyFIPS <- as.integer(gsub(",","", as.character(BRFSS.complete.allvars$CntyFIPS)))
BRFSS.complete.allvars$STSTR <- BRFSS.complete.allvars$CntyFIPS #think this is what STSTR was in Omar's file... 
rename(BRFSS.complete.allvars, c("ASTHMA2" = "ASTHMA", "INCOME2" = "INCOME", "RACE2" = "RACE"))
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHMA2"] <- "ASTHMA"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="INCOME2"] <- "INCOME"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="RACE2"] <- "RACE"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="BMICAT"] <- "BMI"

#FOR SECOND TIME AROUND, NOW USING ASTHNOW INSTEAD OF ASTHMA2
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHMA"] <- "ASTHMA2"
names(BRFSS.complete.allvars)[names(BRFSS.complete.allvars)=="ASTHNOW"] <- "ASTHMA"

##I might be wrongly assigning CNTYFIPS to STSTR variable... don't know where STSTR came from

BRFSS.complete <- select(BRFSS.complete.allvars, ASTHMA, AGE, EDUCA, INCOME, SEX, SMOKER, BMI, CNTYWT, RACE, STSTR)

#____________________
#For this part, depends if we want to do the analysis with ASTHMA2 variable or ASTHMNOW 

#--IF ASTHMA2 -- 
#Asthma (for first time around -- starting with ASTHMA2)
BRFSS.complete$ASTHMA<-recode(BRFSS.complete$ASTHMA, recodes="1='1'; 2='0';c(7,9)=NA", as.factor.result=T)
BRFSS.complete$ASTHMA<-relevel(BRFSS.complete$ASTHMA, ref='0')

#--IF ASTHNOW --
##Asthma Now (for second time around -- starting with ASTHNOW)
##need to confirm that 0 also means N/A... responses 1 and 2 roughly add up to people who responded yes to ever having asthma, so just sticking with these
BRFSS.complete$ASTHMA <- recode(BRFSS.complete$ASTHMA, recodes = "1=1; 2=0; else = NA")
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

BRFSS.complete <- BRFSS.complete[ -c(1) ]

BRFSS.complete <- BRFSS.complete[ which(BRFSS.complete$ASTHMA !='NA' ), ]

BRFSS.test <- BRFSS.complete[complete.cases(BRFSS.complete), ]

###NA's

library("dplyr")
library("tidyr")
library("broom")
library("Amelia")

misstalbe <-BRFSS.complete[, c(2,3,4,5,6,7,8,10)]
colnames(misstalbe) <- c("Asthma", "Age","Education","Income","Sex","Smoking", "BMI","Race")

frac_missing <- function(x) {
  sum(is.na(x))/length(x)
}

miss_by_var <- misstalbe %>% gather(variable, value) %>% group_by(variable) %>% 
  summarise(missing = frac_missing(value)) %>% arrange(-missing)


png(filename="C:/Users/omansour/Desktop/AsthmaProject/missmap.png", width=4, height = 3, units="in", pointsize=8, res=1000)
missmap(misstalbe, main="BRFSS 2002-2012 Data - Missingness Map", col=c("yellow", "black"), legend=FALSE, rank.order=TRUE,x.cex=0.8)
dev.off()


#########

#VIF

vif(glm(ASTHM ~ incomecat + sexcat+agecat+racecat+educat+bmi3cat+smokercat, family =quasibinomial,data=BRFSS.complete))

BRFSS.complete <- read.csv("C:/Users/omansour/Desktop/AsthmaProject/Model1/BRFSS.complete.csv")

#Race/ethnicity
BRFSS.complete$racetest <- recode(BRFSS.complete$RACE, recodes="1='1'; 2='2'; 3:4='4'; 5='5'; 8='8'; c(6,7,9,0)=NA", as.numeric.result=T)

#Income <$25K, $25-75K, >$75k
BRFSS.complete$incometest <- recode(BRFSS.complete$INCOME, recodes="1:4='1'; 5:7='2'; 8='3'; c(0,77,99)=NA", as.numeric.result=T)

#Age
BRFSS.complete$agetest <- recode(BRFSS.complete$AGE, recodes="0:9=NA", as.numeric.result=T)

#Education
BRFSS.complete$edutest <- recode(BRFSS.complete$EDUCA, recodes="1:2='1'; 3:4='2'; 5:6='3';c(0,9)=NA", as.numeric.result=T)

#Sex
BRFSS.complete$sextest <- recode(BRFSS.complete$SEX, recodes="1='1'; 2='2'", as.numeric.result=T)

#Smoking
BRFSS.complete$smokertes <- recode(BRFSS.complete$SMOKER, recodes="1:2='1'; 3='2'; 4='3';9=NA", as.numeric.result=T)

#BMI
BRFSS.complete$bmi3test <- recode(BRFSS.complete$BMI, recodes="1='1'; 2='2'; 3='3';c(0,9)=NA", as.numeric.result=T)

vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  require(fmsb)
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  for(val in names(in_frame)){
    form_in<-formula(paste(val,' ~ .'))
    vif_init<-rbind(vif_init,c(val,VIF(lm(form_in,data=in_frame,...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]))
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(names(in_frame))
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      
      for(val in names(in_dat)){
        form_in<-formula(paste(val,' ~ .'))
        vif_add<-VIF(lm(form_in,data=in_dat,...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2])))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}



InterestVars <-as.matrix(BRFSS.complete[, c(17,18,19,20,21,22,23,24)])
colnames(InterestVars) <- c("Asthma", "Race","Income", "Age", "Education","Sex", "Smoking", "BMI")
NoNas <- na.omit(InterestVars)



test1 <-BRFSS.complete[, c(25,26,27,28,29,30,31)]
colnames(test1) <- c("Race","Income","Age", "Education", "Sex", "Smoking","BMI")
test2 <- na.omit(test1)

vif_func(in_frame=test2,thresh=5,trace=T)

####

#survey information
options(survey.lonely.psu = "adjust")
des <- svydesign(ids=~1, strata=~STSTR, weights=~as.numeric(CNTYWT),  data=BRFSS.test)


###Descriptive analysis
prop.table(svytable(~ASTHM, design = des),)*100

prop.table(svytable(~racecat, design = des),)*100

prop.table(svytable(~incomecat, design = des),)*100

prop.table(svytable(~educat, design = des),)*100

prop.table(svytable(~sexcat, design = des),)*100

prop.table(svytable(~smokercat, design = des),)*100

prop.table(svytable(~bmi3cat, design = des),)*100


###Asthma by factor
gg<-as.matrix(svytable(~racecat+ASTHM, design = des))
gg2<-as.vector(svytable(~racecat, design = des))

tb <- gg/gg2*100

dataR= as.data.frame(tb)



##graphs Asthma and Race
graph1 <- read.csv("C:/Users/omansour/Desktop/AsthmaProject/graph1.csv")


png(filename="C:/Users/omansour/Desktop/AsthmaProject/AsthmabyRace2.png", width=9, height = 7.5, units="in", pointsize=7, res=1000)
graph1$Raceorder <- factor(graph1$Race, c("AI/AN", "Black", "Hispanic", "White", "Asian/Pacific Islander"))
RaceColors = c("#1f78b4", "#33a02c", "#d95f02","#1c9099","#7570b3")
p<-ggplot(graph1, aes(y=AsthmaRate))
p + geom_bar(aes(x=Raceorder), data=graph1, stat="identity", fill= RaceColors,colour="black") + scale_x_discrete(name="Race/Ethnicity",limits=c("AI/AN", "Black", "Hispanic", "White", "Asian/Pacific Islander")) +  scale_y_continuous("Asthma Prevalence (%)",limits=c(0, 18), breaks=c(0,2,4,6,8,10,12,14,16,18))+  labs(title="Adult Self-Reported Lifetime Asthma\nPrevalence Rate by Race/Ethnicity") + theme( plot.title = element_text(size=23, face="bold",vjust=4,lineheight=1),axis.title.x=element_text(size=13,face="bold",vjust=-3.5),axis.title.y= element_text(size=13, face="bold",vjust=3),axis.text.x = element_text(size=12, vjust=-3),axis.text.y = element_text(size=11) ) + coord_fixed(ratio = 0.2)+ theme(plot.margin=unit(c(1,0.7,1,1),"cm"))

dev.off()


##graphs Asthma and Income

in22 <- read.csv("C:/Users/omansour/Desktop/AsthmaProject/in2.csv")

png(filename="C:/Users/omansour/Desktop/AsthmaProject/AsthmabyIncome.png", width=5.5, height = 6, units="in", pointsize=7, res=1000)

IncomeColors = c("#1b9e77","#7570b3","#d95f02")
p<-ggplot(in22, aes(y=AsthmaRate))
p + geom_bar(aes(x=Income), data=in22, stat="identity", fill= IncomeColors,colour="black") + scale_x_discrete(name="Household Income (USD)",limits = c("<$25,000","$25,000-$75,000" ,">$75,000"), labels = c("<$25,000","$25,000-$75,000", "\u2265$75,000")) +  scale_y_continuous("Asthma prevalence (%)",limits=c(0, 18), breaks=c(0,2,4,6,8,10,12,14,16,18))+  labs(title="Adult Self-Reported Lifetime Asthma\nPrevalence Rate by Household Income") + theme( plot.title = element_text(size=18, face="bold",vjust=4,lineheight=1),axis.title.x=element_text(size=12,face="bold",vjust=-3.5),axis.title.y= element_text(size=12, face="bold",vjust=3),axis.text.x = element_text(size=11, vjust=-3),axis.text.y = element_text(size=10) ) + theme(plot.margin=unit(c(1,1,1,1),"cm"))

dev.off()

limits = c("<$25,000","$25,000-$75,000" ,">$75,000"),
+coord_fixed(ratio = 0.1)
###Asthma by income and race

native <- subset(des, racecat =="native")

hh1 <- as.matrix(svytable(~incomecat+ASTHM, design = white))
hh2<-as.vector(svytable(~incomecat, design = white))

gh <- hh1/hh2*100

dataR= as.data.frame(tb)

dr <- as.matrix(svytable(~incomecat+ASTHM, design = native))/as.vector(svytable(~incomecat, design = native)) *100

#Graph

raceincome <- read.csv("C:/Users/omansour/Desktop/AsthmaProject/raceincome.csv")
colnames(raceincome) <- c("Race","Income","per")
raceincome$Income <- factor(raceincome$Income, levels = c("<$25,000", "$25,000-$75,000", ">$75,000") )



png(filename="C:/Users/omansour/Desktop/AsthmaProject/AsthmabyIncomerace.png", width=10, height = 10, units="in", pointsize=8, res=1000)

IncomeColors = c("#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02")
RCin <- ggplot(raceincome,aes(x=Race, y=per,,group=Income,fill=Income))
RCin + geom_bar( stat="identity",position="dodge",guide="legend")+  scale_y_continuous("Asthma prevalence (%)",limits=c(0, 22), breaks=c(0,2,4,6,8,10,12,14,16,18,20,22))+scale_x_discrete(name="Race/Ethnicity", limits = c("AI/AN", "Black", "Hispanic", "White", "Asian/Pacific Islander"))+scale_fill_manual(values=IncomeColors,limits = c("<$25,000","$25,000-$75,000" ,">$75,000"), labels = c(" <$25,000"," $25,000-$75,000", " \u2265$75,000"), name="Household Income (USD)")+guides(fill = guide_legend(reverse=TRUE))+theme(legend.title = element_text( size=12),legend.text = element_text(size=12),plot.title=element_text(size=25,face="bold",vjust=4,lineheight=1),axis.title.x=element_text(size=15,face="bold",vjust=-3),axis.title.y= element_text(size=15, face="bold",vjust=3),axis.text.x = element_text(size=13, vjust=-3),axis.text.y = element_text(size=12),legend.title = element_text(colour="black", size=14, face="bold"),legend.text = element_text(size = 14),legend.position=c(0.87, 0.913),legend.key.height=unit(2.5,"line"),legend.key.width=unit(2.5,"line"), legend.key = element_rect(colour = "black"),legend.title = element_text(size = 15, hjust = 0.5) ) + coord_fixed(ratio = 0.2)+ theme(plot.margin=unit(c(1,0.7,1,1),"cm"))+ labs(title="Adult Self-Reported Lifetime Asthma Prevalence\nRate by Household Income and Race/Ethnicity")+ geom_bar(colour="black", show_guide=FALSE,stat="identity",position="dodge")

dev.off()




##Income and Race


graph4 <- read.csv("C:/Users/omansour/Desktop/AsthmaProject/incomeRace.csv")
colnames(graph4) <- c("Race","Income","per")
graph4$Income <- factor(raceincome$Income, levels = c("<$25,000", "$25,000-$75,000", ">$75,000") )


png(filename="C:/Users/omansour/Desktop/AsthmaProject/IncomeRace.png", width=10, height = 10, units="in", pointsize=8, res=1000)

IncomeColors = c("#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02","#1b9e77","#7570b3","#d95f02")
IncmeRace <- ggplot(graph4,aes(x=Race, y=per,group=Income,fill=Income))
IncmeRace + geom_bar( stat="identity",position="dodge",guide="legend")+  scale_y_continuous("Percentage (%)",limits=c(0, 45), breaks=c(0,3,6,9,12,15,18,21,24,27,30,33,36,39,42,45))+scale_x_discrete(name="Race/Ethnicity", limits = c("Hispanic","AI/AN", "Black", "White", "Asian/Pacific Islander"))+scale_fill_manual(values=IncomeColors,limits = c("<$25,000","$25,000-$75,000" ,">$75,000"), labels = c(" <$25,000"," $25,000-$75,000", " \u2265$75,000"), name="Household Income (USD)")+guides(fill = guide_legend(reverse=TRUE))+theme(legend.title = element_text( size=12),legend.text = element_text(size=12),plot.title=element_text(size=25,face="bold",vjust=4,lineheight=1),axis.title.x=element_text(size=15,face="bold",vjust=-3),axis.title.y= element_text(size=15, face="bold",vjust=3),axis.text.x = element_text(size=13, vjust=-3),axis.text.y = element_text(size=12),legend.title = element_text(colour="black", size=14, face="bold"),legend.text = element_text(size = 14),legend.position=c(0.873, 0.9175),legend.key.height=unit(2.5,"line"),legend.key.width=unit(2.5,"line"), legend.key = element_rect(colour = "black"),legend.title = element_text(size = 15, hjust = 0.5) ) + theme(plot.margin=unit(c(1,0.7,1,1),"cm"))+ labs(title="Household Income by Race/Ethnicity")+ geom_bar(colour="black", show_guide=FALSE,stat="identity",position="dodge")

dev.off()


##

aa1<-as.matrix(svytable(~sexcat+ASTHM, design = des))
aa2<-as.vector(svytable(~smokercat, design = des))

at <- aa1/aa2



###Race and income
svytable(~RACE+INCOME, design = des)
svytable(~RACE, design = des)

m1 <- as.matrix(svytable(~RACE+INCOME, design = des))
m2 <- as.vector(svytable(~RACE, design = des))
percentages <- m1/m2*100


colours <- c("#ffffb3", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3")
barplot(percentages, main="Income by race",ylim=c(0,0.5), ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topleft", bty = "n", inset=c(-0.0005, -0.09)))


###

###Race and sex
a1 <- as.matrix(svytable(~racecat+sexcat, design = des))
a2 <- as.vector(svytable(~racecat, design = des))
perA <- a1/a2


colours <- c("#ffffb3", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3")
barplot(perA, main="Sex by race", ylab = "Percenatge",ylim=c(0,0.75), cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topleft", bty = "n", inset=c(-0.0005, -0.09)))

###Race and education
b1 <- as.matrix(svytable(~racecat+educat, design = des))
b2 <- as.vector(svytable(~racecat, design = des))
perB <- b1/b2


colours <- c("#ffffb3", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3")
barplot(perB, main="Education by race", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Race and smoking
c1 <- as.matrix(svytable(~racecat+smokercat, design = des))
c2 <- as.vector(svytable(~racecat, design = des))
perC <- c1/c2


colours <- c("#ffffb3", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3")
barplot(perC, main="Smoking by race", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Race and BMI
d1 <- as.matrix(svytable(~racecat+bmi3cat, design = des))
d2 <- as.vector(svytable(~racecat, design = des))
perD <- d1/d2


colours <- c("#ffffb3", "#8dd3c7", "#bebada", "#fb8072", "#80b1d3")
barplot(perD, main="BMI by race", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Race and age



###Sex and income
f1 <- as.matrix(svytable(~sexcat+incomecat, design = des))
f2 <- as.vector(svytable(~sexcat, design = des))
perF <- f1/f2


colours <- c("#ffffb3", "#8dd3c7")
barplot(perF, main="Income by sex", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topleft", bty = "n", inset=c(-0.0005, -0.05)))

###Sex and education
g1 <- as.matrix(svytable(~sexcat+educat, design = des))
g2 <- as.vector(svytable(~sexcat, design = des))
perG <- g1/g2


colours <- c("#ffffb3", "#8dd3c7")
barplot(perG, main="Education by sex", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Sex and smoking
h1 <- as.matrix(svytable(~sexcat+smokercat, design = des))
h2 <- as.vector(svytable(~sexcat, design = des))
perH <- h1/h2


colours <- c("#ffffb3", "#8dd3c7")
barplot(perH, main="Smoking by sex",ylim = c(0,0.6), ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Sex and BMI
i1 <- as.matrix(svytable(~sexcat+bmi3cat, design = des))
i2 <- as.vector(svytable(~sexcat, design = des))
perI <- i1/i2


colours <- c("#ffffb3", "#8dd3c7")
barplot(perI, main="BMI by sex", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))


###Income and education
j1 <- as.matrix(svytable(~educat+incomecat, design = des))
j2 <- as.vector(svytable(~educat, design = des))
perJ <- j1/j2


colours <- c("#ffffb3", "#8dd3c7", "#bebada")
barplot(perJ, main="Income by education", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Income and smoking
k1 <- as.matrix(svytable(~smokercat+incomecat, design = des))
k2 <- as.vector(svytable(~smokercat, design = des))
perK <- k1/k2


colours <- c("#ffffb3", "#8dd3c7", "#bebada")
barplot(perK, main="Smoking by income", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topleft", bty = "n", inset=c(-0.0005, -0.05)))

###Income and BMI
l1 <- as.matrix(svytable(~bmi3cat+incomecat, design = des))
l2 <- as.vector(svytable(~bmi3cat, design = des))
perL <- l1/l2


colours <- c("#ffffb3", "#8dd3c7", "#bebada")
barplot(perL, main="BMI by income", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topleft", bty = "n", inset=c(-0.0005, -0.05)))

###BMI and education
m1 <- as.matrix(svytable(~bmi3cat+educat, design = des))
m2 <- as.vector(svytable(~bmi3cat, design = des))
perM <- m1/m2


colours <- c("#ffffb3", "#8dd3c7", "#bebada")
barplot(perM, main="BMI by education", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###BMI and smoking
n1 <- as.matrix(svytable(~bmi3cat+smokercat, design = des))
n2 <- as.vector(svytable(~bmi3cat, design = des))
perN <- n1/n2


colours <- c("#ffffb3", "#8dd3c7", "#bebada")
barplot(perN, main="BMI by smoking", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))

###Education and smoking
o1 <- as.matrix(svytable(~educat+smokercat, design = des))
o2 <- as.vector(svytable(~educat, design = des))
perO <- o1/o2


colours <- c("#ffffb3", "#8dd3c7", "#bebada")
barplot(perO, main="smoking by education", ylab = "Percenatge", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours, legend.text = TRUE, args.legend = list(x = "topright", bty = "n", inset=c(-0.0005, -0.05)))



###Univariate Analysis
svychisq(~ASTHM+sexcat, des, statistic="adjWald")

svychisq(~ASTHM+racecat, des, statistic="adjWald")

svychisq(~ASTHM+incomecat, des, statistic="adjWald")

svychisq(~ASTHM+educat, des, statistic="adjWald")

svychisq(~ASTHM+smokercat, des, statistic="adjWald")

svychisq(~ASTHM+agecat, des, statistic="adjWald")

svychisq(~ASTHM+bmi3cat, des, statistic="adjWald")



###Bivariate Analysis
#Race
svychisq(~racecat+incomecat, des, statistic="adjWald")
svychisq(~racecat+sexcat, des, statistic="adjWald")
svychisq(~racecat+educat, des, statistic="adjWald")
svychisq(~racecat+smokercat, des, statistic="adjWald")
svychisq(~racecat+agecat, des, statistic="adjWald")
svychisq(~racecat+bmi3cat, des, statistic="adjWald")

#Sex
svychisq(~sexcat+incomecat, des, statistic="adjWald")
svychisq(~sexcat+educat, des, statistic="adjWald")
svychisq(~sexcat+smokercat, des, statistic="adjWald")
svychisq(~sexcat+agecat, des, statistic="adjWald")
svychisq(~sexcat+bmi3cat, des, statistic="adjWald")

#Income
svychisq(~incomecat+educat, des, statistic="adjWald")
svychisq(~incomecat+smokercat, des, statistic="adjWald")
svychisq(~incomecat+agecat, des, statistic="adjWald")
svychisq(~incomecat+bmi3cat, des, statistic="adjWald")

#BMI
svychisq(~bmi3cat+educat, des, statistic="adjWald")
svychisq(~bmi3cat+smokercat, des, statistic="adjWald")
svychisq(~bmi3cat+agecat, des, statistic="adjWald")

#Education
svychisq(~educat+smokercat, des, statistic="adjWald")
svychisq(~educat+agecat, des, statistic="adjWald")

#Age
svychisq(~agecat+smokercat, des, statistic="adjWald")


#Model 1
model1 <- svyglm(ASTHMA ~ AGE+EDUCA+INCOME+RACE+SEX+SMOKER+BMI, des, family=quasibinomial)

summary(model1)

####OMAR RESULTS#########
# > summary(model1)
# 
# Call:
# svyglm(formula = ASTHMA ~ AGE + EDUCA + INCOME + RACE + SEX + 
#     SMOKER + BMI, des, family = quasibinomial)
# 
# Survey design:
# svydesign(ids = ~1, strata = ~STSTR, weights = ~as.numeric(CNTYWT), 
#     data = BRFSS.test)
# 
# Coefficients:
#                              Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)                -2.0172160  0.0116980 -172.441  < 2e-16 ***
# AGE                        -0.0107122  0.0001778  -60.249  < 2e-16 ***
# EDUCAnoHighSchool          -0.0678902  0.0191499   -3.545 0.000392 ***
# EDUCASome/highSchool       -0.1357975  0.0064130  -21.175  < 2e-16 ***
# INCOMElow                   0.4103907  0.0080360   51.069  < 2e-16 ***
# INCOMEmiddle                0.0662692  0.0069219    9.574  < 2e-16 ***
# RACEasian/pacific islander -0.1708504  0.0173622   -9.840  < 2e-16 ***
# RACEblack                  -0.0222454  0.0092478   -2.405 0.016151 *  
# RACEhispanic               -0.1447235  0.0111832  -12.941  < 2e-16 ***
# RACEnative                  0.1811671  0.0254848    7.109 1.17e-12 ***
# SEXfemale                   0.4243220  0.0059933   70.800  < 2e-16 ***
# SMOKERcurrent smoker        0.1894103  0.0075319   25.148  < 2e-16 ***
# SMOKERformer smoker         0.2319079  0.0064468   35.972  < 2e-16 ***
# BMIobese                    0.5733671  0.0069052   83.034  < 2e-16 ***
# BMIoverweight               0.1700711  0.0068789   24.724  < 2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for quasibinomial family taken to be 1.003703)
# 
# Number of Fisher Scoring iterations: 4
#exp(coefficients(model1))
# 
# > exp(coefficients(model1))
#                (Intercept)                        AGE          EDUCAnoHighSchool       EDUCASome/highSchool                  INCOMElow 
#                  0.1330253                  0.9893450                  0.9343630                  0.8730194                  1.5074066 
#               INCOMEmiddle RACEasian/pacific islander                  RACEblack               RACEhispanic                 RACEnative 
#                  1.0685143                  0.8429476                  0.9780002                  0.8652615                  1.1986155 
#                  SEXfemale       SMOKERcurrent smoker        SMOKERformer smoker                   BMIobese              BMIoverweight 
#                  1.5285537                  1.2085367                  1.2610036                  1.7742311                  1.1853892 

##############################################################################
##############################################################################
##############################################################################

###REBECCA RESULTS
#WITH ASHTMA2 VARIABLE AS "ASTHMA"
# > summary(model1)
# 
# Call:
#   svyglm(formula = ASTHMA ~ AGE + EDUCA + INCOME + RACE + SEX + 
#            SMOKER + BMI, des, family = quasibinomial)
# 
# Survey design:
#   svydesign(ids = ~1, strata = ~STSTR, weights = ~as.numeric(CNTYWT), 
#             data = BRFSS.test)
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -1.804529   0.022019 -81.953  < 2e-16 ***
#   AGE                        -0.012266   0.000344 -35.655  < 2e-16 ***
#   EDUCAnoHighSchool          -0.362302   0.040175  -9.018  < 2e-16 ***
#   EDUCASome/highSchool       -0.107816   0.012731  -8.468  < 2e-16 ***
#   INCOMElow                   0.275492   0.015320  17.983  < 2e-16 ***
#   INCOMEmiddle                0.039624   0.012358   3.206  0.00134 ** 
#   RACEasian/pacific islander -0.307876   0.034405  -8.949  < 2e-16 ***
#   RACEblack                  -0.015525   0.016056  -0.967  0.33357    
#   RACEhispanic               -0.342235   0.020916 -16.362  < 2e-16 ***
#   RACEnative                  0.336236   0.052429   6.413 1.43e-10 ***
#   SEXfemale                   0.351897   0.010958  32.114  < 2e-16 ***
#   SMOKERcurrent smoker        0.187826   0.014225  13.204  < 2e-16 ***
#   SMOKERformer smoker         0.187970   0.011837  15.880  < 2e-16 ***
#   BMIobese                    0.505771   0.013043  38.777  < 2e-16 ***
#   BMIoverweight               0.137009   0.012950  10.580  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for quasibinomial family taken to be 1.002044)
# 
# Number of Fisher Scoring iterations: 4

# > exp(coefficients(model1))
# (Intercept)                        AGE 
# 0.1645519                  0.9878092 
# EDUCAnoHighSchool       EDUCASome/highSchool 
# 0.6960723                  0.8977931 
# INCOMElow               INCOMEmiddle 
# 1.3171793                  1.0404195 
# RACEasian/pacific islander                  RACEblack 
# 0.7350062                  0.9845950 
# RACEhispanic                 RACEnative 
# 0.7101812                  1.3996695 
# SEXfemale       SMOKERcurrent smoker 
# 1.4217628                  1.2066238 
# SMOKERformer smoker                   BMIobese 
# 1.2067974                  1.6582639 
# BMIoverweight 
# 1.1468387 

#############################################################
#############################################################
#############################################################

###REBECCA RESULTS
#WITH ASTHNOW AS "ASTHMA" -- [ASHNOW only includes people who said yes to ASTHMA2]
# > summary(model1)
# 
# Call:
#   svyglm(formula = ASTHMA ~ AGE + EDUCA + INCOME + RACE + SEX + 
#            SMOKER + BMI, des, family = quasibinomial)
# 
# Survey design:
#   svydesign(ids = ~1, strata = ~STSTR, weights = ~as.numeric(CNTYWT), 
#             data = BRFSS.test)
# 
# Coefficients:
#                               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                -0.4487147  0.0422297 -10.626  < 2e-16 ***
# AGE                         0.0104909  0.0007046  14.889  < 2e-16 ***
# EDUCAnoHighSchool           0.1100182  0.0891628   1.234 0.217241    
# EDUCASome/highSchool        0.1260156  0.0271842   4.636 3.56e-06 ***
# INCOMElow                   0.4097505  0.0322828  12.693  < 2e-16 ***
# INCOMEmiddle                0.1127913  0.0256363   4.400 1.08e-05 ***
# RACEasian/pacific islander -0.3696127  0.0674051  -5.483 4.18e-08 ***
# RACEblack                  -0.0420989  0.0342299  -1.230 0.218741    
# RACEhispanic               -0.4169324  0.0399531 -10.436  < 2e-16 ***
# RACEnative                  0.0196519  0.1171418   0.168 0.866771    
# SEXfemale                   0.5936765  0.0222857  26.639  < 2e-16 ***
# SMOKERcurrent smoker        0.0212950  0.0304264   0.700 0.484000    
# SMOKERformer smoker        -0.0390574  0.0251919  -1.550 0.121048    
# BMIobese                    0.3085261  0.0272772  11.311  < 2e-16 ***
# BMIoverweight               0.0950927  0.0267214   3.559 0.000373 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for quasibinomial family taken to be 1.000161)
# 
# Number of Fisher Scoring iterations: 4
# 
# >exp(coefficients(model1))
# (Intercept)                 AGE           EDUCAnoHighSchool 
# 0.6384482                   1.0105462     1.1162984 
# EDUCASome/highSchool        INCOMElow     INCOMEmiddle 
# 1.1342998                   1.5064419     1.1193983 
# RACEasian/pacific islander  RACEblack     RACEhispanic 
# 0.6910019                   0.9587750     0.6590655 
# RACEnative                  SEXfemale     SMOKERcurrent smoker 
# 1.0198463                   1.8106330     1.0215233 
# SMOKERformer smoker         BMIobese      BMIoverweight 
# 0.9616955                   1.3614171     1.0997608 



