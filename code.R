rm(list=ls())
library(survival)
library(dplyr)
library(car)

setwd("~/Documents/RA/Provost/Provost_Database_072915")
provost <- read.csv("Updated\ From\ SPSS-Table\ 1.csv")


#####RECODE###############################
#male=0, female=1
provost$gender_recode <- as.numeric(provost$X1Gender==1)

#hispanic/latino=0, other=1
provost$race_recode <- as.numeric(provost$X8Racial!=2)

#(birthcountry=US)=0, (birthcountry=Others)=1
provost$birthctry_recode <- as.numeric(provost$birthctry!=1)

#less than high school=0, high school and high school above=1
provost$educ_recode <- as.numeric(provost$X3HighLevelEdu >= 3)

# unmarried=0, married =1
provost$mar_recode <- as.numeric(provost$X9MaritalSt==1)

# not living with person in relationship = 0, living =1
provost$live_recode <- as.numeric(provost$X10LivingPersRela==1)

# religious level
provost$relig_recode <- as.numeric(provost$X7HowRelig>=3)

# (HIV KQ <= 0.72) = 0, (HIV KQ > 0.72) = 1
provost$HIVKQ_median <- as.numeric(provost$HIVKQ.Per > 0.72)

# Did not have unprotected sex = 0, Had = 1
provost$unprot_sex_recode <- as.numeric(provost$X31UnprotectedSex==1)

# Others=0, Medicaid = 1
provost$medic_recode <- as.numeric(provost$X13aKindHealthIns==1)

# (Age<=48)=0, (Age>48)=1
provost$age_recode <- as.numeric(provost$X2Age > 48)

# (Nparter<=1)=0, (Nparter>1)=1
provost$sex_part_recode <- as.numeric(provost$X31aNpartner > 1)

# (income<=852)=0, (income>852)=1
provost$income_recode <- as.numeric(provost$X5IncomeLast30D > 852)

# Time in US: (time in the us <= 20)=0, (time in the us > 20)=1
wicer <- read.csv("wicer\ linked\ march\ 28\ 2015-Table\ 1.csv")
provost$age_us <- wicer$age_us
provost$age_us_cat <- as.numeric(provost$age_us > 20)

# Billboards
provost$X29Billboards <- recode(provost$X29Billboards, "7=NA")

# HIV Last year
provost$X25HIVLastYear <- recode(provost$X25HIVLastYear, "7=NA")

# Select all relevant variables
provost <- provost %>% 
  select(X46TestToday,X2Age,age_recode,gender_recode,educ_recode,
         X5IncomeLast30D,medic_recode,mar_recode,live_recode, birthctry_recode,
         age_us_cat,relig_recode, HIVKQ_median, X31UnprotectedSex, sex_part_recode, 
         X27TVAnnt, X28DisplatAnnout.,X29Billboards,X24EverTest,X25HIVLastYear)


############FUNCTION TO CREATE DESCRIPTIVE TABLE FOR CATEGORICAL VAR#############
update <- function(vector){
  v <- vector %>%
    as.factor() %>%
    recode("NA='7'")
  out <- matrix(NA, nrow = 2, ncol = length(levels(v)))
  for (i in 1:length(levels(v))){out[1,i] <- sum(v==levels(v)[i])}
  total <- sum(out[1,])
  for (i in 1:length(levels(v))){out[2,i] <- sum(v==levels(v)[i])*100/total}
  colnames(out) <- levels(factor(v))
  rownames(out) <- c("count", "percent")
  return (out)
}


############TABLE1: DESCRIPTIVE ANALYSIS#####################
accept <- subset(provost, X46TestToday==1)
refused <- subset(provost, X46TestToday==0)

#categorical var: frequency tables
provost %>%
  select(-X2Age,-X5IncomeLast30D) %>%
  apply(2,update)

accept %>%
  select(-X2Age,-X5IncomeLast30D) %>%
  apply(2,update)

refused %>%
  select(-X2Age,-X5IncomeLast30D) %>%
  apply(2,update)

#continuous var: mean and sd
provost %>%
  select(X2Age,X5IncomeLast30D) %>%
  apply(2,function(x){return(c(mean(x), sd(x)))})

accept %>%
  select(X2Age,X5IncomeLast30D) %>%
  apply(2,function(x){return(c(mean(x), sd(x)))})

refused %>%
  select(X2Age,X5IncomeLast30D) %>%
  apply(2,function(x){return(c(mean(x), sd(x)))})


################TABLE1: BIVARIABLE################
provost$time <- 1

provost[,-c(1,ncol(provost))] %>% #exclude outcome and time variable as predictor
  apply(2, function(x){
  return(summary(coxph(Surv(time,X46TestToday) ~ x,data=provost[]))) #create summary, coef, confint
  })


##########TABLE2: MULTIVARIABLE###################
#select var under .1 significance for multivariable analysis
coxph(Surv(time,X46TestToday) ~ age_recode + educ_recode + X5IncomeLast30D + live_recode + 
              age_us_cat + relig_recode + X29Billboards + X24EverTest + X25HIVLastYear,
              method="efron",
              robust=T,
              data=provost) %>%
  summary()



