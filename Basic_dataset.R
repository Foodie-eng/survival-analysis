library(tidyverse)
library(data.table)
library(survival)

Date_shock <- c('2018-03-05', 
                          '2018-11-12', 
                          '2018-08-04', 
                          '2018-07-13', 
                          '2018-06-30', 
                          NA, 
                          '2018-12-06')
Date_event <- c(NA, 
                    '2018-12-12', 
                    '2018-06-04', 
                    '2018-08-13', 
                    '2018-09-30', 
                    NA, 
                    NA)

Date_of-birth <- c('1936-04-12',
                   '1940-03-21',
                   '1938-05-16',
                   '1941-11-03',
                   '1938-10-25',
                   '1929-09-18',
                   '1941-07-14')

Id <- c(1,2,3,4,5,6,7)

Dataset <- data.frame(Id, Date_event, Date_shock, Date_of-birth)
rm(Id, Date_event, Date_shock, Date_of-birth)

Dataset$Date_of-birth <- as.Date(Dataset$Date_of-birth)
Dataset$Leeftijd <- as.Date('2018-01-01') - Dataset$Date_of-birth
Dataset$Leeftijd <- as.numeric(Dataset$Leeftijd)

Dataset$Leeftijdvanaf75 <- Dataset$Leeftijd - (75 * 365.25)

Dataset$Date_event <- as.Date(Dataset$Date_event)
Dataset$Date_shock <- as.Date(Dataset$Date_shock)

Dataset$Survival <- Dataset$Date_event - as.Date('2018-01-01')
Dataset$Survival <- as.numeric(Dataset$Survival)
Dataset$Survival <- Dataset$Leeftijdvanaf75 + Dataset$Survival

Dataset$Cens <- 1
Dataset$Cens <- replace(Dataset$Cens, 
                            is.na(Dataset$Date_event), 
                            0)

Dataset$Survival_event <- Dataset$Date_shock - as.Date('2018-01-01')
Dataset$Survival_event <- (Dataset$Leeftijdvanaf75) + Dataset$Survival_event
Dataset$Survival_event <- as.numeric(Dataset$Survival-event)

Dataset$cens_partner <- c(1,1,1,1,1,1,1)
Dataset$cens_partner <- replace(Dataset$cens_partner, 
                           is.na(Dataset$Date_shock), 
                           0)

#maak er een survivalmodel set van
Dataset$sobj <- with(Dataset, 
             Surv(Survival, Cens == 1))
Dataset
km <- survfit(Dataset$sobj ~ 1)
plot(km)

data.frame(time = km$time, 
           n.risk = km$n.risk, 
           n.event = km$n.event,
           n.censor = km$n.censor, 
           surv = km$surv)
