library(tidyverse)
library(data.table)
library(survival)

#oefendataframe maken
Datum_partnerverlies <- c('2018-03-05', 
                          '2018-11-12', 
                          '2018-08-04', 
                          '2018-07-13', 
                          '2018-06-30', 
                          NA, 
                          '2018-12-06')
Datum_aanvraag <- c(NA, 
                    '2018-12-12', 
                    '2018-06-04', 
                    '2018-08-13', 
                    '2018-09-30', 
                    NA, 
                    NA)

Geboortedatum <- c('1936-04-12',
                   '1940-03-21',
                   '1938-05-16',
                   '1941-11-03',
                   '1938-10-25',
                   '1929-09-18',
                   '1941-07-14')

Id <- c(1,2,3,4,5,6,7)

Shocks_data <- data.frame(Id, Datum_aanvraag, Datum_partnerverlies, Geboortedatum)
rm(Id, Datum_aanvraag, Datum_partnerverlies, Geboortedatum)

#leeftijd op 1-1-2018
Shocks_data$Geboortedatum <- as.Date(Shocks_data$Geboortedatum)
Shocks_data$Leeftijd <- as.Date('2018-01-01') - Shocks_data$Geboortedatum
Shocks_data$Leeftijd <- as.numeric(Shocks_data$Leeftijd)

#maak een variabele met overlevingstijd (thuis wonen overleven)
Shocks_data$Leeftijdvanaf75 <- Shocks_data$Leeftijd - (75 * 365.25)

#Shocks_data$Overleving_thuis <- Leeftijdvanaf75 + aantal dagen van 01-01-2018 tot aanvraagdatum
Shocks_data$Datum_aanvraag <- as.Date(Shocks_data$Datum_aanvraag)
Shocks_data$Datum_overlijden <- as.Date(Shocks_data$Datum_overlijden)
Shocks_data$Datum_partnerverlies <- as.Date(Shocks_data$Datum_partnerverlies)

Shocks_data$Overleving_thuis <- Shocks_data$Datum_aanvraag - as.Date('2018-01-01')
Shocks_data$Overleving_thuis <- as.numeric(Shocks_data$Overleving_thuis)
Shocks_data$Overleving_thuis <- Shocks_data$Leeftijdvanaf75 + Shocks_data$Overleving_thuis

#make a variable for censoring (yes/no)
Shocks_data$Cens <- 1
Shocks_data$Cens <- replace(Shocks_data$Cens, 
                            is.na(Shocks_data$Datum_aanvraag), 
                            0)

# #maak een variabele met overlevingstijd tot shock (overlevingstijd partner in dit geval)
Shocks_data$Overleving_partner <- Shocks_data$Datum_partnerverlies - as.Date('2018-01-01')
Shocks_data$Overleving_partner <- (Shocks_data$Leeftijdvanaf75) + Shocks_data$Overleving_partner
Shocks_data$Overleving_partner <- as.numeric(Shocks_data$Overleving_partner)

# #moet deze variabele ook geconsored worden als de partner de volledige periode heeft overleefd?
Shocks_data$cens_partner <- c(1,1,1,1,1,1,1)
Shocks_data$cens_partner <- replace(Shocks_data$cens_partner, 
                           is.na(Shocks_data$Datum_partnerverlies), 
                           0)

#maak er een survivalmodel set van
Shocks_data$sobj <- with(Shocks_data, 
             Surv(Overleving_thuis, Cens == 1))
Shocks_data
km <- survfit(Shocks_data$sobj ~ 1)
plot(km)

data.frame(time = km$time, 
           n.risk = km$n.risk, 
           n.event = km$n.event,
           n.censor = km$n.censor, 
           surv = km$surv)