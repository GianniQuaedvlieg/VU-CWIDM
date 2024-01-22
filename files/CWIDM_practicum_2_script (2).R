#Practicum CWIDM_practicum_2 (regressie, mediatie, mediatie met Process)

#Dataset openen
library(tidyverse)
getwd()
setwd("/Users/gianniquaedvlieg/Downloads") #Meestal komen we terecht in de download folder dus dit zet ik als working directory
url <- "CWIDM_practicum_2_dataset.csv" #deze stap kan ook overgeslagen worden en dan de dataset zelf tussen de haakjes zetten bij volgende stap
D <- read_csv(url)
view(D)

#Descriptives en correlatietabel (vraag 1)
library(summarytools)
view(dfSummary(D))

library(psych)
corr.test(D, alpha = T)

#Vraag 2: regressieanalsye met twee onafhankelijke variabelen (continu) en 1 afhankelijke variabele
regr1 <- lm(SuWB ~ Lichaamstevredenheid + Prestatietevredenheid, data = D)
summary(regr1)

library(texreg)
table <- screenreg(regr1) #Indien ik het allemaal wat overzichtelijker voor mezelf wil maken (voornamelijk handig met verschillende regressiemodellen --> niet alle info beschikbaar hier) 
table

confint(regr1, level = 0.95) #Indien ik ook de betrouwbaarheidsintervallen wil weten

#Nu willen we gestandaardiseerde waarden (goed voor vergelijken sterkte effecten)
library(rockchalk)
regr1_st <- standardize(regr1) #Als ik de Beta's wil inzien kan ik de gehele regressie standaardiseren. 
summary(regr1_st)  

table <- screenreg(regr1_st) #Indien ik het allemaal wat overzichtelijker voor mezelf wil maken (voornamelijk handig met verschillende regressiemodellen --> niet alle info beschikbaar hier) 
table

confint(regr1_st, level = 0.95) #Indien ik ook de betrouwbaarheidsintervallen wil weten

#Vraag 3: regressie waarbij we willen weten of het effect van de X'en zwakker wordt als we nieuwe variabelen erbij doen (kan met confounders zijn of met mediatoren - zonder missende data)
regr2 <- lm(SuWB ~ Lichaamstevredenheid + Prestatietevredenheid  + Extraversion + Neuroticism, data = D)
summary(regr2)

table <- screenreg(list(regr1, regr2))
table

anova(regr1, regr2) #Hier kunnen we zien of het model significant beter wordt na het toevoegen van de nieuwe variabelen (test verschil in F)

#Vraag 4: Nu gaan we hetzelfde doen maar dan willen we dit uitvoeren met dummy variabelen
regr3 <- lm(Zelfver ~ Dummy_typeblootstelling, data = D)
summary(regr3)
table <- screenreg(regr3)
table

#Vraag 5: Hier gaan we een mediatie doen volgens de Baaron and Kenny methode  
regr4 <- lm(Zelfver ~ Dummy_typeblootstelling, data = D) # c
regr5 <- lm(Zelfver ~ Dummy_typeblootstelling + Lichaamstevredenheid, data = D) #c' en b
regr6 <- lm(Lichaamstevredenheid ~ Dummy_typeblootstelling, data = D) #a
summary(regr4)
summary(regr5)
summary(regr6)
table <- screenreg(list(regr4, regr5)) #Hier helpt het om het te visualiseren want we vergelijken c met c'
table
table <- screenreg(regr6)
table

#Vraag 6: Hier gaan we een medatie doen volgens de Baaron en Kenny methode met missende data (Deze stap is om te laten zien waarom je de volgende stap wil gebruiken bij missende waarden)
regr7 <- lm(Zelfver ~ Dummy_typeblootstelling, data = D) # c
regr8 <- lm(Zelfver ~ Dummy_typeblootstelling + Prestatietevredenheid, data = D) #c' en b
regr9 <- lm(Prestatietevredenheid ~ Dummy_typeblootstelling, data = D) #a
summary(regr7)
summary(regr8)
summary(regr9)
table <- screenreg(list(regr7, regr8)) #Hier helpt het om het te visualiseren want we vergelijken c met c'
table
table <- screenreg(regr9)
table

#Vraag 6: Hier gaan we een medatie doen volgens de Baaron en Kenny methode met missende data 
D2 <- filter(D, !is.na(Prestatietevredenheid)) #Je kan een nieuwe subset maken van je data en dan hoef je niet met filter te werken in je volgende code (maar met D2)
View(D2)

regr7 <- lm(Zelfver ~ Dummy_typeblootstelling, data = filter(D, !is.na(Prestatietevredenheid))) # c
regr8 <- lm(Zelfver ~ Dummy_typeblootstelling + Prestatietevredenheid, data = filter(D, !is.na(Prestatietevredenheid))) #c' en b
regr9 <- lm(Prestatietevredenheid ~ Dummy_typeblootstelling, data = filter(D, !is.na(Prestatietevredenheid))) #a
summary(regr7)
summary(regr8)
summary(regr9)
table <- screenreg(list(regr7, regr8)) #Hier helpt het om het te visualiseren want we vergelijken c met c'
table

#Vraag 7: Zelfde vraag met Prestatietevredenheid met Process (kan zowel Baron en Kenny als Sobel aflezen)
library(bruceR)
PROCESS(D, y = "Zelfver", x = "Dummy_typeblootstelling", meds = "Prestatietevredenheid")


