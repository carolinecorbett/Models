library(readr)
cat <- read_csv("Desktop/Catalyst Data/enh_1pct_2017_60cols (1).csv")
View(cat)
attach (cat)

library(readr)
census <- read_csv("Desktop/state_facts_2016.csv")
View(census)
attach(state_facts_2016)

#cleaning data
state <- as.ordered(cat$state)
cat$es_n2008g = as.factor (es_n2008g)
cat$es_n2010g = as.factor (es_n2010g)
cat$es_n2012g = as.factor (es_n2012g)
cat$es_n2014g = as.factor (es_n2014g)
cat$es_n2016p = as.factor (es_n2016p)
cat$es_n2016g = as.factor (es_n2016g)
cat$ca_gender = as.factor (ca_gender)
cat$ca_race = as.factor (ca_race)
cat$ca_voterstatus = as.factor (ca_voterstatus)
cat$ca_partyaffiliation = as.factor (ca_partyaffiliation)
cat$state = as.factor (cat$state)
cat$es_n2016g = as.ordered (es_n2016g)

#creating dummies for voting in GE
library (dplyr)

cat$dummy16 <- NA
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'N',0) 
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'U',0) 
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'D',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'R',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'E',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'M',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'Y',1)

dummy12 <- NA
dummy12 <- replace(dummy12, cat$es_n2012g == 'N',0) 
dummy12 <- replace(dummy12, cat$es_n2012g == 'U',0) 
dummy12 <- replace(dummy12, cat$es_n2012g == 'D',1)
dummy12 <- replace(dummy12, cat$es_n2012g == 'R',1)
dummy12 <- replace(dummy12, cat$es_n2012g == 'E',1)
dummy12 <- replace(dummy12, cat$es_n2012g == 'M',1)
dummy12 <- replace(dummy12, cat$es_n2012g == 'Y',1)

dummy08 <- NA
dummy08 <- replace(dummy08, cat$es_n2008g == 'N',0) 
dummy08 <- replace(dummy08, cat$es_n2008g == 'U',0) 
dummy08 <- replace(dummy08, cat$es_n2008g == 'D',1)
dummy08 <- replace(dummy08, cat$es_n2008g == 'R',1)
dummy08 <- replace(dummy08, cat$es_n2008g == 'E',1)
dummy08 <- replace(dummy08, cat$es_n2008g == 'M',1)
dummy08 <- replace(dummy08, cat$es_n2008g == 'Y',1)

#Creating indicator variable for race
library (dplyr)
race <- NA 
race <- replace(race, cat$ca_race == 'B',2) 
race <- replace(race, cat$ca_race == 'H',1) 
race <- replace(race, cat$ca_race == 'C',0)

#Creating indicator variable for gender
female <- NA
female <- replace(female, cat$ca_gender == 'M',1) 
female <- replace(female, cat$ca_gender == 'F',2)

#Creating indicator variables for education
summary (cat$sy_educscore)
edu <- NA
edu[cat$edubins < .992] = 3
edu[cat$edubins < .751] = 2
edu[cat$edubins < .512] = 1
edu[cat$edubins < .272] = 0

edu1 <- NA
edu1[census$

#creating indicator for age
summary (cat$ca_age)
age <- NA
age[cat$ca_age < 100] = 4
age[cat$ca_age < 65] = 3
age[cat$ca_age < 45] = 2
age[cat$ca_age < 30] = 1

#creating index variables from Catalyst data
race.female <- (female *3) + race
age.edu <- (age *3 ) + edu
dummyboth <- (dummy08) + dummy12

#creating holdout sample
library(caTools)
set.seed(111) 
sample = sample.split(cat$dummy16, SplitRatio = .75)
train = subset(cat, sample == TRUE)
test  = subset(cat, sample == FALSE)

#creating individual level model
library(lme4)
individual.model <- glmer(formula = cat$dummy16 ~ (1|race.female) + (1|age)
                          + (1|edu) + (1|age.edu) + (1|state)  
                          + (1|dummy08) + (1|dummy12),data=cat, 
                          family=binomial (link="logit"))
display(individual.model)

#testing
best <- individual.model$df[which.min(individual.model$RSS)]

predictions <- predict(cartmodel, x, s=best, type="fit")$cartmodel

mse <- mean((y - predictions)^2)
print(mse)

#poststratification
cellpred <- invlogit(fixef(individual.model)["(Intercept)"]
                     +(individual.model)$race.female[State$race.female1,1]
                     +(individual.model)$age[State$age1,1]
                     +(individual.model)$edu[State$edu1,1]
                     +(individual.model)$age.edu1[Census$cage.edu1,1]
                     )
