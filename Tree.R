library(readr)
cat <- read_csv("Desktop/C****** Data/******** (1).csv")
View(cat)
attach (cat)

#cleaning data
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

#creating holdout sample
library(caTools)
set.seed(111) 
sample = sample.split(cat$dummy16, SplitRatio = .75)
train = subset(cat, sample == TRUE)
test  = subset(cat, sample == FALSE)

#model
library(rpart)
tree <- rpart(train$dummy16~ train$es_n2014g + 
                train$es_n2016p + train$es_n2012g +
                train$es_n2008g + train$ca_age +
                train$ca_race + train$sy_educscore + 
                train$ac_medianhhincome,data = train, method="anova",
              control=rpart.control(minsplit=1,minbucket=1, cp=0))

printcp(tree)
plotcp(tree)
summary(tree)
plot(tree, uniform=TRUE, 
     main="Classification Tree for 2016G Voter Turnout")
text(tree, use.n=TRUE, all=TRUE, cex=.8)



