library(readr)
cat <- read_csv("Desktop/Catalyst Data/enh_1pct_2017_60cols (1).csv")
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
sample = sample.split(cat$dummy16, SplitRatio = .5)
train = subset(cat, sample == TRUE)
test  = subset(cat, sample == FALSE)

#the model works with the first method of creating holdout which I got from datacamp
library(caTools)
cat$split <- NA
cat$split <- (sample.split(cat, SplitRatio = 0.5)) 
head(cat)
train <- subset(cat, cat$split==TRUE) 
test <- subset(cat,cat$split ==FALSE)

#model
library(rpart)
tree <- rpart(train$dummy16~ train$es_n2014g + 
                train$es_n2016p + train$es_n2012g +
                train$es_n2008g + train$ca_age +
                train$ca_race + train$sy_educscore + 
                train$ac_medianhhincome,data = train)

printcp(tree)
plotcp(tree)
summary(tree)
plot(tree, uniform=TRUE, 
     main="Classification Tree for 2016G Voter Turnout")
text(tree, use.n=TRUE, all=TRUE, cex=.4)

#testing model

predictions <- predict(tree)
mse <- mean((test$dummy16 - predict(tree)) ^ 2)
print(mse)


