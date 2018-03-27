library(readr)
cat <- read_csv("enh_1pct_2017_60cols (1).csv")
View(cat)
attach (cat)

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
cat$dummy16 <- NA
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'N',0) 
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'U',0) 
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'D',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'R',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'E',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'M',1)
cat$dummy16 <- replace(cat$dummy16, cat$es_n2016g == 'Y',1)

cat$dummy12 <- NA
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'N',0) 
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'U',0) 
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'D',1)
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'R',1)
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'E',1)
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'M',1)
cat$dummy12 <- replace(cat$dummy12, cat$es_n2012g == 'Y',1)

cat$dummy08 <- NA
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'N',0) 
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'U',0) 
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'D',1)
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'R',1)
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'E',1)
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'M',1)
cat$dummy08 <- replace(cat$dummy08, cat$es_n2008g == 'Y',1)

#holdout
library(caTools)
set.seed(449) 
sample = sample.split(cat$dummy16, SplitRatio = .75)
train = subset(cat, sample == TRUE)
test  = subset(cat, sample == FALSE)

#gbm model
library(gbm)
gbm1 <- gbm(train$dummy16 ~ train$es_n2016p + train$dummy12 + train$dummy08 + 
              train$es_n2010g + train$es_n2014g,
            data=train, distribution = "bernoulli",
            var.monotone = NULL, n.trees = 300, interaction.depth = 1,
            n.minobsinnode = 10, shrinkage = 0.001, bag.fraction = 0.5,
            train.fraction = 1.0, cv.folds=2, keep.data = T,
            verbose = "CV", class.stratify.cv=NULL, n.cores = NULL)
# check performance using an out-of-bag estimator
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
# check performance using a 50% heldout test set
best.iter <- gbm.perf(gbm1,method="test")
print(best.iter)
# check performance using 5-fold cross-validation
best.iter <- gbm.perf(gbm1,method="cv")
print(best.iter)
# plot the performance # plot variable influence
summary(gbm1,n.trees=1) # based on the first tree
summary(gbm1,n.trees=best.iter) # based on the estimated best number of trees
# compactly print the first and last trees for curiosity
print(pretty.gbm.tree(gbm1,1))
print(pretty.gbm.tree(gbm1,gbm1$n.trees))
