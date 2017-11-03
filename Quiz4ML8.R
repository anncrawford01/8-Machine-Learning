## Quiz 4 ML8
##https://rstudio-pubs-static.s3.amazonaws.com/69536_511babf1999048239092bf75899420ee.html

install.packages("AppliedPredictiveModeling")
install.packages("caret")
install.packages("Hmisc")
install.packages("e1071")
install.packages("pgmm")
install.packages("tree")
install.packages("ElemStatLearn")
install.packages("randomForest")
##install.packages("rattle")
##library(Hmisc)
##library(gridExtra)


## Q1
##https://www.r-bloggers.com/random-forests-in-r/
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
library(ElemStatLearn)
require(randomForest)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

##str(vowel.train)


library(randomForest)
library(caret)

##fit11=randomForest(y ~ . , data = vowel.train )
##fit12=gbm(y ~ . , data = vowel.train )

##http://topepo.github.io/caret/model-training-and-tuning.html
## use set.seed just prior to calling train
set.seed(33833)
fit1rf = train(y ~ . , data = vowel.train, method = 'rf' )
fit1gbm =train(y ~ . , data = vowel.train , method = 'gbm', 
            verbose=FALSE )

prf = predict(fit1rf,vowel.test)
pgbm = predict(fit1gbm,vowel.test)

actualY = vowel.test$y
## compare actual to predicted , cut
cmrf = confusionMatrix(actualY, prf)    ## RF
cmgbm = confusionMatrix(actualY, pgbm)    ## gbm





### Q2  #################
##stacking classification 
##http://amsantac.co/blog/en/2016/10/22/model-stacking-classification-r.html
library(caret)
library(gbm)

set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)

inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

set.seed(62433)

fit2rf =train(diagnosis ~ . , data = training, method = 'rf' )
fit2gbm =train(diagnosis ~ . , data = training , method = 'gbm', 
            verbose=FALSE )
fit2ida =train(diagnosis ~ . , data = training , method = 'ida', 
            verbose=FALSE )

## stack perdictions using rf


#### Q3
#fit a lasso model to predict Compressive Strength.
#Which variable is the last coefficient to be set to zero 
#as the penalty increases? (Hint: it may be useful to look up ?plot.enet).

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)  

fit3las = train(CompressiveStrength ~ ., data = training, method = 'lasso')


