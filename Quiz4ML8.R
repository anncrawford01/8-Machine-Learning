## Quiz 4 ML8
##https://rstudio-pubs-static.s3.amazonaws.com/69536_511babf1999048239092bf75899420ee.html
##
##train function documentation
#https://www.rdocumentation.org/packages/caret/versions/6.0-77/topics/train

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


## Q1   ##########
##https://www.r-bloggers.com/random-forests-in-r/
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr


library(ElemStatLearn)
require(randomForest)
data(vowel.train)
data(vowel.test)

#Set the variable y to be a factor variable in both the training and test set
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

##str(vowel.train)

library(randomForest)
library(caret)

##http://topepo.github.io/caret/model-training-and-tuning.html
## use set.seed just prior to calling train
## set the seed to 33833.
set.seed(33833)
#Fit (1) a random forest predictor relating the factor variable y to the
#remaining variables
# Fit  (2) a boosted predictor using the "gbm" method.
# Fit these both with the train() command in the caret package.
fit1rf = train(y ~ . , data = vowel.train, method = 'rf' )
fit1gbm =train(y ~ . , data = vowel.train , method = 'gbm', 
            verbose=FALSE )

#What are the accuracies for the two approaches on the test data set? 
# predict based on trained
actualY = vowel.test$y

prf = predict(fit1rf,newdata =vowel.test)
pgbm = predict(fit1gbm, newdata=vowel.test[-y])


## compare actual to predicted , cut
cmrf = confusionMatrix( prf, actualY)    ## RF
cmgbm = confusionMatrix(actualY, pgbm)    ## gbm


#What is the accuracy among the test set samples where the two methods agree?

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

#Set the seed to 62433 
#predict diagnosis with all the other variables using:
#       a random forest ("rf")
#       boosted trees ("gbm") 
#       linear discriminant analysis ("lda") model.

#Does the data contain missing values
sum(is.na(training))
set.seed(62433)

fit2rf =train(diagnosis ~ .  , data = training, method = 'rf' )
p2rf <- predict(fit2rf,newdata = testing[-1])

fit2gbm =train(diagnosis ~ . , data = training , method = 'gbm', 
            verbose=FALSE )
p2gbm <- predict(fit2gbm,newdata = testing)

fit2lda =train(diagnosis ~ . , data = training , method = 'lda', 
            verbose=FALSE )

p2lda <- predict(fit2lda,newdata = testing[-1])

cm2rf = confusionMatrix(testing$dianosis,p2rf)

## stack perdictions using rf
## ** good link
# https://www.analyticsvidhya.com/blog/2017/02/introduction-to-ensembling-along-with-implementation-in-r/

## What is the resulting accuracy on the test set?
## Is it better or worse than each of the individual predictions?

#### Q3  ##################
#fit a lasso model to predict Compressive Strength.
#Which variable is the last coefficient to be set to zero 
#as the penalty increases? (Hint: it may be useful to look up ?plot.enet).

##http://www4.stat.ncsu.edu/~post/josh/LASSO_Ridge_Elastic_Net_-_Examples.html
## https://stats.stackexchange.com/questions/68431/interpretting-lasso-variable-trace-plots
## 3.4.2-3.4.3 ESLII
#install.packages("glmnet")
set.seed(3523)
library(AppliedPredictiveModeling)
library(caret)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

set.seed(233)  
fit3lasso =train(CompressiveStrength ~ . , data = training , method = 'lasso')
class(lasso$finalModel)
plot.enet(fit3lasso$finalModel,xvar="penalty",use.color = TRUE)


#### Q4 ###############
##install.packages('forecast', dependencies = TRUE)
library(data.table)
library(forecast)
library(magrittr)   ## for %>% operator
library(rpart)


##"https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv'"
##download into current dir
##setwd("D:/Data/Coursera/DataScience/8-Machine-Learning")
## understanding timeseries
###https://www.r-bloggers.com/time-series-analysis-using-r-forecast-package/

library(lubridate) # For year() function below
library(ggplot2)
dat = read.csv("gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)

tsx = ts(dat)
plot(tsx[,3])

#Fit a model using the bats() function in the forecast package to the training time series.
fit4bats = bats(tstrain)
# Then forecast this model for the remaining time points. 
p4 = predict(fit4bats, newdata = testing)

#For how many of the testing points is the true value within
#the 95% prediction interval bounds?


#### Q5 #################
#https://cran.r-project.org/web/packages/e1071/vignettes/svmdoc.pdf

set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)

data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

#Set the seed to 325
set.seed(325)
#fit a support vector machine using the e1071
#package to predict Compressive Strength using the default settings. 
#Predict on the testing set. What is the RMSE?

fit5  <- svm(CompressiveStrength ~ ., data = training)


