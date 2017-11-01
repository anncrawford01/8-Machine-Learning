## Quiz 4 ML8

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
require(randomForest)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

str(vowel.train)


library(randomForest)
library(caret)

##fit11=randomForest(y ~ . , data = vowel.train )
##fit12=gbm(y ~ . , data = vowel.train )

##http://topepo.github.io/caret/model-training-and-tuning.html
## use set.seed just prior to calling train
set.seed(33833)
fit11=train(y ~ . , data = vowel.test, method = 'rf' )
fit12=train(y ~ . , data = vowel.test , method = 'gbm', 
            verbose=FALSE )

fit11$results
fit12$results

cm1 = confusionMatrix(fit11)
cm2 = confusionMatrix(fit12)

cm1
cm2


