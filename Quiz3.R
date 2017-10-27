## Quiz 3

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


#### Q1 classifcation tree
# https://www.statmethods.net/advstats/cart.html
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)


inTrain = createDataPartition(segmentationOriginal$Case, p=.7,list=FALSE)
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[- inTrain,]

set.seed(125)

fit1 <- rpart(Class ~ . , data = testing)

plot(fit1, uniform = TRUE)

#text(fit1, use.n=TRUE, all=TRUE, cex=.8)
text(fit1)



## Q3 K Fold
library(pgmm)
library(caret)
data(olive)
## remove region
olive = olive[,-1]
require(tree)

fit3 <- tree(Area ~ . , data = olive)
newdata <- as.data.frame(t(colMeans(olive)))

tree1 <-predict(fit3,newdata=newdata )
summary(tree1)

### Q4
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
library(ElemStatLearn)
data(SAheart)

set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
fit4 <- glm(chd ~ age + alcohol + obesity + tobacco + typea + ldl ,family=binomial,data=trainSA)
fit4results <- predict(fit4,newdata=testSA,type='response')

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
mistest <- missClass(testSA$chd,fit4results)
mistrain <- missClass(trainSA$chd,fit4results)


## Q5 
##https://www.r-bloggers.com/random-forests-in-r/
#https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr
require(randomForest)
data(vowel.train)
data(vowel.test)

vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)

str(vowel.train)

set.seed(33833)

library(randomForest)
library(caret)

fit5=randomForest(y ~ . , data = vowel.train )
summary(fit5)
importanceOrder=order(-fit5$importance)

