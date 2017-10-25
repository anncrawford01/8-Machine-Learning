## Quiz 3

install.packages("AppliedPredictiveModeling")
install.packages("caret")
install.packages("Hmisc")
install.packages("e1071")
##library(Hmisc)
##library(gridExtra)


#### Q1 classifcation tree
# https://www.statmethods.net/advstats/cart.html
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
library(rpart)

set.seed(125)

inTrain = createDataPartition(segmentationOriginal$Case, p=3/4)[[1]]
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[- inTrain,]

fit1 <- rpart(Case ~ . , data = training, method ="class")

plot(fit1, uniform = TRUE)
#text(fit1, use.n=TRUE, all=TRUE, cex=.8)
text(fit1)


## Q2 K Fold

