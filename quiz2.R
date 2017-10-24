install.packages("AppliedPredictiveModeling")
install.packages("caret")
install.packages("Hmisc")
install.packages("e1071")


#### Q2
library(AppliedPredictiveModeling)
data("concrete")
library(caret)
library(Hmisc)
library(gridExtra)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[- inTrain,]
###
cutmix <- cut2(training$CompressiveStrength, g= 3)


p1 <-qplot(cutmix,training$CompressiveStrength,color=cutmix)
p2 <-qplot(Cement,CompressiveStrength,color=cutmix,data=training)
p3 <-qplot(BlastFurnaceSlag,CompressiveStrength,color=cutmix,data=training)
p4 <-qplot(FlyAsh,CompressiveStrength,color=cutmix,data=training)
p5 <-qplot(Water,CompressiveStrength,color=cutmix,data=training)
p6 <-qplot(Superplasticizer,CompressiveStrength,color=cutmix,data=training)
p7 <-qplot(CoarseAggregate,CompressiveStrength,color=cutmix,data=training)
p8 <-qplot(FineAggregate,CompressiveStrength,color=cutmix,data=training)
p9 <-qplot(Age,CompressiveStrength,color=cutmix,data=training)

grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, nrow=3, ncol=3)

### Q3 -- Log transform
## https://www.youtube.com/watch?v=5571wc0iWCI
library(AppliedPredictiveModeling)
library(caret)
library(Hmisc)
data("concrete")

set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[- inTrain,]


pQ3 <-qplot(training$Superplasticizer, geom="histogram") 

pQ3l <- qplot(log(training$Superplasticizer), geom="histogram") 
 
grid.arrange(pQ3,pQ3l, ncol=2)
## the plot of log is more normal
## summary shows lots of zeros
summary(training$Superplasticizer)
##

## shows - inf ( negative infinity) because of zero values
summary(log(training$Superplasticizer))



### Q4 
library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
intrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
## get all IL predictors
df1 <- training[ , grepl( "^IL" , names( training ) ) ]

prePr1 <- preProcess(df1,method = "pca", thresh = 0.8)
prePr1$numComp

### Q5  Accuracy of 2 models

library(caret)
library(AppliedPredictiveModeling)
library(dplyr)
set.seed(3433)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
intrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[inTrain,]
testing = adData[-inTrain,]
## get all IL predictors
df1 <- training[ , grepl( "^IL" , names( training ) ) ]
## add the diagnosis column
trainingIL <- cbind(df1, training$diagnosis)
names(trainingIL)[names(trainingIL) == 'training$diagnosis'] <- 'diagnosis'

preProc1 <- preProcess(df1,method = "pca", thresh = 0.8)


fit1 <- train(diagnosis ~ . , method = "glm", data=training,na.action="na.exclude")
confusionMatrix(predict(fit1),training$diagnosis,dnn=c("Impaired","Control"))

# train with pca built in
fit2 <- train(diagnosis ~ . , method = "glm", preProcess= "pca", data=trainingIL,na.action="na.exclude")
confusionMatrix(predict(fit2))
