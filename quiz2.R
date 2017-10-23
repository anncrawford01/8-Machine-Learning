

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

### Q3
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p=3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[- inTrain,]


pQ3 <-qplot(training$Superplasticizer, geom="histogram") 

pQ3l <- qplot(log(training$Superplasticizer), geom="histogram") 
grid.arrange(pQ3,pQ3l, ncol=2)

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

### Q5
nstall.packages('e1071', dependencies=TRUE)
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

prePr1 <- preProcess(df1,method = "pca", thresh = 0.8)

alPC <- predict(prePr1,df1)
fit1 <- train(trainingIL$diagnosis ~ . , method = "glm", data= alPC, na.action="na.exclude")
# train with pca built in
fit2 <- train(trainingIL$diagnosis ~ . , method = "glm", preProcess= "pca", data=trainingIL)
confusionMatrix(trainingIL$diagnosis,predict(fit2))
