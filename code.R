traindata <- read.csv("./data/train.csv")


library(caret)
library(reshape2)
traindata$Survived <- as.factor(traindata$Survived)
levels(traindata$Survived) <- c("Dead", "Survived")
traindata$Pclass <- as.factor(traindata$Pclass)
traindata$Embarked[traindata$Embarked==""] <- NA

train2 <- traindata[-c(1,4,9,11)]
# fill NA
Na.fill <- preProcess(train2[,-1], method = "bagImpute")
train2 <- predict(Na.fill, train2[,-1])
train2$Embarked <- droplevels(train2$Embarked)
par(mfrow=c(2,4), mai=c(.25,.25,.2,.25))
for (i in c(3:6, 1,2,7)){
        plot(traindata$Survived, train2[,i], main =names(train2)[i], col=c('orange', 'blue', 'green'))
}


sum(is.na(train2))

train_control <- trainControl(method = "cv", number = 10)
system.time(
        modelRf <- train(train2, traindata$Survived, model="rf", trControl=train_control, ntree=100, data=train2)
)
# test on Kaggle test data
test <- read.csv("./data/test.csv")
names(test)
test$Pclass <- as.factor(test$Pclass)
test2 <- test[,-c(1,3,8,10)]
test2 <- predict(Na.fill, test2)
pred <- predict(modelRf, test2)

# # partiotion in train and test
# trainInd <- createDataPartition(train2$Survived, p=.7, list = F)
# train2train <- train2[trainInd,]
# train2test <- train2[-trainInd,]
# # train RF
# Rfmodel <- train(Survived~., model="rf", data=train2train)
# Rfmodel$finalModel
# pred <- predict(Rfmodel, train2test)
# confusionMatrix(train2test$Survived, pred)
