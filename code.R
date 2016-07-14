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
levels(pred) = c(0,1)
submission <- data.frame(PassengerId = test$PassengerId, Survived = pred)
write.csv(submission, "./submission.csv", quote = F, row.names = F)
# see variable importance
imp <- varImp(modelRf)
dev.off()
ord <- order(imp$importance$Overall)
barplot(imp$importance$Overall[ord], horiz = T, col = '#53cfff', border = F, 
        names.arg = row.names(imp$importance)[ord], las=2)

# choose the most five mpotant variables
vars <- tail(names(train2)[ord],4)
head(train2[,vars])
# new model by important variables. similar accuracy
system.time(
        modelRf2 <- train(train2[,vars], traindata$Survived, model="rf", trControl=train_control, ntree=100, data=train2)
)
pred2 <- predict(modelRf2, test2[,vars])
levels(pred2) = c(0,1)
submission2 <- data.frame(PassengerId = test$PassengerId, Survived = pred2)
write.csv(submission2, "./submission2.csv", quote = F, row.names = F)

# choose variable by personal judjment based on plots
vars <- c("Fare", "Pclass", "Sex", "Embarked")
head(train2[,vars])
# new model by important variables. similar accuracy
system.time(
        modelRf3 <- train(train2[,vars], traindata$Survived, model="rf", trControl=train_control, ntree=100, data=train2)
)
pred3 <- predict(modelRf3, test2[,vars])
levels(pred3) = c(0,1)
submission3 <- data.frame(PassengerId = test$PassengerId, Survived = pred3)
write.csv(submission3, "./submission3.csv", quote = F, row.names = F)

# regression
system.time(
        modellm <- train(train2[,vars], traindata$Survived, model="lm", trControl=train_control,data=train2)
)
