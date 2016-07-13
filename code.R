train <- read.csv("./data/train.csv")
head(train)

library(caret)
library(reshape2)
train$Survived <- as.factor(train$Survived)
levels(train$Survived) <- c("Dead", "Survived")
train$Pclass <- as.factor(train$Pclass)

mosaicplot(train$Sex~train$Survived)
plot(train$Sex,train$Survived)
plot(train$Survived,train$Fare)
plot(train$Survived,train$Pclass)

train2 <- train[-c(1,4,9,11)]
par(mfrow=c(2,4), mai=c(.25,.25,.2,.25))
for (i in c(4:7, 2,3,8)){
        plot(train2$Survived, train2[,i], main =names(train2)[i], col=c('orange', 'blue', 'green'))
}

