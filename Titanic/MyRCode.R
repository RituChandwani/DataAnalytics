# Set working directory and import datafiles
setwd("~/RWorkshop/Titanic")

library(readr)
test <- read_csv("test.csv")
View(test)

library(readr)
train <- read_csv("train.csv")

str(train)
table(train$Survived)
prop.table(table(train$Survived))

# Simple Model - Nobody survived
test$Survived <- rep(0, 418)
View(test)

submit <- data.frame(PassengerId=test$PassengerId,Survived=test$Survived)
write.csv(submit,file="theyallPerish.csv",row.names=FALSE)

summary(train$Sex)
prop.table(table(train$Sex,train$Survived),1)
test$Survived <- 0
#Female Survived
test$Survived[test$Sex == 'female'] <- 1

summary(train$Age)

train$child <- 0
train$child[train$Age < 18] <- 1
aggregate(Survived ~ child + Sex, data=train, FUN=sum)
aggregate(Survived ~ child +Sex,data=train, FUN =length)
# this function shows that our prediction of female survived regardless of it is a child or no is true
aggregate(Survived ~ child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

# Analysis on fare column data - divide it into categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
# observation : female of pclass 3 even with expensive tickets died

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20] <- 0

library(rpart)
?rpart

fit <- rpart(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data=train, method="class")
plot(fit)
text(fit)

install.packages('rattle')
install.packages('rpart.plot')
install.packages('RColorBrewer')

library(rattle)
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(fit)
Prediction <- predict(fit,test,type="class")
Prediction
submit <- data.frame(PassengerId = test$PassengerId, Survived= Prediction)
write.csv(submit, file="myfirstDtree.csv",row.names = FALSE)

?rpart.control
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class", 
             control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)
new.fit <- prp(fit,snip=TRUE)$obj

fancyRpartPlot(new.fit)

# Feature Engineering on Name Title
test$Survived <- NA
combi <- rbind(train,test)
summary(train)
summary(test)

within(train, rm(child,Fare2))
summary(train)
train <- subset(train, select = -c(child) )
train <- subset(train, select = -c(Fare2) )

combi$Name <- as.character(combi$Name)
combi$Title <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
combi$Title <- sub(' ', '', combi$Title)
table(combi$Title)

combi$Title[combi$Title %in% c('Mme', 'Mlle')] <- 'Mlle'
combi$Title[combi$Title %in% c('Capt', 'Don', 'Major', 'Sir')] <- 'Sir'
combi$Title[combi$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] <- 'Lady'
combi$Title <- factor(combi$Title)

combi$FamilySize <- combi$SibSp + combi$Parch + 1
combi$Surname <- sapply(combi$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
combi$FamilyID <- paste(as.character(combi$FamilySize), combi$Surname, sep="")
combi$FamilyID

combi$FamilyID[combi$FamilySize <= 2] <- 'Small'
famIDs <- data.frame(table(combi$FamilyID))
famIDs

train <- combi[1:891,]
test <- combi[892:1309,]


fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + Title + FamilySize + FamilyID,
             data=train, 
             method="class")
fancyRpartPlot(fit)
