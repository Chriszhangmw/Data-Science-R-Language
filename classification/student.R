library(ggplot2)
install.packages("party")
library(dplyr)
library(randomForest)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(caTools)
library(party)
data <- read.csv("./data/xAPI-Edu-Data.csv")
str(data)
summary(data)
ggplot(data = data, aes(x = raisedhands)) + geom_histogram(color = "red") +
  scale_x_continuous(breaks = seq(0,100,5)) + 
  labs(x = "Raised Hands", y = "Student Count")
ggplot(data = data, aes(x = raisedhands, color = gender)) + geom_density()
ggplot(data = data, aes(x = gender)) + geom_bar() + 
  labs(x = "Gender", y = "Student Count") +
  scale_y_continuous(breaks = seq(0,300,30)) + coord_flip()
ggplot(data = data, aes(x = PlaceofBirth)) + geom_bar(aes(fill = NationalITy)) + 
  labs(x = "Birth Place", y = "Student Count") + coord_flip()
ggplot(data = data, aes(x = Topic, fill = gender)) + geom_bar() +
  labs(x = "Topic", y = "Student Count") +
  scale_y_continuous(breaks = seq(0,100,4)) + coord_flip()
ggplot(data = data, aes(x = gender, y = raisedhands)) + geom_boxplot()
ggplot(data = data, aes( x = raisedhands, y = VisITedResources)) + geom_point() +
  geom_smooth(method = "lm")
ggplot(data = data, aes(x = raisedhands, color = gender)) + geom_density()
set.seed(55)
split <- sample.split(data$Class, SplitRatio = 0.7)
train <- subset(data, split == T)
cv <- subset(data, split == F)
library(C50)
model <- C5.0(train,train$Class)
summary(model)
prediction <- predict(model, cv)
library(gmodels)
CrossTable(prediction, cv$Class,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default'))
