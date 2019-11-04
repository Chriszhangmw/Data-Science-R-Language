
data <- read.csv("./data/xAPI-Edu-Data.csv")
table(data$Class)
library(dplyr)
library(randomForest)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(caTools)
library(party)
install.packages("psych")

data$Class <- factor(data$Class,levels=c("L","M","H"),
                     labels=c("low","medial","high"))


data <- subset(data, select = -gender )
data <- subset(data, select = -NationalITy )
data <- subset(data, select = -PlaceofBirth )
data <- subset(data, select = -StageID )
data <- subset(data, select = -GradeID )
data <- subset(data, select = -SectionID )
data <- subset(data, select = -Topic )
data <- subset(data, select = -Semester )
data <- subset(data, select = -Relation )
data <- subset(data, select = -ParentAnsweringSurvey )
data <- subset(data, select = -ParentschoolSatisfaction
 )
data <- subset(data, select = -StudentAbsenceDays )


data_x <- data[1:4]


train_x<-data_x[1:336,]
test_x<-data_x[337:480,]
train_label<-data[1:336,5]
test_label <- data[337:480,5]

prc_test_pred <- knn(train = train_x, test = test_x,cl = train_label, k=30)
library(gmodels)
CrossTable(prc_test_pred, test_label,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default'))



