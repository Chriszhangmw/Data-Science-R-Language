wineRed = read.csv("./data/winequality-red.csv", header = TRUE, sep=",", na.strings= "*")
set.seed(4242)
wineRed$quality = lapply(wineRed[,12], function (x)
{
  if(x > 5)  { "good"}
  else { "bad"}   
})
wineRed$quality <- factor(wineRed$quality,levels=c("good","bad"),
                     labels=c("G","B"))
library(dplyr)
library(randomForest)
library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(caret)
library(caTools)
library(party)
data_x <- wineRed[1:11]

train_x<-data_x[1:1120,]
test_x<-data_x[1121:1599,]
train_label<-wineRed[1:1120,12]
test_label <- wineRed[1121:1599,12]

prc_test_pred <- knn(train = train_x, test = test_x,cl = train_label, k=30)
library(gmodels)
CrossTable(prc_test_pred, test_label,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('predicted default', 'actual default'))
