library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
insurance <- read.csv("./insurance.csv")
head(insurance, n = 5)
str(insurance)
summary(insurance)
describeBy(insurance$charges,insurance$region)
ggplot(data = insurance,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges per Region")
ggplot(data = insurance,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Smoking Status")
ggplot(data = insurance,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Gender")
ggplot(data = insurance,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Boxplot of Medical Charges by Number of Children")
insurance$bmi30 <- ifelse(insurance$bmi>=30,"yes","no")
ggplot(data = insurance,aes(bmi30,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Obesity")
pairs.panels(insurance[c("age", "bmi", "children", "charges")])


ins_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance)
summary(ins_model)
insurance$age2 <- insurance$age^2
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)
ins_model2_shapley<-calc.relimp(ins_model2,type="lmg")
ins_model2_shapley
ins_model2_shapley$lmg
sum(ins_model2_shapley$lmg)
barplot(sort(ins_model2_shapley$lmg,decreasing = TRUE),col=c(2:10),main="Relative Importance of Predictors",xlab="Predictor Labels",ylab="Shapley Value Regression",font.lab=2)
