library(ISLR)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
library(iterators)
library(parallel)
library(doParallel)

str(OJ)
head(OJ)

# Spliting data as training and test set. 
set.seed(1711)
inTraining <- createDataPartition(OJ$Purchase, p = .75, list = FALSE)
OJ.train <- OJ[inTraining,]
OJ.test  <- OJ[-inTraining,]
table(OJ.train$Purchase)
table(OJ.test$Purchase)

round(prop.table(table(OJ.train$Purchase)),2)
round(prop.table(table(OJ.test$Purchase)),2)

# Linear Model
set.seed(1711)
svm.linear = svm(Purchase ~ ., OJ.train, kernel = 'linear')

summary(svm.linear)

# Evaluation
train.pred = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.pred)

conf <- confusionMatrix(train.pred, OJ.train$Purchase, dnn = c("Prediction", "Actual"))
conf$table
round(conf$overall,2)
round(conf$byClass,2)

test.pred = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.pred)
conf <- confusionMatrix(test.pred, OJ.test$Purchase, dnn = c("Prediction", "Actual"))
conf$table
round(conf$overall,2)
round(conf$byClass,2)


# Tune Model
# 
set.seed(1711)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, 
                kernel = "linear", 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))
summary(tune.out)

# Train tuned linear model

svm.linear = svm(Purchase ~ ., kernel = "linear", data = OJ.train, 
                 cost = tune.out$best.parameters$cost)

train.linear = predict(svm.linear, OJ.train)
table(OJ.train$Purchase, train.linear)

test.lineal = predict(svm.linear, OJ.test)
table(OJ.test$Purchase, test.lineal)

# SVM Radial Model 
set.seed(1771)
svm.radial = svm(Purchase ~ ., OJ.train, kernel = 'radial') 

train.radial = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.radial)

summary(tune.out)

# Tune Radial Model 
set.seed(1711)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, 
                kernel = "radial", 
                ranges = list(cost = 10^seq(-2, 1, by = 0.25)))

train.pred = predict(tune.out, OJ.train)
table(OJ.train$Purchase, train.pred)

summary(tune.out)

# Train tuned radial model 
svm.radial = svm(Purchase ~ ., kernel = "radial", data = OJ.train, 
                 cost = tune.out$best.parameters$cost)

train.pred = predict(svm.radial, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.radial, OJ.test)
table(OJ.test$Purchase, test.pred)

# SVM Ploynomial Model 
set.seed(1711)
svm.ploy = svm(Purchase ~ ., OJ.train, kernel = 'polynomial') 

train.pred = predict(svm.ploy, OJ.train)
table(OJ.train$Purchase, train.pred)


# Tune Model 
set.seed(1711)
tune.out = tune(svm, Purchase ~ ., data = OJ.train, 
               kernel = "polynomial", 
               ranges = list(cost = 10^seq(-2, 1, by = 0.25)))

train.ploy = predict(tune.out, OJ.train)
table(OJ.train$Purchase, train.ploy)

summary(tune.out)

svm.poly = svm(Purchase ~ ., kernel = "polynomial", data = OJ.train, 
                            cost = tune.out$best.parameters$cost)

train.pred = predict(svm.ploy, OJ.train)
table(OJ.train$Purchase, train.pred)

test.pred = predict(svm.ploy, OJ.test)
table(OJ.test$Purchase, test.pred)

