library(lattice)
library(ggplot2)
library(caret)

# Normalization of the values
normalize = function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}

# Dowloading Data
# https://archive.ics.uci.edu/ml/datasets/Statlog+(Vehicle+Silhouettes)
url = "https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/vehicle/xaa.dat"
veh = read.csv(file=url, header = T)

# TDOO: Clean up of the data needs to be done, here, first
veh = read.csv("data/xaa.csv", header = TRUE, sep = " ", quote = "\"")
str(veh)

# Spliting data into training and test set
set.seed(825)
training = createDataPartition(veh$Class, p = .75, list = FALSE)
dfTrain = veh[training,]
dfTest  = veh[-training,]
table(dfTrain$Class)
table(dfTest$Class)

round(prop.table(table(dfTrain$Class)),2)
round(prop.table(table(dfTest$Class)),2)

# Is it important to normalize numeric data for k-NN?
# If "yes" why?
# Usually, we want to compare scores, against each other. Althought, it happens that 
# the scores are not at the same scale and thus, needs to be normalized, 
# i.e. the units will disappear.

set.seed(825)
ctrl = trainControl(method = "repeatedcv", repeats = 5) 

# List of all possible models
# http://topepo.github.io/caret/train-models-by-tag.html
tuneLengthVar = 3
knn = train(Class ~ ., data = dfTrain, method="rf", trControl = ctrl, tuneLength = tuneLengthVar)
knn

# http://topepo.github.io/caret/train-models-by-tag.html
# Search after: method = 'rf' (Random Forest)
# Tuning parameters:
# mtry (#Randomly Selected Predictors)

# Train a model with normalization 
# this applies the function normalize to columns 1-18
dfTrainN = as.data.frame(lapply(dfTrain[1:18], FUN=normalize))
dfTestN = as.data.frame(lapply(dfTest[1:18], FUN=normalize))
dfTrainN
dfTestN

# Added the class labels again. 
# It was removed because of the normalization. 
dfTrainN$Class = veh[training, 19]
dfTestN$Class = veh[-training, 19]
dfTrainN
dfTestN

knn.n = train(Class ~ ., data = dfTrainN, method="rf", trControl = ctrl, tuneLength = tuneLengthVar)
knn.n

# z-Transformation
dfTrainZ = as.data.frame(scale(dfTrain[-19]))
dfTestZ = as.data.frame(scale(dfTest[-19]))
dfTrainZ$Class = veh[training, 19]
dfTestZ$Class = veh[-training, 19]
dfTrainZ
dfTestZ

knn.z = train(Class ~ ., data = dfTrainN, method="rf", trControl = ctrl, tuneLength = tuneLengthVar)
knn.z

# What does pre-process mean?
# This function can be used for centering and scaling, 
# imputation, applying the spatial sign transformation 
# and feature extraction via principal component analysis 
# or independent component analysis.
# http://topepo.github.io/caret/model-training-and-tuning.html#basic-parameter-tuning

# Comparission of the Models 
knnPredict = predict(knn, newdata = dfTest)
confusionMatrix(knnPredict, dfTest$Class )

knnPredictN = predict(knn.n, newdata = dfTestN)
confusionMatrix(knnPredictN, dfTestN$Class )

knnPredictZ = predict(knn.z, newdata = dfTestZ)
confusionMatrix(knnPredictZ, dfTestZ$Class )
