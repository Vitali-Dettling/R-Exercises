library(lattice)
library(ggplot2)
library(caret)
library(tm) 
library(RColorBrewer)
library(wordcloud) 
library(gmodels) 
library(e1071)  


# Read sms data
sms = read.csv("data/spam.csv", stringsAsFactors = FALSE)

# Any R object about which you want to have some information.
str(sms)

# Factor. In an experiment, the factor (also called an independent variable) 
# is an explanatory variable manipulated by the experimenter. 
# Each factor has two or more levels, i.e., different values of the factor. 
# Combinations of factor levels are called treatments.
# http://stattrek.com/statistics/dictionary.aspx?definition=factor
sms$type = factor(sms$type)
str(sms$type)
table(sms$type)

# Get the text messages from the sms
smsText = Corpus(VectorSource(sms$text))
print(smsText)
inspect(smsText[1:3])

# Clean up the messages
messageClean = tm_map(smsText, tolower)
messageClean = tm_map(messageClean, removeNumbers)
messageClean = tm_map(messageClean, removeWords, stopwords())
messageClean = tm_map(messageClean, removePunctuation)
messageClean = tm_map(messageClean, stripWhitespace)
messageClean = tm_map(messageClean, PlainTextDocument)

inspect(smsText[1:3])
inspect(messageClean[1:3])

# Create a document-term sparse matrix
smsDtm = DocumentTermMatrix(messageClean)
smsDtm

# Test and training data
smsRawTrain = sms[1:4169, ]
smsRawTest  = sms[4170:5559, ]

smsDtmTrain = smsDtm[1:4169, ]
smsDtmTest  = smsDtm[4170:5559, ]

smsTextTrain = messageClean[1:4169]
smsTextTest  = messageClean[4170:5559]

# Similar proportion check
checkTrain = prop.table(table(smsRawTrain$type))
checkTest = prop.table(table(smsRawTest$type))

meanRelativerDifferenceIsOK = 0.007
if(all.equal(checkTrain, checkTest) > meanRelativerDifferenceIsOK){
  print("Is similar, you are good to go.")
}else{
  print("Not similar needs to be checked again!")
}

# Find terms which are less then 5
freqTerms = findFreqTerms(smsDtmTrain, 5)

# Removes all the friquent terms
smsTrain = DocumentTermMatrix(smsTextTrain, list(dictionary = freqTerms))
smsTest  = DocumentTermMatrix(smsTextTest, list(dictionary = freqTerms))

# Simple check
if(NCOL(smsTextTrain) < NCOL(smsTrain) &&
   NCOL(smsTextTest) < NCOL(smsTest)){
  print("Columns has been removed successfully")
}else{
  print("No columns has been removed?")
}

# Convert counts into factor for Naive Bayes
converterOfNumbers = function(x) {
  x = ifelse(x > 0, 1, 0)
  x = factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}
column = 2
smsTrain = apply(smsTrain, MARGIN = column, converterOfNumbers)
smsTest  = apply(smsTest, MARGIN = column, converterOfNumbers)

# Visualiz all word in a cloud, most fequent words are bigger.
wordcloud(smsTextTrain, random.order = F, min.freq = 50)
wordcloud(smsTextTest, random.order = F, min.freq = 20)

# Split into spam and ham
spam = subset(smsRawTrain, type == "spam")
ham  = subset(smsRawTrain, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))


# Model training of data
model1 = naiveBayes(smsTrain, smsRawTrain$type)
# Evaluation the performance of the model
smsTestPred = predict(model1, smsTest)
CrossTable(smsTestPred, smsRawTest$type,
           prop.chisq = FALSE, 
           prop.t = FALSE, 
           prop.r = FALSE, 
           dnn = c('predicted', 'actual'))


# A better model performance (Laplace correction)
model2 = naiveBayes(smsTrain, smsRawTrain$type, laplace = 1)
smsTestPred2 = predict(model2, smsTest)
# Evaluation the performance of the second model
CrossTable(smsTestPred2, smsRawTest$type,
           prop.chisq = FALSE, 
           prop.t = FALSE, 
           prop.r = FALSE, 
           dnn = c('predicted', 'actual'))








