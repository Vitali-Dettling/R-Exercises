library(Matrix)
library(arules)    
library(arulesViz) 

# Information:
# https://data-science-blog.com/blog/2016/10/08/warenkorbanalyse-in-r/
# http://www.ms.unimelb.edu.au/~odj/Teaching/dm/arules_examples_Graham_Williams.pdf
  
# Support:
# Support ist das Signifikanzmaß der Regel. 
# Wie of eine Regel (e.g.: {Bier, Chips, Windel}) in einer Iteration enthalten sein muss.
# Regel kommt 2 mal vor aber support is > 3, somit rausgenommen.

# Confidence: 
# Confidence ist das Qualitätsmaß der Regel. 
# Es beschreibt, wie oft die Regel richtig ist. 

# Lift: (Bedeutung der Regel)
# Wie oft die Confidence den Erwartungswert übersteigt. 

# Description:
# The Groceries data set contains 1 month (30 days) of real-world point-of-sale transaction data from
# a typical local grocery outlet. The data set contains 9835 transactions and the items are aggregated
# to 169 categories.
tx = read.transactions("data/groceries.csv", sep = ",",  format = "basket")

# look at the first five transactions
inspect(tx[1:5])
inspect(tx)
image(tx)

# Summary
summary(tx)
# Items: 169
nnzero(itemInfo(tx))
# Avarage/Mean: ~4.4 items

# Items frequency: 
tx[, 1:3]
inspect(tx[, 1:3])
summary(tx[, 1:3])
itemFrequency(tx[, 1:3], type='absolute')
??itemFrequency
# Plot frequency
itemFrequencyPlot(tx, support = 1000, type='absolute') 

#Plot topN
itemFrequencyPlot(tx, topN = 3, type='absolute')

# visualize sparse matrix
image(tx[1:30,])
image(sample(tx, 100))

# default settings
apriori(tx)
??apriori
# support: 0.1
# confidence: 0.8
# rules: 0

# Interested in items which are purchased twice a day.
# 9835/30 ~ 327 Tx/Day 
# 2/327 = 6x10^-3 
# Support: 0.006
apriori(tx, parameter = list(support = 0.006, confidence = 0.65))
# min_confident: 0.65

# Start with a confidence threshold of 0.25
# Set minlen = 2 as we are only interested in in rules with two or more items
groceriesrules = apriori(tx, parameter = list(minlen = 2, support = 0.006, confidence = 0.25))

# Summery and first 3 rules
summary(groceriesrules)
inspect(groceriesrules[1:3])

# Sort rules by lift
inspect(sort(groceriesrules, by='lift')[1:3])

# Finding rules containing berry items
berryrules = subset(groceriesrules, items %in% "berries")
inspect(berryrules)

write(groceriesrules, file = "data/groceriesrules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)
