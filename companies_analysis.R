# Example of Modeling using caret

# Import necessary libraries
library(dplyr)
library(readr)
library(caret)
library(corrplot)
library(ggplot2)
library(reshape2)


ds = read_csv("companies_data.csv")

# Preprocess Dataset
## Converting appropriately categories into categorical variables (factors)
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)

# Remove the nace column, it is a categorical variable too big, and should need
# a specific particular treatment
ds = ds %>% select(-c('nace','nace_group'))
ds = ds %>% select(-c('nace'))
names(ds)


ds.m <- melt(ds, id.var = "Default")
ggplot(data = ds.m, aes(x=variable, y=value)) + geom_boxplot(aes(fill=Default))
# Split dataset into training and testing
## In this case we are performing a 70/30 split, so we will
## use the 70% of the dataset to build the model (training phase)
## and the remaining 30% of the dataset to assess the performance of the builded
## model (testing phase)
train.index <- createDataPartition(ds$Default, p = .8, list = FALSE)

ds.train = ds[train.index, ]
ds.test  = ds[-train.index, ]


# We build a classification model which can be used to predict Default,
# by using method = "ranger" we use random forests
# we can change algorithm by changing the string in method, we just need
# to consult the caret documentation
model <- train(Default ~ ., data = ds.train, method = "ranger")

# Visualize all the characteristics of the regression model
model
summary(model)

# Perform prediction on the test set
# remember to set the type='response' in order to obrain probabilities,
# since by default it will predict log odds of probability
predicted_Y = predict(model, ds.test)

# Measure performance of the model in terms of F1-Score 
result <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")
