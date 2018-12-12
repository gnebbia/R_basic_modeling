# Example of Classification, using the logistic regression model and
# predicting Default as a function of all the other variables

# Import necessary libraries
library(dplyr)
library(readr)
library(caret)
#library(randomForest)


ds = read_csv("companies_data.csv")

# Preprocess Dataset
## Converting appropriately categories into categorical variables (factors)
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)

# Remove the nace column, it is a categorical variable too big, and should need
# a specific particular treatment
ds = ds %>% select(-c('nace'))

# Split dataset into training and testing
## In this case we are performing a 70/30 split, so we will
## use the 70% of the dataset to build the model (training phase)
## and the remaining 30% of the dataset to assess the performance of the builded
## model (testing phase)
train.index <- createDataPartition(ds$Default, p = .7, list = FALSE)

ds.train = ds[train.index, ]
ds.test  = ds[-train.index, ]


# We build a classification model which can be used to predict Default, so Default
# will be our dependent variable, change Default to whatever other categorical variable
# to create a model which is predicting something else
classification_model = glm(Default ~ ., family=binomial(), data=ds.train)

# Visualize all the characteristics of the classification model
summary(classification_model)


# Perform prediction on the test set
# remember to set the type='response' in order to obrain probabilities,
# since by default it will predict log odds of probability
predicted_Y_p = predict(classification_model, ds.test, type="response")

# We convert probability prediction to a categorical variable, (0 or 1),
# in order to compare it to the real response data contained in ds.test$Default
predicted_Y   = factor(ifelse(predicted_Y_p > 0.5, 1, 0), levels=c(0,1))

# Measure performance of the model in terms of F1-Score and precision/recall
result <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")



# Another approach with random forest
model <- randomForest(Default ~ ., data=ds.train, importance=TRUE, ntree=500, mtry = 2, do.trace=100)
predicted_Y = predict(model, ds.test)

result0 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result1 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")

result0
result1

reprtree:::plot.getTree(model)
