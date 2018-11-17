# Import necessary libraries
library(dplyr)
library(readr)
library(caret)
library(pROC)
library(DMwR)



ds = read_csv("companies_data.csv")

# Preprocess Dataset
## Converting appropriately categories into categorical variables (factors)
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)

# Remove the nace column, it is a categorical variable too big, and should need
# a specific particular treatment
ds = ds %>% select(-c('nace', 'nace_group'))

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
ctrl <- trainControl(method = "cv", number = 10)


grid_default <- expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
)

train_control <- caret::trainControl(
    method = "none",
    verboseIter = FALSE, # no training log
    allowParallel = TRUE # FALSE for reproducible results 
)


model <- caret::train(
    Default ~ .,
    data = ds.train,
    trControl = train_control,
    tuneGrid = grid_default,
    method = "xgbTree",
    verbose = TRUE
)


model <- train(Default ~ ., data = ds.train, method = "treebag", trControl=ctrl, preProc=c("center", "scale"))


# Visualize all the characteristics of the regression model
model
summary(model)

# Perform prediction on the test set

# remember to set the type='response' in order to obrain probabilities,
# since by default it will predict log odds of probability
predictors <- names(ds.train)[names(ds.train) != 'Default']
predicted_Y = predict(model, ds.test[,predictors])

# Measure performance of the model in terms of F1-Score for both classes
result0 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result1 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")

# auc <- roc(ds.test$Default, predicted_Y)
#ds.train$Default <- as.factor(ds.train$Default)
ds.train = ds[train.index, ]
ds.train_smoted <- SMOTE(Default ~ ., as.data.frame(ds.train), k=1, perc.over = 5000, perc.under=200)



table(ds.train_smoted$Default)
prop.table(table(ds.train_smoted$Default))
newtrain = ds.train_smoted %>% filter (Default == 1)

ds.train = rbind(ds.train, newtrain)

table(ds.train$Default)
prop.table(table(ds.train$Default))

model <- caret::train(
    Default ~ .,
    data = ds.train,
    trControl = train_control,
    tuneGrid = grid_default,
    method = "xgbTree",
    verbose = TRUE
)
model <- train(Default ~ ., data = ds.train, method = "ranger", trControl=ctrl, preProc=c("center", "scale"))


predicted_Y = predict(model, ds.test)

# Measure performance of the model in terms of F1-Score for both classes
result0 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result1 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")

result0
result1

