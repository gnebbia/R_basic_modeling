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
ds = ds %>% select(-c('nace'))

# Split dataset into training and testing
## In this case we are performing a 70/30 split, so we will
## use the 70% of the dataset to build the model (training phase)
## and the remaining 30% of the dataset to assess the performance of the builded
## model (testing phase)
train.index <- createDataPartition(ds$Default, p = .7, list = FALSE)

ds.train = ds[train.index, ]
ds.test  = ds[-train.index, ]


# We build a classification model which can be used to predict Default,
# by using method = "ranger" we use random forests
# we can change algorithm by changing the string in method, we just need
# to consult the caret documentation
ctrl <- trainControl(method = "cv", number = 5)


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
ds.train_smoted <- SMOTE(nace_group ~ ., as.data.frame(ds.train), k=1, perc.over = 1200, perc.under=200)



table(ds.train_smoted$Default)
prop.table(table(ds.train_smoted$Default))
newtrain = ds.train_smoted %>% filter (Default == 1)

ds.train = rbind(ds.train, newtrain)

table(ds.train$Default)
prop.table(table(ds.train$Default))

table(ds.train$nace_group)
prop.table(table(ds.train$nace_group))

model <- caret::train(
    Default ~ .,
    data = ds.train,
    trControl = train_control,
    tuneGrid = grid_default,
    method = "xgbTree",
    verbose = TRUE
)
model <- train(Default ~ ., data = ds.train, method = "ranger", trControl=ctrl, preProc=c("center", "scale"))
model <- train(nace_group ~ ., data = ds.train, method = "ranger", trControl=ctrl, preProc=c("center", "scale"))


predicted_Y = predict(model, ds.test)
predicted_Y = predict(model$finalModel, newdata=ds.test) 

library(pROC)
selectedIndices <- model$pred$mtry == 2
plot.roc(model$pred$obs[selectedIndices], model$pred$M[selectedIndices])
#plot(roc(test_set$bad_widget, predicted_Y, direction="<"), col="yellow", lwd=3, main="ROC Curve by using caret random forests algorithm")

# Measure performance of the model in terms of F1-Score for both classes
result0 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result1 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")

result0
result1


# Random Forest Plots
data.train_1 = data.train_1 %>% select(-c("nace"))
fit3 <- randomForest(Default ~ ., data = data.train_1)

fit3_fitForest <- predict(fit3, newdata = data.test_1, type="prob")[,2]
fit3_pred <- prediction(fit3_fitForest, data.test_1$Default)
fit3_perf <- performance(fit3_pred, "tpr", "fpr")

#plot variable importance
varImpPlot(fit3, main="Random Forest: Variable Importance")

# Model Performance plot
plot(fit3_perf,colorize=TRUE, lwd=2, main = "fit3 ROC: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
