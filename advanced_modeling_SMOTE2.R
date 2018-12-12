# Import necessary libraries
library(dplyr)
library(readr)
library(caret)
library(pROC)
library(DMwR)
library(randomForest)
library(readr)
library(readxl)
library(DescTools)
library(rpart)
library(partykit)
library(evtree)
library(rattle)
library(rpart.plot)
library(caret)
library(RColorBrewer)
library(ggplot2)
library(aod)
library(ROCR)
library(pROC)
library(Deducer)
library(dplyr)
library(randomForest)



ds = read_csv("companies_data.csv")

# Preprocess Dataset
## Converting appropriately categories into categorical variables (factors)
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)

ds = ds %>% mutate(roiroe = ROI * ROE)
ds = ds %>% mutate(dt2 = debt_ratio^2)
ds = ds %>% mutate(leverage_ratio2 = leverage_ratio^2)
ds = ds %>% select(-debt_ratio)
ds$healthy = ifelse(ds$ROE > ds$ROI, "unhealthy", "healthy")

ds$healthy = as.factor(ds$healthy)
# Remove the nace column, it is a categorical variable too big, and should need
# a specific particular treatment
# ds = ds %>% select(-c('nace'))

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


ds.train = ds[train.index, ]
ds.train_smoted <- SMOTE(Default ~ ., as.data.frame(ds.train), k=1, perc.over = 5000, perc.under=200)



table(ds.train_smoted$Default)
prop.table(table(ds.train_smoted$Default))
newtrain = ds.train_smoted %>% filter (Default == 1)

ds.train = rbind(ds.train, newtrain)


table(ds.train$Default)
prop.table(table(ds.train$Default))

model <- train(Default ~ ., data = ds.train, method = "ranger", trControl=ctrl, tuneLength=10, preProc=c("center", "scale"))

xgb.grid <- expand.grid(nrounds = 500, #the maximum number of iterations
                        eta = c(0.01,0.1), # shrinkage
                        max_depth = c(2,6,10))
 
xgb.tune <-train(x=ds.train %>% select(-c("Default")),y=ds.train$Default,
                 method="xgbTree",
                 metric="ROC",
                 trControl=ctrl,
                 tuneGrid=xgb.grid)

predicted_Y = predict(model, ds.test)

# Measure performance of the model in terms of F1-Score for both classes
result0 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result1 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")

result0
result1


# Random Forest Plots
ds.train = ds.train %>% select(-c("nace"))
ds= ds%>% select(-c("nace"))


colnames(ds)[colnames(ds)=="leverage_ratio2"] <- "leverage_ratio_squared"
colnames(ds)[colnames(ds)=="roiroe"] <- "ROI_by_ROE"
fitrf <- randomForest(Default ~ ., data = ds.train)
# score test data
ds.test$fit_score <- predict(fitrf,type='prob',ds.test)
fitrf_pred <- prediction(ds.test$fit_score[,2],ds.test$Default)
fitrf_perf <- performance(fitrf_pred,"tpr","fpr")

# Model performance plot
pdf("~/rf_performance.pdf")
plot(fitrf_perf, lwd=2, colorize=TRUE, main="ROC Random Forest")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
dev.off()

predicted_Y = predict(fitrf, ds.test)

# Measure performance of the model in terms of F1-Score for both classes
result0 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="0")
result1 <- confusionMatrix(predicted_Y, ds.test$Default, mode="prec_recall", positive="1")

result0
result1

#plot variable importance
varImpPlot(fitrf, main="Random Forest: Variable Importance")
pdf("~/variable_importance.pdf")
dev.off()


# Model Performance plot
plot(fitrf_perf,colorize=TRUE, lwd=2, main = "fitrf ROC: Random Forest", col = "blue")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);
