# Example of Regression, using the classical linear regression model and
# predicting ROI as a function of all the other variables

# Import necessary libraries
library(dplyr)
library(readr)
library(caret)
library(modelr)


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
train.index <- createDataPartition(ds$ROI, p = .7, list = FALSE)

ds.train = ds[train.index, ]
ds.test  = ds[-train.index, ]


# We build a regression model which can be used to predict ROI, so ROI
# will be our dependent variable, change ROI to whatever other real number variable
# to create a model which is predicting something else
regression_model = lm(ROI ~ ., ds.train)

# Visualize all the characteristics of the regression model
summary(regression_model)


# Perform prediction on the test set
predicted_Y <- predict(regression_model, ds.test) 

# Measure performance of the model
rmse(regression_model, ds.test)
rsquare(regression_model, ds.test)



