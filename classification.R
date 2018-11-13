# Import necessary libraries
library(dplyr)
library(readr)


ds = read_csv("companies_data.csv")

# Preprocess Dataset
## Converting appropriately categories into categorical variables (factors)
ds$Default    = as.factor(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group)

classification_model = glm(ROI ~ ., ds, )

# Visualize all the characteristics of the regression model
summary(regression_model)


# Perform prediction on the test set

# Measure performance of the model in terms of F1-Score 

# Measure performance of the model in terms of precision recall
