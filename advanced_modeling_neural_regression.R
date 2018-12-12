# Example of Modeling using caret

# Import necessary libraries
library(dplyr)
library(readr)
library(caret)
library(neuralnet)


ds = read_csv("~/Desktop/exchanges.csv")

# Split dataset into training and testing
## In this case we are performing a 70/30 split, so we will
## use the 70% of the dataset to build the model (training phase)
## and the remaining 30% of the dataset to assess the performance of the builded
## model (testing phase)
train.index <- createDataPartition(ds$Date, p = .8, list = FALSE)

ds.train = ds[train.index, ]
ds.test  = ds[-train.index, ]


# We build a classification model which can be used to predict Default,
# by using method = "nnet" we use neural networks
# we can change algorithm by changing the string in method, we just need
# to consult the caret documentation

# Here we set the parameters of the neural network, so it should have 5 hidden
# units and a learning rate of 0.2
num_hidden_layers
t.grid = expand.grid(size=num_hidden_layers, decay=0.2)

# Now we build the model and apply the following preprocessing steps:
# 1) To decorrelate variables, 2) Scaling and 3) Centering
# These preprocessing steps on data improve the overall neural network
# classifier
model <- train(Default ~ ., data = ds.train, preProcess = c("pca","scale","center"), method = "nnet", linout = 0, tuneGrid = t.grid)

# Plot the neural network model
n = names(ds.train)
formula <- as.formula(paste("Default ~", paste(n[!n %in% "Default"], collapse = " + ")))



# Plot the neural network hidden = 5, this is just the graph of the network used
# for the classification problem
plot(model)


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



