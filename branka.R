###################################################################################################
####################################### TREE MODELS ############################################
####################################################################################################


# Install the following packages if not already installed                         
#install.packages("readr")
#install.packages("readxl")
#install.packages("DescTools")
#install.packages("rpart")
#install.packages("partykit")
#install.packages("evtree")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("RColorBrewer")
#install.packages("ggplot2")
#install.packages("aod")
#install.packages("ROCR")
#install.packages("pROC")
#install.packages("Deducer")
#install.packages("dplyr")
#install.packages("randomForest")


# Call libraries
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


# Import dataset companies_data.csv 
companies_data <- read_csv("companies_data.csv")
data <- companies_data
data$Default <- as.factor(data$Default)
data$nace <- as.factor(data$nace)
data$nace_group <- as.factor(data$nace_group)

# Stratified Sampling --> considering the default variable as a strata
set.seed(1000)
div <- createDataPartition(y = data$Default, p = 0.7, list = F)

# Training Sample
data.train_1 <- data[div,] # 70% here

ds.train = data.train_1
# Test Sample
data.test_1 <- data[-div,] # rest of the 30% data goes here
ds.test = data.test_1



# First model we will use is called rpart for "Recursive Partitioning and Regression Trees" 
# and uses the CART decision tree algorithm
# The CART algorithm for classification trees uses the Gini impurity - which is
# a measure of how often a randomly chosen element from the set would be incorrectly 
# labeled if it was randomly labeled according to the distribution of labels in the subset. 
# The Gini impurity is simply the probability of obtaining two different outputs, which is an "impurity measure"


fit1 <- rpart(Default ~ ., data=data.train_1, method = "class")


# Print tree detail
printcp(fit1)

# Plot the tree
plot(fit1, margin = 0.2, main="Tree:Recursive Partitioning")
text(fit1, cex=0.8)

prp(fit1,type=2,extra=1,  main="Tree:Recursive Partitioning")

fancyRpartPlot(fit1)

# score test data
data.test_1$fit1_score <- predict(fit1,type='prob',data.test_1)
fit1_pred <- prediction(data.test_1$fit1_score[,2],data.test_1$Default)
fit1_perf <- performance(fit1_pred,"tpr","fpr")

# Model performance plot
plot(fit1_perf, lwd=2, colorize=TRUE, main="ROC Fit1: Traditional Recursive Partitioning")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);

# KS & AUC m1
fit1_AUROC <- round(performance(fit1_pred, measure = "auc")@y.values[[1]]*100, 2)
fit1_KS <- round(max(attr(fit1_perf,'y.values')[[1]]-attr(fit1_perf,'x.values')[[1]])*100, 2)
fit1_Gini <- (2*fit1_AUROC - 100)
cat("AUROC: ",fit1_AUROC,"\tKS: ", fit1_KS, "\tGini:", fit1_Gini, "\n")


# Conditional inference tree --> 
# Both rpart and ctree recursively perform univariate splits 
# of the Default variable based on values on the other variables in the dataset
# Different from the CART,ctree uses a significance test procedure 
# in order to select variables instead of selecting the variable that minimize the Gini impurity.



ds.train_smoted <- SMOTE(Default ~ ., as.data.frame(data.train_1), k=1, perc.over = 5000, perc.under=200)
ds.train_smoted <- SMOTE(nace_group ~ ., as.data.frame(ds.train), k=1, perc.over = 1200, perc.under=200)



table(ds.train_smoted$Default)
prop.table(table(ds.train_smoted$Default))
newtrain = ds.train_smoted %>% filter (Default == 1)

ds.train = rbind(ds.train, newtrain)

table(ds.train$Default)
prop.table(table(ds.train$Default))
data.train_1 = ds.train

# Summary
fit2

fit2 <- ctree(Default ~ ., data=data.train_1)

# This is essentially a decision tree but with extra information in the terminal nodes.
plot(fit2, gp = gpar(fontsize = 6),     # font size set to 6
     inner_panel=node_inner,
     ip_args=list(abbreviate = TRUE, 
                  id = FALSE))

# score test data
data.test_1$fit2_score <- predict(fit2,type='prob',data.test_1)
fit2_pred <- prediction(data.test_1$fit2_score[,2],data.test_1$Default)
fit2_perf <- performance(fit2_pred,"tpr","fpr")

# Model performance plot
plot(fit2_perf, lwd=2, colorize=TRUE, main="ROC Fit2: Conditional Inference Tree")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);

# KS & AUC m1
fit2_AUROC <- round(performance(fit2_pred, measure = "auc")@y.values[[1]]*100, 2)
fit2_KS <- round(max(attr(fit2_perf,'y.values')[[1]]-attr(fit2_perf,'x.values')[[1]])*100, 2)
fit2_Gini <- (2*fit2_AUROC - 100)
cat("AUROC: ",fit2_AUROC,"\tKS: ", fit2_KS, "\tGini:", fit2_Gini, "\n")


#========================= Random Forest ===============================#

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


#KS & AUC  fit5
fit3_AUROC <- round(performance(fit3_pred, measure = "auc")@y.values[[1]]*100, 2)
fit3_KS <- round(max(attr(fit3_perf,'y.values')[[1]] - attr(fit3_perf,'x.values')[[1]])*100, 2)
fit3_Gini <- (2*fit3_AUROC - 100)
cat("AUROC: ",fit3_AUROC,"\tKS: ", fit3_KS, "\tGini:", fit3_Gini, "\n")


#Compare ROC Performance of Models
plot(fit1_perf, col='blue', lty=1, main='ROCs: Model Performance Comparision') # Logistic regression
plot(fit2_perf, col='grey',lty=2, add=TRUE); # Ctree
plot(fit3_perf, col='red',lty=3, add=TRUE); # Random forest
lines(c(0,1),c(0,1),col = "green", lty = 4 ) # random line

legend(0.6,0.5,
       c('Fit1: Logistic reg','Fit2: CTree','Fit3:Random forest', 'Random line'),
       col=c('blue','grey', 'red', 'green'),
       lwd=3);



######## EVTREE #########

# Yet another alternative way to search over the parameter space of trees is to use global optimization 
# methods like evolutionary algorithms. 
# The 'evtree' package implements an evolutionary algorithm for learning 
# globally optimal classification and regression trees

fit4 <- evtree(Default ~ ., data = data.train_1, maxdepth = 5)

# Plot
plot(fit4, gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner)

# score test data
data.test_1$fit4_score <- predict(fit4,type='prob',data.test_1)
fit4_pred <- prediction(data.test_1$fit4_score[,2],data.test_1$Default)
fit4_perf <- performance(fit4_pred,"tpr","fpr")

# Model performance plot
plot(fit4_perf, lwd=2, colorize=TRUE, main="ROC Fit4: EV Tree")
lines(x=c(0, 1), y=c(0, 1), col="red", lwd=1, lty=3);

# KS & AUC m1
fit4_AUROC <- round(performance(fit4_pred, measure = "auc")@y.values[[1]]*100, 2)
fit4_KS <- round(max(attr(fit4_perf,'y.values')[[1]]-attr(fit4_perf,'x.values')[[1]])*100, 2)
fit4_Gini <- (2*fit4_AUROC - 100)
cat("AUROC: ",fit4_AUROC,"\tKS: ", fit4_KS, "\tGini:", fit4_Gini, "\n")
