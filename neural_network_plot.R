##########################
##### NEURAL NETWORKS ####
##########################

# Clear the environment 
rm(list=ls())


# Install packages by removing the # and running the lines 
#install.packages("readxl")
#install.packages("readr")
#install.packages("ggplot2")                                  
#install.packages("nnet")
#install.packages("neuralnet")


# Call libraries 
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(nnet)
library(neuralnet)


# Import data on the return of 50 Italian ETF (Exchange trade funds) 
# over a period of 31 days. File name: "ETF_Italian_31days"
# Process: Environment --> Import Dataset --> From Text (readr) --> Browse the file --> Import


# Or set the working directory (where your csv is -- Session --> Set working directory --> Choose Directory)

# and run the following line
ds <- read_csv("NN/companies_data.csv")

ds$Default    = as.numeric(ds$Default)
ds$nace       = as.factor(ds$nace)
ds$nace_group = as.factor(ds$nace_group) 

# Make a copy which we use for the analysis                                  

ds = ds %>% select(-c("nace", "nace_group"))

ds = as.data.frame(ds)
n = names(ds)

formula <- as.formula(paste("Default ~", paste(n[!n %in% "Default"], collapse = " + "))) 


# Print to check the formula that we will apply in the following step
formula


# Let's fit a neural network with the full dataset. 
set.seed(123)
nn5 <- neuralnet(formula,
                 data = ds,
                 hidden = c(5),
                 act.fct = "logistic",
                 linear.output = FALSE,
                 lifesign = "minimal")


# Plot the neural network hidden = 5
plot(nn5)


