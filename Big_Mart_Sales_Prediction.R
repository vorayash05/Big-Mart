 #Regression Analysis
library(tidyverse)
library(modeest)
#Importing and Pre processing the dataset
training_set <-  read.csv('Big_Mart_Train.csv')
test_set <- read.csv('Big_Mart_Test.csv')
test_set$Item_Outlet_Sales <- NA
dataset <- rbind(training_set, test_set)
t_id <- data.frame(test_set$Item_Identifier,test_set$Outlet_Identifier)
dataset = dataset[c(-1,-7)]
#Checking the summary of the dataset
summary(dataset)

# Replacing level names as ordered in the summary (alphabetically)
levels(dataset$Item_Fat_Content) <- c("LF","LF", "LF","REG", "REG") 
levels(dataset$Item_Type) <- c("Food","Food","Food","Food","Food","Food","Food","Drinks","Non-Consumables","Non-Consumables","Food",
                               "Non-Consumables", "Food", "Food", "Drinks", "Food" )
#Encoding Categorical Data
dataset$Item_Type = factor(dataset$Item_Type,
                           levels = c("Food", "Drinks", "Non-Consumables"),
                           labels = c(1,2,3))
                           
dataset$Outlet_Location_Type = factor(dataset$Outlet_Location_Type,
                                       levels = c('Tier 1', 'Tier 2', 'Tier 3'),
                                       labels = c(1,2,3))
dataset$Outlet_Size = factor(dataset$Outlet_Size,
                                      levels = c('Small', 'Medium', 'High'),
                                      labels = c(1,2,3)) 
dataset$Outlet_Type = factor(dataset$Outlet_Type,
                                      levels = c('Grocery Store', 'Supermarket Type1', 'Supermarket Type2',
                                                 'Supermarket Type3'),
                                      labels = c(1,2,3,4))
dataset$Item_Fat_Content = factor(dataset$Item_Fat_Content,
                             levels = c('LF','REG'),
                             labels = c(1,2))

#Taking care of Missing Data
dataset$Item_Weight = ifelse(is.na(dataset$Item_Weight),
                             ave(dataset$Item_Weight, FUN = function(x) median(x, na.rm = TRUE)),
                             dataset$Item_Weight)

dataset$Outlet_Size = ifelse(is.na(dataset$Outlet_Size),
                             mfv(dataset$Outlet_Size, FUN = function(x) mfv(x, na.rm = TRUE)),
                             dataset$Outlet_Size)

dataset$Item_Visibility = ifelse(dataset$Item_Visibility == 0.0,
                             ave(dataset$Item_Visibility, FUN = function(x) mean(x, na.rm = TRUE)),
                             dataset$Item_Visibility)
#Calculating Age of the Stores
dataset$Outlet_Establishment_Year = 2013 - dataset$Outlet_Establishment_Year

#Fitting Regression Model
library(randomForest)
training_set <- dataset[1:8523,]
test_set <- dataset [8524:14204,]
regressor = randomForest(x = training_set[-10],
                         y = training_set$Item_Outlet_Sales,
                         ntree = 500)
#Predicting results
y_pred = predict(regressor,test_set[-10])
results <- data.frame(t_id, y_pred)
write.csv(results, "sample_sub.csv")