library(readr) 
library(ggplot2)
library(ggcorrplot)
library(plotly)
library(reshape2)
library(dplyr)
library(tidyverse)
library(data.table)
library(skimr)
library(psych)
library(corrplot)
library(glmnet)
library(caret)
library(package = "lattice")

#load data
house <- read.csv("https://raw.githubusercontent.com/alazowski/810-Machine-Learning-Project/main/kc_house_data%203%20(1).csv")

#examine the dataset
str(house) #21613 obs. of  21 variables
summary(house) # Mean price: 540088
skim(house) # no mmissing values

# find missing values 
missmap(house, col=c('yellow','black'), y.at= 1, y.labels= ' ', legend= TRUE)

#change year of renovation to boolean. (0 for no renovation, 1 for renovation)
house$yr_renovated  [house$yr_renovated  > 0] <- 1
house$yr_renovated  [house$yr_renovated  == 0] <- 0

##Exploratory Analysis: 
#see the relationship between house price and number of bedrooms
ggplot(house, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple') #one outlier in the plot 

#removing the extreme outliers
room_outlier <-
  house %>% filter(bedrooms >= 30) #filter the entire row

house2 <- house[!(house$bedrooms > 30), ]

ggplot(house2, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple')

#condition and grade is to determined the quality of the house (pick one)
drops <- c('grade', 'zipcode')
house3 <- house2[,!(names(house2) %in% drops)]

summary(house3)

#The average price of the house: 
Avg_house_price <- mean(house3$price)
Avg_house_price


#price distribution: 
ggplot(house3, aes(x= price)) + geom_histogram(binwidth = 10)

#relationship with price and square foot space
ggplot(house3, aes(x = sqft_living, y = price)) + geom_point(alpha = 0.2)

#plot sqft_lot and price
ggplot(house3, aes(x = sqft_lot, y = price)) + geom_point(alpha = 0.2)

#relationship with price and conditions
ggplot(house3, aes(x = condition)) + geom_bar(alpha = 0.5)
#why does the highest condition house has lower range? 
#could there be other factor that influence the price of the house?

#relationship with price and square foot space
ggplot(final, aes(x = sqft_above, y = price)) + geom_point(alpha = 0.2)

#bathroom and Price
ggplot(final, aes(x = bathrooms, y = price)) + geom_point(alpha = 0.2)

#plot sqft_lot and price
ggplot(final, aes(x = sqft_lot, y = price)) + geom_point(alpha = 0.2)

ggplot(final, aes(x = sqft_living, y = price)) + geom_point(alpha = 0.2)

final <- house3[, 3:17]
M = cor(final)
corrplot(M, method='color') # colorful number
#numerical correlation matrix https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/
correlationMatrix <- cor(final)
correlationMatrix
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#visualizing density of variables 
house3 %>% ggplot(aes(price))+stat_density()+theme_bw()
house3 %>% ggplot(aes(sqft_living))+stat_density()+theme_bw()
house3 %>% ggplot(aes(sqft_living15))+stat_density()+theme_bw()

library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)

#split the data set to train and test:
set.seed(1234)
rnorm(1)
test_set <- sample(nrow(final), nrow(final) * 0.8)
house_train <- final[test_set, ]
house_test <- final[-test_set, ]

# independent variables:
x_train <- data.matrix(house_train[, c('bedrooms', 'bathrooms', 'sqft_living', 'floors', 'waterfront', 'view', 'condition', 'sqft_above', 'sqft_basement', 'yr_renovated')])
# dependent variables:
y_train <- house_train['price'][0:17289,]

#independent variable for test:
x_test <- data.matrix(house_test[, c('bedrooms', 'bathrooms', 'sqft_living', 'floors', 'waterfront', 'view', 'condition', 'sqft_above', 'sqft_basement', 'yr_renovated')])
# dependent variables for test:
y_test <- house_test['price'][0:4323,]


# Setting the range of lambda values
lambda_seq <- 10^seq(2, -2, by = -.1)
# Using glmnet function to build the ridge regression in r
fit <- glmnet(x_train, y_train, alpha = 0, lambda  = lambda_seq)
#check the model 
summary(fit)

# Using cross validation glmnet
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, lambda = lambda_seq)
summary(ridge_cv) 

# Best lambda value
best_lambda <- ridge_cv$lambda.min
plot(ridge_cv)

best_fit <- ridge_cv$glmnet.fit
head(best_fit)

# Rebuilding the model with optimal lambda value
best_ridge <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)

#coefficient 
coef(best_ridge)

#Y predict 
yhat_train_ridge <- predict(best_ridge, x_train, s= best_ridge$best_lambda)
mse_train_ridge <- mean((y_train - yhat_train_ridge)^2)
mse_train_ridge

yhat_test_ridge <- predict(best_ridge, x_test, s= best_ridge$best_lambda)
mse_test_ridge <- mean((y_test - yhat_test_ridge)^2)
mse_test_ridge

#lasso regression: 
lambda_seq <- 10^seq(2, -2, by = -.1)

# Using glmnet function to build the ridge regression in r
fit <- glmnet(x_train, y_train, alpha = 1, lambda  = lambda_seq)
#check the model 
summary(fit)

# Using cross validation glmnet
lasso_cv <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambda_seq)
summary(lasso_cv) 

# Best lambda value
best_lambda <- lasso_cv$lambda.min
plot(lasso_cv)

best_fit <- lasso_cv$glmnet.fit
head(best_fit)

# Rebuilding the model with optimal lambda value
best_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda)

#coefficient 
coef(best_lasso)

#Y predict 
yhat_train_lasso <- predict(best_lasso, x_train, s= best_lasso$best_lambda)
mse_train_lasso <- mean((y_train - yhat_train_lasso)^2)
mse_train_lasso

yhat_test_lasso <- predict(best_lasso, x_test, s= best_lasso$best_lambda)
mse_test_lasso <- mean((y_test - yhat_test_lasso)^2)
mse_test_lasso


##Linear Regression
fit.lm <- lm(final, house_train)
fit.lm
yhat_train_lm <- predict(fit.lm)
mse_train_lm <- mean((y_train - yhat_train_lm)^2)
mse_train_lm
yhat_test_lm <- predict(fit.lm, house_test)
mse_test_lm <- mean((y_test - yhat_test_lm)^2)
mse_test_lm

# regression tree: 
library(rpart)
library(rpart.plot)

# grow tree
fit <- rpart(price~ bedrooms + bathrooms + sqft_living + sqft_lot + floors + waterfront + view + condition + sqft_above + sqft_basement + yr_built + yr_renovated + zipcode,
             method="anova", data=final)
printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits
# create additional plots
par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(fit) # visualize cross-validation results
# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree for Price ")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
# prune the tree
pfit<- prune(fit, cp=0.01160389) # from cptable
# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Regression Tree for Price")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
#make predictions
predictions <- predict(fit, house_train)
#mse train
mse.rtree <- mean((y.train - predictions)^2)
print(mse.rtree)
#mse test
#make predictions
predictions.test <- predict(fit, house_test)
mse.rtreetest <- mean((y.test - predictions.test)^2)
print(mse.rtreetest)

#Random Forest: 
library(randomForest)
library(caret)

#split the data set to train and test:
set.seed(1234)
rnorm(1)
test_set <- sample(nrow(final), nrow(final) * 0.8)
house_train <- final[test_set, ]
house_test <- final[-test_set, ]

#head(house_train)
summary(final$price)




