install.packages('readr')
install.packages('mlbench')
install.packages('corrplot')
install.packages('Amelia')
install.packages('caret')
install.packages('plotly')
install.packages('caTools')
install.packages('reshape2')

library(readr)
library(ggplot2)
library(ggcorrplot)
library(mlbench)
library(Amelia)
library(caret)
library(plotly)
library(caTools)
library(reshape2)
library(dplyr)
library(tidyverse)
library(data.table)
library(skimr)
library(psych)

#load data
house <- read.csv("downloads/kc_house_data.csv")

#examine the dataset
str(house)
summary(house)
skim(house)

# find missing values 
missmap(house, col=c('yellow','black'), y.at= 1, y.labels= ' ', legend= TRUE)

#change year of renovation to boolean. (0 for no renovation, 1 for renovation)
house$yr_renovated  [house$yr_renovated  > 0] <- 1
house$yr_renovated  [house$yr_renovated  == 0] <- 0

#see the relationship between house price and number of bedrooms
ggplot(house, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple')

#removing the extreme outliers
room_outlier <-
  house %>% filter(bedrooms >= 30) #filter the entire row
house2 <- house[!(house$bedrooms > 30), ]

ggplot(house2, aes(x = bedrooms, y = price)) + geom_point(alpha = 0.5, color = 'purple')

#condition and grade is to determined the quality of the house (pick one)
drops <- c('grade', 'zipcode')
house3 <- house2[,!(names(house2) %in% drops)]

summary(house3)

#Exploratory Analysis
#The average price of the house: 
Avg_house_price <- mean(house3$price)
Avg_house_price

qplot(price, data= house3)

#relationship with price and square foot space
ggplot(house3, aes(x = sqft_living, y = price)) + geom_point(alpha = 0.2)

#plot sqft_lot and price
ggplot(house3, aes(x = sqft_lot, y = price)) + geom_point(alpha = 0.2)

#relationship with price and conditions
ggplot(house3, aes(x = condition, y = price)) + geom_point(alpha = 0.5)
#why does the highest condition house has lower range? 
#could there be other factor that influence the price of the house?

#visualization-correlation (corrplot)
corrplot(cor(select(house3, -yr_renovated)))

#visualizing density of varaibles 
house3 %>% ggplot(aes(price))+stat_density()+theme_bw()
house3 %>% ggplot(aes(sqft_living))+stat_density()+theme_bw()
house3 %>% ggplot(aes(sqft_living15))+stat_density()+theme_bw()

#effects of variables in the dataset 
house3 %>% select(c(
  bedrooms,
  sqft_living,
  sqft_lot,
  sqft_living15,
  sqft_lot15
)) %>%
  melt(id.vars = 'price') %>%
  ggplot(aes(x = value, y = price, color = variables))
+ geom_point(alpha = 0.5)
+ stat_smooth(aes(colour = 'black'))
+ facet_wrap(~ variable, scales = 'free', ncol = 2)
+ labs(x = 'Variable Value', y = 'House Price (Millions)')
+ theme_minimal()



