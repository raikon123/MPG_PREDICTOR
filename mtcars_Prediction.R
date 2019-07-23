#Nick Johnson 7/11/19
#How to use predict method for Linear Models
#Reference 

rm(list=ls()) #clear lists

install.packages("ggplot2") #adds graphing functions
install.packages("ggcorrplot") #add correlation functions for graphing

#options(scipen = 999) #turns off scientific notation, this is optional

library(ggplot2) #loads the package ggplot2
library(ggcorrplot) #loads Correlogram graphing option
library(datasets) #pre-loaded datasets built into R


theme_set(theme_bw()) #uses pre-set colors for all plots

data("mtcars")
summary(mtcars) #summary gives a brief overview of the data
cor_cars <- round(cor(mtcars), 2) #makes a correlation table to the 2nd decimal place, you can adjust the decimal place by changing the number.

ggcorr <- ggcorrplot(cor_cars, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3, method = "circle", 
                     title = "Correlogram for Variables", ggtheme = theme_bw)
ggcorr #plots the correlation matrix to visually see the correlation between all variables in the dataset


#Can we predict the miles per gallon(mpg) based on the cars engine cylinders(cyl), cubic inches of the engine(disp), weight of the vehicle(wt), 
#and horsepower(hp)?

#This will be a basic linear model with the lm() function in R
mpg_est <- lm(mpg ~ cyl + disp + wt + hp, data = mtcars) 
#mpg is our dependent variable so it will be listed first followed by a ~ to denote the independent variables to follow. 

mpg_est #This will call the formula and the coefficients including the intercept seen below.

#Call:
#  lm(formula = mpg ~ cyl + disp + wt + hp, data = mtcars)

#Coefficients:
#  (Intercept)          cyl         disp           wt           hp  
#     40.82854     -1.29332      0.01160     -3.85390     -0.02054  


#These variables will tell you how the MPG will be effected based on each variable.
#The Intercept denotes where the base MPG will start. 
#For each CYL in the engine it will reduce the MPG by -1.29332.
#For DISP it will add to the MPG based on the amount of cubic inches your engine is, this isn't intuitive. As we would expect that the 
  # larger the engine the more fuel it consumes.
#For WT in terms of thousands has a negative effect on the MPG which seems resonable. 
#For HP, it also has a negative impact on the MPG.

anova(mpg_est)
#Produces the anova table where we want to look at the P-values "Pr(>F)". This will help us determine the importance of variables
  # given for the linear model. The general thinking is that the variables need to have a P-value smaller than 5%, if a variable is higher
  # 5% then that variable could be removed from the linear model. However, the P-value isn't the only things to look at when determining the
  # importance of a variable.

mtcars$car_name <- rownames(mtcars) #this will create a new column for the name of the cars

mtcars$norm_mpg <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2) #normalize the mpg
mtcars$norm_mpg #display normalized values to 2 decimal places

mtcars$mpg_avg <- ifelse(mtcars$norm_mpg < 0, "below", "above") #this will denote if a specific vehicle is above or below average mpg
mtcars$mpg_avg #display above or below to the corresponding values

mtcars <- mtcars[order(mtcars$norm_mpg), ] #sort the cars by mpg
mtcars #displays the car name and above or below value next to it

mtcars$car_name <- factor(mtcars$car_name, levels = mtcars$car_name) #this keeps the order when plotted
mtcars$car_name #lists the cars

gg_avg_mpg <- ggplot(mtcars, aes(x = car_name, y = norm_mpg, label = norm_mpg)) +
  geom_bar(stat = 'identity', aes(fill = mpg_avg), width = .5) +
  scale_fill_manual(name = "MPG", labels = c("Above Avg", "Below Avg"), values = c("above"= "#00FFFF", "below" = "#FF0066")) + 
  labs(title = "Miles per Gallon Averages")+
  coord_flip()
#The geom_bar denotes a bar chart. The scale_fill_manual is used to name the labels, label title, and the colors of bar in the chart
  # the colors are in hexidecimal form. The coord_flip will flip the chart so the bars will be horizontal.

gg_avg_mpg #display the bar graph

#This new dataset is from Kaggle, we will predict the mpg based on the mtcars linear model 
pred_data <- read.csv(file = "C:/Users/s0044798/Desktop/auto-mpg.csv", header = TRUE, sep = ",") #import data
pred_data #list data

pred_mpg_est <- predict.lm(mpg_est, pred_data) #predict the mpg for pred_data dataset
pred_mpg_est #show results

#Compare results to see how accurate the linear model calculates the mpg


#Graph the predicted and actual points
#turn data into a data frame

#adding a row of the differrence between the actual and predicted values
predicted <- as.data.frame(pred_mpg_est-pred_data$mpg)
predicted

mpg_est <- as.data.frame(pred_mpg_est)
mpg_est

predicted1 <- data.frame(pred_data$car.name, pred_mpg_est, pred_data$mpg, predicted)
predicted1

#look at the mean of the difference of actual and predicted values. Does this mean represent an accurate representation of the graphs?
sum1 <- sum(predicted)
mean1 <- sum1/392
mean1

#graph represents the predicted values (Blue Circles) and the actual values (Green Triangles)
#the following two graphs use the x-axis to uniquely identify the vehicle without labeling them

plot_pred <- ggplot(data = predicted1) +
              geom_point(shape = 16, color = "Blue", size = 3, aes(x = 1:nrow(predicted1), y = pred_mpg_est)) +
              geom_point(shape = 17, color = "Green", size = 3, aes(x = 1:nrow(predicted1), y = pred_data$mpg)) +
              labs(title = "MPG Comparison", x = "Car Index", y = "Predicted vs Actual") 
plot_pred

#this graph will show the differences in the actual and predicted values. 
#the points closer to the black line show an accurate prediction of MPG but as a whole this model isnt very accurate.

plot_dif <- ggplot(data = predicted1) +
            geom_point(color = "red", aes(x = 1:nrow(predicted1), y = predicted1$pred_mpg_est...pred_data.mpg)) +
            geom_hline(yintercept = 0, color = "Black")
plot_dif

