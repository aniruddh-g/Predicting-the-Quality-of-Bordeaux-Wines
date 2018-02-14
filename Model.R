# Read in data
wine = read.csv("wine.csv")
str(wine)
summary(wine)

# Linear Regression (one variable)
model1 = lm(Price ~ AGST, data=wine)
summary(model1)

# Sum of Squared Errors
model1$residuals
SSE = sum(model1$residuals^2)
SSE

# Linear Regression (two variables)
model2 = lm(Price ~ AGST + HarvestRain, data=wine)
summary(model2)

#The new R-squared value near the bottom of the output suggests that this variable really helped our model. Our Multiple R-squared 
#and Adjusted R-squared both increased significantly compared to the previous model. This indicates that this new model is probably 
#better than the previous model.

# Sum of Squared Errors
SSE = sum(model2$residuals^2)
SSE

# Linear Regression (all variables)
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(model3)

#If we look at the bottom of the output, we can again see that the Multiple R-squared and Adjusted R-squared have both increased.

# Sum of Squared Errors
SSE = sum(model3$residuals^2)
SSE

#And if we type SSE, we can see that the sum of squared errors for model3 is 1.7, even better than before.
#Now we will create a linear regression model to predict Price using HarvestRain and WinterRain as independent variables.

# Remove FrancePop
model4 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(model4)

#Removing FrancePop variable as it is not significant independent variable to predict wine price and creating a new model.

# Correlations
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)

#We’ve confirmed that Age and FrancePopulation are definitely highly correlated. So we do have multicollinearity problems in our model 
#that uses all of the available independent variables. Keep in mind that multicollinearity refers to the situation when two independent 
#variables are highly correlated. A high correlation between an independent variable and the dependent variable is a good thing since 
#we’re trying to predict the dependent variable using the independent variables. Now we will remove both Age and FrancePopulation, 
#since they were both insignificant in our model and build a new model.

# Remove Age and FrancePop
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data=wine)
summary(model5)

#If we look at each of our independent variables in the new model, and the stars, we can see that something a little strange happened. 
#Before, Age was not significant at all in our model3. But now, Age has two stars, meaning that it’s very significant in this new model. This is due to something called multicollinearity. Age and FrancePopulation are what we call highly correlated.

model6 = lm(Price ~ WinterRain+AGST+HarvestRain, data=wine)
summary(model6)

# Read in test set
wineTest = read.csv("wine_test.csv")
str(wineTest)

# Make test set predictions
predictTest = predict(model6, newdata=wineTest)
predictTest

# Compute R-squared
SSE = sum((wineTest$Price - predictTest)^2)
SST = sum((wineTest$Price - mean(wine$Price))^2)
1 - SSE/SST

#The value of R Squared shows that the prediction is close to what is expected but to have more accurate prediction we need a large data set.
