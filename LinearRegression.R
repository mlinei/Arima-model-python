library(yfR)
library(dplyr)
library(tseries)
library(lmtest)
library(quantmod)
library(sandwich)
#Import the financial series we are using
btc <- yf_get("BTC-USD", first_date = "2018-01-01", last_date = "2023-06-01", freq_data = "monthly")
eth <- yf_get("ETH-USD", first_date = "2018-01-01", last_date = "2023-06-01", freq_data = "monthly")
#Get the columns with the data we need
df <- cbind(btc["price_close"], eth["price_close"])
colnames(df) <- c("btc","eth")
#Estimate the model, The eth price is going to be explained by the btc price and the first lag
#of the eth price. Then we get the most relevant information about it
model <- lm(eth ~ btc + lag(eth), df)
summary(model)
#It seems that all our coefficients are significative to the model.
#the model with the BTC price and the first lag of ETH price as independent variables 
#explains 95% of the variance in the ETH price.

#Now let's check if our model meets certain assumptions of linear regression.
#We are going to use a 0.05 level of significance.

#Are we observing homoscedasticity? Let's check that with the Breusch Pagan test.
bp <- bptest(model)
bp
#The null hypothesis was rejected. This means our model does not meet the assumption of homoscedasticity
#That has to be fixed.

#Are we observing serial autocorrelation? Let's check that with the Breusch-Godfrey test
bg <- bgtest(model)
bg
#We didn't reject the null hypothesis. In other words, we dont have any problem with serial correlation

#One way to address the absence of homoscedasticity in the variance is to estimate the model 
#on a logarithmic scale. In theory, this helps stabilize it. Let's give it a try.
model2 <- lm(log(eth) ~ log(btc) + log(lag(eth)),df)
summary(model2)

bp2 <- bptest(model2)
bp2
#This time we do not reject the null hypothesis. In other words, we are now in presence of
#homoscedasticity and the assumption is now satisfied"

bg2 <- bgtest(model2)
bg2
#We neither have problems with the serial correlation in this model as we do not reject null
#hypothesis in the Breusch Godfrey test.

#Let's also check if our model has the right functional form or it's misspicified with Ramsey's test
ramsey <- resettest(model2)
ramsey
#The null hypothesis has not been rejected meaning that we have the right functional form.

#Time to analize some accuracy metrics of the model. 75% of the data will be used to train the model
#and the other 25% to test it. The metrics to be used will be: MAE, RMSE and Theil's U

train <- as.data.frame(df[1:round(0.75 * nrow(df)), ])
test <- as.data.frame(df[(round(0.75 * nrow(df)) + 1):nrow(df), ])

modeltest <- lm(log(eth) ~ log(btc) + log(lag(eth)), train)
summary(modeltest)

#We dont have to forget to apply the exp() operations to our predictions to get the values in the right scale
predictions <- as.data.frame(predict(modeltest, test))
predictions <- exp(predictions)
predictions <- na.omit(predictions)
colnames(predictions) <- "predicted"
predictions$predicted <- as.numeric(predictions$predicted)
actual <- as.data.frame(test$eth)
actual <- as.data.frame(actual[-1,])
colnames(actual) <- "eth"

# Calculate the Mean Absolute Error (MAE)
mae <- mean(abs(predictions$predicted - actual$eth))
print(mae)

# Calculate the Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predictions$predicted - actual$eth)^2))
print(rmse)

#Theil's U
theil_u <- (rmse/(sqrt(sum(actual$eth**2)/nrow(actual))+sqrt(sum(predictions$predicted**2)/nrow(predictions))))
theil_u
#The closer the theil's u is to 0, the better the model is. 
#In this case we got a value of 0.0864 meaning our model is pretty accurate.