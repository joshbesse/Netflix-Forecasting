# Project: 

# 1. Determine the best model to forecast Netflix's revenue. Revenue is the dependent variable and 
# US GDP, US CPI, US unemployment, and US personal disposable income are the independent variable.

# 2. Producing its own content is costly so management would like to demonstrate that it has been worthwhile in terms of 
# eventual revenue. Determine whether or not Netflix producing its own content has been worthwhile.

# 3. Management would also claim that Netflix is recession-proof. Determine whether or not this claim is true.



# installing packages
# install.packages("haven")
# install.packages("ggplot2")
# install.packages("zoo")
# install.packages("MASS")
# install.packages("lmtest")
# install.packages("forecast")

# loading packages 
library(haven)
library(ggplot2)
library(zoo)
library(MASS)
library(lmtest)
library(stats)
library(forecast)

# importing Netflix Revenue sav file 
data <- read_sav("/Users/joshbesse/Desktop/Projects/Netflix/Netflix Case Study (1).sav")
summary(data)
str(data)

# preparing data
data$Date <- as.yearqtr(data$DATE_, format = "Q%q %Y")
data$trend <- 1:(nrow(data))
data$QUARTER_ <- factor(data$QUARTER_, levels = c(1, 2, 3, 4))
quarter_dummy <- model.matrix(~ QUARTER_ - 1, data = data)[, -4]
dummy_data <- data.frame(quarter_dummy)
colnames(dummy_data) <- c("Q1", "Q2", "Q3")
data <- cbind(data, dummy_data)
rm(quarter_dummy)
rm(dummy_data)

# graph data 
ggplot(data, aes(x = Date, y = Total_Revenue_mil, group = 1)) + 
  geom_line() + 
  geom_point(size = 0.5) + 
  scale_x_yearqtr(format = "Q%q %Y") + 
  theme_minimal() + 
  labs(title = "Sequence Chart", x = "Date", y = "Revenue in Millions") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7))
# Notes: The data has a long-term trend, as revenue seems to be generally increasing over time. 
# The data also appears to have a seasonal component, indicated by the regular pattern of peaks and troughs. 

# ACF
acf(data$Total_Revenue_mil, main = "Revenue", ylab = "ACF", xlab = "Lag Number")
# Notes: The Autocorrelation table suggests that the data is autocorrelated, since all the autocorrelation coefficients 
# are significant. The ACF suggests that the data has a trend and a seasonal component. 
# The declining nature of the ACF indicates the data has a trend. The repeating spikes at every 4th lag indicates 
# a quarterly seasonal pattern.

# separate data 
train_data <- data[1:(nrow(data) - 4), ]
test_data <- data[(nrow(data) - 3): nrow(data), ]

# MULTIPLE REGRESSION
# Plan: The data has a quarterly seasonal pattern, so we need to create and use three dummy variables to see if 
# the seasonality is really significant. In this case, we will leave out quarter 4 
# (quarter 4 will serve as the average). We also need to create a trend variable to account for the long-term trend. 
# This will be done in the preparing data section of code. 

# all independent variables 
full_model <- lm(Total_Revenue_mil ~ GDP_Billions + US_CPI + US_Unemployment_Rate + Personal_Disposable_Income_Trillions +
                   trend + Q1 + Q2 + Q3,
                 data = train_data)
summary(full_model)

# excluding non-significant variables from full model 
adjusted_full_model <- lm(Total_Revenue_mil ~ GDP_Billions + Q1 + Q2 + Q3, data = train_data)
summary(adjusted_full_model)
  # scatter plot of residuals
  plot(train_data$Total_Revenue_mil, rstandard(adjusted_full_model), xlab = "Revenue", ylab = "Standardized Residual",
     main = "Scatter Plot of Standardized Residual by Revenue")
  abline(h = 0, col = "red")
  print(dwtest(adjusted_full_model))
  # Notes: All estimated coefficients are significant. Residuals have a pattern. Durbin-Watson indicates positive autocorrelation.
  # calculate MSE 
  predictions <- predict(adjusted_full_model, newdata = test_data)
  sq_residuals <- (test_data$Total_Revenue_mil - predictions)^2
  mse <- mean(sq_residuals)
  results <- data.frame(model = "Significant Full Model", MSE = mse)

# Forward Stepwise Regression 
empty_model <- lm(Total_Revenue_mil ~ 1, data = train_data)

forward_stepwise_model <- stepAIC(empty_model, direction = "forward")
summary(forward_stepwise_model)
# Notes: Model consists of only the intercept.
  # scatter plot of residuals 
  plot(train_data$Total_Revenue_mil, rstandard(forward_stepwise_model), xlab = "Revenue", ylab = "Standardized Residual",
     main = "Scatter Plot of Standardized Residual by Revenue")
  abline(h = 0, col = "red")
  print(dwtest(forward_stepwise_model))
  # Notes: Residuals have a pattern. Durbin-Watson indicates positive autocorrelation.
  # calculate MSE 
  predictions <- predict(forward_stepwise_model, newdata = test_data)
  sq_residuals <- (test_data$Total_Revenue_mil - predictions)^2
  mse <- mean(sq_residuals)
  mse_data <- data.frame(model = "Forward Stepwise Regression", MSE = mse)
  results <- rbind(results, mse_data)
  
# Backward Stepwise Regression 
backward_stepwise_model <- stepAIC(full_model, direction = "backward")
summary(backward_stepwise_model)
  # scatter plot of residuals 
  plot(train_data$Total_Revenue_mil, rstandard(backward_stepwise_model), xlab = "Revenue", ylab = "Standardized Residual",
       main = "Scatter Plot of Standardized Residual by Revenue")
  abline(h = 0, col = "red")
  print(dwtest(backward_stepwise_model))
  # calculate MSE 
  predictions <- predict(backward_stepwise_model, newdata = test_data)
  sq_residuals <- (test_data$Total_Revenue_mil - predictions)^2
  mse <- mean(sq_residuals)
  mse_data <- data.frame(model = "Backward Stepwise Regression", MSE = mse)
  results <- rbind(results, mse_data)  

# Mixed Stepwise Regression
mixed_stepwise_model <- stepAIC(full_model, direction = "both")
summary(mixed_stepwise_model)  
# Notes: Model is the same as backward stepwise model 
  # scatter plot of residuals 
  plot(train_data$Total_Revenue_mil, rstandard(mixed_stepwise_model), xlab = "Revenue", ylab = "Standardized Residual",
       main = "Scatter Plot of Standardized Residual by Revenue")
  abline(h = 0, col = "red")
  print(dwtest(mixed_stepwise_model))
  # Notes: The ANOVA table p-value indicates that the entire model is significant, and all parameters 
  # are significant. The assumptions for OLS are being met. The error terms are normally distributed. 
  # There are no error terms greater than three standard deviations away from the mean, and the number of 
  # error terms that are positive is approximately equal to the number of error terms that are negative. 
  # The error terms have a constant variance, meaning they are homoscedastic. There does not appear to be a 
  # pattern to the error terms. The error terms are statistically independent. The Durbin-Watson statistic is 
  # 1.7469, which is within the range of a “good” DW statistic and means there is no problem with autocorrelation. 
  # calculate MSE 
  predictions <- predict(mixed_stepwise_model, newdata = test_data)
  sq_residuals <- (test_data$Total_Revenue_mil - predictions)^2
  mse <- mean(sq_residuals)
  mse_data <- data.frame(model = "Mixed Stepwise Regression", MSE = mse)
  results <- rbind(results, mse_data)   

# EXPONENTIAL SMOOTHING
# Plan: Since there is a seasonal pattern to the data, try a Winters’ additive and Winters’ multiplicative model. 
train_series <- ts(train_data$Total_Revenue_mil, start = c(2010, 1), frequency = 4)
test_series <- ts(test_data$Total_Revenue_mil, start = c(2019, 1), frequency = 4)
  
# Simple Exponential Smoothing 
simple_expo_smooth <- ses(train_series)
summary(simple_expo_smooth)
  simple_expo_forecasts <- forecast(simple_expo_smooth, h = 4)
  plot(simple_expo_forecasts)
  # Notes: Model does not fit the data well.
  mse <- mean((test_data$Total_Revenue_mil - simple_expo_forecasts$mean)^2)
  mse_data <- data.frame(model = "Simple Exponential Smoothing", MSE = mse)
  results <- rbind(results, mse_data)

# Winters' Additive
winters_additive <- hw(train_series, seasonal = 'additive')
summary(winters_additive)
  winters_additive_forecasts <- forecast(winters_additive, h = 4)
  plot(forecast(winters_additive))
  # Notes: Model seems to fit the data well. 
  mse <- mean((test_data$Total_Revenue_mil - winters_additive_forecasts$mean)^2)
  mse_data <- data.frame(model = "Winters' Additive", MSE = mse)
  results <- rbind(results, mse_data)

# Winters' Multiplicative
winters_multiplicative <- hw(train_series, seasonal = 'multiplicative')
summary(winters_multiplicative)
  winters_multiplicative_forecasts <- forecast(winters_multiplicative, h = 4)
  plot(forecast(winters_multiplicative))
  # Notes: Model seems to fit the data well.
  mse <- mean((test_data$Total_Revenue_mil - winters_multiplicative_forecasts$mean)^2)
  mse_data <- data.frame(model = "Winters' Multiplicative", MSE = mse)
  results <- rbind(results, mse_data)  
  
# ARIMA 
pacf(data$Total_Revenue_mil, main = "Revenue", ylab = "Partial ACF", xlab = "Lag Number")
  
# Plan: The data has a long-term trend and is seasonal. After checking the ACF, we know the data is 
# autocorrelated and there is seasonality. Also, there are spikes in the PACF. Therefore, we need differencing, 
# seasonal components, and AR and MA components. 

arima_model <- auto.arima(train_series)
summary(arima_model)
# Notes: ARIMA(0,1,1)(0,1,0) selected 
  # check residuals for white noise
  checkresiduals(arima_model)
  # Notes: The graph of the residuals over time shows that the residuals display a pattern over time, 
  # specifically a drop at quarter 1 starting in 2012. There are spikes in the ACF of the residuals, meaning there is 
  # significant autocorrelation. The histogram of the residuals shows that the residuals are not normally distributed. 
  # Overall, the graphs suggest that the residuals are not white noise. 
  arima_forecasts <- forecast(arima_model, h = 4)
  plot(forecast(arima_model))
  # Notes: Model seems to follow the data well. 
  mse <- mean((test_data$Total_Revenue_mil - arima_forecasts$mean)^2)
  mse_data <- data.frame(model = "ARIMA(0,1,1)(0,1,0)", MSE = mse)  
  results <- rbind(results, mse_data)  
  
# COMBINATION
# Plan: Try combining multiple regression and exponential smoothing. Try combining multiple regression and ARIMA. 
  
# MR + ARIMA 
mr_component <- data.frame(forecasts = predict(mixed_stepwise_model, newdata = data))
arima_fitted_df <- data.frame(forecasts = as.vector(arima_model$fitted))
arima_forecasts_df <- data.frame(forecasts = as.vector(arima_forecasts$mean))
arima_component <- rbind(arima_fitted_df, arima_forecasts_df)
mr_arima <- lm(data$Total_Revenue_mil ~ mr_component$forecasts + arima_component$forecasts)
summary(mr_arima)
# Notes: The constant is not significant, meaning these two models can be combined. 
mr_arima <- lm(data$Total_Revenue_mil ~ regression_component$forecasts + arima_component$forecasts - 1)
summary(mr_arima)
# Notes: The results from the regression without the constant indicate that the contribution of the multiple regression 
# model to the prediction of Revenue is nothing as its coefficient is negative. 

# MR + Exponential Smoothing 
winters_fitted_df <- data.frame(forecasts = as.vector(winters_multiplicative$fitted))
winters_forecasts_df <- data.frame(forecasts = as.vector(winters_multiplicative_forecasts$mean))
expo_component <- rbind(winters_fitted_df, winters_forecasts_df)
mr_expo <- lm(data$Total_Revenue_mil ~ mr_component$forecasts + expo_component$forecasts)
summary(mr_expo)
# Notes: The constant is not significant, meaning these two models can be combined.
mr_expo <- lm(data$Total_Revenue_mil ~ mr_component$forecasts + expo_component$forecasts - 1)
summary(mr_expo)
# Notes: The results from the regression without the constant indicate that the contribution of the multiple regression
# model to the prediction of Revenue is nothing as its coefficient is negative.



# OWN CONTENT ANALYSIS 
# Plan: Create a dummy variable that is 1 for years 2013 and greater and is 0 otherwise. 
# Create two variables: 1. The actual Revenue data 2. The actual Revenue data that stops at 2012. 
# Suppose Netflix didn’t start producing its own content, what would Revenue look like? Run ARIMA model and make 
# predictions for data after 2012, then compare to actual data. 

# Method 1 
data_series <- ts(data$Total_Revenue_mil, start = c(2010,1), frequency = 4)
intervention <- as.numeric(time(data_series) >= 2013)
intervention_arima <- auto.arima(data_series, xreg = intervention)
summary(intervention_arima)
intervention_t_statistic <- 106.4603 / 107.8880
# Notes:The t-statistic of 0.9867668 is less than the critical value of 1.96 (for a 95% confidence interval), meaning 
# that the coefficient is not significantly different from zero at the 5% significance level. 

# Method 2 
data_series_2012 <- ts(data$Total_Revenue_mil[1:12], start = c(2010,1), frequency = 4)
arima_2012 <- auto.arima(data_series_2012)
data_series_fitted <- arima_2012$fitted
data_series_pred <- forecast(arima_2012, h = 28)[4]$mean
data_series_combined <- ts(c(data_series_fitted, data_series_pred), start = c(2010,1), frequency = 4)

plot(data_series, col = "blue", xlab = "Date", ylab = "Revenue")
lines(data_series_combined, col = "red")
legend("topleft", 
       legend = c("True Revenue", "Predicted Revenue"),
       col = c("blue", "red"),
       lty = 1,
       cex = 0.7)
# Notes: Significant difference in predictions of Revenue and actual Revenue. 
# This indicates that Netflix producing its own content is worthwhile. 
# Answers question: what would have happened if Netflix just continued without producing its own content? 



# RECESSION-PROOF ANALYSIS
# Plan: Run an ARIMA model with Revenue as the dependent variable and US GDP, US CPI, US unemployment, 
# and US Personal Disposable Income as the independent variables. 
exog_vars <- cbind(data$GDP_Billions, data$US_CPI, data$US_Unemployment_Rate, data$Personal_Disposable_Income_Trillions)
recession_arima <- auto.arima(data_series, xreg = exog_vars)
summary(recession_arima)
