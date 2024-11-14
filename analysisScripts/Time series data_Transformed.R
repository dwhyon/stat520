# Load required libraries
library(readr)
library(forecast)
library(dplyr)
library(tseries)
library(ggplot2)

# Read the dataset
df <- read_csv("C://Users//wisen//Downloads//new_life_expectancy_data.csv")

# Specify the country of interest
country_of_interest <- "China"

# Filter data for the specified country and select relevant columns
country_data <- df %>%
  filter(Name == country_of_interest & Year >= 2000 & Year <= 2015) %>%
  select(Year, `Life Expectancy at Birth`) %>%
  arrange(Year)

# Check the structure of the data
str(country_data)

# Ensure 'Life Expectancy at Birth' is numeric
country_data$`Life Expectancy at Birth` <- as.numeric(country_data$`Life Expectancy at Birth`)

# Check for any NAs introduced during conversion
if (any(is.na(country_data$`Life Expectancy at Birth`))) {
  warning("There are NAs in 'Life Expectancy at Birth' after conversion. Please check the data.")
}

# Convert the data to a time series object
ts_data <- ts(country_data$`Life Expectancy at Birth`, 
              start = c(2000), 
              frequency = 1)

# Check for stationarity
adf_test <- adf.test(ts_data)
print(adf_test)

# If non-stationary, difference the data
if (adf_test$p.value > 0.05) {
  ts_data_diff <- diff(ts_data)
  adf_test_diff <- adf.test(ts_data_diff)
  print(adf_test_diff)
}

# Fit the ARIMA model (using auto.arima)
arima_model <- auto.arima(ts_data)
summary(arima_model)

# Residual analysis
residuals <- residuals(arima_model)

# Plot the residuals
plot(residuals, main="Residuals of ARIMA Model", ylab="Residuals", xlab="Time")
abline(h = 0, col = "red")

# QQ plot for normality of residuals
qqnorm(residuals)
qqline(residuals, col = "red")
title("QQ Plot of Residuals")

# ACF and PACF of residuals
acf(residuals, main="ACF of Residuals")
pacf(residuals, main="PACF of Residuals")

# Ljung-Box test on residuals
ljung_box_test <- Box.test(residuals, type = "Ljung-Box")
print(ljung_box_test)

# Forecasting
forecasts <- forecast(arima_model, h = 5)  # Forecast for the next 5 years

# Plotting forecast
plot(forecasts, main = paste("Forecast for", country_of_interest), 
     xlab = "Year", ylab = "Life Expectancy at Birth")

# Print the forecast values and confidence intervals
forecast_values <- data.frame(
  Year = time(forecasts$mean),
  Forecast = forecasts$mean,
  Lower_95 = forecasts$lower[, 2],  # Lower bound of the 95% CI
  Upper_95 = forecasts$upper[, 2]   # Upper bound of the 95% CI
)

print(forecast_values)
