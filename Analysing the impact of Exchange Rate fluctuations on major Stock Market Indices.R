install.packages(c("tidyr", "rmgarch", "vars", "dplyr",  "readxl", "ggplot2", "forecast", "MswM", "rugarch", "tsDyn", "tseries"))

library(tidyr)
library(rmgarch)
library(vars)
library(dplyr)
library(readxl)
library(ggplot2)
library(forecast)
library(rugarch)
library(MSwM)
library(tsDyn)
library(tseries)


# Loading the data, which I prepared in the Excel file
exchange_data <- read_excel("E:/TANISHQNew folder/MSC QF/Seminar Paper Work/Data/Analysis/Exchange rates per USD.xlsx")
stock_returns <- read_excel("E:/TANISHQNew folder/MSC QF/Seminar Paper Work/Data/Analysis/Stock_Market_data.xlsx")
head(exchange_data)
head(stock_returns)


# Assuming exchange_data_long is already in long format
exchange_data_long <- exchange_data %>%
  pivot_longer(cols = -Date, names_to = "Currency", values_to = "Exchange_Rate")

# Plotting the graph for different currencies exchange rate per Dollar
ggplot(exchange_data_long, aes(x = Date, y = Exchange_Rate)) +
  geom_line() +
  labs(title = "Exchange Rate Trends Over Time for Each Currency",
       x = "Date",
       y = "Exchange Rate per USD") +
  facet_wrap(~ Currency, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plotting the graph for the different stock returns for diversified stock indices
ggplot(data_long, aes(x = Date, y = Returns)) +
  geom_line() +
  labs(title = "Stock Returns for Different Indices Over Time",
       y = "Returns", x = "Date") +
  facet_wrap(~ Index, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Function to perform ADF test and return p-value and test statistic
adf_result <- function(series) {
  test <- adf.test(series, alternative = "stationary")
  return(c("ADF Test Statistic" = test$statistic, "p-value" = test$p.value))
}

# ADF test for each stock market index in `stock_returns`
stock_adf_results <- sapply(stock_returns, adf_result)
stock_adf_results <- t(stock_adf_results)  # Transpose for readability
rownames(stock_adf_results) <- colnames(stock_returns)
print(stock_adf_results)


# ADF test for each exchange rate in `exchange_data`
exchange_adf_results <- sapply(exchange_data, adf_result)
exchange_adf_results <- t(exchange_adf_results)  # Transpose for readability
rownames(exchange_adf_results) <- colnames(exchange_data)
print(exchange_adf_results)

# We can clearly see that data is not stationary


# Making the data stationary
# Removing the 'Date' column
exchange_data_withoutdate <- exchange_data[, -1]  
stock_returns_withoutdate <- stock_returns[, -1]

# ADF Test on log-differenced series to confirm stationarity
# Create an empty list to store ADF test results
exchange_adf_results <- list()  
stock_adf_results <- list()

for (currency in colnames(exchange_data_withoutdate)) {
  # Extract the time series for each currency
  currency_series <- ts(exchange_data_withoutdate[[currency]])
  
  # Log Transformation and First Differencing
  log_diff_series <- diff(log(currency_series))
  
  # Performing the ADF Test and store the result
  adf_test_result <- adf.test(log_diff_series)
  
  # Store the ADF test result in the list with currency name as the key
  results[[currency]] <- adf_test_result
  
  # Print the ADF test result for each currency
  cat("\nADF Test Result for:", currency, "\n")
  print(adf_test_result)
}

for (index in colnames(stock_returns_withoutdate)) {
  
  # Extract the time series for each stock index
  index_series <- ts(stock_returns_withoutdate[[index]])
  
  # Log Transformation and First Differencing to make it stationary
  log_diff_series <- diff(log(index_series))
  
  # Perform the ADF Test and store the result
  adf_test_result <- adf.test(log_diff_series)
  
  # Store the ADF test result in the list with index name as the key
  stock_adf_results[[index]] <- adf_test_result
  
  # Print the ADF test result for each index
  cat("\nADF Test Result for:", index, "\n")
  print(adf_test_result)
}

# Prepare the new stationary time series data using log-differencing
log_diff_data <- lapply(exchange_data_withoutdate, function(x) diff(log(x)))

# Fit ARMA model using AIC/BIC criterion and store results
arma_results <- list()  # Empty list to store ARMA results

for (currency in names(log_diff_data)) {
  
  # Extract the stationary time series
  log_diff_series <- ts(log_diff_data[[currency]])
  
  # Fit the ARMA model using AIC or BIC
  # The auto.arima function will find the best ARMA(p, q) model based on AIC/BIC
  # Specify order=c(0,0,0) and max.order to keep the differencing as it is.
  arma_model <- auto.arima(log_diff_series, d=0, max.order=10, 
                           seasonal=FALSE, ic="bic")  # Use "aic" for AIC criterion if desired
  
  # Store the ARMA model and AIC/BIC value in the results list
  arma_results[[currency]] <- list(model=arma_model, criteria=arma_model$bic)
  
  # Print out the best ARMA model for each currency
  cat("\nBest ARMA model for:", currency, "\n")
  print(arma_model)
}


# Prepare the new stationary time series data using each stock index
log_diff_stock_data <- lapply(stock_returns_withoutdate, function(x) diff(log(x)))

# Fit ARMA model using AIC/BIC criterion and store results
arma_stock_results <- list()  # List to store ARMA model results for each index

for (index in names(log_diff_stock_data)) {
  
  # Extract the stationary time series
  log_diff_series <- ts(log_diff_stock_data[[index]])
  
  # Fit the ARMA model using AIC or BIC
  # The auto.arima function will find the best ARMA(p, q) model based on AIC/BIC
  arma_model <- auto.arima(log_diff_series, d=0, max.order=10, 
                           seasonal=FALSE, ic="bic")  # Use "aic" for AIC criterion if desired
  
  # Store the ARMA model and BIC value in the results list
  arma_stock_results[[index]] <- list(model=arma_model, criteria=arma_model$bic)
  
  # Print out the best ARMA model for each index
  cat("\nBest ARMA model for:", index, "\n")
  print(arma_model)
}


# ACF and PACF plots for exchange rate data (JPY, GBP, INR, EUR, SGD, CNY)
exchange_series <- list("JPY" = log_diff_data$JPY, "GBP" = log_diff_data$GBP, 
                        "INR" = log_diff_data$INR, "EUR" = log_diff_data$EUR, 
                        "SGD" = log_diff_data$SGD, "CNY" = log_diff_data$CNY)

par(mfrow=c(2,3))  # Arrange plots in a 2x3 grid
for (currency in names(exchange_series)) {
  log_diff_series <- exchange_series[[currency]]
  # Plot ACF and PACF for each currency
  acf(log_diff_series, main=paste("ACF for", currency))
  pacf(log_diff_series, main=paste("PACF for", currency))
}

# ACF and PACF plots for stock indices data (NIKKEI 225, FTSE 100, etc.)
stock_series <- list("NIKKEI 225" = log_diff_stock_data$`NIKKEI 225`, 
                     "FTSE 100" = log_diff_stock_data$`FTSE 100`,
                     "NIFTY 50" = log_diff_stock_data$`NIFTY 50`, 
                     "DAX" = log_diff_stock_data$DAX,
                     "S&P 100" = log_diff_stock_data$`S&P 100`,
                     "SGX NIKKEI 225" = log_diff_stock_data$`SGX NIKKEI 225`,
                     "Shanghai Composite" = log_diff_stock_data$`Shanghai Composite`)

par(mfrow=c(3,3))  # Arrange plots in a 3x3 grid
for (index in names(stock_series)) {
  log_diff_series <- stock_series[[index]]
  # Plot ACF and PACF for each stock index
  acf(log_diff_series, main=paste("ACF for", index))
  pacf(log_diff_series, main=paste("PACF for", index))
}



# Combining stationary time series of each pair for MGARCH analysis
pairs_data <- list(
  "JPY_NIKKEI" = cbind(log_diff_data$JPY, log_diff_stock_data$`NIKKEI 225`),
  "GBP_FTSE" = cbind(log_diff_data$GBP, log_diff_stock_data$`FTSE 100`),
  "INR_NIFTY" = cbind(log_diff_data$INR, log_diff_stock_data$`NIFTY 50`),
  "EUR_DAX" = cbind(log_diff_data$EUR, log_diff_stock_data$DAX),
  "SGD_SGX" = cbind(log_diff_data$SGD, log_diff_stock_data$`SGX NIKKEI 225`),
  "CNY_SHANGHAI" = cbind(log_diff_data$CNY, log_diff_stock_data$`Shanghai Composite`)
)

# Rename columns for clarity
names(pairs_data) <- c("JPY vs NIKKEI 225", "GBP vs FTSE 100", "INR vs NIFTY 50", "EUR vs DAX", "SGD vs SGX NIKKEI 225", "CNY vs Shanghai Composite")

# Define and fit DCC-GARCH for each currency-stock pair
dcc_results <- list()

for (pair_name in names(pairs_data)) {
  
  # Get data for current pair
  pair_data <- pairs_data[[pair_name]]
  
  # Define DCC-GARCH specification
  spec <- dccspec(uspec = multispec(replicate(2, ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model = list(armaOrder = c(0, 0), include.mean = TRUE)
  ))), dccOrder = c(1, 1), distribution = "mvnorm")
  
  # Fit the DCC-GARCH model
  dcc_fit <- dccfit(spec, data = pair_data)
  
  # Store results for each pair
  dcc_results[[pair_name]] <- dcc_fit
  
  # Print model summary
  cat("\nDCC-GARCH Results for", pair_name, "\n")
  print(dcc_fit)
}

# Plot the volatility and correlation dynamics
for (pair_name in names(dcc_results)) {
  
  # Get fitted DCC-GARCH model
  dcc_fit <- dcc_results[[pair_name]]
  
  # Extract and plot conditional volatilities
  volatility_plot <- plot(sigma(dcc_fit), main = paste("Conditional Volatilities -", pair_name))
  
  # Extract and plot conditional correlations
  correlation_plot <- plot(rcor(dcc_fit), main = paste("Conditional Correlations -", pair_name))
}

# Summary of DCC-GARCH metrics for each pair
summary_table <- data.frame(Pair = character(), Avg_Volatility_Exchange = numeric(),
                            Avg_Volatility_Stock = numeric(), Avg_Correlation = numeric())

for (pair_name in names(dcc_results)) {
  
  # Get fitted DCC-GARCH model
  dcc_fit <- dcc_results[[pair_name]]
  
  # Average volatility and correlation values
  avg_vol_exchange <- mean(sigma(dcc_fit)[, 1], na.rm = TRUE)
  avg_vol_stock <- mean(sigma(dcc_fit)[, 2], na.rm = TRUE)
  avg_corr <- mean(rcor(dcc_fit)[1, 2, ], na.rm = TRUE)
  
  # Add to summary table
  summary_table <- rbind(summary_table, data.frame(Pair = pair_name,
                                                   Avg_Volatility_Exchange = avg_vol_exchange,
                                                   Avg_Volatility_Stock = avg_vol_stock,
                                                   Avg_Correlation = avg_corr))
}

# Print the summary table
print(summary_table)


# Define a list to store VAR results
var_results <- list()

for (pair_name in names(pairs_data)) {
  
  # Get the data for the current pair
  pair_data <- pairs_data[[pair_name]]
  
  # Fit the VAR model (Choose optimal lag length using information criteria)
  lag_selection <- VARselect(pair_data, lag.max = 10, type = "const")
  optimal_lag <- lag_selection$selection["AIC(n)"]  # Using AIC for lag selection
  
  # Fit the VAR model with optimal lag
  var_model <- VAR(pair_data, p = optimal_lag, type = "const")
  var_results[[pair_name]] <- var_model
  
  # Print the summary of the VAR model
  cat("\nVAR Model Results for:", pair_name, "\n")
  print(summary(var_model))
  
  # Impulse Response Function (IRF) for the VAR model
  irf_result <- irf(var_model, impulse = colnames(pair_data)[1], response = colnames(pair_data)[2], n.ahead = 20)
  
  # Plot the IRF to analyze the effect of exchange rate shocks on stock index returns
  plot(irf_result, main = paste("IRF of Exchange Rate on Stock Index for", pair_name))
}

# Define a list to store MSM results
msm_results <- list()

for (pair_name in names(pairs_data)) {
  
  # Get data for the current pair
  pair_data <- pairs_data[[pair_name]]
  colnames(pair_data) <- c("Exchange_Rate", "Stock_Index")  # Naming for easier interpretation
  
  # Fit a linear model to the data as a base for MSM
  base_model <- lm(Stock_Index ~ Exchange_Rate, data = as.data.frame(pair_data))
  
  # Fit a Markov-Switching Model with 2 regimes
  msm_model <- msmFit(base_model, k = 2, p = 1, sw = c(TRUE, TRUE))  # Switching on intercept and exchange rate coefficient
  
  # Store MSM model results
  msm_results[[pair_name]] <- msm_model
  
  # Print MSM model summary
  cat("\nMSM Model Results for:", pair_name, "\n")
  print(summary(msm_model))
  
  # Plot the switching probabilities
  plot(msm_model, which = 1, main = paste("Regime Switching Probabilities -", pair_name))
}
