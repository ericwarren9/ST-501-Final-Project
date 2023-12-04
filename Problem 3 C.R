# Purpose: To model stocks and find the best portfolios


# Load in packages --------------------------------------------------------

library(tidyverse)
library(quantmod)


# Read in Stock Data ------------------------------------------------------

# Get data
getSymbols(c("AAPL","MSFT"),from = "2018-10-31", to = "2023-11-01")

# daily return values
AAPL.returns <- as.numeric(dailyReturn(AAPL))
MSFT.returns <- as.numeric(dailyReturn(MSFT))

# trading days
days <- as.Date(.indexday(AAPL))

# plot
plot(
  days, 
  AAPL.returns,
  typ = "l", 
  ylab = "Returns",
  col = "red"
)
lines(
  days, 
  MSFT.returns, 
  col = "blue", 
  lwd = 1.5
)
legend(
  "topright", 
  legend = c("AAPL", "MSFT"), 
  col = c("red", "blue"), 
  lty = 1
)

# summary
(summaryStats <- summary(cbind(AAPL.returns, MSFT.returns)))
(variances <- cov(cbind(AAPL.returns, MSFT.returns)))



# Answer Questions for Final Project for Part C ---------------------------


# Do Part i ---------------------------------------------------------------


# Estimate values from Part 1 of the AAPL and MSFT return prices. Note we said that E(X) = mu_2 + (mu_1 - mu_2)w and Var(X) = w^2(sigma_1^2 + sigma_2^2 - 2Cov(X_1, X_2)) - 2w(sigma_2^2 - Cov(X_1, X_2)) + sigma_2^2. Note that X_1 is MSFT and X_2 is AAPL because AAPL variance is bigger
func1 <- function(lst, n){
  sapply(lst, `[`, n)
} # Make function to get certain element in list

# AAPL mean which is c_0 = mu_2
(c_0 <- AAPL_mean <- as.numeric(func1(strsplit(summaryStats[4, 1], "\\s{1,}"), 3)))

# Get MSFT mean
MSFT_mean <- as.numeric(func1(strsplit(summaryStats[4, 1], "\\s{1,}"), 3))

# Get c_1 = mu_1 - mu_2
(c_1 <- MSFT_mean - AAPL_mean)

# Get AAPL variance (sigma_2^2)
AAPL_variance <- variances[1, 1]

# Get MSFT variance (sigma_1^2)
MSFT_variance <- variances[2, 2]

# Get the covariance 
stock_covariance <- variances[2, 1] # Could also say [1, 2]

# Get a
(a <- MSFT_variance + AAPL_variance - (2 * stock_covariance))

# Get b
(b <- AAPL_variance - stock_covariance)

# Get c
(c <- AAPL_variance)


# Do part ii --------------------------------------------------------------

# Solve for w (note we want the smallest w as we showed in Part B) Note it needs to meet constraint of less than or equal to sigma_1^2
d <- AAPL_variance - MSFT_variance # This makes c for quadratic formula

# Now use quadratic formula since we have aw^2 - 2bw + c = 0
lower_quadratic <- ((-1 * -2 * b) - (sqrt((-2 * b)^2 - (4 * a * d)))) / (2 * a)
upper_quadratic <- ((-1 * -2 * b) + (sqrt((-2 * b)^2 - (4 * a * d)))) / (2 * a)

# Since the inequality should hold between the lower and upper quadratic bounds then we use the lower bound to be our answer

# Part ii answer
lower_quadratic


# Part iii ----------------------------------------------------------------

### Estimate the value-at-risk (VaR) separately for both AAPL and MSFT with confidence alpha = 0.95.

# Get the VaR 1 - alpha quantile
alpha = 0.95
quantile_value = 1 - alpha

# VaR is found by doing quantile(X, probs = alpha. Note we have variances right now
# Find value at risk for MSFT
(MSFT_VaR <- quantile(MSFT.returns, probs = quantile_value))

# Find value at risk for AAPL
(AAPL_VaR <- quantile(AAPL.returns, probs = quantile_value))


# Part iv -----------------------------------------------------------------

### Find w that minimizes the VaR for the combined portfolio of AAPL and MSFT for alpha = 0.95.

# Add returns together for the mu_1 - mu_2 part
both_returns <- MSFT.returns - AAPL.returns

# Values that w can be to have variance inequality hold
w_scalar <- seq(round(lower_quadratic, 4), upper_quadratic, by = 0.0001)

# Set up loop to find w values
temp <- data.frame()
VaR_values <- data.frame()

for (i in w_scalar) {
  temp <- quantile(AAPL.returns + (i * both_returns), probs = quantile_value)[1]
  VaR_values <- rbind(VaR_values, temp)
}

# Combine the w values with the VaR for each
VaR_values <- cbind(w_scalar, VaR_values)

# Rename columns
colnames(VaR_values) <- c("w", "VaR")

# Check to make sure all values are negative
VaR_values[VaR_values$VaR >= 0, ] # No rows so all are negative (preceived loss)

# Find the max VaR value (since we want VaR to be as close to zero and all values are negative) and the corresponding w with it
VaR_values[which.max(VaR_values$VaR),] # As we can see when w = 0.6891 our VaR is the best and we know that this meets the criteria for variance

# Check variance if not sure
w_check <- VaR_values[which.max(VaR_values$VaR), "w"]

(a * w_check^2) - (2 * b * w_check) + c <= MSFT_variance # Since true we met condition of variance and our value of w = 0.6891 holds as best to minimize VaR
