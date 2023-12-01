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
summary(cbind(AAPL.returns, MSFT.returns))
cov(cbind(AAPL.returns, MSFT.returns))
