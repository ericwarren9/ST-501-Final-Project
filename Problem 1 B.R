# Purpose: To make table to find the smallest size of n needed with our set parameters


# Initialize values -------------------------------------------------------

# install.packages("tidyverse") # Uncomment code if you do not have package already
library(tidyverse)

percentile <- c(.01, .05, .1, .2)
alpha <- c(.95, .99, .999)
n <- 1:1000

final_table <- expand.grid(percentile = percentile, alpha = alpha, n = n)
value_table <- data.frame()
temp <- data.frame()


# Make for loop for values ------------------------------------------------

for (n in n){
  for (a in alpha){
    for (p in percentile){
      temp <- ceiling(1 - (1-p)^n -p^n >= a)
      value_table <- rbind(value_table, temp)
    }
  }
}


# Update and make final table ---------------------------------------------

colnames(value_table) <- "condition_met"

# Get only values where n is met
(
  final_table <- cbind(final_table, value_table) %>%
    filter(condition_met == 1) %>%
    mutate(Var = map2_chr(percentile, alpha, ~toString(sort(c(.x, .y))))) %>%
    distinct(Var, .keep_all = TRUE) %>%
    select(-c(Var, condition_met)) %>%
    arrange(percentile, alpha)
)