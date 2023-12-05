# Purpose: To make table to find the smallest size of n needed with our set parameters


# Initialize values -------------------------------------------------------

percentile <- c(.01, .05, .1, .2)
alpha <- c(.95, .99, .999)

final_table <- expand.grid(percentile = percentile, alpha = alpha)
value_table <- data.frame()
temp <- data.frame()


# Make for loop for values ------------------------------------------------

for (a in alpha){
  for (p in percentile){
    temp <- ceiling(log(1-a) / log(1-p))
    value_table <- rbind(value_table, temp)
  }
}


# Update and make final table ---------------------------------------------

colnames(value_table) <- "n"

(final_table <- cbind(final_table, value_table))