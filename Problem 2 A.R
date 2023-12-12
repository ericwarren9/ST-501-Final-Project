# install.packages("tidyverse) # Install if needed to run program
library(tidyverse)

# Set the seed for reproducibility
set.seed(123)

# Parameters
lambda <- 1
N <- 1000
max_n <- 50

# Create a list to store the datasets
datasets_list <- vector("list", N * max_n)

# Generate N datasets for sample sizes 1 to 50
counter <- 1
for (i in 1:N) {
  for (n in 1:max_n) {
    # Generate a random sample of size n from exponential distribution
    dataset <- rexp(n, rate = lambda)
    
    # Store the dataset in the list
    datasets_list[[counter]] <- dataset
    counter <- counter + 1
  }
}

# Create a data frame to store minimum values and sample sizes
min_values_df <- data.frame(Minimum = numeric(N * max_n), SampleSize = numeric(N * max_n))

# Populate the data frame with minimum values and sample sizes
counter <- 1
for (i in 1:N) {
  for (n in 1:max_n) {
    # Extract minimum value
    min_value <- min(datasets_list[[counter]])
    
    # Store minimum value and sample size in the data frame
    min_values_df$Minimum[counter] <- min_value
    min_values_df$SampleSize[counter] <- n
    
    counter <- counter + 1
  }
}

min_values_df$prob <- ifelse(abs(min_values_df$Minimum) < 0.05, 0, 1)

percentage_zeros <- min_values_df %>%
  group_by(SampleSize) %>%
  summarize(PercentageZeros = mean(prob == 0) * 100)

ggplot(percentage_zeros, aes(x = SampleSize, y = PercentageZeros)) +
  geom_line() +
  labs(title = "P( |X(1) - 0| < 0.05 ) ",
       x = "Sample Size",
       y = "Probability")