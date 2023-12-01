datasets = list()
for (n in 1:50){
  datasets_n <- matrix(rexp(1000 * n, rate = 1), nrow = 1000, ncol = n)
  datasets[[as.character(n)]] <- datasets_n
}

library(ggplot2)

minvalue = data.frame(n = integer(), minval = double())
for (n in 1:50) {
  min_values <- apply(datasets[[as.character(n)]], 1, min)
  minvalue <- rbind(minvalue, data.frame(n = rep(n, 1000), minval = min_values))
}

install.packages("dyplyr")
library(dyplyr)

# Assuming your dataset is named 'your_dataset'
# Replace 'your_dataset' with the actual name of your dataset

# Summarize the data to get the minimum values for each unique 'n'
summary_data <- minvalue %>%
  group_by(n) %>%
  summarize(min_value = min(minval))

# Create a line chart
ggplot(summary_data, aes(x = n, y = min_value, group = 1)) +
  geom_line() +
  labs(title = "Line Chart of Minimum Values",
       x = "Sample Size (n)",
       y = "Minimum Value")