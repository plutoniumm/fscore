library(ggplot2)

data <- read.csv("data/drivers.csv")

ggplot(data, aes(x = year)) +
  geom_line(aes(y = swaps, color = "Swaps")) +
  geom_line(aes(y = first3, color = "First 3")) +
  labs(y = "Value", color = "Legend") +
  theme_minimal()
