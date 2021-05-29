library(tidyverse)
library(rpart)
library(rpart.plot)

# Load the data.
# Source: Mammal sleep data set (Savage and West, 2007).
sleep <- msleep

# Explore the data.
# Plot a histogram of the response variable sleep_total.
sleep %>% ggplot(aes(x = sleep_total)) + geom_histogram(bins = 15)

# Create a train-test split.
train.index <- sample(1:nrow(sleep), size = floor(0.75 * nrow(sleep)), replace = F)
test.index <- (1:nrow(sleep))[-train.index]

sleep.train <- sleep[train.index,]
sleep.test <- sleep[test.index,]

# Train and plot a regression tree model.
model <- rpart(sleep_total ~ brainwt + bodywt, data = sleep)
rpart.plot(model, main = "Modelling Hours of Sleep for Mammals")

# Use the model to predict sleep hours.
pred <- predict(model, newdata = sleep.test)

# Evaluate the results.
tibble(
  Sleep = sleep.test$sleep_total,
  Predicted = pred,
  Error = (Predicted - Sleep)
) %>%
  ggplot(aes(x = Sleep, y = Predicted, color = Error)) +
  geom_point(size = 3) +
  scale_color_gradient2(low = "firebrick1", mid = "yellow", high = "firebrick1") +
  theme_bw() +
  labs(
    title = "Predicted Sleep for Mammals\n",
    x = "\nActual Sleep (hours)",
    y = "Predicted Sleep (hours)\n"
  ) +
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "lightgray")
