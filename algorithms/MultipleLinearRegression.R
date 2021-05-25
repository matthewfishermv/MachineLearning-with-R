library(tidyverse)
library(ggplot2)
library(plot3D)
library(caret)

# Load the data.
# Source: http://archive.ics.uci.edu/ml/datasets/Auto+MPG
autos <- read.csv("data/auto_mpg.data", header = F) %>% tibble()

# Rename columns.
colnames(autos) <- c(
  "mpg",
  "cylinders",
  "displacement",
  "horsepower",
  "weight",
  "acceleration",
  "model",
  "origin",
  "carname"
)

# Clean the data.
autos <- autos %>%
  mutate(mpg = as.double(mpg)) %>%
  filter(!is.na(mpg)) %>%
  filter(!is.na(weight)) %>%
  filter(!is.na(displacement))

# Plot the data in 3 dimensions.
scatter3D(autos$weight, autos$displacement, autos$mpg,
          pch = 16, bty = "g", cex = 1.25,
          phi = 25, theta = 50,
          colkey = F, ticktype = "detailed",
          col = ramp.col(c("steelblue2", "green", "orange", "firebrick1")),
          main = "Automobile Miles per Gallon",
          xlab = "\nWeight", ylab = "\nDisplacement", zlab = "\nMiles per Gallon")

# Create a train-test split.
train.data <- autos[1:floor(0.75 * nrow(autos)),]
test.data <- autos[(floor(0.75 * nrow(autos)) + 1):nrow(autos),]

# Train a multiple linear regression model on the training data.
model <- lm(mpg ~ weight + displacement, data = autos)

# Predict the test data using the model.
pred <- predict(model, test.data, type = "response")

# Evaluate the model.
tibble(
  Predicted = pred,
  Actual = test.data$mpg,
  Error = Predicted - Actual
) %>%
  ggplot(aes(x = Error)) +
  geom_histogram() +
  labs(
    title = "Prediction Errors in Multiple Regression Model",
    subtitle = "Automobile Miles per Gallon\n",
    x = "\nPredicted Miles per Gallon",
    y = "Frequency\n"
  ) +
  theme_bw()

# Add the regression hyperplane to the 3D plot.
grid.lines <- 14
x.pred <- seq(min(autos$weight), max(autos$weight), length.out = grid.lines)
y.pred <- seq(min(autos$displacement), max(autos$displacement), length.out = grid.lines)
xy <- expand_grid(x = x.pred, y = y.pred)
colnames(xy) = c("weight", "displacement")
z.pred <- matrix(predict(model, xy), nrow = grid.lines, ncol = grid.lines)

scatter3D(autos$weight, autos$displacement, autos$mpg,
          pch = 16, bty = "g", cex = 1.25,
          phi = 25, theta = 50,
          colkey = F, ticktype = "detailed",
          col = ramp.col(c("steelblue2", "green", "orange", "firebrick1")),
          main = "Automobile Miles per Gallon",
          xlab = "\nWeight", ylab = "\nDisplacement", zlab = "\nMiles per Gallon",
          surf = list(x = x.pred, y = y.pred, z = z.pred, facets = NA))
