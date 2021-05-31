library(neuralnet)
library(UsingR)
library(dplyr)
library(caret)

# Load the data.
# Source: 'UsingR' package in R.
# Original Source: Dawson, Robert, 1995.
data <- as.data.frame(Titanic)

# Dummy code the factors (Usually R does this for you, but not with neuralnet models).
data.dummy <- model.matrix(
  ~ Survived + Class + Sex + Age,
  data = data
)

# Create a train-test split.
set.seed(489454) # Included for reproducibility.
train.index <- sample(1:nrow(data), size = floor(0.75 * nrow(data)))
test.index <- (1:nrow(data))[-train.index]

titanic.train <- data.dummy[train.index,]
titanic.test <- data.dummy[test.index,]

# Train a neural network and plot it.
net <- neuralnet(SurvivedYes ~ Class2nd + Class3rd + ClassCrew + SexFemale + AgeAdult,
                 data = titanic.train)
plot(net, radius = 0.1)

# Use the neural network to predict the test data.
pred <- factor(
  ifelse(predict(net, titanic.test) >= 0.5, "No", "Yes"),
  levels = c("No", "Yes")
)

# Evaluate the results of the prediction against the ground truth.
confusionMatrix(data$Survived[test.index], factor(pred))

# Tune hyperparameters to improve model performance.
# Add 5 hidden layers.
net.2 <- neuralnet(SurvivedYes ~ Class2nd + Class3rd + ClassCrew + SexFemale + AgeAdult,
                 data = titanic.train, hidden = 5)
plot(net, radius = 0.1)

pred.2 <- factor(
  ifelse(predict(net.2, titanic.test) >= 0.5, "No", "Yes"),
  levels = c("No", "Yes")
)

confusionMatrix(data$Survived[test.index], factor(pred.2))
