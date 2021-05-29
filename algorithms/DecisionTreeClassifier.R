library(tree)
library(caret)
library(tidyverse)
library(UsingR)
library(grid)
library(gridExtra)
library(rpart)
library(rpart.plot)

# Example 1: Job interview success
##################################
# This example uses the 'tree' package to construct the decision tree.

# Load the job interview success data and wrangle it.
jobs <- read.csv("../data/job_interview_success.csv") %>% tibble()
jobs <- jobs[, -1] # Remove the first column.
jobs$ontime <- as.factor(jobs$ontime)
jobs$suit <- as.factor(jobs$suit)
jobs$landed <- as.factor(jobs$landed)

# Preview the data.
# Does there appear to be a significant difference in landing the job for
# applicants of different ages?
jobs %>%
  ggplot(aes(x = age, y = landed, color = landed)) +
  geom_boxplot() +
  labs(
    title = "Landing the Job by Age",
    x = "Age",
    y = "Landed the Job?"
  )

# View contingency tables for applicants landing the job across all other variables.
table(jobs$ontime, jobs$landed, dnn = c("Applicant was on time", "Applicant landed the job"))
table(jobs$suit, jobs$landed, dnn = c("Applicant wore a suit", "Applicant landed the job"))

# Partition the data into a training set and test set.
test.index <- sample(1:nrow(jobs), size = floor(0.75 * nrow(jobs)))
jobs$test <- F
jobs$test[test.index] <- T

# Train a decision tree classifier and plot it.
set.seed(23488)
dt <- tree(landed ~ ., data = jobs, subset = which(test == T))
summary(dt)

plot(dt)
text(dt)

# Predict test data from the decision tree.
pred <- predict(dt, jobs[jobs$test == F,], type = "class")
confusionMatrix(pred, jobs$landed[jobs$test == F])

# Tune hyperparameters.
set.seed(23488)
dt.2 <- tree(landed ~ ., data = jobs, subset = which(test == T),
             control = tree.control(mincut = 8, minsize = 20, nobs = nrow(jobs[jobs$test == T,])))

plot(dt.2)
text(dt.2)

pred.2 <- predict(dt.2, jobs[jobs$test == F,], type = "class")
confusionMatrix(pred.2, jobs$landed[jobs$test == F])


# Example 2: Starwars character height
######################################
# This example uses the 'rpart' package to construct the decision tree.

# Load the data.
# Source: UsingR package in R.
# Original Source: https://swapi.dev/
data <- starwars

# Explore the data.
p.1 <- data %>%
  ggplot(aes(x = height)) +
  geom_boxplot(alpha = 0.5) +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  scale_fill_discrete("Gender") +
  coord_flip() +
  theme_bw() +
  theme(
    plot.margin = unit(c(0.75, 0.75, 0.75, 0), "cm")
  )

p.2 <- data %>%
  ggplot(aes(x = height, fill = gender)) +
  geom_histogram(bins = 15, alpha = 0.5) +
  labs(
    title = "",
    x = "",
    y = "Frequency"
  ) +
  scale_fill_discrete("Gender") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = unit(c(0.75, 0.75, 0.75, -0.75), "cm"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

grid.arrange(p.1, p.2,
             nrow = 1, ncol = 2,
             top = textGrob("Starwars character weights are approximately normally distributed",
                            vjust = 1,
                            gp = gpar(fontface = "bold", cex = 1.2)),
             left = textGrob("Height (cm.)",
                             rot = 90,
                             gp = gpar(fontface = "bold", cex = 1))
)

# Split the data into training and test partitions.
set.seed(283493) # Included for reproducibility.
train.index <- sample(1:nrow(data), size = floor(0.85 * nrow(data)), replace = F)
test.index <- (1:nrow(data))[-train.index]

train.data <- data[train.index,]
test.data <- data[test.index,]

# Train a decision tree on the training set.
model <- rpart(height ~
                 mass + hair_color + eye_color + birth_year + sex + gender,
               data = data)

# Visualize the model.
rpart.plot(model, main = "Modelling Height of Starwars Characters")

# Use the model to predict heights in the test set.
pred <- predict(model, newdata = test.data)

# Compare the predictions against ground truth.
tibble(
  Height = test.data$height,
  Predicted = pred,
  Error = (Predicted - Height)
) %>%
  ggplot(aes(x = Height, y = Predicted, color = Error)) +
  geom_point(size = 3) +
  scale_color_gradient2(low = "firebrick1", mid = "yellow", high = "firebrick1") +
  theme_bw() +
  labs(
    title = "Predicted Heights of Starwars Characters\n",
    x = "\nActual Height",
    y = "Predicted Height\n"
  ) +
  geom_smooth(method = "lm", se = F, color = "lightgray")
