library(tree)
library(caret)
library(tidyverse)

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
