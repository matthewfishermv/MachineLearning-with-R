library(e1071)
library(caret)
library(tidyverse)
library(RColorBrewer)

# Load the email spam data and wrangle it.
email <- read.csv("data/email-spam.csv") %>% tibble()

# Factorize the output variable with "Yes" and "No".
email$spam <- factor(
  ifelse(email$spam == 1, "Yes", "No"),
  levels = c("Yes", "No"))

# Visualize the distribution of emails.
email %>%
  group_by(spam) %>%
  count(name = "freq") %>%
  ungroup() %>%
  mutate(spam = fct_reorder(spam, levels(spam))) %>%
  ggplot(aes(x = spam, y = freq, fill = spam)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Spam Emails\n",
    x = "Spam\n",
    y = "\n# Emails",
    fill = "Spam"
  ) +
  coord_flip() +
  theme_minimal() +
  scale_fill_brewer(palette = "Paired")

# Partition the data into a training set and test set.
test.index <- sample(1:nrow(email), size = floor(0.75 * nrow(jobs)))
email$test <- F
email$test[test.index] <- T

# Train a Naive Bayes classifier on the training data.
set.seed(7483)
nb <- naiveBayes(spam ~ ., data = email, subset = which(test == T))

# Use the model to predict the test data.
pred <- predict(nb, email[email$test == F,], type = "class")

# Evaluate the model.
confusionMatrix(pred, email$spam[email$test == F])
