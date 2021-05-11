library(e1071)
library(caret)
library(tidyverse)
library(RColorBrewer)
library(tm)
library(SnowballC)

# Example 1: Email spam
#######################

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


# Example 2: SMS spam
#####################
# Data source: https://www.dt.fee.unicamp.br/~tiago/smsspamcollection/
# This example uses text mining to create a corpus and applies Naive Bayes on the corpus.

# Load and wrangle the data.
sms <- read.table("data/sms_spam_collection.txt", sep = "\t", header = F, quote = "")
names(sms) <- c("Label", "Message")
sms$Label <- factor(sms$Label)

# Find frequency and likelihood of spam messages in original data.
tibble(
  SpamMessages = sum(sms$Label == "spam"),
  SpamMessagesPercent = paste0(round(sum(sms$Label == "spam") / nrow(sms) * 100, 1), "%"),
  HamMessages = sum(sms$Label == "ham"),
  HamMessagesPercent = paste0(round(sum(sms$Label == "ham") / nrow(sms) * 100, 1), "%"),
  TotalMessages = nrow(sms)
)

# Show 3 example messages.
set.seed(34389)
sms[runif(3, 1, nrow(sms)),]

# Create a corpus of all the SMS messages.
# Inspect the first 3 documents (messages).
corpus <- VCorpus(VectorSource(sms$Message))
lapply(corpus[1:3], function(x) as.character(x))

# Clean the corpus and perform stemming.
corpus.clean <- tm_map(corpus, content_transformer(tolower))
corpus.clean <- tm_map(corpus.clean, removeNumbers)
corpus.clean <- tm_map(corpus.clean, removeWords, stopwords(kind = "en"))
corpus.clean <- tm_map(corpus.clean, removePunctuation)
corpus.clean <- tm_map(corpus.clean, stemDocument)

# Show the same 3 example messages to confirm they have been cleaned.
set.seed(34389)
lapply(corpus.clean[runif(3, 1, nrow(sms))], as.character)

# Tokenize terms and create a document-term incident matrix (DTM).
# Preview the matrix.
dtm <- DocumentTermMatrix(corpus.clean)
inspect(dtm)

# Convert the DTM to a matrix for computations.
dtm.matrix <- as.matrix(dtm)
colSums(dtm.matrix)[100:110] # Number of occurrences of words 100 - 110.

# Partition the terms into a training set and test set (75%-25% split).
dtm.train <- dtm[1:floor(0.75 * nrow(dtm)), ]
dtm.test <- dtm[(floor(0.75 * nrow(dtm)) + 1):nrow(dtm), ]

train.labels <- sms$Label[1:floor(0.75 * nrow(dtm))]
test.labels <- sms$Label[(floor(0.75 * nrow(dtm)) + 1):nrow(dtm)]

# Find terms occurring in at least 10 messages. Use these to subset the training
# and test sets.
freq.words <- findFreqTerms(dtm.train, lowfreq = 10)

dtm.train.freq <- dtm.train[, freq.words]
dtm.test.freq <- dtm.test[, freq.words]

# Convert terms to categorical (Y = term occurs, N = term does not occur).
train.data <- apply(dtm.train.freq, MARGIN = 2, FUN = function(x) ifelse(x > 0, "Y", "N"))
test.data <- apply(dtm.test.freq, MARGIN = 2, FUN = function(x) ifelse(x > 0, "Y", "N"))

# Train the Naive Bayes model on the training data. Preview the first 5 terms and
# their fitted classification probabilities.
m <- naiveBayes(train.data, train.labels)
m$tables[1:5]

# Use the model to predict the test data labels.
pred <- predict(m, test.data, type = "class")

# Evaluate the model by comparing the predicted labels against the ground truth.
confusionMatrix(pred, test.labels)

# The model performs well, but can be improved by setting the Laplace estimator to 1.
m.2 <- naiveBayes(train.data, train.labels, laplace = 1)
pred.2 <- predict(m.2, test.data, type = "class")
confusionMatrix(pred.2, test.labels)
