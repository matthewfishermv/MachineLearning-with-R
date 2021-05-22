library(tidyverse)
library(effects)

# Load and wrangle the data.
# Data source: https://data.world/crowdflower/blockbuster-database
movies <- read.csv("../data/blockbuster-top_ten_movies_per_year_DFE.csv") %>% tibble()

movies <- movies %>%
  filter(!is.na(movies$length)) # Remove NA values.

# Add an outcome variable of that rates a movie as successful if IMDB rating >= 7.
movies$successful <- factor(ifelse(movies$imdb_rating >= 7, TRUE, FALSE),
                            levels = c(FALSE, TRUE))

# View the underlying distribution of successful vs. unsuccessful movies.
movies %>%
  mutate(successful = ifelse(successful == T, "Yes", "No")) %>%
  ggplot(aes(x = successful, fill = successful)) +
  geom_bar() +
  coord_flip() +
  labs(
    title = "Number of Success and Unsuccessful Movies\n",
    x = "Succesful\n",
    y = "\n# Movies"
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )

# Create a train-test split, using the first 300 movies for training.
train.data <- movies[1:300,]
test.data <- movies[301:nrow(movies),]

# Construct a simple logistic model that predicts whether a movie will be successful
# based on its runtime.
model <- glm(successful ~ length, train.data, family = "binomial")

plot(allEffects(model),
     main = "Movie Success by Rating - Logistic Model",
     xlab = "Runtime (min.)", ylab = "Probability of Success")

# Predict the movie's success using a cutoff of 0.5.
prob <- predict(model, test.data, type = "response")
pred <- ifelse(prob >= 0.5, TRUE, FALSE)

# Evaluate the performance.
confusionMatrix(factor(pred), test.data$successful)

# Construct another logistic model the predicts whether a movie will be successful
# based on its rating and runtime.
model.2 <- glm(successful ~ rating + length, train.data, family = "binomial")

plot(allEffects(model.2),
     ylab = "Successful\n")

# Predict the movie's success using a cutoff of 0.5.
prob.2 <- predict(model.2, test.data, type = "response")
pred.2 <- ifelse(prob.2 >= 0.5, TRUE, FALSE)

# Evaluate the performance.
confusionMatrix(factor(pred.2), test.data$successful)


# Improving model performance
#############################
# Construct a function to test different cutoff values.
assess_model <- function(model, test.data, cutoff) {
  prob <- predict(model, test.data, type = "response")
  pred <- ifelse(prob >= cutoff, TRUE, FALSE)
  
  confuse <- confusionMatrix(factor(pred), test.data$successful)
  accuracy <- confuse$overall[2]
  
  return(tibble(
    Cutoff = cutoff,
    Accuracy = accuracy
  ))
}

# Find the optimal cutoff for model 1 (highest accuracy).
performance <- lapply(seq(0.25, 0.75, by = 0.005), function(x) {
  assess_model(model, test.data, x)
}) %>% bind_rows()

best.cutoff <- performance$Cutoff[which(performance$Accuracy == max(performance$Accuracy))]

plot(Accuracy ~ Cutoff, performance, type = "l",
     main = "Model 1 Optimal Cutoff\n")
abline(v = best.cutoff,
       lwd = 2, col = "steelblue3")
mtext(text = paste0("Best cutoff = ", best.cutoff), col = "steelblue3")

# Find the optimal cutoff for model 2 (highest accuracy).
performance <- lapply(seq(0.25, 0.75, by = 0.005), function(x) {
  assess_model(model.2, test.data, x)
}) %>% bind_rows()

best.cutoff <- performance$Cutoff[which(performance$Accuracy == max(performance$Accuracy))]

plot(Accuracy ~ Cutoff, performance, type = "l",
     main = "Model 1 Optimal Cutoff\n")
abline(v = best.cutoff,
       lwd = 2, col = "steelblue3")
mtext(text = paste0("Best cutoff = ", best.cutoff), col = "steelblue3")
