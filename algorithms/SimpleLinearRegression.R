library(UsingR)
library(tidyverse)
library(caret)

# Load data.
# Data source: Data set used by Pearson to investigate regression. Accessed via UsingR package.
data(father.son)

# Calculate statistics for father and son heights.
tibble(
  FatherMean = mean(father.son$fheight),
  FatherStandardDeviation = sd(father.son$fheight),
  SonMean = mean(father.son$sheight),
  SonStandardDeviation = sd(father.son$sheight)
)

# View the underlying distribution of father and son heights.
# To do this, we need to convert the data from wide format to long format.
father.son %>%
  pivot_longer(cols = c(fheight, sheight), names_to = "group", values_to = "height") %>%
  ggplot(aes(x = height, fill = group)) +
  geom_histogram(bins = 30, alpha = 0.5) +
  labs(
    title = "Father-Son Height Distribution\n",
    x = "\nHeight (in.)",
    y = "Frequency\n"
  ) +
  theme_bw() +
  scale_fill_discrete(name = "Group:", labels = c("Father", "Son"))

# Look for correlation, both visually and using correlation coefficient.
r <- cor(father.son$fheight, father.son$sheight)

father.son %>%
  ggplot(aes(x = fheight, y = sheight)) +
  geom_point() +
  labs(
    title = "Father-Son Height Correlation\n",
    x = "\nFather's Height (in.)",
    y = "Son'S Height (in.)\n"
  ) +
  theme_bw() +
  geom_smooth(method = "lm", formula = y ~ x, se = F, color = "tomato2")

# Find the equation for the linear regression line.
m <- lm(sheight ~ fheight, father.son)
summary(m)

cat(paste0("Son's height is predicted by father's height with the following ",
           "formula:\nson_height = (", round(m$coefficients[[1]], 1), " + ",
           round(m$coefficients[[2]], 1), " * father_height)."))

# Using machine learning methodology, a train-test partition would allow for testing
# the model's accuracy. The model is training on the training set and predicted on
# the test set.
train.heights <- father.son[1:809,]
test.heights <- father.son[810:nrow(father.son),]

model <- lm(sheight ~ fheight, train.heights)
pred <- predict(model, test.heights, type = "response")

# Notice this produces slightly different coefficients.
cat(paste0("Son's height is predicted by father's height with the following ",
           "formula:\nson_height = (", round(model$coefficients[[1]], 1), " + ",
           round(model$coefficients[[2]], 1), " * father_height)."))

# Compare the trained model's predictions against ground truth.
tibble(
  Predicted = pred,
  Actual = test.heights$sheight,
  Error = pred - test.heights$sheight
) %>% View()


# Model Diagnostics
###################
# Create a residual plot to assess regression model assumptions.
ggplot() +
  geom_hline(yintercept = 0, size = 1, col = "steelblue4") +
  geom_point(aes(x = pred, y = (pred - test.heights$sheight))) +
  labs(
    title = "Residual Plot - Linear Regression Model\n",
    x = "\nFitted Height (in.)",
    y = "Residual\n"
  ) +
  theme_bw()

# View the model's residuals, coefficients, coefficient test statistics, R^2 value,
# and F-statistic.
summary(model)

cat(paste0(round((summary(model)$adj.r.squared * 100), 1), "% of the variability in ",
           "son's heights is explained by the variability in father's heights."))
