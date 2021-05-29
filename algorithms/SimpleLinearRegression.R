library(UsingR)
library(tidyverse)
library(caret)

# Example 1: Father-son heights
###############################

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
    y = "Son's Height (in.)\n"
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
  Error = test.heights$sheight - pred
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


# Example 2: M1 money supply
############################
# Load the data.
# Source shown in url below.
url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1168&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=WM1NS&scale=left&cosd=1975-01-06&coed=2021-04-05&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Weekly%2C%20Ending%20Monday&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2021-05-22&revision_date=2021-05-22&nd=1975-01-06"
m1 <- read.csv(url) %>% tibble()

# Wrangle the data.
m1 <- m1 %>%
  rename(Date = DATE, Supply = WM1NS) %>%
  mutate(Date = as.Date(Date))

# Visualize the underlying data.
m1 %>%
  ggplot(aes(x = Date, y = Supply)) +
  geom_line() +
  geom_smooth(method = "lm", formula = y ~ x) +
  scale_y_log10() +
  theme_tq() +
  labs(
    title = "M1 Money Supply\n",
    x = "\nDate",
    y = "Money Supply (Billions of USD)\n",
    caption = "Source: FRED Economic Data"
  )

# Notice there is a large spike in the recent data. Check for outliers.
stats <- tibble(
  Mean = mean(m1$Supply),
  StandardDeviation = sd(m1$Supply),
  Q1 = summary(m1$Supply)[2],
  Q3 = summary(m1$Supply)[5]
)

iqr <- stats$Q3 - stats$Q1

outlier.low <- stats$Q1 - (1.5 * iqr)
outlier.high <- stats$Q3 + (1.5 * iqr)

m1$Supply[m1$Supply > outlier.high]

# Too many outliers to remove with this method.

# Build a train-test split, randomly selecting 75% of the data.
set.seed(23482394) # Included for reproducibility.
train.index <- sample(index(m1), size = floor(nrow(m1) * 0.75), replace = FALSE)
test.index <- which(!(index(m1) %in% train.index))

train.data <- m1[train.index,]
test.data <- m1[test.index,]

# Train a linear regression model.
model <- lm(Supply ~ Date, data = train.data)

# Predict the test data using the trained model.
pred <- predict(model, test.data, type = "response")

# Evaluate the model performance.
summary(model)

tibble(
  GroundTruth = test.data$Supply,
  Prediction = pred,
  Error = Prediction - GroundTruth
)
