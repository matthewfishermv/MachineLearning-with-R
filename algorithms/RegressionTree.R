library(UsingR)
library(tidyverse)
library(grid)
library(gridExtra)
library(rpart)
library(rpart.plot)


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

# Train a regression tree on the training set.
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
