# Load necessary libraries
library(ggplot2)

# Load the data from a CSV file
india_new_cases <- read.csv(sprintf("%s/datasets/cleaned/india_cleaned_globalCases.csv", getwd()), stringsAsFactors = FALSE)
india_transformed_vaccinations <- read.csv(sprintf("%s/datasets/original/india_tf_vaccinations.csv", getwd()), stringsAsFactors = FALSE)

# INDIA
# Convert the columns to numeric if they are not already
india_new_cases$New_cases <- as.numeric(india_new_cases$New_cases)
india_transformed_vaccinations$daily_vaccinations <- as.numeric(india_transformed_vaccinations$daily_vaccinations)

# Handle possible conversion errors if there are non-numeric characters
india_new_cases$New_cases[is.na(india_new_cases$New_cases)] <- 0
india_transformed_vaccinations$daily_vaccinations[is.na(india_transformed_vaccinations$daily_vaccinations)] <- 0

# Merge the data frames
merged_data <- merge(india_new_cases, india_transformed_vaccinations, by = "Date")

# Fit a linear model
model <- lm(New_cases ~ daily_vaccinations, data = merged_data)

# Plot the data and the regression line using base R
plot(merged_data$daily_vaccinations, merged_data$New_cases,
    main = "Linear Regression: New Cases vs Daily Vaccinations",
    xlab = "Daily Vaccinations", ylab = "New Cases", pch = 19
)
abline(model, col = "blue")

# Alternatively, plot the data and the regression line using ggplot2
linear_regression_plot <- ggplot(merged_data, aes(x = daily_vaccinations, y = New_cases)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    ggtitle("Linear Regression: New Cases vs Daily Vaccinations") +
    xlab("Daily Vaccinations") +
    ylab("New Cases")


ggsave(sprintf("%s/results/predictive/india_linear_regression.jpg", getwd()), linear_regression_plot, width = 10, height = 6)

# ITALY
# Convert the columns to numeric if they are not already
italy_new_cases$New_cases <- as.numeric(italy_new_cases$New_cases)
transformed_vaccinations$daily_vaccinations <- as.numeric(transformed_vaccinations$daily_vaccinations)

# Handle possible conversion errors if there are non-numeric characters
italy_new_cases$New_cases[is.na(italy_new_cases$New_cases)] <- 0
transformed_vaccinations$daily_vaccinations[is.na(transformed_vaccinations$daily_vaccinations)] <- 0

# Merge the data frames
merged_data <- merge(india_new_cases, transformed_vaccinations, by = "Date")

# Fit a linear model
model <- lm(New_cases ~ daily_vaccinations, data = merged_data)

# Plot the data and the regression line using base R
plot(merged_data$daily_vaccinations, merged_data$New_cases,
    main = "Linear Regression: New Cases vs Daily Vaccinations",
    xlab = "Daily Vaccinations", ylab = "New Cases", pch = 19
)
abline(model, col = "blue")

# Alternatively, plot the data and the regression line using ggplot2
linear_regression_plot <- ggplot(merged_data, aes(x = daily_vaccinations, y = New_cases)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    ggtitle("Linear Regression: New Cases vs Daily Vaccinations") +
    xlab("Daily Vaccinations") +
    ylab("New Cases")


ggsave(sprintf("%s/results/predictive/india_linear_regression.jpg", getwd()), linear_regression_plot, width = 10, height = 6)

# ITALY
italy_new_cases <- read.csv(sprintf("%s/datasets/cleaned/italy_cleaned_globalCases.csv", getwd()), stringsAsFactors = FALSE)
italy_transformed_vaccinations <- read.csv(sprintf("%s/datasets/original/india_tf_vaccinations.csv", getwd()), stringsAsFactors = FALSE)

italy_new_cases$New_cases <- as.numeric(italy_new_cases$New_cases)
italy_transformed_vaccinations$daily_vaccinations <- as.numeric(italy_transformed_vaccinations$daily_vaccinations)

# Handle possible conversion errors if there are non-numeric characters
italy_new_cases$New_cases[is.na(italy_new_cases$New_cases)] <- 0
italy_transformed_vaccinations$daily_vaccinations[is.na(italy_transformed_vaccinations$daily_vaccinations)] <- 0

# Merge the data frames
merged_data <- merge(italy_new_cases, italy_transformed_vaccinations, by = "Date")

# Fit a linear model
model <- lm(New_cases ~ daily_vaccinations, data = merged_data)

# Plot the data and the regression line using base R
plot(merged_data$daily_vaccinations, merged_data$New_cases,
    main = "Linear Regression: New Cases in vs Daily Vaccinations",
    xlab = "Daily Vaccinations", ylab = "New Cases", pch = 19
)
abline(model, col = "blue")

# Alternatively, plot the data and the regression line using ggplot2
linear_regression_plot <- ggplot(merged_data, aes(x = daily_vaccinations, y = New_cases)) +
    geom_point() +
    geom_smooth(method = "lm", color = "blue") +
    ggtitle("Linear Regression: New Cases vs Daily Vaccinations") +
    xlab("Daily Vaccinations") +
    ylab("New Cases")


ggsave(sprintf("%s/results/predictive/italy_linear_regression.jpg", getwd()), linear_regression_plot, width = 10, height = 6)
