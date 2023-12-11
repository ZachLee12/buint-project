# Load necessary libraries
library(ggplot2)

# Load the data from a CSV file
india_new_cases <- read.csv(sprintf("%s/datasets/cleaned/india_cleaned_globalCases.csv", getwd()), stringsAsFactors = FALSE)
transformed_vaccinations <- read.csv(sprintf("%s/datasets/original/tf_vaccinations.csv", getwd()), stringsAsFactors = FALSE)


# Convert the columns to numeric if they are not already
india_new_cases$New_cases <- as.numeric(india_new_cases$New_cases)
transformed_vaccinations$daily_vaccinations <- as.numeric(transformed_vaccinations$daily_vaccinations)

# Handle possible conversion errors if there are non-numeric characters
india_new_cases$New_cases[is.na(india_new_cases$New_cases)] <- 0
transformed_vaccinations$daily_vaccinations[is.na(transformed_vaccinations$daily_vaccinations)] <- 0


print(transformed_vaccinations$daily_vaccinations)
