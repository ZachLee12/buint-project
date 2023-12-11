# Load necessary library
library(tidyverse)



# Enter desired country name here
country1 <- "italy"
country2 <- "india"

# ITALY
# Load the data
italy_data <- read.csv(sprintf("%s/datasets/original/cases/%s_globalCases.csv", getwd(), country1))
italy_data$Date <- as.Date(italy_data$Date_reported, format = "%Y-%m-%d")

# Check for columns and inspect the first few rows
str(italy_data)
head(italy_data)

# Remove rows with NAs or null values in the 'New Cases' column
italy_cleaned_data <- italy_data %>%
    filter(!is.na(`New_cases`)) %>%
    select(Date, Country_code, Country, `New_cases`)

# Optionally, write the cleaned data to a new CSV file
write.csv(italy_cleaned_data, sprintf("%s/datasets/cleaned/%s_cleaned_globalCases.csv", getwd(), country1), row.names = FALSE)


# INDIA
# Load the data
india_data <- read.csv(sprintf("%s/datasets/original/cases/%s_globalCases.csv", getwd(), country2))
india_data$Date <- as.Date(india_data$Date_reported, format = "%Y-%m-%d")

# Check for columns and inspect the first few rows
str(india_data)
head(india_data)

# Remove rows with NAs or null values in the 'New Cases' column
india_cleaned_data <- india_data %>%
    filter(!is.na(`New_cases`)) %>%
    select(Date, Country_code, Country, `New_cases`)

# Optionally, write the cleaned data to a new CSV file
write.csv(india_cleaned_data, sprintf("%s/datasets/cleaned/%s_cleaned_globalCases.csv", getwd(), country2), row.names = FALSE)
