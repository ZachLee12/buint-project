# Load libraries
library(dplyr)
library(zoo)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)


##############################
#       Loading Data         #
##############################
write.csv(tf_vaccinations, "/Users/vanessa/Documents/HSLU/Semester_5/BUINT/BI_Project/tf_vaccinations.csv", row.names = FALSE)

# Upload the CSV files and convert the dates immediately to ensure format consistency
ds_vaccinations_india <- read.csv("original_vaccinations_india.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Upload the CSV files and convert the dates immediately to ensure format consistency
ds_vaccinations_italy <- read.csv("original_vaccinations_italy.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Upload the CSV files and convert the dates immediately to ensure format consistency
ds_covidCases_global <- read.csv("original_covidCases_global.csv")

# Source: https://data.oecd.org/pop/population.html
ds_population_data <- read.csv("original_oecd_population_data.csv")


##############################
#       Cleaning Data        #
##############################

### Clean Vaccinations Data ###
# Performing union
union_vaccinations <- union(ds_vaccinations_india, ds_vaccinations_italy)

# Replacing NA values in total_boosters with 0
cln_union_vaccinations <- union_vaccinations %>%
  mutate(total_boosters = replace_na(total_boosters, 0))

# people_fully_vaccinated contains NA. instead of deleting these rows or using a default value,
# the NA will be replaced with the arithemtic mean of the previous day and the day after
cln_union_vaccinations <- cln_union_vaccinations %>%
  arrange(location, date) %>%
  group_by(location) %>%
  mutate(
    people_fully_vaccinated = ifelse(
      is.na(people_fully_vaccinated),
      (lag(people_fully_vaccinated, 1, default = NA) + lead(people_fully_vaccinated, 1, default = NA)) / 2,
      people_fully_vaccinated
    )
  )

# Remove 'source_url' column from dataset
cln_union_vaccinations <- select(cln_union_vaccinations, -source_url)

# Extract the year of date into a separate column
cln_union_vaccinations <- cln_union_vaccinations %>%
  mutate(Year = as.integer(format(date, "%Y")))

# Add Month column based on the Date column
cln_union_vaccinations <- cln_union_vaccinations %>%
  mutate(Month = month(date))

# Convert Year-Month into a Date format
cln_union_vaccinations <- cln_union_vaccinations %>%
  mutate(
    YearMonth = paste(Year, Month, "01", sep = "-"), # Adding "01" as the day
    YearMonth = as.Date(YearMonth, format = "%Y-%m-%d")
  )

# Renaming columns
cln_union_vaccinations <- cln_union_vaccinations %>%
  rename(
    Location = location,
    Date = date,
    Vaccine = vaccine
  )



### Clean Covid Cases Data ###
# Dropping columns Country_code and WHO_region
cln_covidCases_global <- ds_covidCases_global %>%
  select(-Country_code, -WHO_region)

# Filtering for Italy and India in the Location column
cln_covidCases <- cln_covidCases_global %>%
  filter(Country %in% c("Italy", "India"))



### Clean Population Data ###
# Selecting and renaming columns for ds_population_data
cln_population_data <- ds_population_data %>%
  select(Location = LOCATION, Year = TIME, Population = Value)

# Filtering for ITA and IND, and then replacing with Italy and India
# adjusting the Population column -> float numbers in millions to integer value
cln_population_data <- cln_population_data %>%
  filter(Location %in% c("ITA", "IND")) %>%
  mutate(
    Location = case_when(
      Location == "ITA" ~ "Italy",
      Location == "IND" ~ "India",
      TRUE ~ Location # Default case, if you add more countries later
    ),
    Population = Population * 1000000
  )

# Add population for the year 2023 based on the growth between 2021-2022
# Filter data for 2021 and 2022 for Italy and India
recent_population_data <- cln_population_data %>%
  filter(Year %in% c(2021, 2022), Location %in% c("Italy", "India"))

# Calculate the difference between 2022 and 2021 for each location
population_difference <- recent_population_data %>%
  arrange(Location, Year) %>%
  group_by(Location) %>%
  summarise(Difference = diff(Population))

# Extract the population data for 2022
population_2022 <- recent_population_data %>%
  filter(Year == 2022)

# Estimate 2023 population
population_2023 <- population_2022 %>%
  left_join(population_difference, by = "Location") %>%
  mutate(
    Year = 2023,
    Population = Population + Difference
  ) %>%
  select(Location, Year, Population)

# Add the 2023 estimates to the original data frame
cln_population_data <- bind_rows(cln_population_data, population_2023)



##############################
#     Transforming Data      #
##############################

# Joining the data frame cln_union_vaccinations with cln_population_data
tf_vaccinations <- left_join(cln_union_vaccinations, cln_population_data, by = c("Location" = "Location", "Year" = "Year")) %>%
  select(Location, Date, Year, Month, YearMonth, Vaccine, total_vaccinations, people_vaccinated, people_fully_vaccinated, total_boosters, Population)

# Calculate deltas for total_vaccinations, people_vaccinated, people_fully_vaccinated and total_boosters
tf_vaccinations <- tf_vaccinations %>%
  arrange(Location, Date) %>%
  group_by(Location) %>%
  mutate(
    daily_vaccinations = ifelse(is.na(lag(total_vaccinations)), total_vaccinations, total_vaccinations - lag(total_vaccinations)),
    daily_people_vaccinated = ifelse(is.na(lag(people_vaccinated)), people_vaccinated, people_vaccinated - lag(people_vaccinated)),
    daily_people_fully_vaccinated = ifelse(is.na(lag(people_fully_vaccinated)), people_fully_vaccinated, people_fully_vaccinated - lag(people_fully_vaccinated)),
    daily_total_boosters = ifelse(is.na(lag(total_boosters)), total_boosters, total_boosters - lag(total_boosters))
  )

# Calculate
# Daily Vaccination Rate: Calculate the rate of vaccination per day as a percentage of the total population
# Cumulative Vaccination Rate: The proportion of the total population that has been vaccinated up to each date
# Proportion Fully Vaccinated: This is a key metric to understand how much of the population has completed the full vaccination regimen
# Booster Shot Rate: Analyze how many people have received booster shots as a percentage of those fully vaccinated.
tf_vaccinations <- tf_vaccinations %>%
  mutate(
    Daily_Vaccination_Rate = (daily_vaccinations / Population) * 1000, # per 1000 people
    Cumulative_Vaccination_Rate = (total_vaccinations / Population) * 100, # as a percentage
    Proportion_Fully_Vaccinated = (people_fully_vaccinated / Population) * 100, # as a percentage
    Booster_Shot_Rate = ifelse(people_fully_vaccinated > 0, (total_boosters / people_fully_vaccinated) * 100, 0) # as a percentage
  )



##############################
#       Plotting Data        #
##############################

# Plotting over time
ggplot(tf_vaccinations, aes(x = Date, y = Daily_Vaccination_Rate, group = Location, color = Location)) +
  geom_line() +
  labs(title = "Daily Vaccination Rate Over Time", x = "Date", y = "Daily Vaccination Rate") +
  theme_minimal()


tf_vaccinations_long <- tf_vaccinations %>%
  select(Location, Date, Daily_Vaccination_Rate, Cumulative_Vaccination_Rate, Proportion_Fully_Vaccinated, Booster_Shot_Rate) %>%
  pivot_longer(cols = -c(Location, Date), names_to = "Metric", values_to = "Value")

vaccination_metrics_plot <- ggplot(tf_vaccinations_long, aes(x = Date, y = Value, color = Location)) +
  geom_line() +
  facet_wrap(~Metric, scales = "free_y") +
  labs(title = "Vaccination Metrics Over Time", x = "Date", y = "Value") +
  theme_minimal()

ggsave("one.jpg", vaccination_metrics_plot, width = 10, height = 6)


# Order data by YearMonth
tf_vaccinations <- tf_vaccinations %>%
  arrange(YearMonth)

# Grouped Bar Plot for Cumulative Vaccination Rate for Each Year-Month
cumulative_vaccination_plot <- ggplot(tf_vaccinations, aes(x = YearMonth, y = Cumulative_Vaccination_Rate, fill = Location)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Cumulative Vaccination Rate by Year-Month and Location", x = "Year-Month", y = "Cumulative Vaccination Rate (%)", fill = "Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # Rotate x-axis labels for readability

ggsave("two.jpg", cumulative_vaccination_plot, width = 10, height = 6)
