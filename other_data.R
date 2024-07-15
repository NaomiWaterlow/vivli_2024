# ------------------------------------------------------------------------------
### Cleaning other data
# July 10
# ------------------------------------------------------------------------------
# remove all objects from workspace
rm (list = ls ())

# load libraries
library(countrycode)
library(dplyr)
library(tidyr)
# ------------------------------------------------------------------------------


# C-section---------------------------------------------------------------------
# Import caesarean section data
dt_CaesareanSection <- read.csv("Other_data/CaesareanSection.csv")

# Rename columns
colnames(dt_CaesareanSection) <- c("country_name", "year", "c_section")

# Add country code using country names
dt_CaesareanSection$country_code <- countrycode(dt_CaesareanSection$country_name, "country.name", "iso3c")

# Select only the relevant columns
dt_CaesareanSection <- dt_CaesareanSection[, c("country_code", "c_section")]
# ------------------------------------------------------------------------------


# Birth rate--------------------------------------------------------------------
# Import birth rate data
dt_BirthRate <- read.csv("Other_data/BirthRate.csv", skip = 4, header = FALSE)

# Set the column names to be the values of the first row
colnames(dt_BirthRate) <- dt_BirthRate[1, ]

# Remove the first row, which is now the header
dt_BirthRate <- dt_BirthRate[-1, ]

# Create a new column 'birth_rate' which is the average of the specified years
# average of 2015-2019 (before covid) 
dt_BirthRate <- dt_BirthRate %>%
  mutate(birth_rate = rowMeans(select(., `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE))

# Rename column
dt_BirthRate <- dt_BirthRate %>%
  rename(country_code = `Country Code`)

# Select only the relevant columns
dt_BirthRate <- dt_BirthRate[, c("country_code", "birth_rate")]
# ------------------------------------------------------------------------------


# GDP---------------------------------------------------------------------------
# Import data
dt_GDP <- read.csv("Other_data/GDP.csv", skip = 4, header = FALSE)

# Set the column names to be the values of the first row
colnames(dt_GDP) <- dt_GDP[1, ]

# Remove the first row, which is now the header
dt_GDP <- dt_GDP[-1, ]

# Create a new column 'GDP' which is the average of the specified years
# average of 2015-2019 (before covid) 
dt_GDP <- dt_GDP %>%
  mutate(GDP = rowMeans(select(., `2015`, `2016`, `2017`, `2018`, `2019`), na.rm = TRUE))

# Rename column
dt_GDP <- dt_GDP %>%
  rename(country_code = `Country Code`)

# Select only the relevant columns
dt_GDP <- dt_GDP[, c("country_code", "GDP")]
# ------------------------------------------------------------------------------


# Education---------------------------------------------------------------------
# Import data
dt_education <- read.csv("Other_data/Education.csv")

# Rename column
dt_education <- dt_education %>%
  rename(country_code = `Country.Code`)

# Define the indicators to filter
indicators <- c(
  "Primary completion rate, male, based on completers",
  "Primary completion rate, female, based on completers"
)

# Filter the dt_education data frame
dt_education <- dt_education %>%
  filter(Indicator.Name %in% indicators)

# Pivot the data to have years as columns
dt_education <- dt_education %>%
  pivot_wider(names_from = Year, values_from = Value)

# Create a new column 'primary_completion' which is the average of the specified years
# average of 2015-2018 (before covid) - 2019 not available
dt_education <- dt_education  %>%
  mutate(primary_completion = rowMeans(select(., `2015`, `2016`, `2017`, `2018`), na.rm = TRUE))

# Create the gender column based on Indicator.Name
dt_education <- dt_education %>%
  mutate(Indicator.Name = case_when(
    Indicator.Name == "Primary completion rate, male, based on completers" ~ "primary_completion_M",
    Indicator.Name == "Primary completion rate, female, based on completers" ~ "primary_completion_F",
    TRUE ~ NA_character_  # Default case for other Indicator.Name values
  )) 

# Select only the relevant columns
dt_education <- dt_education[, c("country_code", "Indicator.Name", "primary_completion")]

# Create a data table for male primary completion rates
dt_education_M <- dt_education %>%
  filter(Indicator.Name == "primary_completion_M")

# Create a data table for female primary completion rates
dt_education_F <- dt_education %>%
  filter(Indicator.Name == "primary_completion_F")

# Merge male and female primary completion rates data frames
dt_education <- merge(dt_education_M, dt_education_F, 
                      by = "country_code", 
                      suffixes = c("_M", "_F"))

# Create the primary_completion_female_over_male column
dt_education <- dt_education %>%
  mutate(primary_completion_female_over_male = primary_completion_F / primary_completion_M)

# Select only the relevant columns
dt_education <- dt_education[, c("country_code", "primary_completion_female_over_male")]

# Merge data--------------------------------------------------------------------
# Get all country codes and create a data frame
all_country_codes <- unique(countrycode::codelist$iso3c)
dt_all_country_codes <- data.frame(country_code = all_country_codes)

# Merge datasets with the all_country_codes_df
Other_data <- dt_all_country_codes  %>%
  left_join(dt_BirthRate, by = "country_code") %>%
  left_join(dt_GDP, by = "country_code") %>%
  left_join(dt_CaesareanSection, by = "country_code") %>%
  left_join(dt_education, by = "country_code")

# Save the merged data
write.csv(Other_data, "other_data_v1.csv", row.names = FALSE)
# ------------------------------------------------------------------------------

