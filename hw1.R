library(dplyr)
library(lubridate)

# Load the dataset
crime_data <- read.csv("crime_data.csv", stringsAsFactors = FALSE)

# Show the first 5 rows
head(crime_data, 5)

# Check missing values per column
missing_data <- colSums(is.na(crime_data) | crime_data == "" | sapply(crime_data, is.nan))
totalRows = nrow(crime_data)

# Drop columns with more than 50% missing values
threshold <- totalRows * 0.5
# Identify columns to drop (more than 50% missing data)
columns_to_drop <- names(missing_data[missing_data >= threshold])
cat("Columns with more than 50% missing values and will be dropped:\n")
print(columns_to_drop)

cleaned_data <- crime_data[, missing_data < threshold]

head(cleaned_data, 5)

cleaned_data_dateformatted <- cleaned_data %>%
  # Convert DATE OCC to Date format using lubridate
  mutate(DATE.OCC = mdy_hms(DATE.OCC),  # or use ymd(), dmy() based on your date format
         # Extract Year, Month, Day using lubridate
         Year = year(DATE.OCC),
         Month = month(DATE.OCC),
         Day = day(DATE.OCC),
         # Extract Hour from TIME OCC (assuming HHMM format)
         Hour = substr(TIME.OCC, 1, 2))

# Show the first few rows of the updated data
head(cleaned_data_dateformatted, 5)

#Filtering by year
# Filter for crimes in 2023 and with BURGLARY
crime_data_2023 <- cleaned_data_dateformatted %>%
  filter(Year == 2023)

crime_data_2023_burglary <- crime_data_2023 %>%
  filter(Crm.Cd.Desc == "BURGLARY")

cat('Clean Data:', nrow(cleaned_data_dateformatted), '\n')
cat('Only 2023 crimes:', nrow(crime_data_2023), '\n')
cat('Only 2023 crimes that are burglary:', nrow(crime_data_2023_burglary), '\n')

head(crime_data_2023_burglary)

# Group by AREA NAME, calculate total crimes and average victim age
grouped_data <- cleaned_data %>%
  group_by(AREA.NAME) %>%
  summarise(
    total_crimes = n(),
    avg_victim_age = mean(Vict.Age, na.rm = TRUE)
  ) %>%
  arrange(desc(total_crimes))

  head(grouped_data, nrow(grouped_data))

  crimes_by_month <- cleaned_data_dateformatted %>%
  group_by(Month) %>%
  summarise(total_crimes = n())

head(crimes_by_month, nrow(crimes_by_month))

crimes_with_weapon <- crime_data %>%
  filter(!is.na(Weapon.Used.Cd)) %>%
  summarise(total_crimes = n())

  head(crimes_with_weapon)

  crimes_by_premis_desc <- crime_data %>%
  group_by(Premis.Desc) %>%
  summarise(total_crimes = n())

head(crimes_by_premis_desc, nrow(crimes_by_premis_desc))

# Create Severity Score based on rules
crime_data$Severity.Score <- ifelse(!is.na(crime_data$Weapon.Used.Cd), 5,
                                   ifelse(crime_data$Crm.Cd.Desc == "BURGLARY", 3, 1))

# Group by AREA NAME and find total severity score
severity_by_area <- crime_data %>%
  group_by(AREA.NAME) %>%
  summarise(total_severity_score = sum(Severity.Score))

head(severity_by_area, nrow(severity_by_area))

# Define bounding box (latitude and longitude)
lat_min <- 34.04
lat_max <- 34.05
lon_min <- -118.26
lon_max <- -118.25

# Filter crimes within bounding box
filtered_by_location <- crime_data %>%
  filter(LAT >= lat_min, LAT <= lat_max, LON >= lon_min, LON <= lon_max)
cat('Total crimes in downtown area:', nrow(filtered_by_location))
head(filtered_by_location, 5)


