#loading the library dplyr
library(dplyr) 

#loading the library stringr
library(stringr)

#we have skipped the first 11 lines beacuse it includes the metadata
data<- read.csv('DATA.csv',skip=11)

#we use head to see some of the data
head(data)

# Convert 'Start Year' and 'End Year' columns to numeric after removing non-numeric characters
data <- data %>%
  mutate(
    Start.Year = as.numeric(gsub("[^0-9]", "", Start.Year)),
    End.Year = as.numeric(gsub("[^0-9]", "", End.Year))
  )

# Calculate 'Number of Years' if it is NA, using 'End Year' - 'Start Year'
data <- data %>%
  mutate(Number.of.Years = ifelse(Number.of.Years == "N/A", End.Year - Start.Year, Number.of.Years))

# Clean the 'Statistic Element' column by removing text after "for years"
data <- data %>%
  mutate(Statistic.Element = str_remove(Statistic.Element, "for years.*"))

# Create a metadata table containing 'Statistic Element', 'Start Year', 'End Year', 'Number of Years', and 'Annual'
metadata <- data %>%
  select(Statistic.Element, Start.Year, End.Year, Number.of.Years, Annual)

# Remove metadata columns from the original data
data <- data %>%
  select(-Start.Year, -End.Year, -Number.of.Years, -Annual)

# We first separate the date data and keep all the data pertaining to dates together.
date_data <- data %>%
  filter(grepl("date", Statistic.Element, ignore.case = TRUE))

# we then remove date data from the original table for more cleaning. 
# Remove date rows from the original table
data <- data %>%
  filter(!grepl("date", Statistic.Element, ignore.case = TRUE))

# now we extract rows in rainfall metrics
rainfall_data <- data %>%
  filter(grepl("rainfall|rain", Statistic.Element, ignore.case = TRUE))

# now we have to extract rows in sunshine and solar exposure metrics
sunshine_data <- data %>%
  filter(grepl("sunshine|solar", Statistic.Element, ignore.case = TRUE))

# now we extract The temperature data
temperature_data <- data %>%
  filter(grepl("temperature|°C", Statistic.Element, ignore.case = TRUE))

# now we extract rows into other metrics like humidity, cloud cover, and evaporation
other_metrics_data <- data %>%
  filter(grepl("humidity|cloud|evaporation|dew", Statistic.Element, ignore.case = TRUE))

# now we extract rows into wind speed metrics
wind_data <- data %>%
  filter(grepl("wind|gust", Statistic.Element, ignore.case = TRUE))

# The Process for rainfall_data

rainfall_data <- rainfall_data %>% rename(Months = `Statistic.Element`) %>% t() %>%
  as.data.frame(stringsAsFactors = FALSE)
head(rainfall_data[,1:4])

# The Process for other_metrics_data
other_metrics_data <- other_metrics_data %>% rename(Months = `Statistic.Element`) %>% t() %>%
  as.data.frame(stringsAsFactors = FALSE)
head(other_metrics_data[,1:4])

# The Process for sunshine_data
sunshine_data <- sunshine_data %>% rename(Months = `Statistic.Element`) %>% t() %>%
  as.data.frame(stringsAsFactors = FALSE)
head(sunshine_data[,1:2])

head(temperature_data)

#The Process for temperature data
temperature_data <- temperature_data %>% rename(Months = Statistic.Element)
temperature_data<-t(temperature_data) 
temperature_data<-as.data.frame(temperature_data) 
head(temperature_data[,1:4])

#The Process for wind_data
wind_data <- wind_data %>%
  rename(Months = `Statistic.Element`) %>% t() %>%
  as.data.frame(stringsAsFactors = FALSE)
head(wind_data[,1:4])

