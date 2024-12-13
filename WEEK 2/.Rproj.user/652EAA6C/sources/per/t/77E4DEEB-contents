# Define the string
info_string <- "Name: Shadan Khan, Unit: Statistical Data Analysis, Task: Probability and Distributions Week 2-3"

# Print the string
print(info_string)

#Question 2 
data <- read.csv("weather (2).csv")

#Question 3 
head(data)

#Question 4: What is the number of observations and the number of variables?

# Get the dimensions of the dataset
dimensions <- dim(data)

# Number of observations (rows)
num_observations <- dimensions[1]

# Number of variables (columns)
num_variables <- dimensions[2]

# Print the results
cat("Number of Observations:", num_observations, "\n")
cat("Number of Variables:", num_variables, "\n")

#Ques 5: Use piping and the appropriate commands to change the variable “origin” to have the
#factor data type. Show that the data type was successfully changed using the class()
#function.

library(dplyr)

# Change the variable 'origin' to a factor and show the data type
data <- data %>%
  mutate(origin = as.factor(origin))

# Verify the data type of 'origin'
class(data$origin)

#question 6: Use piping and summarise() (or reframe) to display the mean and median for each of
#the levels in the origin variable.


# Summarize mean and median for each level of 'origin'
summary_stats <- data %>%
  group_by(origin) %>%
  summarise(
    mean_value = mean(temp, na.rm = TRUE),  
    median_value = median(temp, na.rm = TRUE) 
     )

# Print the summary statistics
print(summary_stats)

#Question 7: Read in the airports data from the nycflights13 library and merge the latitude and
#longitude variables with your dataset according to the origin airports as well as the
#destination airports. Name these variables “o_lat”, “o_lon”, “d_lat” and “d_lon”.

library(nycflights13)
flights_data <- nycflights13::flights
airports_data <- nycflights13::airports

# Merge the latitude and longitude for the origin airports
flights_data <- flights_data %>%
  left_join(airports_data, by = c("origin" = "faa")) %>%
  rename(o_lat = lat, o_lon = lon)

# Merge the latitude and longitude for the destination airports
flights_data <- flights_data %>%
  left_join(airports_data, by = c("dest" = "faa")) %>%
  rename(d_lat = lat, d_lon = lon)
# Select relevant columns to display
flights_data <- flights_data %>% select(year, month, day, dep_time, arr_time, origin, dest, o_lat, o_lon, d_lat, d_lon, everything())
# Display the first few rows of the updated dataset
print(head(flights_data))

