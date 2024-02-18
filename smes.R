# Load necessary libraries
library(readr)
library(lubridate)
library(forecast)
library(ggplot2)

library(tseries)  # For statistical tests
library(stats)
library(strucchange)
library(zoo)
library(nlme)

# Set locale to English
Sys.setlocale("LC_TIME", "English")

# Read the data with custom delimiter
data <- read_delim("C:\\Users\\777\\Desktop\\data\\Data.csv", 
                   delim = ";", 
                   col_types = cols(
                     id = col_integer(),
                     date = col_date(format = "%d.%m.%Y"),
                     number = col_double()
                   ))
# Convert the data to a time series object
# Assuming the data is daily, but we want to analyze weekly patterns
ts_data <- ts(data$number, frequency = 7)

# Decompose the time series
decomposed <- stl(ts_data, s.window = "periodic")
