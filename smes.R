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
                     number = col_double()
                   ))
