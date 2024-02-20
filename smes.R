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

# Plot the data
plot(decomposed)

# Using ggplot to create a time series plot
ggplot(data, aes(x = date, y = number)) +
  geom_line() +
  labs(title = "Time Series Data", x = "Date", y = "Firms Registered")

# Check for autocorrelation using ACF and PACF plots
acf(ts_data, main = "Autocorrelation Function")
pacf(ts_data, main = "Partial Autocorrelation Function")

# Conduct a Ljung-Box test
Box.test(ts_data, lag = as.integer(log(length(ts_data))), type = "Ljung-Box")

# Identify structural breaks
bp <- breakpoints(ts_data ~ 1)
bp
coef(bp)

# Convert the time series object to a numeric vector
data_vector <- as.numeric(ts_data)

# Run breakpoint analysis
bp <- breakpoints(data_vector ~ 1)

# Summary of breakpoints
summary(bp)

# Plot the breakpoints
plot(bp)

# Assuming 'ts_data' is your time series object and 'bp' is the breakpoint object you obtained previously
data_vector <- as.numeric(ts_data)
breakpoints <- as.numeric(bp$breakpoints)

# Create a data frame for the regression
reg_data <- data.frame(
  y = data_vector,
  break1 = ifelse(1:length(data_vector) > breakpoints[1], 1, 0)
)

# If you have more than one breakpoint, add more dummy variables
if(length(breakpoints) > 1) {
  for(i in 2:length(breakpoints)) {
    reg_data[paste0('break', i)] <- ifelse(1:length(data_vector) > breakpoints[i], 1, 0)
  }
}

# Fit the OLS model with the dummy variables
ols_model <- lm(y ~ ., data=reg_data)

# Check the summary for p-values
summary(ols_model)

# Assuming 'ts_data' is our time series object and 'breakpoints' are the indices of breaks
data_vector <- as.numeric(ts_data) # Convert time series to numeric vector if needed
breakpoints <- c(72, 275) # Replace this with your actual breakpoints

# Create a data frame for plotting
time_index <- 1:length(data_vector)
data_df <- data.frame(Time = time_index, Value = data_vector)

# Fit a linear model with breakpoints (create dummy variables for each breakpoint)
break_dummies <- sapply(breakpoints, function(bp) as.numeric(time_index > bp))
colnames(break_dummies) <- paste0("Break", seq_along(breakpoints))
model_data <- cbind(data_df, break_dummies)
fit <- lm(Value ~ ., data=model_data)

# Get the fitted trendline values
data_df$Fitted <- predict(fit)

# Create the ggplot object
p <- ggplot(data_df, aes(x = Time, y = Value)) +
  geom_line() +
  geom_line(aes(y = Fitted), color = "blue") +
  geom_vline(xintercept = breakpoints, color = "red", linetype = "dashed") +
  labs(title = "Time Series Analysis of Foreign Participation Company Registrations with Indicated Structural Breaks",
       x = "Time",
       y = "Foreign Firms Registered") +
  theme_minimal()

# Print the plot
print(p)

# Assuming we have already created the 'reg_data' dataframe with the dummy variables for the breakpoints.
ols_model <- lm(y ~ ., data=reg_data)

# Obtain the fitted values from the model.
reg_data$Fitted <- predict(ols_model, newdata = reg_data)

# Update the 'data_df' with the fitted values.
data_df$Fitted <- reg_data$Fitted
                        
# Create a date sequence based on our data's date range
date_seq <- seq(from = min(data$date), to = max(data$date), by = "day")                        


# Ensure that the length of date_seq matches the length of our time series data
date_seq <- date_seq[1:length(ts_data)]

# Now, when we can create a data frame for plotting, include the date sequence
data_df <- data.frame(Date = date_seq, Value = data_vector)

# Update the plotting function to use Date instead of Time
p <- ggplot(data_df, aes(x = Date, y = Value)) +
  geom_line() +
  geom_line(aes(y = Fitted), color = "blue") +
  geom_vline(xintercept = breakpoints, color = "red", linetype = "dashed") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") + # Change date breaks and labels as needed
  labs(title = "Time Series Analysis of Foreign Participation Company Registrations with Indicated Structural Breaks",
       x = "Date",
       y = "Number of Foreign Firms Registered") +
  theme_minimal()

# Print the plot
print(p)

# Convert breakpoint indices to actual dates
breakpoint_dates <- date_seq[breakpoints]                        

# Use these dates in our plot
p <- ggplot(data_df, aes(x = Date, y = Value)) +
  geom_line() +
  geom_line(aes(y = Fitted), color = "blue") +
  geom_vline(xintercept = breakpoint_dates, color = "red", linetype = "dashed") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  labs(title = "Time Series Analysis of Foreign Participation Company Registrations with Indicated Structural Breaks",
       x = "Date",
       y = "Number of Foreign Firms Registered") +
  theme_minimal()

# Print the plot
print(p)                      

# Let's start over with new dataset

# Assuming 'reg_data' already has the dummy variables for the breakpoints
ols_model <- lm(y ~ ., data=reg_data)

# Add the fitted values to the 'reg_data' dataframe
reg_data$Fitted <- predict(ols_model)

# Assuming 'breakpoints' contains the indices of the breakpoints
# And 'data$date' is the column with the actual dates in our original dataset
breakpoint_dates <- data$date[breakpoints]

# Merge the fitted values and the actual dates into 'data_df' for plotting
data_df <- merge(data_df, data.frame(Date = data$date, Fitted = reg_data$Fitted), by = "Date", all.x = TRUE)

# Create the plot using 'Date' for the x-axis and including the fitted trendline and breakpoints
p <- ggplot(data_df, aes(x = Date, y = Value)) +
  geom_line() +
  geom_line(aes(y = Fitted), color = "blue") +
  geom_vline(xintercept = breakpoint_dates, color = "red", linetype = "dashed") +
  scale_x_date(date_breaks = "months", date_labels = "%b %Y") +
  labs(title = "",
       x = "Date",
       y = "Number of Foreign Firms Registered") +
  theme_minimal()

# Print the plot
print(p)
                        
# Decompose the time series with STL considering weekly seasonality
decomposed <- stl(ts_data, s.window = "periodic", robust = TRUE)

# Seasonally adjust the time series data
ts_data_adj <- seasadj(decomposed)

# Re-run the breakpoint analysis and regression on the seasonally adjusted data
bp_adj <- breakpoints(ts_data_adj ~ 1)
summary(bp_adj)

# Convert the seasonally adjusted time series to a numeric vector
data_vector_adj <- as.numeric(ts_data_adj)

# Create a new data frame for the adjusted regression
reg_data_adj <- data.frame(
  y = data_vector_adj,
  break1 = ifelse(1:length(data_vector_adj) > breakpoints[1], 1, 0)
)

# If we have more than one breakpoint, add more dummy variables
if(length(breakpoints) > 1) {
  for(i in 2:length(breakpoints)) {
    reg_data_adj[paste0('break', i)] <- ifelse(1:length(data_vector_adj) > breakpoints[i], 1, 0)
  }
}
