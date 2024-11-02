# Objective of the analysis:
# - Forecast electricity load (measured in MW) over time by combining datasets
# - Apply Prophet and Neural Network Time Series (NNETAR) models for prediction
# - Compare models to select the best one based on accuracy metrics like MAE and MAPE

# Load necessary libraries for data manipulation, time series, and modeling
library(tidyverse) # For data wrangling
library(datasets)
library(fpp3) # Time series toolkit
library(ggplot2) # Data visualization
library(fabletools) # Forecasting tools
library(epiDisplay)
library(dplyr) # Data manipulation
library(lubridate) # Date and time manipulation

# Read in datasets for training, validation, and testing
train <- read_csv("~/Downloads/Homework1_TS2/hrl_load_metered.csv")
validation <- read_csv("~/Downloads/Homework1_TS2/hrl_load_metered - test1.csv")
test <- read_csv("~/Downloads/hrl_load_metered - test2.csv")
test2 <- read_csv("~/Downloads/hrl_load_metered - test3.csv")

# Combine datasets to form the full training set
full_train <- rbind(train, validation, test)

# Preprocess the training data for time series analysis
training_set <- full_train %>%
  mutate(
    datetime = as.POSIXct(datetime_beginning_ept, format = "%m/%d/%y %H:%M", tz = "America/New_York")
  ) %>% dplyr::select(datetime, mw)

# Adjust for Daylight Saving Time by averaging duplicate timestamps and removing incomplete cases
training_set <- aggregate(mw ~ datetime, data = training_set, FUN = mean) %>%
  filter(complete.cases(.)) %>%
  as_tsibble(index = datetime) %>%
  fill_gaps(.full = TRUE) %>%
  arrange() %>%
  mutate(mw_duplicate = mw) %>%
  fill(mw_duplicate) %>%
  mutate(mw = ifelse(mw_duplicate == "C", mw, mw_duplicate)) %>%
  dplyr::select(-mw_duplicate)

# Preprocess the validation set similarly
validation_set <- test2 %>%
  mutate(
    datetime = as.POSIXct(datetime_beginning_ept, format = "%m/%d/%y %H:%M", tz = "America/New_York")
  ) %>% dplyr::select(datetime, mw)

# Adjust for Daylight Saving Time in validation data
validation_set <- aggregate(mw ~ datetime, data = validation_set, FUN = mean) %>%
  filter(complete.cases(.)) %>%
  as_tsibble(index = datetime) %>%
  fill_gaps(.full = TRUE) %>%
  arrange() %>%
  mutate(mw_duplicate = mw) %>%
  fill(mw_duplicate) %>%
  mutate(mw = ifelse(mw_duplicate == "C", mw, mw_duplicate)) %>%
  dplyr::select(-mw_duplicate)

# Plot training data to examine electric load (MW) over time
ggplot(data = training_set, aes(x = datetime, y = mw)) + 
  geom_line() + 
  labs(title = "Hourly Electric Load (MW)", x = 'Hour', y = 'MW')

# Define and apply Prophet models for time series forecasting
library(fable.prophet)
model_prophet <- training_set %>%
  model(
    prophet1 = prophet(mw ~ growth("linear") + season(period = 24, order = 12, type = "multiplicative")),
    prophet2 = prophet(mw),
    prophet3 = prophet(mw ~ growth("linear") + season(period = 24, order = 12, type = "multiplicative")),
    prophet4 = prophet(mw ~ growth("linear") + season(period = 24, type = 'multiplicative', name = 'day')),
    prophet5 = prophet(mw ~ growth("linear", n_changepoints = 23) + season(period = 24, type = 'additive', name = 'day')),
    prophet6 = prophet(mw ~ season(period = 24, name = 'day'))
  )

# Compare model components for Prophet variations
model_prophet %>% dplyr::select(prophet1) %>% components() %>% autoplot()
model_prophet %>% dplyr::select(prophet2) %>% components() %>% autoplot()
model_prophet %>% dplyr::select(prophet4) %>% components() %>% autoplot()
model_prophet %>% dplyr::select(prophet5) %>% components() %>% autoplot()
model_prophet %>% dplyr::select(prophet6) %>% components() %>% autoplot()

# Forecast with selected Prophet model and calculate accuracy
model_prophet_for2 <- model_prophet %>%
  dplyr::select(prophet2) %>%
  forecast(validation_set)

# Accuracy metrics
acc <- fabletools::accuracy(model_prophet_for2, validation_set)
MAPE <- acc$MAPE
MAE <- acc$MAE

# Plot the best Prophet model forecast against validation set
ggplot(data = validation_set, aes(x = datetime, y = mw, color = "validation")) + 
  geom_line(col = 'black') +
  geom_line(aes(y = .mean), col = "red", data = model_prophet_for2) +
  labs(y = "MW", x = "Hour", 
       title = paste("Best Prophet Model (MAPE | MAE)", round(MAPE, 3), "|", round(MAE, 3)),
       color = "validation")

# Neural Network Time Series (NNETAR) model for time series forecasting
set.seed(12345)
model_nnet <- training_set %>%
  mutate(diff_mw = difference(mw, 24)) %>%
  model(
    nn23 = NNETAR(diff_mw ~ AR(p = 2, P = 3)),
    nn22 = NNETAR(diff_mw ~ AR(p = 2, P = 2)),
    auto = NNETAR(diff_mw)
  )
