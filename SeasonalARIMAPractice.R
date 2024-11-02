# My objective in this analysis is to forecast electricity load (MW) over time:
# - Use historical data to model and predict future consumption patterns
# - Apply time series decomposition and forecasting techniques
#     - Techniques include Exponential Smoothing State Space (ETS) and ARIMA with seasonal adjustments
# - Aim to identify the most accurate model for predicting energy demand

# Additional methods:
# - Use STL and Fourier series decomposition to capture recurring patterns
# - Enhance model accuracy by leveraging seasonal and trend components

# Purpose of analysis:
# - Support effective energy planning and resource allocation
# - Enable anticipation of demand fluctuations to optimize resource use


# Load required libraries for data manipulation, time series analysis, and plotting
library(tidyverse)
library(datasets)
library(fpp3) # For time series analysis
library(ggplot2) # For plotting
library(fabletools) # For model construction and forecasting
library(epiDisplay)
library(dplyr) # For data manipulation
library(lubridate) # For date/time manipulation
library(forecast) # For time series modeling

# Preprocess the training dataset by converting datetime column
energy.train1 <- train %>%
  mutate(
    datetime = as.POSIXct(datetime_beginning_ept, format = "%m/%d/%y %H:%M", tz = "America/New_York")
  ) %>%
  dplyr::select(datetime, mw)

# Handle daylight saving time issues by averaging duplicated timestamps and removing missing values
energy.train1 <- aggregate(mw ~ datetime, data = energy.train1, FUN = mean) %>%
  filter(complete.cases(.)) %>%
  as_tsibble(index = datetime) %>%
  fill_gaps(.full = TRUE) %>%
  arrange(datetime) %>%
  mutate(mw_duplicate = mw) %>%
  fill(mw_duplicate) %>%
  mutate(mw = ifelse(mw_duplicate == "C", mw, mw_duplicate)) %>%
  dplyr::select(-mw_duplicate)

# Preprocess the validation dataset similarly to the training dataset
energy.validation <- validation %>%
  mutate(
    datetime = as.POSIXct(datetime_beginning_ept, format = "%m/%d/%y %H:%M", tz = "America/New_York")
  ) %>%
  dplyr::select(datetime, mw)

# Handle daylight saving time issues in validation data
energy.validation <- aggregate(mw ~ datetime, data = energy.validation, FUN = mean) %>%
  filter(complete.cases(.)) %>%
  as_tsibble(index = datetime) %>%
  fill_gaps(.full = TRUE) %>%
  arrange(datetime) %>%
  mutate(mw_duplicate = mw) %>%
  fill(mw_duplicate) %>%
  mutate(mw = ifelse(mw_duplicate == "C", mw, mw_duplicate)) %>%
  dplyr::select(-mw_duplicate)

# Plot the training data to visualize MW consumption over time
ggplot(data = energy.train1, aes(x = datetime, y = mw)) +
  geom_line() +
  labs(title = "Hourly MW Consumption", x = 'Hour', y = 'MW')

# Decompose the time series using STL
dcmp <- energy.train1 %>%
  fabletools::model(stl = STL(mw))

# Plot the STL components
components(dcmp) %>%
  autoplot() + theme_light()

# Calculate features for seasonal-trend decomposition
energy.train1 %>%
  features(mw, feat_stl)

# Plot the decomposition with seasonally adjusted series
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(mw, colour = 'gray') +
  geom_line(aes(y = season_adjust), colour = 'darkorange') +
  geom_line(aes(y = trend), colour = 'darkviolet') +
  labs(y = "Monthly MW Load", title = "Electric Load (MW) Across Months")

# Check for stationarity using unit root tests
energy.train1 %>% features(mw, unitroot_kpss)
energy.train1 %>% features(mw, unitroot_ndiffs)

# Fit ETS models with various configurations
mw_fit <- energy.train1 %>%
  model(
    ETSMAM = ETS(mw ~ error("M") + trend("A") + season("M")),
    ETSMAA = ETS(mw ~ error("M") + trend("A") + season("A")),
    # ...other ETS model configurations...
    AutoBIC = ETS(mw, ic = 'bic')
  )

# Report and compare models based on AICc
report(mw_fit) %>% arrange(AICc)

# Perform forecasting with the selected models and validate accuracy
mw_fit_f <- energy.train1 %>%
  model(
    Auto_A_Ad_N = ETS(mw),
    ETSAMN = ETS(mw ~ error("A") + trend("M") + season("N")),
    ETSANM = ETS(mw ~ error("A") + trend("N") + season("M"))
  )

# Forecast and validate with the validation set
mw_fc_f <- mw_fit_f %>% fabletools::forecast(h = 168)
fabletools::accuracy(mw_fc_f, energy.validation) %>% arrange(MAPE)

# Plot validation forecasts vs actual data
ggplot(data = energy.validation, aes(x = datetime, y = mw)) +
  geom_line(color = "black") +
  geom_line(aes(y = .mean), col="purple", data = ets_fc) +
  labs(y="MW", x= "Month", title="Electric Load (MW) over Months")

# Seasonal differencing and analysis of PACF and ACF plots
energy.train1 %>% gg_tsdisplay(difference(mw, 24), plot_type = 'partial', lag = 72)

# Define ARIMA models with Fourier terms and seasonal differencing
fit.diff <- energy.train.df %>%
  model(
    mod1 = ARIMA(mw ~ pdq(2,0,0) + PDQ(1,1,0)),
    mod2 = ARIMA(mw ~ pdq(1,0,0) + PDQ(1,1,1)),
    # ...other ARIMA configurations...
  )

fit.fourier <- energy.train1 %>%
  model(
    mod1 = ARIMA(mw ~ fourier(K=1) + PDQ(0,0,0)),
    mod2 = ARIMA(mw ~ fourier(K=2) + PDQ(0,0,0)),
    # ...other Fourier term configurations...
  )

# Report and compare model accuracy for forecasting
fabletools::report(fit.diff) %>% arrange(AICc)
fabletools::report(fit.fourier) %>% arrange(AICc)
