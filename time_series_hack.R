# Script for Hackaton on Time Series Analysis (Analytics vidhya)
# https://datahack.analyticsvidhya.com/contest/practice-problem-time-series-2/

# Packages ----
library(ggplot2)
library(TSA)
library(forecast)
library(scales)
library(stats)
library(urca)
library(dplyr)
library(readr)
library(lubridate)

# Reading data ----
train_df <- read_csv("data/TrainData.csv")
train_df <- train_df %>%
  mutate(Datetime = dmy_hm(Datetime))

ggplot(train_df, aes(Datetime, Count)) + geom_line() +
  geom_vline(xintercept = seq(ymd_hm("2012-08-25 00:00"), ymd_hm("2012-11-30 00:00"), by = "days"), col = 2, size = 0.1, linetype = 3)

ggplot(train_df[1:2500, ], aes(Datetime, Count)) + geom_line() +
  geom_vline(xintercept = seq(ymd_hm("2012-08-25 00:00"), ymd_hm("2012-11-30 00:00"), by = "days"), col = 2, size = 0.1, linetype = 3)

# Time series object ----
traffic_ts <- ts(train_df$Count, frequency = 24)

plot(traffic_ts)
grid()

boxplot(traffic_ts ~ cycle(traffic_ts))
abline(h = 106, col = 2, lty = 2)

acf(traffic_ts, lag.max=240,
    main="ACF de la serie",
    ylab="Autocorrelaciones",
    xlab="Retardo")

abline(v = 1:5, col = 2, lty = 3)

pacf(traffic_ts, lag.max=240,
     main="PACF de la serie",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")

# Exploring diffs ----
# diff with lag = 1 (regular diff)
acf(diff(traffic_ts), lag.max=240,
    main="ACF de la serie",
    ylab="Autocorrelaciones",
    xlab="Retardo")
grid()
abline(v = 1:5, col = 2, lty = 3)

pacf(diff(traffic_ts), lag.max=240,
     main="PACF de la serie",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")
grid()
abline(v = 1:5, col = 2, lty = 3)

# diff with lag = 24 (seasonal diff)
acf(diff(traffic_ts, 24), lag.max=240,
    main="ACF de la serie",
    ylab="Autocorrelaciones",
    xlab="Retardo")
grid()
abline(v = 1:5, col = 2, lty = 3)

pacf(diff(traffic_ts, 24), lag.max=240,
     main="PACF de la serie",
     ylab="Autocorrelaciones parciales",
     xlab="Retardo")
grid()
abline(v = 1:5, col = 2, lty = 3)

# ARIMA(_, 1, _)(_,1,_)[24]
#      (AR, Dif, MA)(AR, Dif, MA)
#     (No Estacional)(Estacional)


# ARIMA(0, 1, 2)(0, 1, 2)[24]
# ARIMA(1, 1, 2)(1, 1, 2)[24]
# ARIMA(2, 1, 2)(2, 1, 2)[24]

# ARIMA(2, 1, 0)(2, 1, 0)[24]
# ARIMA(2, 1, 1)(2,1,1)[24]

# diff with lag = 24 * 7




#forcast( , h = 5112)