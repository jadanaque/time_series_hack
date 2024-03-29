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

ggplot(train_df, aes(Datetime, Count)) + geom_line() +
  coord_cartesian(xlim = c(ymd_hm("2014-01-01 00:00"), ymd_hm("2014-09-25 23:00")),
                  ylim = c(0, 1250))  # explored many plots in this way, with different date ranges

# Time series object ----
traffic_ts <- ts(train_df$Count, frequency = 24)

plot(traffic_ts)
grid()

boxplot(traffic_ts ~ cycle(traffic_ts))
abline(h = 106, col = 2, lty = 2)
points(x = cycle(traffic_ts)[(length(traffic_ts)-1000):length(traffic_ts)],
       y = traffic_ts[(length(traffic_ts)-1000):length(traffic_ts)],
       col = 2)

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

adf.test(diff(traffic_ts))
adf.test(diff(traffic_ts), k = 24)
adf.test(diff(traffic_ts), k = 0)

# SARIMA model ----
traffic_fit<-Arima(traffic_ts, order=c(0,1,2), seasonal = list(order = c(0,1,2), period=24))
traffic_fit
summary(traffic_fit)

# Trying out different combinations of parameters ----
arima_pars <- list(
  list(c(0, 1, 2), c(0, 1, 2)),
  list(c(1, 1, 2), c(1, 1, 2)),
  list(c(2, 1, 2), c(2, 1, 2)),
  list(c(2, 1, 0), c(2, 1, 0)),
  list(c(2, 1, 1), c(2, 1, 1))
)

traffic_fit_l <- vector("list", length = length(arima_pars))

results_df <- data.frame(model = as.character(arima_pars),
                         AIC = numeric(length(arima_pars)),
                         BIC = numeric(length(arima_pars)),
                         RMSE = numeric(length(arima_pars)))

for(i in seq_along(arima_pars)){
    traffic_fit_l[[i]] <- Arima(traffic_ts, order = arima_pars[[i]][[1]],
                            seasonal = list(order = arima_pars[[i]][[2]], period = 24))
    results_df[i, 2:4] <- c(traffic_fit_l[[i]][["aic"]],
                            traffic_fit_l[[i]][["bic"]],
                            sqrt(mean((traffic_ts - traffic_fit_l[[i]][["fitted"]])^2))
                            )
}


# ARIMA(_, 1, _)(_,1,_)[24]
#      (AR, Dif, MA)(AR, Dif, MA)
#     (No Estacional)(Estacional)


# ARIMA(0, 1, 2)(0, 1, 2)[24]
# ARIMA(1, 1, 2)(1, 1, 2)[24]
# ARIMA(2, 1, 2)(2, 1, 2)[24]

# ARIMA(2, 1, 0)(2, 1, 0)[24]
# ARIMA(2, 1, 1)(2,1,1)[24]

# diff with lag = 24 * 7

# Split 'traffic_ts' in train-test and model building ----

traffic_ts_train <- traffic_ts %>% head(13716)
traffic_ts_test <- traffic_ts %>% tail(4572)

## Trying out different combinations of parameters
arima_pars <- list(
  list(c(0, 1, 2), c(0, 1, 2)),
  list(c(1, 1, 2), c(1, 1, 2))
  # list(c(2, 1, 2), c(2, 1, 2)),
  # list(c(2, 1, 0), c(2, 1, 0)),
  # list(c(2, 1, 1), c(2, 1, 1))
)

traffic_fit_l <- vector("list", length = length(arima_pars))

results_df <- data.frame(model = as.character(arima_pars),
                         AIC = numeric(length(arima_pars)),
                         BIC = numeric(length(arima_pars)),
                         RMSE = numeric(length(arima_pars)),
                         test_RMSE = numeric(length(arima_pars)))

for(i in seq_along(arima_pars)){
  traffic_fit_l[[i]] <- Arima(traffic_ts_train, order = arima_pars[[i]][[1]],
                              seasonal = list(order = arima_pars[[i]][[2]], period = 24))
  results_df[i, 2:5] <- c(traffic_fit_l[[i]][["aic"]],
                          traffic_fit_l[[i]][["bic"]],
                          sqrt(mean((traffic_ts_train - traffic_fit_l[[i]][["fitted"]])^2)),
                          sqrt(mean((traffic_ts_test - forecast(traffic_fit_l[[i]], h=4572)[["mean"]])^2))
  )
}

# best model: list(c(1, 1, 2), c(1, 1, 2))

# Now, we build our final model with the full dataset: 'traffic_ts' ----
traffic_fit_fv <- Arima(traffic_ts, order=c(1, 1, 2),
                        seasonal = list(order = c(1, 1, 2), period=24))
traffic_fit_fv
summary(traffic_fit_fv)


my_predictions <- forecast(traffic_fit_fv, h = 5112)

my_predictions %>% summary()

# Writing results in csv file
Count <- my_predictions[["mean"]] %>% as.numeric

submission_df <- data.frame(ID = 18288:23399,
                            Count = Count)

write_csv(submission_df, "submission_file.csv")
