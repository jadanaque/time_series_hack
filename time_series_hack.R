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

# Reading data
train_df <- read_csv("data/TrainData.csv")
train_df <- train_df %>%
  mutate(Datetime = dmy_hm(Datetime))

ggplot(train_df[1:2500, ], aes(Datetime, Count)) + geom_line() +
  geom_vline(xintercept = seq(ymd_hm("2012-08-25 00:00"), ymd_hm("2012-11-30 00:00"), by = "days"), col = 2, size = 0.1, linetype = 3)








#forcast( , h = 5112)