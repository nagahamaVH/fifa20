rm(list = ls())

library(readr)
library(dplyr)
library(ggplot2)
source('./R/lp-functions.R')

players <- read_delim('./data/players_20.csv', delim = ',', col_names = T) 

ggplot(data = players, aes(x = overall, y = value_eur)) +
  geom_jitter(aes(color = age)) +
  geom_smooth(se = F) +
  scale_color_gradient(low = 'green', high = 'red')
