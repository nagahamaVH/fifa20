library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(fmsb)
library(scales)
library(wesanderson)

line_color <- wes_palette("Darjeeling2", n = 2, type = 'discrete')[2]

teams <- read_csv2('./data/teams.csv', locale = locale(encoding = 'latin1'))

min_score <- 0
max_score <- 100

score_limits <- tibble(
  attack = c(max_score, min_score),
  defense = c(max_score, min_score),
  midfield = c(max_score, min_score),
)

score_category <- dreamTeam %>%
  group_by(category) %>%
  summarise(
    mean = mean(score)
  ) %>%
  spread(category, mean) %>%
  bind_rows(score_limits, .)

png('./images/radar-scores.png', width = 320, height = 320)
radarchart(
  score_category,
  axistype = 0,
  pcol = line_color, 
  pfcol = alpha(line_color, 0.4), 
  plwd = 2, 
  plty = 1,
  cglcol = "grey", 
  cglty = 1, 
  axislabcol = "black", 
  cglwd = 0.8,
  vlcex = 0.8
)
dev.off()

