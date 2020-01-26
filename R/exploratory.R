library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(fmsb)
library(scales)
library(wesanderson)

line_color <- wes_palette("Darjeeling2", n = 2, type = 'discrete')[2]

teams <- read_csv2('./data/teams.csv')

score_category <- dreamTeam %>%
  group_by(category) %>%
  summarise(
    score = mean(score) %>%
      round(),
    type = 'value'
  )

score_complementary <- score_category %>%
  mutate(
    score = 100 - score,
    type = 'complementary'
  )

score_donut <- bind_rows(score_category, score_complementary) %>%
  arrange(category) %>%
  group_by(category) %>%
  mutate(
    fraction = score / sum(score),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1))
  )

attack_score <- score_donut %>%
  filter(category == 'attack')

ggplot(attack_score, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, 
                         fill = type)) +
  geom_rect() +
  coord_polar(theta = 'y') +
  xlim(c(0, 4)) +
  theme_void() +
  theme(legend.position = "none")

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")
