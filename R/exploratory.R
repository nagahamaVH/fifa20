library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
#library(fmsb)
library(scales)
#library(wesanderson)

teams <- read_csv2('./data/teams.csv')

fill_color <- 'green2'

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
    ymin = c(0, head(ymax, n = -1)),
    label = paste0('atop(bold("', category, ': ', score, '")')
  )

category_i <- 'attack'

attack_score <- score_donut %>%
  filter(category == category_i)

ggplot(attack_score, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, 
                         fill = type)) +
  geom_rect(col = 'grey50') +
  coord_polar(theta = 'y') +
  geom_label(x = 0, aes(y = ymax[1], label = 'atop(bold("attack: 90"), "7% > Real Madrid")', 
                        size = 6), parse = T) +
  xlim(c(0, 4)) +
  theme_void() +
  theme(legend.position = "none") +
  #labs(title = category_i) +
  scale_fill_manual(values = c(alpha('grey', alpha = 0.4), fill_color))
