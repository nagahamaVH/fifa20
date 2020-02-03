library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(gridExtra)
library(scales)

source('lp-results.R', enconding = 'UTF-8')

teams <- read_csv2('./data/teams.csv')

fill_color <- 'green2'

second_best_team <- teams %>%
  slice(1) %>%
  pull(team)

score_second_best <- teams %>%
  slice(1) %>%
  select(attack, defense, midfield, overall) %>%
  gather(category, second_best) %>%
  mutate(second_best_team)

score_overall <- dream_team %>%
  summarise(
    category = 'overall',
    score = mean(score) %>%
      round(),
    type = 'value'
  )

score_category <- dream_team %>%
  group_by(category) %>%
  summarise(
    score = mean(score) %>%
      round(),
    type = 'value'
  ) %>%
  bind_rows(score_overall) %>%
  bind_cols(score_second_best)

score_complementary <- score_category %>%
  mutate(
    score = 100 - score,
    type = 'complementary'
  )

score_donut <- bind_rows(score_category, score_complementary) %>%
  arrange(category) %>%
  group_by(category) %>%
  mutate(
    gain = round((score - second_best) / second_best * 100, 1),
    fraction = score / sum(score),
    ymax = cumsum(fraction),
    ymin = c(0, head(ymax, n = -1)),
    label = paste0('atop(bold("', category, ': ', score, '"), "', 
                   gain, '% > ', second_best_team, '")')
  )

all_categories <- score_donut %>%
  distinct(category) %>%
  pull()

plot_list <- list()

for (i in seq_along(all_categories)) {
  score_i <- score_donut %>%
    filter(category == all_categories[i])
  
  plot_list[[i]] <- 
    ggplot(score_i, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, 
                        fill = type)) +
    geom_rect(col = 'grey50') +
    coord_polar(theta = 'y') +
    geom_label(x = 0, aes(y = ymax[1], label = label[1]), size = 4, parse = T) +
    xlim(c(0, 4)) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c(alpha('grey', alpha = 0.4), fill_color))
}

grid_plot <- do.call('grid.arrange', c(plot_list, ncol = 2))

ggsave('./images/score-categories.png', plot = grid_plot, units = 'cm', width = 18, 
       height = 14)
