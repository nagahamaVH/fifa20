rm(list = ls())

library(readr)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.symphony)
source('./R/lp-functions.R')

formations_matrix <- read_csv2('./data/formations-matrix.csv') %>%
  as.matrix()

players_stat <- read_csv2('./data/players-stat.csv') %>%
  as.matrix()

# Informacoes dos jogadores
n <- nrow(players_stat)
m <- ncol(players_stat)

# Informacoes da formacao 
p <- nrow(formations_matrix)

M <- 50

model <- MILPModel() %>%
  
  # Variavel indicadora se o i-esimo jogador assume a j-esima posicao
  add_variable(dummy_position[i, j], i = 1:n, j = 1:m, type = 'binary') %>%
  
  # Variavel indicadora da formação escolhida
  add_variable(b[k], type = "binary", k = 1:p) %>%
  
  # Função objetiva: maximizar a soma do escore do time
  set_objective(sum_expr(colwise(
    getFromMatrix(players_stat, i, j)) * dummy_position[i, j], i = 1:n, j = 1:m), 
    sense = 'max') %>%
  
  # R: O jogador só pode ocupar uma única posição
  add_constraint(sum_expr(dummy_position[i, j], j = 1:m) <= 1, i = 1:n) %>%

  # R: A quantidade total de jogadores na posição deve ser igual a quantidade
  # especificada na formação escolhida
  add_constraint(
    sum_expr(dummy_position[i, j], i = 1:n) - 
      getFromMatrix(formations_matrix, k, j) + 
      (1 - b[k]) * M >= 0, j = 1:m, k = 1:p) %>%
  
  # R: A formação tática deve ser única
  add_constraint(sum_expr(b[k], k = 1:p) == 1) %>%
  
  # R: O time deve ter 11 jogadores
  add_constraint(sum_expr(dummy_position[i, j], i = 1:n, j = 1:m) == 11)

solution <- solve_model(model, with_ROI(solver = "symphony", presolve = T, 
                                        verbosity = 1))

saveRDS(solution, './R/solution.rds')
