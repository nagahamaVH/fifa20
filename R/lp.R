rm(list = ls())

library(readr)
library(dplyr)
library(ompr)
library(ompr.roi)
library(ROI.plugin.symphony)
source('./R/utils.R')

formations <- read_delim('./data/formations.csv', delim = ';')

formationsMatrix <- formations %>%
  select(-c('id', 'formation')) %>%
  as.matrix()

load('./data/playersInfo.RData')
load('./data/playersStat.RData')

# playersStat  <- playersStat[1:50,]

# Informacoes dos jogadores
n <- nrow(playersStat)
m <- ncol(playersStat)

# Informacoes da formacao 
p <- nrow(formationsMatrix)

M <- 10^10

model <- MILPModel() %>%
  
  # Variavel indicadora se o i-esimo jogador assume a j-esima posicao
  add_variable(dummyPosition[i, j], i = 1:n, j = 1:m, type = 'binary') %>%
  
  # Variavel indicadora da formação escolhida
  add_variable(b[k], type = "binary", k = 1:p) %>%
  
  # Função objetiva: maximizar a soma do escore do time
  set_objective(sum_expr(colwise(
    getFromMatrix(playersStat, i, j)) * dummyPosition[i, j], i = 1:n, j = 1:m), 
    sense = 'max') %>%
  
  # R: O jogador só pode ocupar uma única posição
  add_constraint(sum_expr(dummyPosition[i, j], j = 1:m) <= 1, i = 1:n) %>%

  # R: A quantidade total de jogadores na posição deve ser igual a quantidade
  # especificada na formação escolhida
  add_constraint(
    sum_expr(dummyPosition[i, j], i = 1:n) - 
      getFromMatrix(formationsMatrix, k, j) + 
      (1 - b[k]) * M >= 0, j = 1:m, k = 1:p) %>%
  
  # R: A formação tática deve ser única
  add_constraint(sum_expr(b[k], k = 1:p) == 1) %>%
  
  # R: O time deve ter 11 jogadores
  add_constraint(sum_expr(dummyPosition[i, j], i = 1:n, j = 1:m) == 11)

solution <- solve_model(model, with_ROI(solver = "symphony", presolve = T, 
                                        verbosity = 1))

save(file = './data/solution.RData', solution)
