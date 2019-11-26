library(NCmisc)
must.use.package('dplyr')
must.use.package("ompr", quietly = TRUE)
must.use.package("ompr.roi", quietly = TRUE)
must.use.package("ROI.plugin.symphony", quietly = TRUE)
source('./R/utils.R')

load('./data/fifa.RData')

model <- MILPModel() %>%
  # 1 se a op i for alocada na facçao j
  add_variable(x[i, j], i = 1:n, j = 1:m, type = "binary") %>%
  
  # Determina o número de dias necessários para a facção j produzir as OP's designadas
  add_variable(y[j], j = 1:m, type = "integer", lb = 1, ub = max_day) %>%
  
  # Variavel indicadora se a faccao recebe OP
  add_variable(b1[j], type = "binary", j = 1:m) %>%
  
  # Designar cada Op para as facções minimizando as preferencias
  set_objective(sum_expr(colwise(get_from_matrix(i, j, weight_matrix)) * x[i, j], i = 1:n, j = 1:m) + 7 * sum_expr(y[j], j = 1:m), sense = 'min') %>%
  
  # Toda Op precisa ser designada para uma facção
  add_constraint(sum_expr(x[i, j], j = 1:m) == 1, i = 1:n) %>%
  
  # Restricao da especificidade da OP 
  add_constraint(x[i, j] <= colwise(get_from_matrix(i, j, restriction_matrix)), i = 1:n, j = 1:m) %>%
  
  # Número de dias precisa ser maior ou igual até a próxima vez que um caminhão for abastecer novamente a facção
  add_constraint(y[j] >= faction_rank$diffDay[j] * b1[j], j = 1:m) %>%
  
  # Numero maximo de dias de trabalho que a faccao pode receber
  add_constraint(y[j] <= faction_rank$maxDay[j], j = 1:m) %>%
  
  # THE BIG M METHOD: se a faccao recebe OP, entao o numero minimo de carga é conforme a variavel de carga minima 
  add_constraint(-y[j] + M * b1[j] >= 0, j = 1:m) %>%
  add_constraint(-y[j] + M * b1[j] <= M - 1, j = 1:m) %>%
  
  # A soma de todas as Op's enviadas para a mesma facção não pode ultrapassar a capacidade da facção vezes o número de dias
  add_constraint(sum_expr(colwise(pendingForProduction_optimization$piecesQuantityInMinutes[i]) * x[i, j], i = 1:n) <= faction_rank$factionProductionCapacities[j] * y[j], j = 1:m)

result <- solve_model(model, with_ROI(solver = "symphony", presolve = T, time_limit = 3 * 60))
