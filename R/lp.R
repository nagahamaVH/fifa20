library(NCmisc)
must.use.package('dplyr')
must.use.package("ompr", quietly = TRUE)
must.use.package("ompr.roi", quietly = TRUE)
must.use.package("ROI.plugin.symphony", quietly = TRUE)
source('./R/utils.R')

load('./data/weightMatrix.RData')
load('./data/players.RData')

# Informacoes dos jogadores
n <- nrow(playersStat)
m <- ncol(playersStat)

# Informacoes da formacao tatica (k: niveis, l = jogadores por linha)
#matriz

model <- MILPModel() #%>%
  # # Variavel indicadora se o i-esimo jogador assume a j-esima posicao
  # add_variable(p[i], i = 1:n, j = 1:m, type = 'binary') %>%
  # 
  # # Variavel indicadora da formacao
  # add_variable(f[i, j], i = 1:k, j = 1:l, type = 'binary') %>%


result <- solve_model(model, with_ROI(solver = "symphony", 
                                      presolve = T, time_limit = 5 * 60))
