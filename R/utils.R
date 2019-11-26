library(NCmisc)
must.use.package('dplyr')
must.use.package('stringr')

getFromMatrix <- function(i, j, matrix) {
  vapply(seq_along(i), function(k) matrix[i[k], j[k]], numeric(1L))
}