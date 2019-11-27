getFromMatrix <- function(matrix, i, j) {
  vapply(seq_along(i), function(k) matrix[i[k], j[k]], numeric(1L))
}
