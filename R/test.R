library(NCmisc)
must.use.package('testthat')

source('./R/utils.R')

# GOOD INPUT
test_that('O teste deve retornar a string sem a unidade de medida, converter 
          de milhão para milhar e retornar o valor numérico', {
  expect_equal(StandardizeMoney('€77K'), 77)
  expect_equal(StandardizeMoney('€77M'), 77000)
  
  expect_equal(StandardizeMoney('US391K'), 391)
  expect_equal(StandardizeMoney('R$1.1M'), 1100)
  
  expect_equal(StandardizeMoney('250K'), 250)
  
  expect_equal(StandardizeMoney('R$0'), 0)
})

# BAD INPUT
test_that('O teste deve falhar para unidades de grandezas invalidas', {
  expect_warning(StandardizeMoney('€25L'))
  expect_warning(StandardizeMoney('€0.8MM'))
})
