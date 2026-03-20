testthat::test_that("calcular_alerta filtra a unidade geográfica e ordena por sem_not", {
  skip_if_not_installed("mockery")

  dados <- data.frame(
    id_unidade = c(1, 1, 2, 1),
    sem_not = c(202403, 202401, 202401, 202402),
    casos = c(30, 10, 99, 20)
  )

  params <- list(
    codmodelo = "dummy_model",
    limiar1 = 10,
    limiar2 = 20
  )

  legacy_assign_params(params)

  fake_Rt <- function(data, count, gtdist, meangt, sdgt) {
    testthat::expect_equal(count, "casos")
    testthat::expect_equal(gtdist, "normal")
    testthat::expect_equal(meangt, 3)
    testthat::expect_equal(sdgt, 1)

    # depois do filter + arrange, devem restar apenas id = 1 em ordem crescente
    testthat::expect_equal(data$id_unidade, c(1, 1, 1))
    testthat::expect_equal(data$sem_not, c(202401, 202402, 202403))

    data
  }

  fake_setCriteria <- function(rule, values) {
    testthat::expect_equal(rule, "dummy_model")
    testthat::expect_true(is.character(values))
    testthat::expect_equal(names(values), names(params))
    list(rule = rule, values = values)
  }

  fake_fouralert <- function(data, crit) {
    testthat::expect_equal(nrow(data), 3)
    testthat::expect_equal(crit$rule, "dummy_model")

    list(
      indices = list(
        level = c(1, 2, 3),
        cytrue = c(TRUE, FALSE, TRUE)
      )
    )
  }

  mockery::stub(calcular_alerta, "AlertTools::Rt", fake_Rt)
  mockery::stub(calcular_alerta, "setCriteria", fake_setCriteria)
  mockery::stub(calcular_alerta, "AlertTools::fouralert", fake_fouralert)

  res <- calcular_alerta(
    id = 1,
    nome = "Unidade A",
    coluna_id = "id_unidade",
    data = dados,
    gtdist = "normal",
    meangt = 3,
    sdgt = 1
  )

  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(res$sem_not, c(202401, 202402, 202403))
  testthat::expect_equal(res$nivel, c(1, 2, 3))
  testthat::expect_equal(res$receptivo, c(TRUE, FALSE, TRUE))
})


# Teste para verificar se params é obrigatório
#Esse teste é importante porque documenta o acoplamento atual da função.
testthat::test_that("calcular_alerta falha quando params nao existe no ambiente", {
  skip_if_not_installed("mockery")

  if (exists("params", envir = .GlobalEnv)) {
    rm("params", envir = .GlobalEnv)
  }

  dados <- data.frame(
    id_unidade = 1,
    sem_not = 202401,
    casos = 10
  )

  fake_Rt <- function(data, count, gtdist, meangt, sdgt) {
    data
  }

  mockery::stub(calcular_alerta, "AlertTools::Rt", fake_Rt)

  testthat::expect_error(
    calcular_alerta(
      id = 1,
      nome = "Unidade A",
      coluna_id = "id_unidade",
      data = dados,
      gtdist = "normal",
      meangt = 3,
      sdgt = 1
    )
  )
})


#Teste para garantir que o tamanho de nivel e receptivo acompanha o número de linhas

testthat::test_that("calcular_alerta retorna colunas nivel e receptivo com mesmo tamanho da base filtrada", {
  skip_if_not_installed("mockery")

  dados <- data.frame(
    id_unidade = c(1, 1, 1, 2),
    sem_not = c(202401, 202402, 202403, 202401),
    casos = c(5, 10, 15, 20)
  )

  params <- list(
    codmodelo = "dummy_model",
    limiar1 = 10
  )
  legacy_assign_params(params)

  fake_Rt <- function(data, count, gtdist, meangt, sdgt) {
    data
  }

  fake_setCriteria <- function(rule, values) {
    list(rule = rule, values = values)
  }

  fake_fouralert <- function(data, crit) {
    n <- nrow(data)
    list(
      indices = list(
        level = rep(2, n),
        cytrue = rep(FALSE, n)
      )
    )
  }

  mockery::stub(calcular_alerta, "AlertTools::Rt", fake_Rt)
  mockery::stub(calcular_alerta, "setCriteria", fake_setCriteria)
  mockery::stub(calcular_alerta, "AlertTools::fouralert", fake_fouralert)

  res <- calcular_alerta(
    id = 1,
    nome = "Unidade A",
    coluna_id = "id_unidade",
    data = dados,
    gtdist = "normal",
    meangt = 3,
    sdgt = 1
  )

  testthat::expect_equal(nrow(res), 3)
  testthat::expect_equal(length(res$nivel), 3)
  testthat::expect_equal(length(res$receptivo), 3)
  testthat::expect_true(all(res$nivel == 2))
  testthat::expect_true(all(res$receptivo == FALSE))
})

#Teste para coluna de id dinâmica
# Como no .qmd a função recebe coluna_id variável, isso merece um teste próprio.
testthat::test_that("calcular_alerta respeita coluna_id dinamica", {
  skip_if_not_installed("mockery")

  dados <- data.frame(
    bairro_id = c(10, 10, 20),
    sem_not = c(202402, 202401, 202401),
    casos = c(8, 4, 30)
  )

  params <- list(
    codmodelo = "dummy_model",
    limiar1 = 10
  )
  legacy_assign_params(params)

  fake_Rt <- function(data, count, gtdist, meangt, sdgt) {
    testthat::expect_equal(data$bairro_id, c(10, 10))
    testthat::expect_equal(data$sem_not, c(202401, 202402))
    data
  }

  fake_setCriteria <- function(rule, values) {
    list(rule = rule, values = values)
  }

  fake_fouralert <- function(data, crit) {
    list(
      indices = list(
        level = c(3, 4),
        cytrue = c(TRUE, TRUE)
      )
    )
  }

  mockery::stub(calcular_alerta, "AlertTools::Rt", fake_Rt)
  mockery::stub(calcular_alerta, "setCriteria", fake_setCriteria)
  mockery::stub(calcular_alerta, "AlertTools::fouralert", fake_fouralert)

  res <- calcular_alerta(
    id = 10,
    nome = "Bairro X",
    coluna_id = "bairro_id",
    data = dados,
    gtdist = "normal",
    meangt = 3,
    sdgt = 1
  )

  testthat::expect_equal(res$nivel, c(3, 4))
  testthat::expect_equal(res$receptivo, c(TRUE, TRUE))
})
