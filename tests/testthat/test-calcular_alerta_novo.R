testthat::test_that("classificar_alerta reproduz a classificacao esperada com criteria_fn explicita", {
  skip_if_not_installed("mockery")

  dados <- data.frame(
    sem_not = c(202401, 202402, 202403),
    casos = c(10, 20, 30)
  )

  params <- list(
    codmodelo = "dummy_model",
    limiar1 = 10
  )

  fake_setCriteria <- function(rule, values) {
    testthat::expect_equal(rule, "dummy_model")
    testthat::expect_true(is.character(values))
    list(rule = rule, values = values)
  }

  fake_fouralert <- function(data, crit) {
    testthat::expect_equal(crit$rule, "dummy_model")
    list(indices = list(level = c(1, 2, 3), cytrue = c(TRUE, FALSE, TRUE)))
  }

  mockery::stub(classificar_alerta, "AlertTools::fouralert", fake_fouralert)

  res <- classificar_alerta(
    data = dados,
    params = params,
    criteria_fn = fake_setCriteria
  )

  testthat::expect_equal(res$nivel, c(1, 2, 3))
  testthat::expect_equal(res$receptivo, c(TRUE, FALSE, TRUE))
})

testthat::test_that("calcular_alerta_novo reproduz a logica principal da antiga sem params global", {
  skip_if_not_installed("mockery")

  dados <- data.frame(
    id_unidade = c(1, 1, 2, 1),
    sem_not = c(202403, 202401, 202401, 202402),
    casos = c(30, 10, 99, 20)
  )

  params <- list(
    codmodelo = "dummy_model",
    limiar1 = 10
  )

  fake_Rt <- function(data, count, gtdist, meangt, sdgt) {
    data
  }

  fake_setCriteria <- function(rule, values) {
    list(rule = rule, values = values)
  }

  fake_fouralert <- function(data, crit) {
    list(indices = list(level = c(1, 2, 3), cytrue = c(TRUE, FALSE, TRUE)))
  }

  mockery::stub(calcular_alerta_novo, "calcular_rt", fake_Rt)
  mockery::stub(calcular_alerta_novo, "classificar_alerta", function(data, params, criteria_fn) {
    fake_fouralert_res <- fake_fouralert(data, fake_setCriteria(params$codmodelo, as.character(params)))
    data$nivel <- fake_fouralert_res$indices$level
    data$receptivo <- fake_fouralert_res$indices$cytrue
    data
  })

  res <- calcular_alerta_novo(
    id = 1,
    data = dados,
    coluna_id = "id_unidade",
    params = params,
    criteria_fn = fake_setCriteria
  )

  testthat::expect_equal(res$sem_not, c(202401, 202402, 202403))
  testthat::expect_equal(res$nivel, c(1, 2, 3))
})

testthat::test_that("calcular_alerta_cidade itera sobre todas as unidades sem depender de globais", {
  testthat::skip_if_not_installed("mockery")

  dados <- data.frame(
    id_unidade = c(1, 1, 2, 2),
    sem_not = c(202401, 202402, 202401, 202402),
    casos = c(10, 20, 30, 40)
  )

  params <- list(codmodelo = "dummy_model")

  mockery::stub(calcular_alerta_cidade, "calcular_alerta_novo", function(id, data, coluna_id, params, gtdist, meangt, sdgt, criteria_fn) {
    subset <- data[data[[coluna_id]] == id, , drop = FALSE]
    subset$nivel <- 1
    subset$receptivo <- FALSE
    subset
  })

  resultado <- calcular_alerta_cidade(
    data = dados,
    coluna_id = "id_unidade",
    params = params,
    criteria_fn = function(rule, values) list(rule = rule, values = values)
  )

  testthat::expect_true(all(c("id_unidade", "sem_not") %in% names(resultado)))
  testthat::expect_equal(sort(unique(resultado$id_unidade)), c(1, 2))
})
