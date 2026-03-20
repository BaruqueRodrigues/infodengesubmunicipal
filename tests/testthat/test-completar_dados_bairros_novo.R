testthat::test_that("completar_dados_bairros_novo reproduz o comportamento principal da antiga", {
  dados_teste <- tibble::tibble(
    arbo = "dengue",
    nm_bairro_ref = "CENTRO",
    sem_not = c(202401, 202403),
    notificações = c(5, 10)
  )

  antigo <- completar_dados_bairros(
    dados_bairros = dados_teste,
    bairros_unicos = "CENTRO",
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = 202403
  )

  novo <- completar_dados_bairros_novo(
    dados_bairros = dados_teste,
    bairros_unicos = "CENTRO",
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = 202403
  )

  testthat::expect_equal(antigo, novo)
})

testthat::test_that("completar_dados_bairros_novo lida com base vazia sem erro", {
  dados_teste <- tibble::tibble(
    arbo = character(),
    nm_bairro_ref = character(),
    sem_not = numeric(),
    notificações = numeric()
  )

  resultado <- completar_dados_bairros_novo(
    dados_bairros = dados_teste,
    bairros_unicos = character(),
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo"
  )

  testthat::expect_s3_class(resultado, "data.frame")
  testthat::expect_equal(nrow(resultado), 0)
})

testthat::test_that("completar_dados_bairros_novo permite count_var customizado", {
  dados_teste <- tibble::tibble(
    arbo = "dengue",
    nm_bairro_ref = "CENTRO",
    sem_not = c(202401, 202403),
    casos = c(5, 10)
  )

  resultado <- completar_dados_bairros_novo(
    dados_bairros = dados_teste,
    bairros_unicos = "CENTRO",
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = 202403,
    count_var = "casos"
  )

  testthat::expect_true(202402 %in% resultado$sem_not)
  testthat::expect_equal(resultado$casos[resultado$sem_not == 202402], 0)
})
