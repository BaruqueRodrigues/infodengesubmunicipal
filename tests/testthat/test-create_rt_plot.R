testthat::test_that("create_rt_plot retorna grafico combinado e individual sem facet", {
  legacy_assign_globals(list(breaks_1ano = c(202401, 202402, 202403)))

  dados <- tibble::tibble(
    sem_not = c(202401, 202402, 202403),
    Rt = c(0.9, 1.1, 1.2),
    Rt_lower = c(0.8, 1.0, 1.1),
    Rt_upper = c(1.0, 1.2, 1.3)
  )

  resultado <- create_rt_plot(
    api_data = dados,
    weeks_limit = 202403,
    title_suffix = "Rt",
    lwr = "Rt_lower",
    upr = "Rt_upper"
  )

  testthat::expect_named(resultado, c("combined_plot", "individual_plots"))
  testthat::expect_s3_class(resultado$combined_plot, "ggplot")
  testthat::expect_length(resultado$individual_plots, 1)
})

testthat::test_that("create_rt_plot cria graficos por grupo quando facet_by e informado", {
  legacy_assign_globals(list(breaks_1ano = c(202401, 202402, 202403)))

  dados <- tibble::tibble(
    sem_not = c(202401, 202402, 202403, 202401, 202402, 202403),
    Rt = c(0.9, 1.1, 1.2, 0.7, 0.8, 1.0),
    distrito = c("A", "A", "A", "B", "B", "B")
  )

  resultado <- create_rt_plot(
    api_data = dados,
    weeks_limit = 202403,
    title_suffix = "Rt por distrito",
    facet_by = "distrito"
  )

  testthat::expect_s3_class(resultado$combined_plot, "ggplot")
  testthat::expect_named(resultado$individual_plots, c("A", "B"))
  testthat::expect_true(all(vapply(resultado$individual_plots, inherits, logical(1), "ggplot")))
})

testthat::test_that("create_rt_plot avisa quando colunas ribbon nao existem", {
  legacy_assign_globals(list(breaks_1ano = c(202401, 202402, 202403)))

  dados <- tibble::tibble(
    sem_not = c(202401, 202402, 202403),
    Rt = c(0.9, 1.1, 1.2)
  )

  testthat::expect_warning(
    resultado <- create_rt_plot(
      api_data = dados,
      weeks_limit = 202403,
      title_suffix = "Rt",
      lwr = "inexistente"
    )
  )

  testthat::expect_s3_class(resultado$combined_plot, "ggplot")
})

testthat::test_that("create_rt_plot falha quando coluna obrigatoria nao existe", {
  legacy_assign_globals(list(breaks_1ano = c(202401, 202402, 202403)))

  dados <- tibble::tibble(
    semana = c(202401, 202402, 202403),
    valor = c(0.9, 1.1, 1.2)
  )

  testthat::expect_error(
    create_rt_plot(
      api_data = dados,
      weeks_limit = 202403,
      title_suffix = "Rt"
    ),
    "Colunas obrigatorias|Colunas obrigatórias"
  )
})
