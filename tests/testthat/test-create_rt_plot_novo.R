testthat::test_that("create_rt_plot_novo reproduz o comportamento da funcao antiga", {
  breaks <- c(202401, 202402, 202403)
  legacy_assign_globals(list(breaks_1ano = breaks))

  dados <- tibble::tibble(
    sem_not = c(202401, 202402, 202403),
    Rt = c(0.9, 1.1, 1.2),
    Rt_lower = c(0.8, 1.0, 1.1),
    Rt_upper = c(1.0, 1.2, 1.3)
  )

  antigo <- create_rt_plot(
    api_data = dados,
    weeks_limit = 202403,
    title_suffix = "Rt",
    lwr = "Rt_lower",
    upr = "Rt_upper"
  )

  novo <- create_rt_plot_novo(
    api_data = dados,
    weeks_limit = 202403,
    title_suffix = "Rt",
    breaks_1ano = breaks,
    lwr = "Rt_lower",
    upr = "Rt_upper"
  )

  testthat::expect_equal(ggplot2::ggplot_build(antigo$combined_plot)$data, ggplot2::ggplot_build(novo$combined_plot)$data)
})

testthat::test_that("create_rt_plot_novo aceita weeks_limit e semana como factor", {
  dados <- tibble::tibble(
    sem_not = factor(c(202401, 202402, 202403)),
    Rt = c(0.9, 1.1, 1.2),
    lwr = c(0.8, 1.0, 1.1),
    upr = c(1.0, 1.2, 1.3)
  )

  resultado <- create_rt_plot_novo(
    api_data = dados,
    weeks_limit = factor("202403"),
    title_suffix = "Rt",
    breaks_1ano = c(202401, 202402, 202403),
    lwr = "lwr",
    upr = "upr"
  )

  dados_geom <- ggplot2::ggplot_build(resultado$combined_plot)$data

  testthat::expect_true(any(vapply(dados_geom, nrow, integer(1)) > 0))
})
