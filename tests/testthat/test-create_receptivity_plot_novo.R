testthat::test_that("create_receptivity_plot_novo reproduz o legado quando parametrizado", {
  breaks <- c(202401, 202402, 202403)
  legacy_assign_globals(list(
    ano_atual = 2024,
    weeks_2_plot = c(202401, 202402, 202403),
    breaks_1ano = breaks
  ))

  dados <- tibble::tibble(
    SE = c(202401, 202402, 202403),
    tempmin = c(17, 19, 20)
  )

  antigo <- create_receptivity_plot(
    api_data = dados,
    weeks_limit = 202403
  )

  novo <- create_receptivity_plot_novo(
    api_data = dados,
    weeks_limit = 202403,
    breaks_1ano = breaks
  )

  testthat::expect_equal(ggplot2::ggplot_build(antigo)$data, ggplot2::ggplot_build(novo)$data)
})

testthat::test_that("create_receptivity_plot_novo usa weeks_limit explicitamente", {
  breaks <- c(202401, 202402, 202403, 202404)

  dados <- tibble::tibble(
    SE = c(202401, 202402, 202403, 202404),
    tempmin = c(17, 19, 20, 21)
  )

  plot_a <- create_receptivity_plot_novo(dados, weeks_limit = 202403, breaks_1ano = breaks)
  plot_b <- create_receptivity_plot_novo(dados, weeks_limit = 202404, breaks_1ano = breaks)

  testthat::expect_false(identical(ggplot2::ggplot_build(plot_a)$data, ggplot2::ggplot_build(plot_b)$data))
})
