testthat::test_that("create_receptivity_plot retorna ggplot quando globais legadas existem", {
  legacy_assign_globals(list(
    ano_atual = 2024,
    weeks_2_plot = c(202401, 202402, 202403),
    breaks_1ano = c(202401, 202402, 202403)
  ))

  dados <- tibble::tibble(
    SE = c(202401, 202402, 202403),
    tempmin = c(17, 19, 20)
  )

  resultado <- create_receptivity_plot(
    api_data = dados,
    weeks_limit = 202403
  )

  testthat::expect_s3_class(resultado, "ggplot")
  testthat::expect_equal(resultado$labels$title, "Receptividade climática e Temperatura Mínima por Semana")
})

testthat::test_that("create_receptivity_plot ignora o argumento weeks_limit no comportamento legado", {
  legacy_assign_globals(list(
    ano_atual = 2024,
    weeks_2_plot = c(202401, 202402, 202403),
    breaks_1ano = c(202401, 202402, 202403)
  ))

  dados <- tibble::tibble(
    SE = c(202401, 202402, 202403),
    tempmin = c(17, 19, 20)
  )

  plot_a <- create_receptivity_plot(dados, weeks_limit = 202401)
  plot_b <- create_receptivity_plot(dados, weeks_limit = 202499)

  testthat::expect_equal(ggplot2::ggplot_build(plot_a)$data, ggplot2::ggplot_build(plot_b)$data)
})
