testthat::test_that("create_incidence_plot retorna ggplot com globais legadas definidas", {
  legacy_assign_globals(list(
    ano_atual = 2024,
    weeks_2_plot = c(202401, 202402, 202403),
    limiar_epidemico = 72,
    breaks_1ano = c(202401, 202402, 202403)
  ))

  observed_data <- tibble::tibble(
    ano_epi = c(202401, 202402, 202403),
    total_Y = c(10, 20, 30),
    nivel = c(1, 2, 3)
  )

  nowcast_data <- tibble::tibble(
    ano_epi = c(202401, 202402, 202403),
    Median = c(8, 18, 25),
    LI = c(7, 15, 20),
    LS = c(9, 19, 28),
    type = c("Nowcasting", "Nowcasting", "Nowcasting")
  )

  resultado <- create_incidence_plot(
    nowcast_data = nowcast_data,
    observed_data = observed_data,
    title_suffix = "Curva teste"
  )

  testthat::expect_s3_class(resultado, "ggplot")
  testthat::expect_equal(resultado$labels$title, "Curva teste")
})

testthat::test_that("create_incidence_plot ajusta linha de nowcasting para nao ficar abaixo do observado", {
  legacy_assign_globals(list(
    ano_atual = 2024,
    weeks_2_plot = c(202401, 202402, 202403),
    limiar_epidemico = 72,
    breaks_1ano = c(202401, 202402, 202403)
  ))

  observed_data <- tibble::tibble(
    ano_epi = c(202401, 202402, 202403),
    total_Y = c(10, 20, 30),
    nivel = c(1, 2, 3)
  )

  nowcast_data <- tibble::tibble(
    ano_epi = c(202401, 202402, 202403),
    Median = c(8, 18, 25),
    LI = c(7, 15, 20),
    LS = c(9, 19, 28),
    type = c("Nowcasting", "Nowcasting", "Nowcasting")
  )

  resultado <- create_incidence_plot(
    nowcast_data = nowcast_data,
    observed_data = observed_data,
    title_suffix = "Curva teste"
  )

  dados_plot <- ggplot2::ggplot_build(resultado)$data[[3]]

  testthat::expect_equal(dados_plot$y, observed_data$total_Y)
})
