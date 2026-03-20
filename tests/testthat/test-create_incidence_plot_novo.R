testthat::test_that("create_incidence_plot_novo reproduz o comportamento principal da antiga", {
  breaks <- c(202401, 202402, 202403)
  legacy_assign_globals(list(
    ano_atual = 2024,
    weeks_2_plot = c(202401, 202402, 202403),
    limiar_epidemico = 72,
    breaks_1ano = breaks
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

  antigo <- create_incidence_plot(
    nowcast_data = nowcast_data,
    observed_data = observed_data,
    title_suffix = "Curva teste"
  )

  novo <- create_incidence_plot_novo(
    nowcast_data = nowcast_data,
    observed_data = observed_data,
    weeks_limit = 202403,
    breaks_1ano = breaks,
    limiar_epidemico = 72,
    title_suffix = "Curva teste"
  )

  testthat::expect_equal(ggplot2::ggplot_build(antigo)$data, ggplot2::ggplot_build(novo)$data)
  testthat::expect_equal(novo$labels$title, "Curva teste")
})

testthat::test_that("create_incidence_plot_novo exige pop para converter incidencia", {
  observed_data <- tibble::tibble(
    ano_epi = c(202401, 202402),
    total_Y = c(10, 20),
    nivel = c(1, 2)
  )

  nowcast_data <- tibble::tibble(
    ano_epi = c(202401, 202402),
    Median = c(8, 18),
    LI = c(7, 15),
    LS = c(9, 19),
    type = c("Nowcasting", "Nowcasting")
  )

  testthat::expect_error(
    create_incidence_plot_novo(
      nowcast_data = nowcast_data,
      observed_data = observed_data,
      weeks_limit = 202402,
      breaks_1ano = c(202401, 202402),
      limiar_epidemico = 72,
      convert_to_incidence = TRUE
    ),
    "pop"
  )
})
