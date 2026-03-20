testthat::test_that("create_map_panel combina os tres ultimos mapas e retorna ggplot", {
  plots_list <- list(
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) + ggplot2::geom_point(),
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, color = factor(cyl))) + ggplot2::geom_point(),
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, fill = factor(cyl))) + ggplot2::geom_point(shape = 21),
    ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg, fill = factor(cyl))) + ggplot2::geom_point(shape = 21)
  )

  resultado <- create_map_panel(
    plots_list = plots_list,
    legend_title = "Legenda teste"
  )

  testthat::expect_s3_class(resultado, "ggplot")
})
