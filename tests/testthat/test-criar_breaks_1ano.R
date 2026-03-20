testthat::test_that("criar_breaks_1ano inclui a semana final e respeita o passo", {
  resultado <- criar_breaks_1ano(
    ano_fim = 2024,
    semana_fim = 35,
    passo = 10
  )

  testthat::expect_type(resultado, "double")
  testthat::expect_equal(tail(resultado, 1), 202435)
  testthat::expect_true(length(resultado) >= 6)
})

testthat::test_that("criar_breaks_1ano trata corretamente a virada em semana 52", {
  resultado <- criar_breaks_1ano(
    ano_fim = 2024,
    semana_fim = 52,
    passo = 26
  )

  testthat::expect_equal(resultado, c(202352, 202426, 202452))
})
