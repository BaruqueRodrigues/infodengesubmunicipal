testthat::test_that("ler_dados_dengue_novo parametriza o codigo do municipio", {
  testthat::skip_if_not_installed("mockery")

  dados_brutos <- data.frame(
    NU_NOTIFIC = 1:3,
    NU_ANO = c(2024, 2024, 2024),
    SEM_NOT = c(202401, 202401, 202402),
    SG_UF_NOT = c("SC", "SC", "SC"),
    ID_UNIDADE = c("A", "B", "C"),
    ID_BAIRRO = c("10", "11", "12"),
    NM_BAIRRO = c("Centro", "Sul", "Norte"),
    ID_MN_RESI = c("999999", "999999", "420910"),
    ID_MUNICIP = c("999999", "420910", "999999"),
    CLASSI_FIN = c(1, 2, 3),
    DT_SIN_PRI = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    DT_DIGITA = as.Date(c("2024-01-04", "2024-01-05", "2024-01-06"))
  )

  mockery::stub(ler_dados_dengue_novo, "foreign::read.dbf", function(caminho) dados_brutos)

  resultado <- ler_dados_dengue_novo(
    caminho = "arquivo.dbf",
    codigo_municipio = "999999"
  )

  testthat::expect_equal(nrow(resultado), 1)
  testthat::expect_equal(resultado$NU_NOTIFIC, 1)
})

testthat::test_that("ler_dados_dengue_novo permite selecionar subconjunto de colunas", {
  testthat::skip_if_not_installed("mockery")

  dados_brutos <- data.frame(
    NU_NOTIFIC = 1,
    NU_ANO = 2024,
    SEM_NOT = 202401,
    SG_UF_NOT = "SC",
    ID_UNIDADE = "A",
    ID_BAIRRO = "10",
    NM_BAIRRO = "Centro",
    ID_MN_RESI = "420910",
    ID_MUNICIP = "420910",
    CLASSI_FIN = 1,
    DT_SIN_PRI = as.Date("2024-01-01"),
    DT_DIGITA = as.Date("2024-01-04")
  )

  mockery::stub(ler_dados_dengue_novo, "foreign::read.dbf", function(caminho) dados_brutos)

  resultado <- ler_dados_dengue_novo(
    caminho = "arquivo.dbf",
    colunas = c("NU_NOTIFIC", "SEM_NOT")
  )

  testthat::expect_equal(names(resultado), c("NU_NOTIFIC", "SEM_NOT"))
})
