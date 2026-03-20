testthat::test_that("ler_dados_dengue filtra municipio fixo e seleciona colunas esperadas", {
  testthat::skip_if_not_installed("mockery")

  dados_brutos <- data.frame(
    NU_NOTIFIC = 1:3,
    NU_ANO = c(2024, 2024, 2024),
    SEM_NOT = c(202401, 202401, 202402),
    SG_UF_NOT = c("SC", "SC", "SC"),
    ID_UNIDADE = c("A", "B", "C"),
    ID_BAIRRO = c("10", "11", "12"),
    NM_BAIRRO = c("Centro", "Sul", "Norte"),
    ID_MN_RESI = c("420910", "420910", "999999"),
    ID_MUNICIP = c("420910", "999999", "420910"),
    CLASSI_FIN = c(1, 2, 3),
    DT_SIN_PRI = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    DT_DIGITA = as.Date(c("2024-01-04", "2024-01-05", "2024-01-06")),
    EXTRA = c("x", "y", "z")
  )

  fake_read_dbf <- function(caminho) {
    testthat::expect_equal(caminho, "arquivo.dbf")
    dados_brutos
  }

  mockery::stub(ler_dados_dengue, "foreign::read.dbf", fake_read_dbf)

  resultado <- ler_dados_dengue("arquivo.dbf")

  testthat::expect_equal(nrow(resultado), 1)
  testthat::expect_equal(resultado$NU_NOTIFIC, 1)
  testthat::expect_equal(
    names(resultado),
    c(
      "NU_NOTIFIC", "NU_ANO", "SEM_NOT", "SG_UF_NOT", "ID_UNIDADE",
      "ID_BAIRRO", "NM_BAIRRO", "ID_MN_RESI", "CLASSI_FIN",
      "DT_SIN_PRI", "DT_DIGITA"
    )
  )
})
