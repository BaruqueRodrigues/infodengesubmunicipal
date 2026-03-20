testthat::test_that("classificar_bairros_distrito associa bairros por intersecao de centroides", {
  fixtures <- legacy_make_spatial_fixtures()

  resultado <- suppressWarnings(
    classificar_bairros_distrito(
      sp_distritos = fixtures$sp_distritos,
      sp_bairros = fixtures$sp_bairros[1:2, ]
    )
  )

  testthat::expect_s3_class(resultado, "sf")
  testthat::expect_true("geometry_distritos" %in% names(resultado))
  testthat::expect_equal(resultado$distrito, c("NORTE", "SUL"))
})

testthat::test_that("classificar_bairros_distrito atribui distrito mais proximo quando nao ha intersecao", {
  fixtures <- legacy_make_spatial_fixtures()

  resultado <- suppressWarnings(
    classificar_bairros_distrito(
      sp_distritos = fixtures$sp_distritos,
      sp_bairros = fixtures$sp_bairros
    )
  )

  testthat::expect_equal(resultado$distrito, c("NORTE", "SUL", "SUL"))
})

testthat::test_that("classificar_bairros_distrito transforma bairros para o CRS dos distritos", {
  fixtures <- legacy_make_spatial_fixtures()
  bairros_3857 <- sf::st_transform(fixtures$sp_bairros, 3857)

  resultado <- suppressWarnings(
    classificar_bairros_distrito(
      sp_distritos = fixtures$sp_distritos,
      sp_bairros = bairros_3857[1:2, ]
    )
  )

  testthat::expect_equal(sf::st_crs(resultado), sf::st_crs(fixtures$sp_distritos))
  testthat::expect_equal(resultado$distrito, c("NORTE", "SUL"))
})
