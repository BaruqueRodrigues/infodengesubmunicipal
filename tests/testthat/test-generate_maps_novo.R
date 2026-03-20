testthat::test_that("generate_maps_novo reproduz o comportamento da antiga quando recebe as bases explicitas", {
  fixtures <- legacy_make_spatial_fixtures()

  dados <- fixtures$sp_bairros |>
    dplyr::slice(c(1, 2, 1, 2)) |>
    dplyr::mutate(
      sem_not = c(202401, 202401, 202402, 202402),
      arbo = "dengue",
      p_inc100k = c(10, 20, 15, 30)
    )

  df_mun_ok <- tibble::tibble(sem_not = c(202401, 202402), arbo = c("dengue", "dengue"))
  df_mun <- tibble::tibble(
    sem_not = c(202401, 202401, 202402, 202402),
    arbo = c("dengue", "dengue", "dengue", "dengue")
  )

  legacy_assign_globals(list(
    weeks_2_plot = c(202401, 202402),
    df_mun_ok = df_mun_ok,
    df_mun = df_mun
  ))

  antigo <- generate_maps(
    data = dados,
    weeks_2_plotdistrito_nome = c(202401, 202402),
    arbo = "dengue"
  )

  novo <- generate_maps_novo(
    data = dados,
    weeks_2_plot = c(202401, 202402),
    arbo = "dengue",
    allocated_data = df_mun_ok,
    total_data = df_mun
  )

  testthat::expect_length(novo, length(antigo))
  testthat::expect_equal(antigo[[1]]$labels$title, novo[[1]]$labels$title)
  testthat::expect_equal(antigo[[1]]$labels$subtitle, novo[[1]]$labels$subtitle)
})
