test_that("completar_dados_bairros preenche semanas faltantes com 0", {

  dados_teste <- tibble::tibble(
    arbo = "dengue",
    nm_bairro_ref = "CENTRO",
    sem_not = c(202401, 202403),
    notificações = c(5, 10)
  )

  resultado <- completar_dados_bairros(
    dados_bairros = dados_teste,
    bairros_unicos = "CENTRO",
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = 202403
  )

  expect_true(202402 %in% resultado$sem_not)

  semana_202402 <- resultado |>
    dplyr::filter(sem_not == 202402)

  expect_equal(semana_202402$notificações, 0)

})


test_that("completar_dados_bairros retorna colunas esperadas", {

  dados_teste <- tibble::tibble(
    arbo = "dengue",
    nm_bairro_ref = "CENTRO",
    sem_not = 202401,
    notificações = 5
  )

  resultado <- completar_dados_bairros(
    dados_bairros = dados_teste,
    bairros_unicos = "CENTRO",
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = 202401
  )

  expect_true(all(c("arbo", "nm_bairro_ref", "sem_not", "notificações") %in% names(resultado)))

})


test_that("completar_dados_bairros falha se colunas obrigatórias faltarem", {

  dados_teste <- tibble::tibble(
    arbo = "dengue",
    nm_bairro_ref = "CENTRO"
  )

  expect_error(
    completar_dados_bairros(
      dados_bairros = dados_teste,
      bairros_unicos = "CENTRO",
      se_var = "sem_not",
      nm_bairr_var = "nm_bairro_ref",
      group_var = "arbo"
    )
  )

})

test_that("completar_dados_bairros preenche semanas faltantes com zero usando mock real", {

  dados_teste <- df_mun %>%
    dplyr::filter(!is.na(nm_bairro)) %>%
    dplyr::mutate(
      nm_bairro_ref = normalizar_bairro(nm_bairro),
      sem_not = as.numeric(sem_not)
    ) %>%
    dplyr::count(arbo, nm_bairro_ref, sem_not, name = "notificações") %>%
    dplyr::arrange(sem_not) %>%
    dplyr::slice_head(n = 50)

  bairros_unicos <- unique(dados_teste$nm_bairro_ref)
  max_semana <- max(dados_teste$sem_not)

  resultado <- completar_dados_bairros(
    dados_bairros = dados_teste,
    bairros_unicos = bairros_unicos,
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = max_semana
  )

  expect_true(all(dados_teste$sem_not %in% resultado$sem_not))

  # notificações não pode ter NA
  expect_false(any(is.na(resultado$notificações)))

})


test_that("completar_dados_bairros preserva valores existentes", {

  dados_teste <- tibble::tibble(
    arbo = "dengue",
    nm_bairro_ref = "CENTRO",
    sem_not = c(201001, 201002),
    notificações = c(10, 20)
  )

  resultado <- completar_dados_bairros(
    dados_bairros = dados_teste,
    bairros_unicos = "CENTRO",
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = 201002
  )

  expect_equal(
    resultado$notificações[resultado$sem_not == 201001],
    10
  )

  expect_equal(
    resultado$notificações[resultado$sem_not == 201002],
    20
  )

})


test_that("completar_dados_bairros retorna estrutura esperada", {

  dados_teste <- df_mun %>%
    dplyr::filter(!is.na(nm_bairro)) %>%
    dplyr::mutate(
      nm_bairro_ref = normalizar_bairro(nm_bairro),
      sem_not = as.numeric(sem_not)
    ) %>%
    dplyr::count(arbo, nm_bairro_ref, sem_not, name = "notificações") %>%
    dplyr::slice_head(n = 10)

  resultado <- completar_dados_bairros(
    dados_bairros = dados_teste,
    bairros_unicos = unique(dados_teste$nm_bairro_ref),
    se_var = "sem_not",
    nm_bairr_var = "nm_bairro_ref",
    group_var = "arbo",
    max_semana_epidemiologica = max(dados_teste$sem_not)
  )

  expect_true(all(
    c("arbo", "nm_bairro_ref", "sem_not", "notificações") %in%
      names(resultado)
  ))

})

