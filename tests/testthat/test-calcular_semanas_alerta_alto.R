# Teste básico — contar semanas com alerta alto
testthat::test_that("calcular_semanas_alerta_alto conta semanas com nivel >=3", {

  dados <- data.frame(
    nm_bairro_ref = rep("Centro", 10),
    sem_not = 202441:202450,
    nivel = c(1,2,3,4,2,3,1,4,3,2)
  )

  res <- calcular_semanas_alerta_alto(
    data = dados,
    semana_atual = 202450,
    bairro = "Centro",
    janela = 5
  )

  # últimas 5 semanas: 202446–202450
  # níveis: 3,1,4,3,2 → três >=3
  testthat::expect_equal(res, 3)

})
# Teste quando o bairro não existe
#A função deve retornar 0.
testthat::test_that("retorna 0 quando bairro não existe", {

  dados <- data.frame(
    nm_bairro_ref = rep("Centro", 5),
    sem_not = 202446:202450,
    nivel = c(1,2,3,4,3)
  )

  res <- calcular_semanas_alerta_alto(
    data = dados,
    semana_atual = 202450,
    bairro = "Bairro Inexistente",
    janela = 5
  )

  testthat::expect_equal(res, 0)

})
# Teste quando a semana não existe
#Também deve retornar 0.
testthat::test_that("retorna 0 quando semana não existe na série", {

  dados <- data.frame(
    nm_bairro_ref = rep("Centro", 5),
    sem_not = 202446:202450,
    nivel = c(1,2,3,4,3)
  )

  res <- calcular_semanas_alerta_alto(
    data = dados,
    semana_atual = 202460,
    bairro = "Centro",
    janela = 5
  )

  testthat::expect_equal(res, 0)

})
# Teste quando a janela é maior que a série
#A função usa:
#  max(1, pos_atual - janela + 1)
#Então ela deve usar toda a série disponível.
testthat::test_that("janela maior que serie funciona corretamente", {

  dados <- data.frame(
    nm_bairro_ref = rep("Centro", 3),
    sem_not = 202448:202450,
    nivel = c(3,4,1)
  )

  res <- calcular_semanas_alerta_alto(
    data = dados,
    semana_atual = 202450,
    bairro = "Centro",
    janela = 10
  )

  # níveis: 3,4,1 → dois >=3
  testthat::expect_equal(res, 2)

})
#Teste ignorando NA
#A função ignora NA.
testthat::test_that("NA em nivel nao contam como alerta", {

  dados <- data.frame(
    nm_bairro_ref = rep("Centro", 5),
    sem_not = 202446:202450,
    nivel = c(3, NA, 4, 2, NA)
  )

  res <- calcular_semanas_alerta_alto(
    data = dados,
    semana_atual = 202450,
    bairro = "Centro",
    janela = 5
  )

  # apenas 3 e 4 contam
  testthat::expect_equal(res, 2)

})

#Teste garantindo tipo inteiro
testthat::test_that("retorna valor numerico", {

  dados <- data.frame(
    nm_bairro_ref = rep("Centro", 3),
    sem_not = 202448:202450,
    nivel = c(3,4,1)
  )

  res <- calcular_semanas_alerta_alto(
    data = dados,
    semana_atual = 202450,
    bairro = "Centro",
    janela = 3
  )

  testthat::expect_type(res, "integer")

})
