#' Contar semanas com alerta epidemiológico alto em uma janela temporal
#'
#' Calcula quantas semanas recentes um bairro apresentou nível elevado
#' de alerta epidemiológico (níveis 3 ou 4) dentro de uma janela temporal
#' retrospectiva. A função percorre a série temporal do bairro até a
#' semana epidemiológica especificada e contabiliza quantas semanas da
#' janela possuem `nivel >= 3`.
#'
#' @param data Data frame contendo os dados epidemiológicos. Deve conter
#' ao menos as colunas `nm_bairro_ref`, `sem_not` e `nivel`.
#' @param semana_atual Semana epidemiológica de referência no formato
#' numérico `YYYYWW`.
#' @param bairro Nome do bairro para o qual será calculado o número
#' de semanas com alerta alto.
#' @param janela Número de semanas a considerar na janela retrospectiva.
#'
#' @return
#' Um número inteiro representando a quantidade de semanas dentro da
#' janela especificada em que o bairro apresentou nível de alerta
#' epidemiológico alto (`nivel >= 3`). Caso o bairro ou a semana não
#' existam nos dados, a função retorna `0`.
#'
#' @details
#' O cálculo segue os seguintes passos:
#'
#' \enumerate{
#'   \item Filtra os dados para o bairro especificado
#'   \item Ordena a série temporal por semana epidemiológica
#'   \item Mantém apenas semanas até `semana_atual`
#'   \item Seleciona as últimas `janela` semanas disponíveis
#'   \item Conta quantas possuem `nivel >= 3`
#' }
#'
#' Valores `NA` na coluna `nivel` são ignorados no cálculo.
#'
#' @examples
#' \dontrun{
#' dados <- data.frame(
#'   nm_bairro_ref = rep("Centro", 10),
#'   sem_not = 202441:202450,
#'   nivel = c(1,2,3,4,2,3,1,4,3,2)
#' )
#'
#' calcular_semanas_alerta_alto(
#'   data = dados,
#'   semana_atual = 202450,
#'   bairro = "Centro",
#'   janela = 5
#' )
#' }
#'
#' @export
calcular_semanas_alerta_alto_novo <- function(data, semana_atual, bairro, janela) {

  resultado <-
    data |>
    dplyr::filter(.data$nm_bairro_ref == bairro) |>
    dplyr::arrange(.data$sem_not) |>
    dplyr::filter(.data$sem_not <= semana_atual) |>
    dplyr::slice_tail(n = janela) |>
    dplyr::summarise(
      semanas_alerta = sum(.data$nivel >= 3, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::pull(.data$semanas_alerta)

  if (length(resultado) == 0) return(0)

  resultado
}
