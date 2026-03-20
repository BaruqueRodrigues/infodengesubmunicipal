#' Criar breaks do eixo X para janela móvel de 1 ano
#'
#' Gera pontos de quebra (breaks) para o eixo X de gráficos baseados em
#' semanas epidemiológicas, considerando uma janela móvel de 1 ano
#' (52 semanas). Os breaks são calculados em intervalos definidos
#' pelo argumento `passo` e retornados no formato `YYYYWW`.
#'
#' A função converte o ano e a semana final em um índice absoluto de
#' semanas, calcula os pontos de quebra dentro da janela de 52 semanas
#' anteriores e depois reconverte para o formato `ano-semana`.
#'
#' @param ano_fim Inteiro. Ano da última semana da série.
#' @param semana_fim Inteiro. Semana epidemiológica final (1–52).
#' @param passo Inteiro. Intervalo entre os breaks em semanas.
#' Default é `10`.
#'
#' @return
#' Um vetor numérico contendo os breaks do eixo X no formato `YYYYWW`.
#'
#' @examples
#' \dontrun{
#' # Gerar breaks para uma série que termina na semana 35 de 2024
#' criar_breaks_1ano(
#'   ano_fim = 2024,
#'   semana_fim = 35
#' )
#'
#' # Usando passo menor
#' criar_breaks_1ano(
#'   ano_fim = 2024,
#'   semana_fim = 35,
#'   passo = 5
#' )
#' }
#'
#' @export
criar_breaks_1ano <- function(ano_fim, semana_fim, passo = 10) {
  abs_fim    <- ano_fim * 52 + semana_fim
  abs_inicio <- abs_fim - 52

  abs_breaks <- seq(abs_inicio, abs_fim, by = passo)

  if (tail(abs_breaks, 1) != abs_fim) {
    abs_breaks <- c(abs_breaks, abs_fim)
  }

  anos    <- abs_breaks %/% 52
  semanas <- abs_breaks %% 52

  mask <- semanas == 0
  semanas[mask] <- 52
  anos[mask]    <- anos[mask] - 1

  breaks <- anos * 100 + semanas
  return(breaks)
}
