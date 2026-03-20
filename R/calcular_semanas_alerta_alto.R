#' Contar semanas com alerta alto em uma janela temporal
#'
#' Calcula o número de semanas recentes em que um bairro apresentou
#' níveis elevados de alerta epidemiológico (níveis 3 ou 4) dentro de
#' uma janela temporal definida. A função percorre a série temporal
#' do bairro até a semana epidemiológica especificada e contabiliza
#' quantas semanas da janela apresentam nível de alerta alto.
#'
#' @param data Data frame contendo os dados epidemiológicos. Deve
#' incluir ao menos as colunas `nm_bairro_ref`, `sem_not` e `nivel`.
#' @param semana_atual Valor da semana epidemiológica de referência
#' (formato numérico `YYYYWW`).
#' @param bairro Nome do bairro para o qual será calculado o número
#' de semanas com alerta alto.
#' @param janela Número de semanas a considerar na janela temporal
#' retrospectiva.
#'
#' @return
#' Um número inteiro representando a quantidade de semanas dentro da
#' janela especificada em que o bairro apresentou nível de alerta
#' epidemiológico alto (`nivel >= 3`).
#'
#' @details
#' O cálculo segue os passos:
#'
#' * Filtra os dados para o bairro especificado
#' * Ordena a série temporal por semana epidemiológica
#' * Localiza a posição da semana atual na série
#' * Seleciona a janela retrospectiva de semanas
#' * Conta quantas semanas possuem `nivel >= 3`
#'
#' Caso o bairro ou a semana não estejam presentes nos dados, a
#' função retorna `0`.
#'
#' @examples
#' \dontrun{
#' calcular_semanas_alerta_alto(
#'   data = df_bairros,
#'   semana_atual = 202452,
#'   bairro = "Boa Vista",
#'   janela = 10
#' )
#' }
#'
#' @export
calcular_semanas_alerta_alto <- function(data, semana_atual, bairro, janela) {
  bairro_char <- as.character(bairro)
  dados_bairro <- data[data$nm_bairro_ref == bairro_char, ]

  if (nrow(dados_bairro) == 0) return(0)

  dados_bairro <- dados_bairro[order(dados_bairro$sem_not), ]
  pos_atual <- match(semana_atual, dados_bairro$sem_not)

  if (is.na(pos_atual)) return(0)

  inicio_janela <- max(1, pos_atual - janela + 1)
  niveis <- dados_bairro$nivel[inicio_janela:pos_atual]

  # Contar níveis 3 e 4 (alerta alto)
  sum(!is.na(niveis) & niveis >= 3)
}
