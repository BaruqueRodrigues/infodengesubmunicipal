#' Calcular níveis de alerta epidemiológico por unidade geográfica
#'
#' Aplica o modelo de estimativa de Rt do pacote `AlertTools` e classifica
#' os níveis de alerta epidemiológico para uma unidade geográfica específica
#' (ex.: distrito, bairro ou município). A função filtra os dados pelo
#' identificador informado, ordena as observações por semana epidemiológica
#' e calcula os níveis de alerta utilizando regras definidas no objeto
#' `params`.
#'
#' Os níveis de alerta são gerados pela função `AlertTools::fouralert`,
#' retornando indicadores como nível de alerta (`nivel`) e receptividade
#' (`receptivo`).
#'
#' @param id Valor do identificador da unidade geográfica que será analisada.
#' @param nome Nome da unidade geográfica (não utilizado diretamente no
#' processamento, mas útil para controle em iterações com `purrr::map2`).
#' @param coluna_id String com o nome da coluna que contém o identificador
#' da unidade geográfica em `data`.
#' @param data Data frame contendo os dados epidemiológicos. Deve incluir
#' ao menos as colunas de identificação geográfica, `sem_not` (semana
#' epidemiológica) e `casos`.
#' @param gtdist Distribuição do tempo de geração utilizada no cálculo
#' de Rt. Exemplo: `"normal"`.
#' @param meangt Média do tempo de geração.
#' @param sdgt Desvio padrão do tempo de geração.
#'
#' @return
#' Um `data.frame` contendo os dados filtrados para a unidade geográfica
#' analisada, acrescidos das colunas:
#'
#' * `nivel` — nível de alerta epidemiológico
#' * `receptivo` — indicador de receptividade do sistema de alerta
#'
#' @details
#' A função utiliza:
#'
#' * `AlertTools::Rt()` para estimar o número reprodutivo efetivo (Rt)
#' * `AlertTools::fouralert()` para classificar o nível de alerta
#'
#' Os critérios de classificação são definidos a partir do objeto `params`,
#' que deve estar disponível no ambiente de execução.
#'
#' @examples
#' \dontrun{
#' calcular_alerta(
#'   id = 1,
#'   nome = "Distrito Centro",
#'   coluna_id = "id_distrito_nome",
#'   data = df_distritos,
#'   gtdist = "normal",
#'   meangt = 3,
#'   sdgt = 1
#' )
#' }
#'
#' @export
calcular_alerta <- function(id, nome, coluna_id, data, gtdist, meangt, sdgt) {

  bdi <- data %>%
    dplyr::filter(.data[[coluna_id]] == id) %>%
    dplyr::arrange(sem_not) %>%
    AlertTools::Rt(count = "casos", gtdist = gtdist, meangt = meangt, sdgt = sdgt)

  crit.x.vector <- structure(as.character(params), names = names(params))
  criteriaU <- setCriteria(rule = params$codmodelo, values = crit.x.vector)

  y <- AlertTools::fouralert(bdi, crit = criteriaU)

  bdi$nivel <- y$indices$level
  bdi$receptivo <- y$indices$cytrue

  return(bdi)
}
