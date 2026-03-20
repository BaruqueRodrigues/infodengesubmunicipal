#' Lê e filtra dados históricos de dengue
#'
#' @description
#' Lê arquivo DBF contendo notificações de dengue
#' e retorna subconjunto filtrado por município
#' e com colunas selecionadas.
#'
#' @details
#' Atualmente o filtro do município está fixado para o código
#' IBGE "420910". Recomenda-se refatorar a função para
#' receber o código como parâmetro para permitir escalabilidade.
#'
#' @param caminho Caminho para o arquivo DBF contendo os dados.
#'
#' @return
#' Um `data.frame` contendo as colunas:
#' \itemize{
#'   \item NU_NOTIFIC
#'   \item NU_ANO
#'   \item SEM_NOT
#'   \item SG_UF_NOT
#'   \item ID_UNIDADE
#'   \item ID_BAIRRO
#'   \item NM_BAIRRO
#'   \item ID_MN_RESI
#'   \item CLASSI_FIN
#'   \item DT_SIN_PRI
#'   \item DT_DIGITA
#' }
#'
#' @examples
#' \dontrun{
#' dados <- ler_dados_dengue("dados_dengue.dbf")
#' }
#'
#' @export
#' @importFrom foreign read.dbf
#' @importFrom dplyr filter select
ler_dados_dengue <- function(caminho) {
  foreign::read.dbf(caminho) %>%
    dplyr::filter(ID_MUNICIP == "420910", ID_MN_RESI == "420910") %>%
    dplyr::select(
      NU_NOTIFIC, NU_ANO, SEM_NOT, SG_UF_NOT, ID_UNIDADE,
      ID_BAIRRO, NM_BAIRRO, ID_MN_RESI, CLASSI_FIN,
      DT_SIN_PRI, DT_DIGITA
    )
}
