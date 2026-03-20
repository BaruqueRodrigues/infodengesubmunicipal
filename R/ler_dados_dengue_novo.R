#' Ler e filtrar dados de dengue com municipio parametrizado
#'
#' Refatora `ler_dados_dengue()` para explicitar o codigo do municipio e as
#' colunas desejadas.
#'
#' @param caminho Caminho para o arquivo DBF.
#' @param codigo_municipio Codigo do municipio para filtro em `ID_MUNICIP` e
#'   `ID_MN_RESI`.
#' @param colunas Vetor com colunas a serem retornadas.
#'
#' @return Data frame filtrado e selecionado.
#' @export
ler_dados_dengue_novo <- function(caminho, codigo_municipio = "420910",
                                  colunas = c(
                                    "NU_NOTIFIC", "NU_ANO", "SEM_NOT", "SG_UF_NOT",
                                    "ID_UNIDADE", "ID_BAIRRO", "NM_BAIRRO",
                                    "ID_MN_RESI", "CLASSI_FIN", "DT_SIN_PRI",
                                    "DT_DIGITA"
                                  )) {
  foreign::read.dbf(caminho) %>%
    dplyr::filter(
      .data$ID_MUNICIP == codigo_municipio,
      .data$ID_MN_RESI == codigo_municipio
    ) %>%
    dplyr::select(dplyr::all_of(colunas))
}
