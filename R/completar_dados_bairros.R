#' Completa grade bairro × semana epidemiológica
#'
#' @description
#' Gera combinação completa entre bairros, semanas epidemiológicas
#' e grupos (ex: arboviroses), preenchendo notificações ausentes com zero.
#'
#' @details
#' A função:
#' \itemize{
#'   \item Extrai ano e semana da variável epidemiológica (formato YYYYWW)
#'   \item Gera grade completa bairro × semana × grupo
#'   \item Preenche notificações ausentes com 0
#'   \item Ordena resultado final
#' }
#'
#' Permite limitar a semana máxima do ano corrente via
#' `max_semana_epidemiologica`.
#'
#' @param dados_bairros Data frame contendo notificações agregadas.
#' @param bairros_unicos Vetor com todos os bairros esperados.
#' @param se_var Nome da coluna da semana epidemiológica (formato "YYYYWW").
#' @param nm_bairr_var Nome da coluna do bairro.
#' @param group_var Nome da coluna de agrupamento (ex: "arbo").
#' @param max_semana_epidemiologica Valor numérico no formato YYYYWW
#' indicando limite superior da série temporal.
#'
#' @return
#' Data frame contendo todas as combinações possíveis de
#' bairro × semana × grupo, com notificações preenchidas.
#'
#' @examples
#' \dontrun{
#' dados <- tibble::tibble(
#'   arbo = "dengue",
#'   nm_bairro_ref = "CENTRO",
#'   sem_not = c(202401, 202403),
#'   notificações = c(5, 10)
#' )
#'
#' completar_dados_bairros(
#'   dados_bairros = dados,
#'   bairros_unicos = "CENTRO",
#'   se_var = "sem_not",
#'   nm_bairr_var = "nm_bairro_ref",
#'   group_var = "arbo",
#'   max_semana_epidemiologica = 202403
#' )
#' }
#'
#' @export

completar_dados_bairros <- function(dados_bairros, bairros_unicos, se_var, nm_bairr_var,
                                    group_var = "arbo", max_semana_epidemiologica = NULL) {

  #library(dplyr)
  #library(tidyr)

  # Verify required columns exist
  required_cols <- c(se_var, nm_bairr_var, group_var, "notificações")
  missing_cols <- setdiff(required_cols, names(dados_bairros))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Get unique arbovirus types
  arbo_unicos <- unique(dados_bairros[[group_var]])

  # Extract year from se_var (assuming format like 202401, 202402, etc.)
  dados_bairros <- dados_bairros %>%
    dplyr::mutate(ano = as.numeric(substr(as.character(.data[[se_var]]), 1, 4)),
           semana = as.numeric(substr(as.character(.data[[se_var]]), 5, 6)))

  # Get all years present in data
  anos_unicos <- unique(dados_bairros$ano)

  # If max_semana_epidemiologica is provided, extract year and week from it
  if (!is.null(max_semana_epidemiologica)) {
    # max_semana_epidemiologica is in format YYYYWW (e.g., 202532)
    ano_atual <- as.numeric(substr(as.character(max_semana_epidemiologica), 1, 4))
    max_week_atual <- as.numeric(substr(as.character(max_semana_epidemiologica), 5, 6))
  } else {
    # Otherwise, determine current year and max week from data
    ano_atual <- max(anos_unicos)
    max_week_atual <- max(dados_bairros$semana[dados_bairros$ano == ano_atual], na.rm = TRUE)
  }

  # Create complete data for each year
  complete_data_list <- list()

  for (year in anos_unicos) {
    # Determine max week for this year
    if (year == ano_atual) {
      max_week <- max_week_atual
    } else {
      max_week <- 52  # Complete 52 weeks for previous years
    }

    # Create complete grid for this year
    year_grid <- tidyr::expand_grid(
      !!nm_bairr_var := bairros_unicos,
      ano = year,
      semana = 1:max_week,
      !!group_var := arbo_unicos
    ) %>%
      dplyr::mutate(!!se_var := as.numeric(paste0(ano, sprintf("%02d", semana))))

    complete_data_list[[as.character(year)]] <- year_grid
  }

  # Combine all years
  complete_data <- bind_rows(complete_data_list)

  # Join with original data and fill missing values with 0
  resultado <- complete_data %>%
    dplyr::left_join(dados_bairros, by = c(nm_bairr_var, se_var, group_var, "ano", "semana")) %>%
    dplyr::mutate(notificações = dplyr::coalesce(notificações, 0)) %>%
    dplyr::select(-ano, -semana) %>%  # Remove auxiliary columns
    dplyr::arrange(!!rlang::sym(group_var), !!rlang::sym(nm_bairr_var), !!rlang::sym(se_var))

  return(resultado)
}
