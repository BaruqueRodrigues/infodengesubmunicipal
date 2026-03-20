#' Completar grade bairro x semana epidemiologica com regras explicitas
#'
#' Refatora `completar_dados_bairros()` para lidar melhor com base vazia,
#' usar namespace explicito e permitir configurar o nome da coluna de contagem.
#'
#' @param dados_bairros Data frame contendo notificacoes agregadas.
#' @param bairros_unicos Vetor com todos os bairros esperados.
#' @param se_var Nome da coluna da semana epidemiologica.
#' @param nm_bairr_var Nome da coluna do bairro.
#' @param group_var Nome da coluna de agrupamento.
#' @param max_semana_epidemiologica Limite superior da serie temporal.
#' @param count_var Nome da coluna de contagem.
#'
#' @return Data frame com a grade completa e contagens preenchidas.
#' @export
completar_dados_bairros_novo <- function(dados_bairros, bairros_unicos, se_var,
                                         nm_bairr_var, group_var = "arbo",
                                         max_semana_epidemiologica = NULL,
                                         count_var = "notificações") {
  required_cols <- c(se_var, nm_bairr_var, group_var, count_var)
  missing_cols <- setdiff(required_cols, names(dados_bairros))

  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  if (length(bairros_unicos) == 0) {
    bairros_unicos <- unique(dados_bairros[[nm_bairr_var]])
  }

  if (nrow(dados_bairros) == 0) {
    empty_result <- dados_bairros
    empty_result[[se_var]] <- numeric(0)
    empty_result[[count_var]] <- numeric(0)
    return(empty_result)
  }

  dados_bairros <- dados_bairros %>%
    dplyr::mutate(
      ano = as.numeric(substr(as.character(.data[[se_var]]), 1, 4)),
      semana = as.numeric(substr(as.character(.data[[se_var]]), 5, 6))
    )

  arbo_unicos <- unique(dados_bairros[[group_var]])
  anos_unicos <- sort(unique(dados_bairros$ano))

  if (!is.null(max_semana_epidemiologica)) {
    ano_atual <- as.numeric(substr(as.character(max_semana_epidemiologica), 1, 4))
    max_week_atual <- as.numeric(substr(as.character(max_semana_epidemiologica), 5, 6))
  } else {
    ano_atual <- max(anos_unicos)
    max_week_atual <- max(dados_bairros$semana[dados_bairros$ano == ano_atual], na.rm = TRUE)
  }

  complete_data_list <- lapply(anos_unicos, function(year) {
    max_week <- if (year == ano_atual) max_week_atual else 52

    tidyr::expand_grid(
      !!nm_bairr_var := bairros_unicos,
      ano = year,
      semana = seq_len(max_week),
      !!group_var := arbo_unicos
    ) %>%
      dplyr::mutate(
        !!se_var := as.numeric(paste0(.data$ano, sprintf("%02d", .data$semana)))
      )
  })

  complete_data <- dplyr::bind_rows(complete_data_list)

  resultado <- complete_data %>%
    dplyr::left_join(
      dados_bairros,
      by = c(nm_bairr_var, se_var, group_var, "ano", "semana")
    ) %>%
    dplyr::mutate(
      !!count_var := dplyr::coalesce(.data[[count_var]], 0)
    ) %>%
    dplyr::select(-"ano", -"semana") %>%
    dplyr::arrange(
      !!rlang::sym(group_var),
      !!rlang::sym(nm_bairr_var),
      !!rlang::sym(se_var)
    )

  resultado
}
