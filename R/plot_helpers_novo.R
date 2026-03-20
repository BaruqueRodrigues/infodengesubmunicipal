#' Calcular inicio de janela de um ano epidemiologico
#'
#' Helper interno para derivar o limite inferior aproximado de uma janela
#' anual no formato `YYYYWW`.
#'
#' @param weeks_limit Semana epidemiologica final no formato numerico `YYYYWW`.
#'
#' @return Valor numerico `YYYYWW` correspondente ao mesmo numero de semana
#' do ano anterior.
#' @keywords internal
calcular_inicio_janela_1ano <- function(weeks_limit) {
  ano <- as.integer(substr(as.character(weeks_limit), 1, 4))
  semana <- substr(as.character(weeks_limit), 5, 6)

  as.numeric(paste0(ano - 1, semana))
}

#' Ajustar nowcasting para nao ficar abaixo do observado
#'
#' Helper interno que garante coerencia minima entre serie observada e
#' estimativas de nowcasting.
#'
#' @param nowcast_data Data frame com colunas `ano_epi`, `Median`, `LI`, `LS`.
#' @param observed_data Data frame com colunas `ano_epi` e `total_Y`.
#'
#' @return Data frame de nowcasting com colunas ajustadas.
#' @keywords internal
ajustar_nowcasting_legado <- function(nowcast_data, observed_data) {
  common_weeks <- intersect(nowcast_data$ano_epi, observed_data$ano_epi)

  nowcast_filtered <- nowcast_data %>%
    dplyr::filter(.data$ano_epi %in% common_weeks)

  observed_filtered <- observed_data %>%
    dplyr::filter(.data$ano_epi %in% common_weeks)

  for (i in seq_along(common_weeks)) {
    if (nowcast_filtered$Median[i] < observed_filtered$total_Y[i]) {
      nowcast_filtered$Median[i] <- observed_filtered$total_Y[i]
    }

    if (nowcast_filtered$LI[i] < observed_filtered$total_Y[i]) {
      nowcast_filtered$LI[i] <- observed_filtered$total_Y[i] - 0.5
    }

    if (nowcast_filtered$LS[i] < observed_filtered$total_Y[i]) {
      nowcast_filtered$LS[i] <- observed_filtered$total_Y[i] + 0.5
    }
  }

  nowcast_data %>%
    dplyr::left_join(
      nowcast_filtered %>% dplyr::select("ano_epi", "Median", "LI", "LS"),
      by = "ano_epi",
      suffix = c(".x", ".y")
    ) %>%
    dplyr::mutate(
      Median = dplyr::coalesce(.data$Median.y, .data$Median.x),
      LI = dplyr::coalesce(.data$LI.y, .data$LI.x),
      LS = dplyr::coalesce(.data$LS.y, .data$LS.x)
    ) %>%
    dplyr::select(-dplyr::all_of(c("Median.x", "Median.y", "LI.x", "LI.y", "LS.x", "LS.y")))
}
