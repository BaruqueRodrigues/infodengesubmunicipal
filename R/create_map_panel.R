#' Criar painel de mapas semanais com legenda compartilhada
#'
#' Organiza uma lista de mapas (`ggplot`) em um painel visual contendo
#' três mapas e uma legenda compartilhada. A função extrai automaticamente
#' a legenda de um dos mapas e a posiciona abaixo do painel, removendo
#' as legendas individuais dos mapas exibidos.
#'
#' Esse formato é útil para apresentar a evolução espacial de indicadores
#' epidemiológicos (ex.: incidência) em diferentes semanas
#' epidemiológicas em um único painel.
#'
#' @param plots_list Lista de objetos `ggplot`, normalmente gerada pela
#' função `generate_maps()`. Espera-se que os três últimos elementos da
#' lista correspondam aos mapas que serão exibidos no painel.
#' @param legend_title String com o título da legenda de cores do mapa.
#' Default é `"Incidência (100.000 pessoas)"`.
#'
#' @return
#' Um objeto `ggplot` combinado contendo três mapas organizados em painel
#' e uma legenda compartilhada posicionada abaixo.
#'
#' @details
#' A função:
#'
#' * Extrai a legenda de um dos mapas usando `ggplotGrob()`
#' * Remove as legendas individuais dos mapas exibidos
#' * Organiza os mapas utilizando `cowplot::ggdraw()` e `cowplot::draw_plot()`
#' * Posiciona a legenda compartilhada abaixo do painel
#'
#' O layout final apresenta dois mapas na parte superior e um mapa
#' centralizado na parte inferior.
#'
#' @examples
#' \dontrun{
#' create_map_panel(
#'   generate_maps(
#'     data = df_distritos,
#'     value_col = "inc",
#'     arbo = "dengue",
#'     weeks_2_plot = c(202440, 202441, 202442)
#'   )
#' )
#' }
#'
#' @export
create_map_panel <- function(plots_list, legend_title = "Incidência (100.000 pessoas)") {
  # Extrair legenda
  legend_plot <- plots_list[[length(plots_list) - 2]] +
    theme(legend.position = "top") +
    labs(fill = legend_title)

  grob_legend <- ggplotGrob(legend_plot)
  legend_grob <- gtable_filter(grob_legend, "guide-box")

  # Criar painel
  ggdraw() +
    draw_plot(plots_list[[length(plots_list) - 2]] + theme(legend.position = "none"),
              x = 0.08, y = 0.1, width = 0.27) +
    draw_plot(plots_list[[length(plots_list) - 1]] + theme(legend.position = "none"),
              x = 0.65, y = 0.1, width = 0.27) +
    draw_plot(plots_list[[length(plots_list)]] + theme(legend.position = "none"),
              x = 0.4, y = -0.3, width = 0.27) +
    draw_grob(legend_grob, x = 0.3, y = 0.05,width = 0.4)
}
