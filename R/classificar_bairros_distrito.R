#' Classificar bairros em distritos por interseção espacial
#'
#' Associa cada bairro a um distrito utilizando operações espaciais do
#' pacote `sf`. A função calcula o centróide dos polígonos de bairros e
#' identifica o distrito correspondente por interseção espacial. Caso
#' algum bairro não intercepte diretamente um distrito (por imperfeições
#' geométricas ou bordas), o distrito mais próximo é atribuído usando
#' `sf::st_nearest_feature`.
#'
#' Antes da operação espacial, a função garante que ambos os objetos
#' estejam no mesmo sistema de referência de coordenadas (CRS).
#'
#' @param sp_distritos Objeto `sf` contendo os polígonos dos distritos.
#' Deve possuir ao menos a coluna `distrito` e geometria poligonal.
#' @param sp_bairros Objeto `sf` contendo os polígonos dos bairros.
#' Deve possuir as colunas `nome_bairr`, `id_bairro` e geometria.
#'
#' @return
#' Um objeto `sf` contendo os bairros com a coluna adicional:
#'
#' * `distrito` — distrito associado ao bairro
#'
#' A geometria retornada corresponde à geometria original dos bairros
#' (renomeada para `geometry_distritos`).
#'
#' @details
#' O procedimento segue três etapas principais:
#'
#' 1. Garantir que os objetos espaciais estejam no mesmo CRS.
#' 2. Calcular os centróides dos bairros e realizar a interseção com
#'    os polígonos de distritos.
#' 3. Para bairros sem interseção direta, atribuir o distrito mais
#'    próximo usando `sf::st_nearest_feature`.
#'
#' Esse procedimento reduz problemas comuns de topologia em bases
#' espaciais administrativas.
#'
#' @examples
#' \dontrun{
#' res_distritos_nomes <- classificar_bairros_distrito(
#'   sp_distritos = jv_distritos_shp,
#'   sp_bairros = jv_shp
#' )
#'
#' res_distritos_nomes <- res_distritos_nomes |>
#'   dplyr::mutate(
#'     nome_bairr = normalizar_bairro(nome_bairr)
#'   )
#' }
#'
#' @export
classificar_bairros_distrito <- function(sp_distritos, sp_bairros) {

  if(sf::st_crs(sp_distritos) != sf::st_crs(sp_bairros)) {
    sp_bairros <- sf::st_transform(sp_bairros, sf::st_crs(sp_distritos))
  }

  centroides <- sf::st_centroid(sp_bairros)

  intersecao <- sf::st_intersection(centroides, sp_distritos) |>
    sf::st_drop_geometry() |>
    dplyr::select(nome_bairr, id_bairro, distrito)

  resultado <- sp_bairros |>
    dplyr::select(nome_bairr, id_bairro, geometry) |>
    dplyr::left_join(intersecao, by = c("nome_bairr", "id_bairro"))

  sem_distrito <- which(is.na(resultado$distrito))

  if(length(sem_distrito) > 0) {
    indices_proximos <- sf::st_nearest_feature(
      sf::st_centroid(resultado[sem_distrito, ]),
      sp_distritos
    )

    resultado$distrito[sem_distrito] <- sp_distritos$distrito[indices_proximos]
  }

  return(
    resultado |>
      dplyr::rename(geometry_distritos = geometry)
  )
}
