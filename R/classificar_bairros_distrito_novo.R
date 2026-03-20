#' Garantir que dois objetos sf estejam no mesmo CRS
#'
#' Verifica se dois objetos espaciais possuem o mesmo sistema de
#' referência de coordenadas (CRS). Caso sejam diferentes, transforma
#' o segundo objeto para o CRS do primeiro.
#'
#' @param sp_ref Objeto `sf` de referência.
#' @param sp_target Objeto `sf` que será transformado se necessário.
#'
#' @return Objeto `sf` com CRS compatível com `sp_ref`.
#'
#' @examples
#' \dontrun{
#' sp_bairros <- garantir_crs(sp_distritos, sp_bairros)
#' }
#'
#' @export
garantir_crs <- function(sp_ref, sp_target) {

  if (sf::st_crs(sp_ref) != sf::st_crs(sp_target)) {
    sp_target <- sf::st_transform(sp_target, sf::st_crs(sp_ref))
  }

  sp_target
}


#' Calcular centróides de polígonos
#'
#' Calcula os centróides de um objeto espacial `sf`.
#'
#' @param sp_obj Objeto `sf` contendo geometria poligonal.
#'
#' @return Objeto `sf` contendo os centróides.
#'
#' @examples
#' \dontrun{
#' centroides <- calcular_centroides(sp_bairros)
#' }
#'
#' @export
calcular_centroides <- function(sp_obj) {

  sf::st_centroid(sp_obj)

}


#' Intersectar bairros e distritos via centróides
#'
#' Identifica o distrito correspondente a cada bairro utilizando
#' a interseção espacial entre os centróides dos bairros e os
#' polígonos de distritos.
#'
#' @param centroides_bairros Objeto `sf` contendo centróides dos bairros.
#' @param sp_distritos Objeto `sf` contendo polígonos de distritos.
#'
#' @return Data frame com colunas `nome_bairr`, `id_bairro` e `distrito`.
#'
#' @examples
#' \dontrun{
#' intersecao <- intersectar_distritos(centroides, sp_distritos)
#' }
#'
#' @export
intersectar_distritos <- function(centroides_bairros, sp_distritos) {

  sf::st_intersection(centroides_bairros, sp_distritos) |>
    sf::st_drop_geometry() |>
    dplyr::select(nome_bairr, id_bairro, distrito)

}


#' Atribuir distrito mais próximo para bairros sem interseção
#'
#' Para bairros que não foram associados a um distrito por interseção
#' espacial, atribui o distrito mais próximo utilizando
#' `sf::st_nearest_feature()`.
#'
#' @param sp_bairros Objeto `sf` contendo bairros.
#' @param sp_distritos Objeto `sf` contendo distritos.
#'
#' @return Objeto `sf` com coluna `distrito` preenchida.
#'
#' @examples
#' \dontrun{
#' bairros <- atribuir_distrito_proximo(bairros, sp_distritos)
#' }
#'
#' @export
atribuir_distrito_proximo <- function(sp_bairros, sp_distritos) {

  centroides <- sf::st_centroid(sp_bairros)

  indices_proximos <- sf::st_nearest_feature(
    centroides,
    sp_distritos
  )

  sp_bairros |>
    dplyr::mutate(
      distrito = dplyr::if_else(
        is.na(.data$distrito),
        sp_distritos$distrito[indices_proximos],
        .data$distrito
      )
    )

}


#' Classificar bairros em distritos por interseção espacial
#'
#' Associa cada bairro a um distrito utilizando operações espaciais
#' do pacote `sf`. O processo utiliza centróides dos bairros para
#' identificar interseções com distritos e, quando necessário,
#' atribui o distrito mais próximo.
#'
#' @param sp_distritos Objeto `sf` contendo polígonos de distritos.
#' Deve possuir ao menos a coluna `distrito`.
#' @param sp_bairros Objeto `sf` contendo polígonos de bairros.
#' Deve possuir as colunas `nome_bairr` e `id_bairro`.
#'
#' @return Objeto `sf` contendo os bairros com a coluna `distrito`
#' associada. A geometria retornada corresponde à geometria original
#' dos bairros (renomeada para `geometry_distritos`).
#'
#' @examples
#' \dontrun{
#' res_distritos <- classificar_bairros_distrito_novo(
#'   sp_distritos = jv_distritos_shp,
#'   sp_bairros = jv_shp
#' )
#' }
#'
#' @export
classificar_bairros_distrito_novo <- function(sp_distritos, sp_bairros) {

  sp_bairros <- garantir_crs(sp_distritos, sp_bairros)

  centroides <- calcular_centroides(sp_bairros)

  intersecao <- intersectar_distritos(
    centroides_bairros = centroides,
    sp_distritos = sp_distritos
  )

  resultado <- sp_bairros |>
    dplyr::select(nome_bairr, id_bairro, geometry) |>
    dplyr::left_join(intersecao, by = c("nome_bairr", "id_bairro"))

  resultado <- atribuir_distrito_proximo(
    sp_bairros = resultado,
    sp_distritos = sp_distritos
  )

  resultado |>
    dplyr::rename(
      geometry_distritos = geometry
    )

}
