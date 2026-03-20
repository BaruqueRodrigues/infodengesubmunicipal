legacy_assign_params <- function(params) {
  old_params <- get0("params", envir = .GlobalEnv)

  assign("params", params, envir = .GlobalEnv)

  withr::defer(
    {
      if (is.null(old_params)) {
        if (exists("params", envir = .GlobalEnv, inherits = FALSE)) {
          rm("params", envir = .GlobalEnv)
        }
      } else {
        assign("params", old_params, envir = .GlobalEnv)
      }
    },
    envir = parent.frame()
  )
}

legacy_assign_globals <- function(values) {
  old_values <- lapply(names(values), function(name) {
    list(
      exists = exists(name, envir = .GlobalEnv, inherits = FALSE),
      value = get0(name, envir = .GlobalEnv, inherits = FALSE)
    )
  })
  names(old_values) <- names(values)

  for (name in names(values)) {
    assign(name, values[[name]], envir = .GlobalEnv)
  }

  withr::defer(
    {
      for (name in names(old_values)) {
        if (old_values[[name]]$exists) {
          assign(name, old_values[[name]]$value, envir = .GlobalEnv)
        } else if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
          rm(list = name, envir = .GlobalEnv)
        }
      }
    },
    envir = parent.frame()
  )
}

legacy_make_square <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(rbind(
    c(xmin, ymin),
    c(xmax, ymin),
    c(xmax, ymax),
    c(xmin, ymax),
    c(xmin, ymin)
  )))
}

legacy_make_spatial_fixtures <- function() {
  distritos <- sf::st_sf(
    distrito = c("NORTE", "SUL"),
    geometry = sf::st_sfc(
      legacy_make_square(0, 0, 1, 1),
      legacy_make_square(2, 0, 3, 1),
      crs = 4326
    )
  )

  bairros <- sf::st_sf(
    nome_bairr = c("Bairro A", "Bairro B", "Bairro C"),
    id_bairro = c(1, 2, 3),
    geometry = sf::st_sfc(
      legacy_make_square(0.1, 0.1, 0.4, 0.4),
      legacy_make_square(2.2, 0.2, 2.6, 0.6),
      legacy_make_square(3.4, 0.2, 3.8, 0.6),
      crs = 4326
    )
  )

  list(
    sp_distritos = distritos,
    sp_bairros = bairros
  )
}
