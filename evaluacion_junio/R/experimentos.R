# =============================================================================
# experimentos.R
# Registro de resultados de experimentos. Cada experimento guarda su
# resultado en exp/<experimento>/resultado.csv; el registro general
# experimentos.csv se obtiene consolidando esos archivos. Así, experimentos
# corridos en máquinas distintas nunca escriben el mismo archivo.
# =============================================================================


# -----------------------------------------------------------------------------
# guardar_resultado()
# Graba el resultado de un experimento en su carpeta.
#
#   carpeta_exp  carpeta del experimento
#   registro     lista con nombre; debe incluir el campo "experimento"
# -----------------------------------------------------------------------------
guardar_resultado <- function(carpeta_exp, registro) {
  fwrite(as.data.table(registro), file.path(carpeta_exp, "resultado.csv"))
}


# -----------------------------------------------------------------------------
# experimento_registrado()
# Indica si un experimento ya tiene su resultado grabado.
#
#   id  identificador del experimento
# -----------------------------------------------------------------------------
experimento_registrado <- function(id) {
  file.exists(here("exp", id, "resultado.csv"))
}


# -----------------------------------------------------------------------------
# consolidar_experimentos()
# Reúne los resultados de todas las carpetas de exp/ en un único archivo,
# ordenado por identificador de experimento, y lo devuelve.
#
#   archivo  ruta del registro consolidado
# -----------------------------------------------------------------------------
consolidar_experimentos <- function(archivo = here("experimentos.csv")) {

  archivos <- list.files(here("exp"), pattern = "^resultado\\.csv$",
                         recursive = TRUE, full.names = TRUE)

  if (length(archivos) == 0) {
    message("No hay resultados en exp/")
    return(invisible(NULL))
  }

  tb <- rbindlist(lapply(archivos, fread, colClasses = "character"),
                  use.names = TRUE, fill = TRUE)
  setorder(tb, experimento)

  fwrite(tb, archivo)
  tb
}
