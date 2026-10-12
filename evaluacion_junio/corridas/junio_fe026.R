# =============================================================================
# junio_fe026.R
# Testeo en junio sobre FE026, que no tiene spec del pipeline (se construye
# con construir_FE026()). Se graba una vez en datasets/FE026.csv.gz y el
# experimento se corre con el mismo Rmd (rmd/03_experimento.Rmd) y los mismos
# parámetros que correr_experimento(), indicando el archivo en lugar del spec.
# Esquema de junio_comun.R: 202103+202104, sin BAJA+1 de 202105, 202106 como
# mes de prueba, sin préstamos, 20 semillas, sin undersampling, 8 hilos.
# =============================================================================

source(here::here("corridas", "junio_comun.R"))
source(here::here("R", "fe_FE026.R"))

asegurar_FE026 <- function() {
  archivo <- here::here("datasets", "FE026.csv.gz")
  if (!file.exists(archivo)) {
    message("Grabando ", archivo)
    temporal <- paste0(archivo, ".parcial")
    data.table::fwrite(construir_FE026(), temporal)
    file.rename(temporal, archivo)
  } else {
    message("Dataset FE026 existente, se reutiliza")
  }
  basename(archivo)
}

correr_experimento_FE026 <- function(experimento, descripcion, hiperparametros) {
  if (experimento_registrado(experimento)) {
    message(experimento, " ya registrado; se omite")
    return(invisible(FALSE))
  }
  config <- config_junio(experimento, descripcion, fe = NULL,
                         campos_excluidos = character(0),
                         hiperparametros = hiperparametros)
  rmarkdown::render(
    input = here::here("rmd", "03_experimento.Rmd"),
    params = list(
      experimento = config$experimento,
      descripcion = config$descripcion,
      dataset = asegurar_FE026(),
      meses_train = config$meses_train,
      meses_baja1 = config$meses_baja1,
      mes_future = config$mes_future,
      campos_excluidos = config$campos_excluidos,
      campos_incluidos = character(0),
      hiperparametros = config$hiperparametros,
      semillas = config$semillas,
      undersampling = config$undersampling,
      clase = "binaria2",
      rf_arboles = 0, rf_hojas = 16, rf_min_data = 500,
      intrusos = 0, intrusos_metodo = "fraccion", intrusos_umbral = 0.5,
      etiquetas = "",
      num_threads = config$num_threads
    ),
    output_file = paste0(experimento, ".md"),
    output_dir = here::here("exp", experimento),
    envir = new.env()
  )
  invisible(TRUE)
}
