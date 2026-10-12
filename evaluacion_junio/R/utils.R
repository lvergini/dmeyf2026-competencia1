# =============================================================================
# utils.R
# Funciones generales de soporte para el proyecto (descarga de datos, etc.).
# Se cargan desde cada .Rmd con source(here::here("R", "utils.R")).
# =============================================================================


# -----------------------------------------------------------------------------
# descargar_dataset()
# Descarga un archivo de una URL solo si todavía
# no existe en la carpeta de destino. Devuelve la ruta local del archivo.
#
#   archivo          nombre del archivo a descargar
#   url_base         URL de la carpeta de origen (terminada en "/")
#   carpeta_destino  carpeta local donde se guarda el archivo
# -----------------------------------------------------------------------------
descargar_dataset <- function(archivo, url_base, carpeta_destino) {

  dir.create(carpeta_destino, showWarnings = FALSE, recursive = TRUE)
  destino <- file.path(carpeta_destino, archivo)

  if (!file.exists(destino)) {
    # el timeout por defecto de R (60 s) no alcanza para archivos grandes;
    # se amplía solo durante la descarga y se restaura al salir
    opciones_previas <- options(timeout = 3600)
    on.exit(options(opciones_previas))

    download.file(
      url      = paste0(url_base, archivo),
      destfile = destino,
      mode     = "wb"
    )
  }

  destino
}


# -----------------------------------------------------------------------------
# particionar()
# Agrega a un data.table una columna (por defecto "fold") con una partición
# estratificada según la columna indicada en agrupa. Modifica el data.table
# por referencia.
#
#   data      data.table a particionar
#   division  vector con los pesos de cada parte, por ejemplo c(70, 30)
#   agrupa    columna de estratificación
#   campo     nombre de la columna que se agrega
#   start     número asignado a la primera parte
#   seed      semilla (NA para no fijarla)
#
# Ejemplo: particionar(dataset, division = c(70, 30),
#                      agrupa = "clase_ternaria", seed = semilla)
# -----------------------------------------------------------------------------
particionar <- function(data, division, agrupa = "", campo = "fold",
                        start = 1, seed = NA) {
  if (!is.na(seed)) set.seed(seed, "L'Ecuyer-CMRG")

  bloque <- unlist(mapply(
    function(x, y) { rep(y, x) },
    division,
    seq(from = start, length.out = length(division))
  ))

  data[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
       by = agrupa]
}


# -----------------------------------------------------------------------------
# loguear()
# Agrega una línea a un archivo de log delimitado por tabulaciones, con fecha
# y hora, a partir de una lista con nombre. Si el archivo no existe, escribe
# primero la línea de títulos. Requiere el paquete rlist.
#
#   preg     lista con los valores a registrar
#   arch     ruta del archivo de log
#   verbose  si es TRUE, imprime la línea por pantalla
# -----------------------------------------------------------------------------
loguear <- function(preg, arch = NA, verbose = TRUE) {
  t0 <- Sys.time()
  reg <- copy(preg)
  if ("feature_contri" %in% names(reg)) {
    reg$feature_contri <- reg$feature_contri[1]
  }
  archivo <- arch
  if (is.na(arch)) archivo <- paste0(folder, substitute(reg), ext)

  if (!file.exists(archivo)) {
    # línea de títulos
    linea <- paste0("fecha\t",
                    paste(list.names(reg), collapse = "\t"), "\n")
    cat(linea, file = archivo)
  }

  # registro
  linea <- paste0(format(t0, "%Y%m%d.%H%M%S"), "\t",
                  gsub(", ", "\t", toString(reg)), "\n")

  cat(linea, file = archivo, append = TRUE)

  if (verbose) cat(linea)
}


# -----------------------------------------------------------------------------
# particionar_por_cliente()
# Agrega una columna de partición asignada por cliente: todos los registros
# de un mismo cliente quedan en la misma parte. Evita que registros casi
# idénticos del mismo cliente en meses distintos queden repartidos entre
# entrenamiento y validación, lo que haría optimista la validación.
# La estratificación se hace a nivel cliente, según si el cliente tiene
# algún registro positivo (clase01 = 1). Modifica el data.table por
# referencia; requiere las columnas numero_de_cliente y clase01.
#
#   data      data.table a particionar
#   division  pesos de cada parte, por ejemplo c(70, 30)
#   seed      semilla
#   campo     nombre de la columna que se agrega
# -----------------------------------------------------------------------------
particionar_por_cliente <- function(data, division, seed, campo = "fold") {

  clientes <- data[, list(positivo = max(clase01)), by = numero_de_cliente]

  set.seed(seed, "L'Ecuyer-CMRG")
  bloque <- rep(seq_along(division), division)
  clientes[, (campo) := sample(rep(bloque, ceiling(.N / length(bloque))))[1:.N],
           by = positivo]

  data[clientes, (campo) := get(paste0("i.", campo)), on = "numero_de_cliente"]
}


# -----------------------------------------------------------------------------
# filas_undersampling()
# Devuelve los índices de fila que se conservan al aplicar undersampling
# sobre la clase negativa: se conservan todos los positivos (clase01 = 1) y
# una fracción aleatoria de los negativos.
#
#   clase01        vector de clase binaria
#   undersampling  fracción de negativos a conservar (1 = todos)
#   seed           semilla
# -----------------------------------------------------------------------------
filas_undersampling <- function(clase01, undersampling, seed) {
  set.seed(seed, "L'Ecuyer-CMRG")
  azar <- runif(length(clase01))
  which(clase01 == 1L | azar <= undersampling)
}
