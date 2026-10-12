# =============================================================================
# 00_pipeline.R
# uso, desde la carpeta de la entrega:
#   Rscript 00_pipeline.R [--bayesiana] [--paralelo]
# Ejecuta la entrega completa, en orden, llamando a los scripts de cada paso
# (cada uno en un proceso de R aparte):
#   1. 01_datasets.R: construye y graba datasets/FE026.csv.gz.
#   2. Bayesiana: por defecto copia el log de la optimización que se usó
#      (modelo_A_FE026_B018/BO_log_corrido.txt) a exp/B018/BO_log.txt y crea
#      exp/B018/COMPLETA; con --bayesiana corre la optimización completa
#      (modelo_A_FE026_B018/01_bayesiana.R).
#   3. Producción: para cada par (sublista, proceso) de
#      modelo_A_FE026_B018/semillas_A.csv corre
#      modelo_A_FE026_B018/02_produccion.R <sublista> <proceso>; los procesos
#      de una misma sublista van en orden. Por defecto las sublistas corren
#      una después de otra; con --paralelo, todas a la vez, cada una en su
#      propio proceso (cada producción usa 8 hilos y carga el dataset completo
#      en memoria, así que el paralelo necesita una máquina con lugar para
#      todas).
#   4. 02_prediccion_final.R: semillerío, corte y archivo final.
# Si un paso falla, se detiene con un mensaje que indica cuál y dónde está su
# registro. La salida de cada paso queda en exp/registros/.
# Las opciones también se pueden fijar abajo, en CORRER_BAYESIANA y
# PRODUCCION_EN_PARALELO.
# =============================================================================

CORRER_BAYESIANA <- FALSE
PRODUCCION_EN_PARALELO <- FALSE

opciones <- commandArgs(trailingOnly = TRUE)
desconocidas <- setdiff(opciones, c("--bayesiana", "--paralelo"))
if (length(desconocidas) > 0) stop("Opciones desconocidas: ", paste(desconocidas, collapse = " "),
                                    "\nUso: Rscript 00_pipeline.R [--bayesiana] [--paralelo]")
if ("--bayesiana" %in% opciones) CORRER_BAYESIANA <- TRUE
if ("--paralelo" %in% opciones) PRODUCCION_EN_PARALELO <- TRUE

rscript <- file.path(R.home("bin"), "Rscript")
registros <- file.path("exp", "registros")
dir.create(registros, showWarnings = FALSE, recursive = TRUE)
avance <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", ...)

# Corre un script en un proceso de R aparte, con su salida en un registro;
# devuelve el código de salida del proceso
correr_script <- function(script, argumentos = character(0), registro) {
  system2(rscript, c(script, argumentos), stdout = registro, stderr = registro)
}

# Corre un paso y se detiene si falla
paso <- function(nombre, script, argumentos = character(0)) {
  registro <- file.path(registros, paste0(gsub("[^A-Za-z0-9]+", "_", nombre), ".log"))
  avance(nombre, ": ", paste(c(script, argumentos), collapse = " "))
  t0 <- Sys.time()
  estado <- correr_script(script, argumentos, registro)
  if (!identical(as.integer(estado), 0L))
    stop("Falló el paso '", nombre, "' (", script, " terminó con código ", estado,
         "). Se detiene la entrega; ver ", registro, call. = FALSE)
  avance(nombre, ": terminado en ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
}

# --- 1. Dataset ---------------------------------------------------------------
paso("1 dataset", "01_datasets.R")
if (!file.exists(file.path("datasets", "FE026.csv.gz")))
  stop("El paso '1 dataset' no dejó datasets/FE026.csv.gz. Se detiene la entrega.", call. = FALSE)

# --- 2. Bayesiana -------------------------------------------------------------
carpeta_bo <- file.path("exp", "B018")
log_bo <- file.path(carpeta_bo, "BO_log.txt")
log_corrido <- file.path("modelo_A_FE026_B018", "BO_log_corrido.txt")
if (CORRER_BAYESIANA) {
  paso("2 bayesiana", file.path("modelo_A_FE026_B018", "01_bayesiana.R"))
} else {
  avance("2 bayesiana: se usa el log de la optimización de la entrega (", log_corrido, ")")
  dir.create(carpeta_bo, showWarnings = FALSE, recursive = TRUE)
  if (file.exists(log_bo) && !identical(readLines(log_bo), readLines(log_corrido)))
    stop("Ya existe ", log_bo, " y no es igual a ", log_corrido, " (¿una optimización corrida antes?). ",
         "Para usarlo, correr con --bayesiana; para usar el de la entrega, borrar ", carpeta_bo, ".", call. = FALSE)
  if (!file.exists(log_bo)) file.copy(log_corrido, log_bo)
  writeLines("log de la optimización de la entrega, copiado por 00_pipeline.R", file.path(carpeta_bo, "COMPLETA"))
}
if (!file.exists(log_bo) || !file.exists(file.path(carpeta_bo, "COMPLETA")))
  stop("Falta ", log_bo, " o ", file.path(carpeta_bo, "COMPLETA"), " después del paso '2 bayesiana'. ",
       "Se detiene la entrega.", call. = FALSE)

# --- 3. Producción ------------------------------------------------------------
lista <- read.csv(file.path("modelo_A_FE026_B018", "semillas_A.csv"))
if (nrow(lista) == 0) stop("modelo_A_FE026_B018/semillas_A.csv no tiene semillas. Se detiene la entrega.", call. = FALSE)
procesos_por_sublista <- lapply(split(lista$proceso, lista$sublista), function(p) sort(unique(p)))
avance("3 producción: ", nrow(lista), " semillas en ", length(procesos_por_sublista), " sublistas (",
       if (PRODUCCION_EN_PARALELO) "en paralelo" else "en secuencia", ")")

# Corre, en orden, los procesos de una sublista; devuelve "" si todos
# terminaron bien, o la descripción del que falló
correr_sublista <- function(sublista, procesos) {
  for (p in procesos) {
    registro <- file.path(registros, sprintf("3_produccion_sublista%d_proceso%d.log", sublista, p))
    estado <- correr_script(file.path("modelo_A_FE026_B018", "02_produccion.R"), c(sublista, p), registro)
    if (!identical(as.integer(estado), 0L))
      return(sprintf("sublista %d, proceso %d (código %s; ver %s)", sublista, p, estado, registro))
  }
  ""
}

t0 <- Sys.time()
sublistas <- as.integer(names(procesos_por_sublista))
if (PRODUCCION_EN_PARALELO && length(sublistas) > 1) {
  cl <- parallel::makePSOCKcluster(length(sublistas))
  parallel::clusterCall(cl, setwd, getwd())
  parallel::clusterExport(cl, c("correr_script", "rscript", "registros"))
  fallas <- tryCatch(
    unlist(parallel::clusterMap(cl, correr_sublista, sublistas, procesos_por_sublista)),
    finally = parallel::stopCluster(cl))
} else {
  fallas <- character(0)
  for (k in seq_along(sublistas)) {
    avance("3 producción: sublista ", sublistas[k], ", procesos ", paste(procesos_por_sublista[[k]], collapse = ", "))
    fallas <- c(fallas, correr_sublista(sublistas[k], procesos_por_sublista[[k]]))
    if (tail(fallas, 1) != "") break
  }
}
fallas <- fallas[fallas != ""]
if (length(fallas) > 0)
  stop("Falló el paso '3 producción' en: ", paste(fallas, collapse = "; "), ". Se detiene la entrega.", call. = FALSE)
faltan <- lista$semilla[!file.exists(file.path("exp", "P069", "semillas", sprintf("prob_%d.csv.gz", lista$semilla)))]
if (length(faltan) > 0)
  stop("El paso '3 producción' no dejó las predicciones de ", length(faltan), " semillas (por ejemplo ",
       faltan[1], "). Se detiene la entrega.", call. = FALSE)
avance("3 producción: terminado en ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")

# --- 4. Predicción final ------------------------------------------------------
paso("4 prediccion final", "02_prediccion_final.R")
avance("Entrega completa. Archivo final en exp/entregas/ (ver ", file.path(registros, "4_prediccion_final.log"), ")")
