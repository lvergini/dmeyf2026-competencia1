# =============================================================================
# produccion.R
# Entrenamiento final: una configuración de una optimización bayesiana,
# entrenada sobre un dataset grabado en datasets/ con los cuatro meses con
# clase (202103-202106), sin undersampling y con todas las variables
# (préstamos personales incluidos), una semilla por vez; predice 202108.
#   - Cada semilla graba exp/<id>/semillas/prob_<semilla>.csv.gz apenas
#     termina (primero como .tmp); si se relanza, saltea las ya grabadas.
#   - Semillas: primos de 6 cifras en orden aleatorio (set.seed 173207),
#     repartidos en 4 sublistas disjuntas. El argumento 'maquina' es el
#     número de sublista (1 a 4; posiciones maquina, maquina + 4, ...), no la
#     máquina virtual que la corre.
#     En la entrega se entrenan solo las semillas de la lista
#     (archivo_semillas), que tienen que ser el tramo inicial de la sublista,
#     en el mismo orden; 'proceso' indica qué tramo de esa lista corrió en un
#     mismo proceso de R.
#   - Importancia de variables: solo en la primera semilla de cada proceso
#     (exp/<id>/importancia_<semilla>.csv), porque lgb.importance() de un
#     modelo grande tarda muchos minutos en un solo hilo.
#   - El avance y los tiempos se escriben en el log (stderr).
# =============================================================================

suppressPackageStartupMessages({ library(data.table); library(lightgbm) })
source(file.path("R", "parametros.R"))

producir <- function(id, archivo_dataset, log_bo, configuracion, maquina, archivo_semillas, proceso = 1L) {
  if (is.na(maquina) || !maquina %in% 1:4) stop("maquina (número de sublista) tiene que ser 1, 2, 3 o 4")
  avance <- function(...) cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [", id, "-", maquina, "] ",
                              ..., "\n", sep = "", file = stderr())

  carpeta_id <- file.path("exp", id)
  carpeta <- file.path(carpeta_id, "semillas")
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)

  # Configuración
  if (!file.exists(file.path(dirname(log_bo), "COMPLETA"))) stop(log_bo, ": la bayesiana no terminó")
  tb <- fread(log_bo)
  setorder(tb, -metrica)
  conf <- as.list(tb[configuracion])
  conf[c("fecha", "iter", "metrica")] <- NULL
  param <- modifyList(param_fijos_tutoria, conf)
  param$num_threads <- 8L
  avance(sprintf("Configuración %d de %s: %d árboles, %d hojas, hessian %.4g, bynode %s",
                 configuracion, log_bo, param$num_iterations, param$num_leaves,
                 param$min_sum_hessian_in_leaf, format(param$feature_fraction_bynode)))

  # Semillas de esta sublista
  es_primo <- rep(TRUE, 999999); es_primo[1] <- FALSE
  for (i in 2:999) if (es_primo[i]) es_primo[seq(i * i, 999999, by = i)] <- FALSE
  primos <- which(es_primo)
  primos <- primos[primos >= 100000]
  set.seed(semilla_primigenia)
  primos <- sample(primos)
  semillas <- primos[seq(maquina, length(primos), by = 4)]

  # Semillas de la lista de la entrega: las de esta sublista forman el tramo
  # inicial de la sublista, en el mismo orden; se toman las de este proceso
  lista <- fread(archivo_semillas)
  lista <- lista[sublista == maquina]
  setorder(lista, posicion)
  if (nrow(lista) == 0) stop(archivo_semillas, ": no hay semillas de la sublista ", maquina)
  stopifnot(identical(as.integer(lista$semilla), as.integer(semillas[seq_len(nrow(lista))])))
  este_proceso <- as.integer(proceso)
  semillas <- lista[proceso == este_proceso, semilla]
  if (length(semillas) == 0) stop(archivo_semillas, ": no hay semillas del proceso ", este_proceso, " en la sublista ", maquina)
  avance("Semillas de la lista: ", length(semillas), " (proceso ", este_proceso, ", de la ", lista[proceso == este_proceso, min(posicion)],
         " a la ", lista[proceso == este_proceso, max(posicion)], " de la sublista)")

  # Datos: todos los meses con clase, todas las variables
  ruta <- file.path("datasets", archivo_dataset)
  if (!file.exists(ruta)) stop("No existe ", ruta)
  avance("Leyendo ", ruta)
  dataset <- fread(ruta)
  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
  campos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))
  tr <- dataset[foto_mes %in% c(202103L, 202104L, 202105L, 202106L)]
  stopifnot(tr[, !anyNA(clase_ternaria)])
  futuro <- dataset[foto_mes == 202108L]
  stopifnot(nrow(futuro) > 0)
  dtrain <- lgb.Dataset(data = data.matrix(tr[, campos, with = FALSE]), label = tr$clase01, free_raw_data = FALSE)
  x_futuro <- data.matrix(futuro[, campos, with = FALSE])
  ids <- futuro$numero_de_cliente
  avance("Entrenamiento: ", nrow(tr), " registros (202103-202106, positivos ", tr[, sum(clase01)],
         "); futuro: ", length(ids), "; variables: ", length(campos))
  rm(dataset, tr, futuro); gc(verbose = FALSE)

  primera <- TRUE
  for (s in semillas) {
    archivo <- file.path(carpeta, sprintf("prob_%d.csv.gz", s))
    if (file.exists(archivo)) next
    t0 <- Sys.time()
    modelo <- lgb.train(data = dtrain, param = modifyList(param, list(seed = s)))
    prob <- predict(modelo, x_futuro)
    tmp <- sub("\\.csv\\.gz$", ".tmp.csv.gz", archivo)
    fwrite(data.table(numero_de_cliente = ids, prob = prob), tmp)
    file.rename(tmp, archivo)
    t1 <- Sys.time()
    if (primera) {
      imp <- lgb.importance(modelo)
      imp[, semilla := s]
      archivo_imp <- file.path(carpeta_id, sprintf("importancia_%d.csv", s))
      fwrite(imp, archivo_imp)
      primera <- FALSE
    }
    rm(modelo); gc(verbose = FALSE)
    avance(sprintf("semilla %d | entrenamiento %.1f min | total %.1f min | semillas en la carpeta: %d", s,
                   as.numeric(difftime(t1, t0, units = "mins")),
                   as.numeric(difftime(Sys.time(), t0, units = "mins")),
                   length(list.files(carpeta, pattern = "^prob_[0-9]+\\.csv\\.gz$"))))
  }
}
