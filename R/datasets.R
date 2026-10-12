# =============================================================================
# datasets.R
# Construcción del dataset FE026 a partir de competencia_01.csv.gz, y su
# grabado en datasets/.
#   construir_FE026()  NA en ccajas_depositos de 202105, lags y deltas de
#                      orden 1 y 2, ranking con cero fijo
#   asegurar_FE026()   graba datasets/FE026.csv.gz si no existe
# Las rutas son relativas a la carpeta de la entrega, que tiene que ser el
# directorio de trabajo.
# =============================================================================

# =============================================================================
# Dataset FE026
# Orden:
#   1. ccajas_depositos a NA en 202105 (el campo está roto ese mes), antes de
#      los lags para que no hereden el valor roto.
#   2. Lag 1, delta lag 1, lag 2 y delta lag 2 de cada variable, por mes
#      calendario: el lag k de un cliente es su valor en el mes foto_mes - k;
#      si el cliente no está en ese mes, el lag queda NA.
#   3. Ranking con cero fijo, por variable y por mes, de todas las variables
#      (originales, lags y deltas) salvo numero_de_cliente, foto_mes y
#      clase_ternaria: positivos -> rank / n+ en (0, 1]; negativos ->
#      -rank(|x|) / n- en [-1, 0); el 0 queda en 0; empates con rango
#      promedio; NA queda NA. El valor rankeado reemplaza al original.
# No se eliminan registros, meses ni variables (préstamos personales incluidos).
# =============================================================================

library(data.table)

construir_FE026 <- function(archivo = file.path("datasets", "competencia_01.csv.gz")) {

  avance <- function(...) message(format(Sys.time(), "%H:%M:%S"), " [FE026] ", ...)
  if (!file.exists(archivo)) stop("No existe ", archivo, ": copiar competencia_01.csv.gz en datasets/")

  avance("Leyendo ", archivo)
  d <- fread(archivo)
  fijas <- c("numero_de_cliente", "foto_mes", "clase_ternaria")
  stopifnot(all(fijas %in% names(d)))
  stopifnot(!anyDuplicated(d, by = c("numero_de_cliente", "foto_mes")))
  d[clase_ternaria == "", clase_ternaria := NA_character_]

  campos <- setdiff(names(d), fijas)
  no_numericas <- campos[!vapply(d[, ..campos], is.numeric, logical(1))]
  if (length(no_numericas) > 0) stop("Variables no numéricas: ", paste(no_numericas, collapse = ", "))
  avance(nrow(d), " registros, ", length(campos), " variables, meses ",
         paste(sort(unique(d$foto_mes)), collapse = ", "))

  # 1. ccajas_depositos roto en 202105
  d[, ccajas_depositos := as.numeric(ccajas_depositos)]
  d[foto_mes == 202105L, ccajas_depositos := NA_real_]

  # 2. Lags y delta lags por mes calendario
  d[, periodo := (foto_mes %/% 100L) * 12L + foto_mes %% 100L]
  for (k in 1:2) {
    nombres_lag <- paste0(campos, "_lag", k)
    previo <- d[, c("numero_de_cliente", "periodo", campos), with = FALSE]
    previo[, periodo := periodo + k]
    setnames(previo, campos, nombres_lag)
    d[previo, on = c("numero_de_cliente", "periodo"), (nombres_lag) := mget(paste0("i.", nombres_lag))]
    rm(previo); gc(verbose = FALSE)
    for (j in seq_along(campos))
      set(d, j = paste0(campos[j], "_delta", k), value = as.numeric(d[[campos[j]]]) - as.numeric(d[[nombres_lag[j]]]))
    avance("Lag y delta de orden ", k, " listos")
  }
  d[, periodo := NULL]

  # 3. Ranking con cero fijo, por variable y por mes
  a_rankear <- setdiff(names(d), fijas)
  filas_mes <- split(seq_len(nrow(d)), d$foto_mes)
  for (v in a_rankear) {
    x <- as.numeric(d[[v]])
    r <- rep(NA_real_, length(x))
    for (filas in filas_mes) {
      xi <- x[filas]
      ri <- rep(NA_real_, length(xi))
      ri[!is.na(xi) & xi == 0] <- 0
      pos <- which(!is.na(xi) & xi > 0)
      if (length(pos) > 0) ri[pos] <- frank(xi[pos], ties.method = "average") / length(pos)
      neg <- which(!is.na(xi) & xi < 0)
      if (length(neg) > 0) ri[neg] <- -frank(-xi[neg], ties.method = "average") / length(neg)
      r[filas] <- ri
    }
    set(d, j = v, value = r)
  }
  avance("Ranking con cero fijo listo: ", length(a_rankear), " variables (", ncol(d), " columnas en total)")
  d
}

# Graba datasets/FE026.csv.gz si no existe
asegurar_FE026 <- function() {
  archivo <- file.path("datasets", "FE026.csv.gz")
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
