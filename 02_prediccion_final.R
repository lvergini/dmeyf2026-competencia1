# =============================================================================
# 02_prediccion_final.R
# uso, desde la carpeta de la entrega: Rscript 02_prediccion_final.R
# Parámetros (fijados en args, más abajo):
#   metodo: prob (promedio de probabilidades) o rank (promedio de rankings)
#   cortes: separados por coma, por ejemplo 10500,11000,11500,12000
#   modelos: ids de producción, por ejemplo P069
# Para cada modelo lee exp/<id>/semillas/prob_*.csv.gz, descarta las semillas
# con predicciones idénticas a otra (misma semilla interna de LightGBM), y
# hace su semillerío (promedio de las semillas). Con más de un
# modelo, los combina por probabilidad o por ranking, con igual peso.
# Escribe un archivo de envío por corte con este formato: los
# numero_de_cliente de los primeros N, sin encabezado, enteros.
# Salida (nunca sobrescribe): exp/entregas/<fecha_hora>_<modelos>_<metodo>/
#   <modelos>_<metodo>_<corte>.csv, probabilidades.csv.gz y resumen.txt
# =============================================================================

suppressPackageStartupMessages(library(data.table))
# Predicción final: semillerío del modelo A (P069), corte de 11.750 envíos.
args <- c("prob", "11750", "P069")
if (length(args) < 3) stop("Uso: Rscript 02_prediccion_final.R")
metodo <- args[1]
cortes <- as.integer(strsplit(args[2], ",")[[1]])
modelos <- args[-(1:2)]
stopifnot(metodo %in% c("prob", "rank"), !anyNA(cortes))

r01 <- function(p) frank(p, ties.method = "average") / length(p)
resumen <- character(0)
anotar <- function(...) { linea <- paste0(...); message(linea); resumen <<- c(resumen, linea) }

semillerio <- function(id) {
  archivos <- sort(list.files(file.path("exp", id, "semillas"), pattern = "^prob_[0-9]+\\.csv\\.gz$", full.names = TRUE))
  if (length(archivos) == 0) stop(id, ": no hay semillas en exp/", id, "/semillas")
  probs <- lapply(archivos, fread)
  repetida <- duplicated(lapply(probs, `[[`, "prob"))
  if (any(repetida)) {
    anotar(id, ": descartadas por predicciones idénticas a otra semilla: ",
           paste(basename(archivos[repetida]), collapse = ", "))
    archivos <- archivos[!repetida]; probs <- probs[!repetida]
  }
  ids <- probs[[1]]$numero_de_cliente
  for (p in probs) stopifnot(identical(p$numero_de_cliente, ids))
  anotar(id, ": ", length(probs), " semillas")
  data.table(numero_de_cliente = ids, prob = rowMeans(sapply(probs, `[[`, "prob")))
}

sems <- lapply(modelos, semillerio)
ids <- sems[[1]]$numero_de_cliente
for (k in seq_along(sems)[-1]) {
  if (!setequal(sems[[k]]$numero_de_cliente, ids)) stop(modelos[k], ": los clientes de 202108 no coinciden con ", modelos[1])
  sems[[k]] <- sems[[k]][match(ids, numero_de_cliente)]
}
puntaje <- if (length(sems) == 1) sems[[1]]$prob else
  if (metodo == "prob") rowMeans(sapply(sems, `[[`, "prob")) else rowMeans(sapply(sems, function(s) r01(s$prob)))

nombre <- paste(modelos, collapse = "+")
carpeta <- file.path("exp", "entregas", paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", gsub("\\+", "_", nombre), "_", metodo))
dir.create(carpeta, recursive = TRUE, showWarnings = FALSE)
tb <- data.table(numero_de_cliente = ids)
for (k in seq_along(sems)) tb[, (paste0("semillerio_", modelos[k])) := sems[[k]]$prob]
tb[, puntaje := puntaje]
fwrite(tb, file.path(carpeta, "probabilidades.csv.gz"))

orden <- ids[order(-puntaje)]
for (n in cortes) {
  f <- file.path(carpeta, sprintf("%s_%s_%d.csv", gsub("\\+", "_", nombre), metodo, n))
  fwrite(data.table(as.integer(orden[seq_len(n)])), f, col.names = FALSE)
}
anotar("Combinación: ", nombre, " por ", if (length(sems) == 1) "semillerío" else metodo,
       " | cortes: ", paste(cortes, collapse = ", "), " | ", length(ids), " clientes")
anotar("Archivos en ", carpeta)
writeLines(resumen, file.path(carpeta, "resumen.txt"))
