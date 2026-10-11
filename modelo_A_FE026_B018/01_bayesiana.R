# =============================================================================
# 01_bayesiana.R del modelo A
# uso, desde la carpeta de la entrega: Rscript modelo_A_FE026_B018/01_bayesiana.R
# Bayesiana frugal del modelo A (B018) sobre FE026, armado en memoria:
#   - parámetros fijos param_fijos_tutoria (incluye feature_fraction_bynode
#     = 0.20)
#   - sin mprestamos_personales, cprestamos_personales ni sus lags y deltas
#     (columnas_sin_excluidas)
#   - entrenamiento: 202103 y 202104 completos; validación: 202106 completo;
#     202105 no se usa (hueco de un mes, como entre los datos de
#     entrenamiento final y 202108)
#   - sin undersampling
#   - métrica AUC; 8 hilos
#   - 10 evaluaciones (mlr3mbo hace igual un diseño inicial al azar de
#     4 x 3 = 12)
# Salida: exp/B018/BO_log.txt; al terminar, exp/B018/COMPLETA.
# Si encuentra un BO_log incompleto (corrida interrumpida), lo renombra y
# empieza de nuevo (mlr3mbo no retoma una corrida).
# =============================================================================

paquetes <- c("data.table", "R.utils", "rlist", "bbotk", "mlr3", "mlr3mbo", "mlr3learners",
              "paradox", "DiceKriging", "lightgbm", "here")
faltan <- paquetes[!vapply(paquetes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan) > 0) stop("Faltan paquetes: ", paste(faltan, collapse = ", "))

suppressPackageStartupMessages({
  library(data.table); library(R.utils); library(rlist); library(bbotk); library(mlr3)
  library(mlr3mbo); library(mlr3learners); library(paradox); library(DiceKriging); library(lightgbm)
})
source(file.path("R", "datasets.R"))
source(file.path("R", "parametros.R"))

if (!exists("param_fijos_tutoria") || !exists("columnas_sin_excluidas"))
  stop("R/parametros.R no tiene param_fijos_tutoria o columnas_sin_excluidas")

avance <- function(...) message(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [B018] ", ...)

carpeta <- file.path("exp", "B018")
dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
archivo_log <- file.path(carpeta, "BO_log.txt")
archivo_completa <- file.path(carpeta, "COMPLETA")

if (file.exists(archivo_completa)) {
  avance("B018 ya está completa; no se repite")
  quit(save = "no", status = 0)
}
if (file.exists(archivo_log)) {
  viejo <- file.path(carpeta, paste0("BO_log_interrumpido_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"))
  file.rename(archivo_log, viejo)
  avance("BO_log incompleto renombrado a ", basename(viejo))
}

PARAM <- list(
  train = c(202103L, 202104L),
  validate = 202106L,
  iteraciones = 10
)

# Dataset
dataset <- construir_FE026()
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]
todas <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))
excluidas <- columnas_excluidas(todas)
campos_buenos <- columnas_sin_excluidas(todas)
avance("Columnas excluidas (", length(excluidas), "): ", paste(excluidas, collapse = ", "))
if (length(excluidas) == 0) stop("No se encontró ninguna columna a excluir: revisar los nombres")
avance("Parámetro agregado: feature_fraction_bynode = ", param_fijos_tutoria$feature_fraction_bynode)

tr <- dataset[foto_mes %in% PARAM$train]
va <- dataset[foto_mes == PARAM$validate]
stopifnot(tr[, !anyNA(clase_ternaria)], va[, !anyNA(clase_ternaria)])
avance("Entrenamiento: ", nrow(tr), " registros (", paste(PARAM$train, collapse = "+"), ", positivos ",
       tr[, sum(clase01)], "); validación: ", nrow(va), " (", PARAM$validate, ", positivos ", va[, sum(clase01)],
       "); variables: ", length(campos_buenos))

dtrain <- lgb.Dataset(data = data.matrix(tr[, campos_buenos, with = FALSE]), label = tr$clase01, free_raw_data = FALSE)
dvalidate <- lgb.Dataset(data = data.matrix(va[, campos_buenos, with = FALSE]), label = va$clase01, free_raw_data = FALSE)
rm(dataset, tr, va); gc(verbose = FALSE)

# Registro de cada evaluación
loguear <- function(preg, arch, verbose = TRUE) {
  t0 <- Sys.time()
  reg <- copy(preg)
  if (!file.exists(arch)) cat(paste0("fecha\t", paste(list.names(reg), collapse = "\t"), "\n"), file = arch)
  linea <- paste0(format(t0, "%Y%m%d.%H%M%S"), "\t", gsub(", ", "\t", toString(reg)), "\n")
  cat(linea, file = arch, append = TRUE)
  if (verbose) cat(linea)
}

giter <- 0
Estimar_lightgbm <- function(xs) {
  flush.console()
  giter <<- giter + 1
  t0 <- Sys.time()
  param_completo <- modifyList(param_fijos_tutoria, xs)
  xx <- copy(param_completo)
  modelo <- lgb.train(data = dtrain, valids = list("val" = dvalidate), param = param_completo)
  xx$iter <- giter
  xx$metrica <- modelo$best_score
  loguear(xx, archivo_log)
  avance(sprintf("evaluación %d | %d árboles, %d hojas, hessian %.4g | AUC %.5f | %.1f min",
                 giter, xs$num_iterations, xs$num_leaves, xs$min_sum_hessian_in_leaf, xx$metrica,
                 as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  set.seed(semilla_primigenia, kind = "L'Ecuyer-CMRG")
  list(metrica = xx$metrica)
}

# Optimización bayesiana
set.seed(semilla_primigenia, kind = "L'Ecuyer-CMRG")
objective <- ObjectiveRFun$new(
  fun = Estimar_lightgbm,
  domain = rangos_z495(),
  codomain = ps(metrica = p_dbl(tags = "maximize")),
  id = "BO_lightgbm"
)
instance <- OptimInstanceBatchSingleCrit$new(
  objective = objective,
  terminator = trm("evals", n_evals = PARAM$iteraciones)
)
surrogate <- srlrn(lrn("regr.km", optim.method = "BFGS", nugget.estim = TRUE, control = list(trace = FALSE)))
acq_function <- acqf("ei")
acq_optimizer <- acqo(optimizer = opt("random_search", batch_size = 1000), terminator = trm("evals", n_evals = 1000))
optimizer <- opt("mbo", loop_function = bayesopt_ego, surrogate = surrogate,
                 acq_function = acq_function, acq_optimizer = acq_optimizer)

avance("Comienza la bayesiana")
optimizer$optimize(instance)

tb <- fread(archivo_log)
setorder(tb, -metrica)
avance("Terminada: ", nrow(tb), " evaluaciones; mejor AUC ", round(tb$metrica[1], 5), " (iter ", tb$iter[1],
       ": ", tb$num_iterations[1], " árboles, ", tb$num_leaves[1], " hojas, hessian ", signif(tb$min_sum_hessian_in_leaf[1], 4), ")")
writeLines(format(Sys.time()), archivo_completa)
