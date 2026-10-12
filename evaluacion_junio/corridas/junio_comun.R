# =============================================================================
# junio_comun.R
# Configuración común del testeo en junio: cada
# modelo enviado (dataset + hiperparámetros) se reentrena con 202103 y 202104
# completos, sin los BAJA+1 de 202105, sin undersampling, y se mide la
# ganancia en 202106 con correr_experimento() (rmd/03_experimento.Rmd).
#   - Préstamos personales excluidos, con todos sus lags y deltas de orden 1
#     a 3 (se excluyen en la optimización y en el chequeo de ganancia; se
#     incluyen solo en producción). La lista se arma con outer(): cada
#     variable con cada sufijo.
#   - Las mismas 20 semillas en todos los modelos, para poder
#     comparar de a pares semilla por semilla (Wilcoxon pareado).
#   - El Rmd toma siempre la fila de mayor métrica del log de la bayesiana;
#     para medir la configuración k se graba un log de una sola fila con la
#     k-ésima configuración (orden por métrica descendente).
#     Con exigir_completa = TRUE, la bayesiana tiene que haber terminado
#     (archivo COMPLETA en su carpeta), para no tomar un log a medias.
# =============================================================================

source(here::here("R", "pipeline.R"))

semillas_junio <- c(173207, 291419, 438203, 672439, 931123, 104729, 224737, 350377, 479909, 611953,
                    760187, 899009, 141803, 287117, 433781, 573493, 715873, 857033, 990371, 312929)

prestamos_todos <- as.vector(outer(
  c("mprestamos_personales", "cprestamos_personales"),
  c("", paste0("_lag", 1:3), paste0("_delta", 1:3)),
  paste0
))

# Log de una sola fila con la configuración k del log indicado.
# Se graba en exp/junio_configuraciones/ y nunca se sobrescribe.
log_configuracion <- function(log, k, exigir_completa = FALSE) {
  origen <- here::here(log)
  if (!file.exists(origen)) stop("No existe ", log)
  if (exigir_completa && !file.exists(file.path(dirname(origen), "COMPLETA")))
    stop("La bayesiana de ", log, " no terminó (falta COMPLETA)")
  carpeta <- here::here("exp", "junio_configuraciones")
  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  nombre <- sprintf("%s_conf%02d.txt", basename(dirname(origen)), k)
  destino <- file.path(carpeta, nombre)
  if (!file.exists(destino)) {
    tb <- data.table::fread(origen)
    data.table::setorder(tb, -metrica)
    if (k > nrow(tb)) stop(log, " tiene ", nrow(tb), " filas; no existe la configuración ", k)
    data.table::fwrite(tb[k], destino, sep = "\t")
  }
  file.path("exp", "junio_configuraciones", nombre)
}

# Configuración de correr_experimento() con el esquema de junio.
config_junio <- function(experimento, descripcion, fe, campos_excluidos, hiperparametros) {
  if (!file.exists(here::here(hiperparametros))) stop("No existe ", hiperparametros)
  list(
    experimento = experimento,
    descripcion = descripcion,
    fe = fe,
    meses_train = c(202103, 202104),
    meses_baja1 = integer(0),
    mes_future = 202106,
    campos_excluidos = unique(c(campos_excluidos, prestamos_todos)),
    hiperparametros = hiperparametros,
    semillas = semillas_junio,
    undersampling = 1.0,
    num_threads = 8
  )
}
