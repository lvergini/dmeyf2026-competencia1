# =============================================================================
# 02_produccion.R del modelo A
# uso, desde la carpeta de la entrega:
#   Rscript modelo_A_FE026_B018/02_produccion.R <sublista 1-4> [<proceso>]
# Producción del modelo A (P069): FE026 con la configuración de mayor AUC de
# B018. Esquema de R/produccion.R (cuatro meses, préstamos incluidos, una
# semilla por vez, cada semilla grabada en exp/P069/semillas/).
# Entrena las semillas de semillas_A.csv de esa sublista y ese proceso
# (1 si no se indica), en el orden en que corrieron. Cada sublista y cada
# proceso se corre en un Rscript aparte, como en la producción original.
# Requiere datasets/FE026.csv.gz (01_datasets.R) y exp/B018/BO_log.txt con
# exp/B018/COMPLETA (01_bayesiana.R, o BO_log_corrido.txt copiado ahí).
# =============================================================================

source(file.path("R", "produccion.R"))
args <- commandArgs(trailingOnly = TRUE)
producir("P069", "FE026.csv.gz", "exp/B018/BO_log.txt", 1L,
         maquina = as.integer(args[1]),
         archivo_semillas = file.path("modelo_A_FE026_B018", "semillas_A.csv"),
         proceso = if (length(args) >= 2) as.integer(args[2]) else 1L)
