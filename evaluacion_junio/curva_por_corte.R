# =============================================================================
# curva_por_corte.R
# uso, desde evaluacion_junio/:
#   Rscript curva_por_corte.R exp/E079/probabilidades_future.csv.gz exp/E079/curva_por_corte.csv
# Ganancia del modelo en 202106 alrededor de cada corte, de 10.500 a 12.500
# envíos cada 250, a partir de las probabilidades por semilla que graba el
# testeo (numero_de_cliente, clase_ternaria, prob_<semilla> de cada semilla y
# prob_media, el semillerío). Para cada corte n, promedio de la ganancia
# acumulada entre n - 500 y n + 500 envíos:
#   - del semillerío (ganancia_ventana_M), y su valor en n (ganancia_corte_M)
#   - de cada semilla, con su media y desvío (semillas_media_M, semillas_desvio_M)
# Ganancia: +1.072.500 por BAJA+2 y -27.500 por cada otro envío.
# =============================================================================

suppressPackageStartupMessages(library(data.table))
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("Uso: Rscript tabla_corte.R <probabilidades_junio.csv.gz> <salida.csv>")

tb <- fread(args[1])
ganancia <- ifelse(as.character(tb$clase_ternaria) == "BAJA+2", 1072500, -27500)
curva <- function(p) cumsum(ganancia[order(-p)])
ventana <- function(cv, n) mean(cv[(n - 500):(n + 500)]) / 1e6

cortes <- seq(10500, 12500, by = 250)
semillas <- grep("^prob_[0-9]+$", names(tb), value = TRUE)
cv_semillerio <- curva(tb$prob_media)
por_semilla <- sapply(semillas, function(s) { cv <- curva(tb[[s]]); sapply(cortes, ventana, cv = cv) })

tabla <- data.table(
  envios = cortes,
  ganancia_ventana_M = round(sapply(cortes, ventana, cv = cv_semillerio), 1),
  ganancia_corte_M = round(cv_semillerio[cortes] / 1e6, 1),
  semillas_media_M = round(rowMeans(por_semilla), 1),
  semillas_desvio_M = round(apply(por_semilla, 1, sd), 1)
)
fwrite(tabla, args[2])
print(tabla)
