# =============================================================================
# E079 - Testeo en junio de la configuración 1 de B018 (bayesiana frugal) sobre
# FE026; esquema de junio_comun.R. Requiere que B018 haya terminado.
# =============================================================================

source(here::here("corridas", "junio_fe026.R"))

correr_experimento_FE026(
  experimento = "E079",
  descripcion = "Junio: FE026 + B018 configuración 1 (bayesiana frugal); 202103+202104; sin préstamos; 20 semillas",
  hiperparametros = log_configuracion("exp/B018/BO_log.txt", 1, exigir_completa = TRUE)
)
