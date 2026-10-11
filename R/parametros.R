# =============================================================================
# parametros.R
# Semilla primigenia, parámetros fijos de LightGBM y rangos de la
# optimización bayesiana. metric = "auc" y num_threads = 8 en todos los
# entrenamientos, para que los resultados sean reproducibles.
#
# param_fijos_tutoria es el conjunto que usan la optimización y la
# producción: param_fijos_z495 más feature_fraction_bynode = 0.20.
#
# Variables excluidas: mprestamos_personales y cprestamos_personales se
# quitan en la optimización y se usan en la producción. La exclusión abarca
# la variable original y todas sus derivadas (lags y deltas).
# =============================================================================

semilla_primigenia <- 173207L

param_fijos_z495 <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "auc",
  feature_pre_filter = FALSE,
  first_metric_only = FALSE,
  boost_from_average = TRUE,
  force_row_wise = TRUE,
  deterministic = TRUE,
  verbosity = -100,
  seed = semilla_primigenia,
  max_depth = -1L,
  min_gain_to_split = 0,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  max_bin = 31L,
  bagging_fraction = 1.0,
  pos_bagging_fraction = 1.0,
  neg_bagging_fraction = 1.0,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  drop_rate = 0.1,
  max_drop = 50,
  skip_drop = 0.5,
  extra_trees = FALSE,
  early_stopping = 0,
  min_data_in_leaf = 0,
  feature_fraction = 0.50,
  learning_rate = 0.005,
  num_iterations = 20000,
  num_leaves = 1024,
  min_sum_hessian_in_leaf = 1e-06,
  num_threads = 8L
)

param_fijos_tutoria <- modifyList(
  param_fijos_z495,
  list(feature_fraction_bynode = 0.20)
)

rangos_z495 <- function() {
  paradox::ps(
    num_iterations = paradox::p_int(lower = 64L, upper = 4096L),
    num_leaves = paradox::p_int(lower = 16L, upper = 2048L),
    min_sum_hessian_in_leaf = paradox::p_dbl(lower = 1e-06, upper = 0.1)
  )
}

# Variables originales que se excluyen en la optimización.
variables_excluidas_base <- c(
  "mprestamos_personales",
  "cprestamos_personales"
)

# Columnas de 'nombres' que corresponden a una variable excluida o a
# cualquier variable derivada de ella: la original y todo lo que empiece con
# su nombre seguido de "_".
columnas_excluidas <- function(nombres, base = variables_excluidas_base) {
  patron <- paste0("^(", paste(base, collapse = "|"), ")(_.+)?$")
  grep(patron, nombres, value = TRUE)
}

# Columnas utilizables: todas salvo las excluidas.
columnas_sin_excluidas <- function(nombres, base = variables_excluidas_base) {
  setdiff(nombres, columnas_excluidas(nombres, base))
}
