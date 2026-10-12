# =============================================================================
# modelo.R
# Funciones de entrenamiento de LightGBM y de cálculo de la ganancia.
# Requiere data.table y lightgbm cargados.
# =============================================================================


# -----------------------------------------------------------------------------
# curva_ganancia()
# Ordena los registros por probabilidad descendente y calcula la ganancia
# acumulada de enviar el estímulo a los primeros n registros, para todo n.
#
#   prob             probabilidad asignada por el modelo a cada registro
#   clase_ternaria   clase real de cada registro
#   ganancia_acierto ganancia por estímulo enviado a un BAJA+2
#   costo_estimulo   ganancia (negativa) por estímulo enviado a quien no es BAJA+2
#
# Devuelve un data.table con pos (cantidad de envíos) y ganancia_acumulada.
# -----------------------------------------------------------------------------
curva_ganancia <- function(prob, clase_ternaria,
                           ganancia_acierto = 1072500,
                           costo_estimulo = -27500) {

  tb <- data.table(
    prob = prob,
    gan = ifelse(clase_ternaria == "BAJA+2", ganancia_acierto, costo_estimulo)
  )

  setorder(tb, -prob)
  tb[, pos := .I]
  tb[, ganancia_acumulada := cumsum(gan)]

  tb[, list(pos, ganancia_acumulada)]
}


# -----------------------------------------------------------------------------
# evaluar_semillas()
# Entrena un modelo final por cada semilla, con los mismos hiperparámetros,
# y calcula la curva de ganancia de cada uno sobre el mes futuro. Si hay
# undersampling, cada semilla toma su propia muestra de negativos, por lo
# que la variación entre modelos incluye tanto la semilla de LightGBM como
# la muestra de entrenamiento.
#
#   dataset_train  data.table de entrenamiento, con clase01
#   campos         atributos usados para entrenar
#   x_future       matriz de atributos del mes futuro
#   clase_future   clase_ternaria real del mes futuro
#   param          lista completa de parámetros de LightGBM
#   semillas       vector de semillas
#   undersampling  fracción de negativos conservada (1 = todos)
#   devolver_probabilidades  si es TRUE, devuelve además la matriz de
#                  probabilidades del mes futuro (una columna por semilla, en
#                  el orden de filas de x_future) y la importancia de
#                  variables de cada modelo
#
# Devuelve un data.table con semilla, pos y ganancia_acumulada o, si
# devolver_probabilidades es TRUE, una lista con curvas, probabilidades e
# importancia.
# Requiere filas_undersampling() (R/utils.R).
# -----------------------------------------------------------------------------
evaluar_semillas <- function(dataset_train, campos, x_future, clase_future,
                             param, semillas, undersampling = 1.0,
                             devolver_probabilidades = FALSE) {

  prob <- matrix(NA_real_, nrow = nrow(x_future), ncol = length(semillas),
                 dimnames = list(NULL, paste0("prob_", semillas)))
  importancias <- vector("list", length(semillas))

  curvas <- lapply(seq_along(semillas), function(i) {

    semilla <- semillas[i]
    filas <- filas_undersampling(dataset_train$clase01, undersampling, semilla)

    dtrain <- lgb.Dataset(
      data = data.matrix(dataset_train[filas, campos, with = FALSE]),
      label = dataset_train$clase01[filas]
    )

    modelo <- lgb.train(data = dtrain,
                        param = modifyList(param, list(seed = semilla)))
    p <- predict(modelo, x_future)
    prob[, i] <<- p

    imp <- lgb.importance(modelo)
    imp[, semilla := semilla]
    importancias[[i]] <<- imp

    curva <- curva_ganancia(p, clase_future)
    curva[, semilla := semilla]

    message("Semilla ", semilla, " finalizada")
    curva
  })

  curvas <- rbindlist(curvas)

  if (devolver_probabilidades) {
    return(list(curvas = curvas, probabilidades = prob,
                importancia = rbindlist(importancias)))
  }
  curvas
}


# -----------------------------------------------------------------------------
# average_precision()
# Precisión promedio: media de la precisión acumulada en la posición de cada
# positivo, con los registros ordenados por probabilidad descendente.
#
#   prob  probabilidad asignada por el modelo
#   y     clase binaria real
# -----------------------------------------------------------------------------
average_precision <- function(prob, y) {
  y <- y[order(-prob)]
  precision <- cumsum(y) / seq_along(y)
  sum(precision[y == 1]) / sum(y)
}


# -----------------------------------------------------------------------------
# resumir_cortes()
# Resume las curvas de varias semillas en un conjunto de cortes (cantidad de
# envíos): ganancia media, desvío estándar, mínimo y máximo entre semillas.
#
#   curvas  data.table devuelto por evaluar_semillas()
#   cortes  vector de cantidades de envíos a evaluar
# -----------------------------------------------------------------------------
resumir_cortes <- function(curvas, cortes) {
  curvas[pos %in% cortes,
         list(ganancia_media = mean(ganancia_acumulada),
              desvio = sd(ganancia_acumulada),
              minimo = min(ganancia_acumulada),
              maximo = max(ganancia_acumulada)),
         by = list(envios = pos)][order(envios)]
}
