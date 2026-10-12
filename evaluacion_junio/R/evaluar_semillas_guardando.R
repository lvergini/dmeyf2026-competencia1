# -----------------------------------------------------------------------------
# evaluar_semillas_guardando()
# Igual que evaluar_semillas() (R/modelo.R), pero graba cada semilla apenas
# termina, en <carpeta>/:
#   prob_<semilla>.csv.gz        probabilidad del mes futuro (orden de x_future)
#   importancia_<semilla>.csv    importancia de variables (solo en las primeras
#                                importancia_semillas semillas; por defecto, 1)
# lgb.importance() de un modelo grande tarda muchos minutos en un solo hilo:
# por eso se calcula solo en la primera semilla.
# Al arrancar, las semillas que ya tienen su archivo de probabilidades (y el
# de importancia, si les corresponde) no se reentrenan: se leen. Así un corte
# solo pierde la semilla en curso y el experimento se retoma donde quedó.
# Cada archivo se escribe primero como .parcial y se renombra al final.
# El lgb.Dataset se arma de nuevo en cada semilla, como en evaluar_semillas():
# la semilla de LightGBM también fija el muestreo con que se construyen los
# bins, así que reutilizarlo cambiaría los modelos respecto de los testeos
# anteriores.
# El avance se escribe en stderr (cat(file = stderr())), que knitr no
# captura, y en <carpeta>/avance.txt.
# Devuelve siempre la lista con curvas, probabilidades e importancia.
# -----------------------------------------------------------------------------
evaluar_semillas_guardando <- function(dataset_train, campos, x_future, clase_future,
                                       param, semillas, carpeta, undersampling = 1.0,
                                       importancia_semillas = 1L) {

  dir.create(carpeta, showWarnings = FALSE, recursive = TRUE)
  prob <- matrix(NA_real_, nrow = nrow(x_future), ncol = length(semillas),
                 dimnames = list(NULL, paste0("prob_", semillas)))
  importancias <- vector("list", length(semillas))

  aviso <- function(...) {
    linea <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", ...)
    cat(linea, "\n", sep = "", file = stderr())
    cat(linea, "\n", sep = "", file = file.path(carpeta, "avance.txt"), append = TRUE)
  }
  minutos <- function(a, b) round(as.numeric(difftime(b, a, units = "mins")), 1)
  aviso("Comienza: ", length(semillas), " semillas; ya grabadas: ",
        sum(file.exists(file.path(carpeta, paste0("prob_", semillas, ".csv.gz")))),
        "; importancia en las primeras ", importancia_semillas)

  grabar <- function(tabla, destino) {
    temporal <- paste0(destino, ".parcial")
    fwrite(tabla, temporal)
    file.rename(temporal, destino)
  }

  armar_dtrain <- function(semilla) {
    filas <- filas_undersampling(dataset_train$clase01, undersampling, semilla)
    lgb.Dataset(data = data.matrix(dataset_train[filas, campos, with = FALSE]),
                label = dataset_train$clase01[filas])
  }

  curvas <- lapply(seq_along(semillas), function(i) {
    semilla <- semillas[i]
    con_importancia <- i <= importancia_semillas
    archivo_prob <- file.path(carpeta, paste0("prob_", semilla, ".csv.gz"))
    archivo_imp <- file.path(carpeta, paste0("importancia_", semilla, ".csv"))

    if (file.exists(archivo_prob) && (!con_importancia || file.exists(archivo_imp))) {
      p <- fread(archivo_prob)$prob
      if (length(p) != nrow(x_future))
        stop("prob_", semilla, " tiene ", length(p), " filas y el mes futuro ", nrow(x_future))
      imp <- if (file.exists(archivo_imp)) fread(archivo_imp) else NULL
      aviso("Semilla ", semilla, " (", i, " de ", length(semillas), ") ya grabada: se reutiliza")
    } else {
      t0 <- Sys.time()
      dtrain <- armar_dtrain(semilla)
      t1 <- Sys.time()
      modelo <- lgb.train(data = dtrain, param = modifyList(param, list(seed = semilla)))
      p <- predict(modelo, x_future)
      t2 <- Sys.time()
      grabar(data.table(prob = p), archivo_prob)
      imp <- NULL
      if (con_importancia) {
        imp <- lgb.importance(modelo)
        imp[, semilla := semilla]
        grabar(imp, archivo_imp)
      }
      t3 <- Sys.time()
      rm(modelo); gc(verbose = FALSE)
      aviso("Semilla ", semilla, " grabada (", i, " de ", length(semillas), "): datos ", minutos(t0, t1),
            " min, entrenamiento y predicción ", minutos(t1, t2), " min, importancia ",
            if (con_importancia) paste0(minutos(t2, t3), " min") else "no", ", total ",
            minutos(t0, Sys.time()), " min")
    }

    prob[, i] <<- p
    importancias[[i]] <<- imp
    curva <- curva_ganancia(p, clase_future)
    curva[, semilla := semilla]
    curva
  })

  list(curvas = rbindlist(curvas), probabilidades = prob,
       importancia = rbindlist(importancias))
}
