# =============================================================================
# pipeline.R
# Ejecución de experimentos de punta a punta: construcción (o reutilización)
# del dataset procesado, entrenamiento con varias semillas, reporte y
# resultado. Incluye la ejecución en cola de varios experimentos.
# Cada script de corridas/ carga este archivo.
# =============================================================================

library(data.table)
library(here)

source(here("R", "fe.R"))
source(here("R", "experimentos.R"))


# -----------------------------------------------------------------------------
# correr_experimento()
# Ejecuta un experimento a partir de su configuración. Si el experimento ya
# tiene su resultado grabado en exp/<experimento>/, se omite, salvo que
# forzar sea TRUE.
#
# Campos de config:
#   experimento       identificador del experimento
#   descripcion       texto breve del cambio que se evalúa
#   fe                list(nombre = <identificador>, pasos = list(...)) o NULL
#   meses_train       meses de entrenamiento con clase completa
#   meses_baja1       meses de los que solo se agregan los BAJA+1
#   mes_future        mes donde se mide la ganancia
#   campos_excluidos  columnas que no se usan para entrenar
#   campos_incluidos  (opcional) lista de variables a usar; si se indica,
#                     se usan solo esas
#   hiperparametros   log de Optimización Bayesiana del que se toman
#   semillas          semillas del modelo final
#   undersampling     (opcional) fracción de negativos conservada; por defecto 1
#   clase             (opcional) "binaria2" (por defecto), "baja2" o "ganadora"
#   rf_arboles        (opcional) árboles del bosque cuyas hojas se agregan como
#                     variables; 0 (por defecto) no agrega hojas
#   rf_hojas, rf_min_data  (opcional) hojas por árbol y registros mínimos por hoja
#   intrusos          (opcional) fracción de los CONTINUA de entrenamiento con
#                     mayor probabilidad fuera de muestra que se quita (clase
#                     07); 0 (por defecto) no quita registros
#   intrusos_metodo   (opcional) "fraccion" (por defecto) o "denicolay"
#   intrusos_umbral   (opcional) umbral del método "denicolay"; por defecto 0,5
#   etiquetas         (opcional) archivo de clase artificial generado por
#                     correr_etiquetador(); sus registros se agregan al
#                     entrenamiento con la probabilidad como clase
#   num_threads       (opcional) hilos de LightGBM; por defecto, todos
# -----------------------------------------------------------------------------
correr_experimento <- function(config, forzar = FALSE) {

  if (!forzar && experimento_registrado(config$experimento)) {
    message(config$experimento, " ya registrado; se omite")
    return(invisible(FALSE))
  }

  archivo_dataset <- construir_dataset_fe(config$fe$nombre, config$fe$pasos)

  rmarkdown::render(
    input = here("rmd", "03_experimento.Rmd"),
    params = list(
      experimento = config$experimento,
      descripcion = config$descripcion,
      dataset = basename(archivo_dataset),
      meses_train = config$meses_train,
      meses_baja1 = config$meses_baja1,
      mes_future = config$mes_future,
      campos_excluidos = config$campos_excluidos,
      campos_incluidos = if (is.null(config$campos_incluidos)) character(0) else config$campos_incluidos,
      hiperparametros = config$hiperparametros,
      semillas = config$semillas,
      undersampling = if (is.null(config$undersampling)) 1.0 else config$undersampling,
      clase = if (is.null(config$clase)) "binaria2" else config$clase,
      rf_arboles = if (is.null(config$rf_arboles)) 0 else config$rf_arboles,
      rf_hojas = if (is.null(config$rf_hojas)) 16 else config$rf_hojas,
      rf_min_data = if (is.null(config$rf_min_data)) 500 else config$rf_min_data,
      intrusos = if (is.null(config$intrusos)) 0 else config$intrusos,
      intrusos_metodo = if (is.null(config$intrusos_metodo)) "fraccion" else config$intrusos_metodo,
      intrusos_umbral = if (is.null(config$intrusos_umbral)) 0.5 else config$intrusos_umbral,
      etiquetas = if (is.null(config$etiquetas)) "" else config$etiquetas,
      num_threads = if (is.null(config$num_threads)) NA else config$num_threads
    ),
    output_file = paste0(config$experimento, ".md"),
    output_dir = here("exp", config$experimento),
    envir = new.env()
  )

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# correr_cola()
# Ejecuta en secuencia los scripts indicados. Los experimentos ya
# registrados se omiten. Si un script falla, el error se anota en el log y
# se muestra en pantalla; por defecto la cola se detiene, porque en general
# cada paso depende del anterior (una evaluación usa los hiperparámetros de
# la optimización previa). Con detener_si_falla = FALSE, continúa con el
# siguiente script (útil para tandas de experimentos independientes).
# Al terminar, consolida experimentos.csv.
#
#   scripts           rutas de los scripts, relativas a la raíz del proyecto
#   nombre            nombre de la cola; el avance queda en exp/cola_<nombre>_log.txt
#   detener_si_falla  si es TRUE, un error interrumpe la cola
# -----------------------------------------------------------------------------
correr_cola <- function(scripts, nombre, detener_si_falla = TRUE) {

  archivo_log <- here("exp", paste0("cola_", nombre, "_log.txt"))
  dir.create(dirname(archivo_log), showWarnings = FALSE, recursive = TRUE)

  anotar <- function(...) {
    cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ..., "\n",
        file = archivo_log, append = TRUE)
  }

  for (script in scripts) {
    anotar("inicio", script)
    resultado <- tryCatch({
      source(here(script), local = new.env())
      "ok"
    }, error = function(e) {
      # el error se muestra también en pantalla, además de anotarse en el log
      message("ERROR en ", script, ": ", conditionMessage(e))
      paste("ERROR:", conditionMessage(e))
    })
    anotar("fin", script, resultado)

    if (detener_si_falla && resultado != "ok") {
      anotar("cola interrumpida por error en", script)
      message("Cola interrumpida por error en ", script)
      break
    }
  }

  consolidar_experimentos()
  anotar("cola finalizada")
}


# -----------------------------------------------------------------------------
# generar_entrega()
# Construye (o reutiliza) el dataset procesado, entrena con todos los meses
# disponibles, predice el mes sin clase y genera los archivos de entrega en
# exp/<entrega>/entregas/.
#
# Campos de config: entrega, descripcion, fe, meses_train, meses_baja1,
# mes_future, campos_excluidos, hiperparametros, semillas, envios y,
# opcionalmente, num_threads.
# -----------------------------------------------------------------------------
generar_entrega <- function(config) {

  archivo_dataset <- construir_dataset_fe(config$fe$nombre, config$fe$pasos)

  rmarkdown::render(
    input = here("rmd", "04_produccion.Rmd"),
    params = list(
      entrega = config$entrega,
      descripcion = config$descripcion,
      dataset = basename(archivo_dataset),
      meses_train = config$meses_train,
      meses_baja1 = config$meses_baja1,
      mes_future = config$mes_future,
      campos_excluidos = config$campos_excluidos,
      campos_incluidos = if (is.null(config$campos_incluidos)) character(0) else config$campos_incluidos,
      hiperparametros = config$hiperparametros,
      semillas = config$semillas,
      envios = config$envios,
      intrusos = if (is.null(config$intrusos)) 0 else config$intrusos,
      undersampling = if (is.null(config$undersampling)) 1.0 else config$undersampling,
      num_threads = if (is.null(config$num_threads)) NA else config$num_threads
    ),
    output_file = paste0(config$entrega, ".md"),
    output_dir = here("exp", config$entrega),
    envir = new.env()
  )

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# correr_bayesiana()
# Construye (o reutiliza) el dataset procesado y ejecuta la Optimización
# Bayesiana de hiperparámetros definida en rmd/05_bayesiana.Rmd. El log
# queda en exp/<bayesiana>/BO_log.txt, que luego se indica como
# hiperparametros en experimentos y entregas.
#
# Campos de config: bayesiana, descripcion, fe, meses_train, meses_baja1,
# campos_excluidos, semilla, iteraciones, undersampling y, opcionalmente,
# num_threads.
# -----------------------------------------------------------------------------
correr_bayesiana <- function(config) {

  archivo_dataset <- construir_dataset_fe(config$fe$nombre, config$fe$pasos)

  rmarkdown::render(
    input = here("rmd", "05_bayesiana.Rmd"),
    params = list(
      bayesiana = config$bayesiana,
      descripcion = config$descripcion,
      dataset = basename(archivo_dataset),
      meses_train = config$meses_train,
      meses_baja1 = config$meses_baja1,
      campos_excluidos = config$campos_excluidos,
      semilla = config$semilla,
      iteraciones = config$iteraciones,
      undersampling = config$undersampling,
      num_threads = if (is.null(config$num_threads)) NA else config$num_threads
    ),
    output_file = paste0(config$bayesiana, ".md"),
    output_dir = here("exp", config$bayesiana),
    envir = new.env()
  )

  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# correr_bayesiana_cv()
# Igual que correr_bayesiana(), pero con validación cruzada por cliente
# (rmd/05_bayesiana_cv.Rmd) y los parámetros fijos y el espacio de búsqueda
# predefinidos. El log queda en exp/<bayesiana>/BO_log.txt.
#
# Campos de config: bayesiana, descripcion, fe, meses_train, meses_baja1,
# campos_excluidos, semilla, folds, undersampling, iteraciones y,
# opcionalmente, hilos_por_fold (hilos de cada modelo; los folds se entrenan
# en paralelo en procesos separados) y espacio ("z495", por defecto, o
# "ampliado"), y etiquetas (archivo de clase artificial de
# correr_etiquetador(): sus registros solo entrenan, nunca validan).
# -----------------------------------------------------------------------------
correr_bayesiana_cv <- function(config) {

  archivo_dataset <- construir_dataset_fe(config$fe$nombre, config$fe$pasos)

  rmarkdown::render(
    input = here("rmd", "05_bayesiana_cv.Rmd"),
    params = list(
      bayesiana = config$bayesiana,
      descripcion = config$descripcion,
      dataset = basename(archivo_dataset),
      meses_train = config$meses_train,
      meses_baja1 = config$meses_baja1,
      campos_excluidos = config$campos_excluidos,
      campos_incluidos = if (is.null(config$campos_incluidos)) character(0) else config$campos_incluidos,
      semilla = config$semilla,
      folds = config$folds,
      undersampling = config$undersampling,
      iteraciones = config$iteraciones,
      hilos_por_fold = if (is.null(config$hilos_por_fold)) 3L else config$hilos_por_fold,
      espacio = if (is.null(config$espacio)) "z495" else config$espacio,
      base_etapa = if (is.null(config$base_etapa)) "" else config$base_etapa,
      etiquetas = if (is.null(config$etiquetas)) "" else config$etiquetas
    ),
    output_file = paste0(config$bayesiana, ".md"),
    output_dir = here("exp", config$bayesiana),
    envir = new.env()
  )

  invisible(TRUE)
}

# -----------------------------------------------------------------------------
# correr_bayesiana_intrusos()
# Optimización bayesiana con partición 70/30 por registro y
# eliminación de intrusos (método "denicolay"):
# un modelo auxiliar entrenado en el 30 % predice el 70 % y se quitan
# de ese 70 % los CONTINUA con probabilidad mayor al umbral antes de
# optimizar (rmd/05_bayesiana_intrusos.Rmd). La limpieza solo afecta a la
# elección de hiperparámetros. El log queda en exp/<bayesiana>/BO_log.txt.
#
# Campos de config: bayesiana, descripcion, fe, meses_train,
# campos_excluidos, semilla, iteraciones y, opcionalmente, training_pct
# (70), undersampling (0.1), intrusos_umbral (0.5), aux_num_iterations
# (2180), aux_num_leaves (30), aux_min_sum_hessian_in_leaf (0.1141634103)
# y num_threads (8).
# -----------------------------------------------------------------------------
correr_bayesiana_intrusos <- function(config) {
  
  archivo_dataset <- construir_dataset_fe(config$fe$nombre, config$fe$pasos)
  
  opcional <- function(campo, defecto) {
    if (is.null(config[[campo]])) defecto else config[[campo]]
  }
  
  rmarkdown::render(
    input = here("rmd", "05_bayesiana_intrusos.Rmd"),
    params = list(
      bayesiana = config$bayesiana,
      descripcion = config$descripcion,
      dataset = basename(archivo_dataset),
      meses_train = config$meses_train,
      campos_excluidos = config$campos_excluidos,
      semilla = config$semilla,
      iteraciones = config$iteraciones,
      training_pct = opcional("training_pct", 70),
      undersampling = opcional("undersampling", 0.1),
      intrusos_umbral = opcional("intrusos_umbral", 0.5),
      aux_num_iterations = opcional("aux_num_iterations", 2180),
      aux_num_leaves = opcional("aux_num_leaves", 30),
      aux_min_sum_hessian_in_leaf = opcional("aux_min_sum_hessian_in_leaf", 0.1141634103),
      num_threads = opcional("num_threads", 8L)
    ),
    output_file = paste0(config$bayesiana, ".md"),
    output_dir = here("exp", config$bayesiana),
    envir = new.env()
  )
  
  invisible(TRUE)
}


# -----------------------------------------------------------------------------
# correr_etiquetador()
# Pseudoetiquetado: entrena un semillerío en los meses
# con clase real y asigna a los meses indicados la probabilidad promedio como
# clase artificial (rmd/10_etiquetador.Rmd). El resultado queda en
# exp/<etiquetador>/etiquetas.csv.gz (numero_de_cliente, foto_mes, prob) y se
# usa con el campo etiquetas de correr_bayesiana_cv() y correr_experimento().
#
# Campos de config: etiquetador, descripcion, fe, meses_train,
# meses_etiquetar, campos_excluidos, hiperparametros, semillas y,
# opcionalmente, num_threads.
# -----------------------------------------------------------------------------
correr_etiquetador <- function(config) {

  archivo_dataset <- construir_dataset_fe(config$fe$nombre, config$fe$pasos)

  rmarkdown::render(
    input = here("rmd", "10_etiquetador.Rmd"),
    params = list(
      etiquetador = config$etiquetador,
      descripcion = config$descripcion,
      dataset = basename(archivo_dataset),
      meses_train = config$meses_train,
      meses_etiquetar = config$meses_etiquetar,
      campos_excluidos = config$campos_excluidos,
      hiperparametros = config$hiperparametros,
      semillas = config$semillas,
      num_threads = if (is.null(config$num_threads)) NA else config$num_threads
    ),
    output_file = paste0(config$etiquetador, ".md"),
    output_dir = here("exp", config$etiquetador),
    envir = new.env()
  )

  invisible(TRUE)
}
