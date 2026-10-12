# =============================================================================
# fe.R
# Feature engineering: transformaciones del dataset y construcción de
# datasets procesados, que se graban en datasets/ y se documentan en
# fe_specs/<nombre>.rds y .txt (por dataset, versionados en git) para
# poder reutilizarlos entre experimentos y reconstruirlos en otra máquina.
#
# Toda transformación de este archivo:
#   - recibe el dataset como primer argumento y lo devuelve transformado;
#   - no utiliza la clase, por lo que puede calcularse una sola vez sobre
#     todos los meses sin filtrar información del futuro.
# =============================================================================


# -----------------------------------------------------------------------------
# construir_dataset_fe()
# Devuelve la ruta del dataset procesado indicado. Si ya fue construido con
# la misma especificación, lo reutiliza; si no existe, lo construye aplicando
# los pasos en orden sobre el dataset base, lo graba y guarda su
# especificación en fe_specs/: un .rds, que se usa para comparar
# especificaciones de forma exacta, y un .txt legible.
#
#   nombre  identificador del dataset procesado
#   pasos   lista de pasos; cada paso es list(funcion = "nombre", args = list(...))
#           si es NULL, se devuelve el dataset base sin transformar
#   base    archivo de partida en datasets/
#
# Si ya existe un dataset con el mismo nombre pero con otra especificación,
# se detiene la ejecución para no mezclar resultados.
# -----------------------------------------------------------------------------
construir_dataset_fe <- function(nombre, pasos, base = "competencia_01.csv.gz") {

  if (is.null(pasos) || length(pasos) == 0) {
    return(here("datasets", base))
  }

  archivo <- here("datasets", paste0(nombre, ".csv.gz"))
  archivo_spec <- here("fe_specs", paste0(nombre, ".rds"))
  especificacion <- list(base = base, pasos = pasos)

  if (file.exists(archivo_spec)) {
    if (!identical(readRDS(archivo_spec), especificacion)) {
      stop("El dataset ", nombre, " ya está registrado con otra especificación. ",
           "Usar otro nombre.")
    }
    if (file.exists(archivo)) {
      message("Dataset ", nombre, " existente, se reutiliza")
      return(archivo)
    }
  } else if (file.exists(archivo)) {
    stop("Existe ", archivo, " sin especificación en fe_specs/. ",
         "Borrarlo para regenerarlo.")
  }

  message("Construyendo dataset ", nombre)
  dataset <- fread(here("datasets", base), stringsAsFactors = TRUE)

  for (paso in pasos) {
    message("  paso: ", paso$funcion)
    dataset <- do.call(paso$funcion, c(list(dataset = dataset), paso$args))
  }

  fwrite(dataset, archivo, sep = ",")

  # especificación: se graba solo si no existía (en otra máquina puede haberse
  # creado antes y llegado por git)
  if (!file.exists(archivo_spec)) {
    dir.create(here("fe_specs"), showWarnings = FALSE)
    saveRDS(especificacion, archivo_spec)
    writeLines(c(
      paste("nombre:", nombre),
      paste("fecha:", format(Sys.time(), "%Y-%m-%d %H:%M")),
      paste("filas:", nrow(dataset)),
      paste("columnas:", ncol(dataset)),
      "especificacion:",
      deparse(especificacion)
    ), here("fe_specs", paste0(nombre, ".txt")))
  }

  archivo
}


# -----------------------------------------------------------------------------
# fe_lags()
# Agrega, para cada atributo numérico, su valor en el registro previo del
# mismo cliente (lag) y, opcionalmente, la diferencia con ese valor (delta
# lag). Para un cliente que aparece por primera vez, o cuya historia no
# alcanza el orden pedido, el lag y el delta quedan en NA.
#
#   dataset  data.table con numero_de_cliente y foto_mes
#   ordenes  órdenes de lag a calcular, por ejemplo 1 o c(1, 2)
#   deltas   si es TRUE, agrega también los deltas de cada orden
#   excluir  columnas que no se transforman
#
# Nombres generados: <atributo>_lag<k> y <atributo>_delta<k>.
# -----------------------------------------------------------------------------
fe_lags <- function(dataset, ordenes = 1L, deltas = TRUE,
                    excluir = c("numero_de_cliente", "foto_mes", "clase_ternaria")) {

  columnas <- setdiff(names(dataset)[sapply(dataset, is.numeric)], excluir)

  setorder(dataset, numero_de_cliente, foto_mes)

  for (k in ordenes) {

    nombres_lag <- paste0(columnas, "_lag", k)

    # desplazamiento sobre todo el dataset ordenado; es mucho más rápido que
    # agrupar por cliente, y luego se anulan las filas donde el registro
    # desplazado pertenece a otro cliente
    dataset[, (nombres_lag) := shift(.SD, n = k, type = "lag"), .SDcols = columnas]

    otro_cliente <- dataset[, numero_de_cliente != shift(numero_de_cliente, n = k)]
    filas_invalidas <- which(is.na(otro_cliente) | otro_cliente)

    for (col in nombres_lag) {
      set(dataset, i = filas_invalidas, j = col, value = NA)
    }

    if (deltas) {
      nombres_delta <- paste0(columnas, "_delta", k)
      for (i in seq_along(columnas)) {
        set(dataset, j = nombres_delta[i],
            value = dataset[[columnas[i]]] - dataset[[nombres_lag[i]]])
      }
    }
  }

  dataset
}


# -----------------------------------------------------------------------------
# rank_cero_fijo()
# Transforma un vector numérico en posiciones relativas, conservando el cero
# como punto fijo: los valores positivos se reemplazan por su ranking
# normalizado en (0, 1], los negativos por su ranking normalizado en [-1, 0)
# (el más negativo vale -1), y los ceros y NA se mantienen.
# -----------------------------------------------------------------------------
rank_cero_fijo <- function(x) {
  r <- as.numeric(x)
  pos <- which(x > 0)
  neg <- which(x < 0)
  if (length(pos) > 0) r[pos] <- frank(x[pos], ties.method = "average") / length(pos)
  if (length(neg) > 0) r[neg] <- -frank(-x[neg], ties.method = "average") / length(neg)
  r
}


# -----------------------------------------------------------------------------
# fe_ranking_cero_fijo()
# Reemplaza los atributos indicados por su ranking con cero fijo, calculado
# dentro de cada foto_mes. Al trabajar con posiciones relativas dentro del
# mes, la transformación es inmune a la inflación, al aguinaldo y a los
# outliers, y preserva la señal de un cliente que baja su posición respecto
# del resto.
#
#   dataset  data.table con foto_mes
#   patron   expresión regular que selecciona los atributos a transformar;
#            por defecto, las variables monetarias (prefijo m, y Visa_m /
#            Master_m para las tarjetas)
#   excluir  columnas que no se transforman
# -----------------------------------------------------------------------------
fe_ranking_cero_fijo <- function(dataset, patron = "^m|^(Visa|Master)_m",
                                 excluir = c("numero_de_cliente", "foto_mes",
                                             "clase_ternaria")) {

  columnas <- grep(patron, names(dataset), value = TRUE)
  columnas <- setdiff(columnas[sapply(dataset[, ..columnas], is.numeric)], excluir)

  message("  ranking con cero fijo sobre ", length(columnas), " atributos")

  # conversión previa a double: el ranking no es entero y data.table no
  # cambia el tipo de una columna al asignar por grupo
  for (col in columnas) set(dataset, j = col, value = as.numeric(dataset[[col]]))

  dataset[, (columnas) := lapply(.SD, rank_cero_fijo),
          by = foto_mes, .SDcols = columnas]

  dataset
}


# -----------------------------------------------------------------------------
# fe_anti_imputacion()
# Detecta variables rotas y reemplaza sus valores espurios por NA. Una
# variable se considera rota en un mes cuando todos sus registros valen 0 en
# ese mes pero tiene valores distintos de 0 en otros meses: el patrón
# corresponde a una falla de carga del origen de datos, no a un
# comportamiento real de los clientes. Al pasar esos ceros a NA, el modelo
# los trata como ausencia de información y no como el valor 0.
#
# Debe aplicarse antes de calcular lags y deltas, para que los ceros
# espurios no generen variaciones falsas en los meses vecinos.
#
#   dataset  data.table con foto_mes
#   excluir  columnas que no se analizan
# -----------------------------------------------------------------------------
fe_anti_imputacion <- function(dataset,
                               excluir = c("numero_de_cliente", "foto_mes",
                                           "clase_ternaria")) {

  atributos <- setdiff(names(dataset)[sapply(dataset, is.numeric)], excluir)

  resumen <- rbindlist(lapply(atributos, function(v) {
    dataset[, list(variable = v,
                   todo_cero = any(!is.na(get(v))) && all(get(v) == 0, na.rm = TRUE),
                   con_datos = any(get(v) != 0, na.rm = TRUE)),
            by = foto_mes]
  }))

  variables_con_datos <- resumen[con_datos == TRUE, unique(variable)]
  rotas <- resumen[todo_cero == TRUE & variable %in% variables_con_datos,
                   list(variable, foto_mes)]

  message("  anti-imputación: ", nrow(rotas), " combinaciones variable-mes")

  for (k in seq_len(nrow(rotas))) {
    message("    ", rotas$variable[k], " en ", rotas$foto_mes[k])
    set(dataset,
        i = which(dataset$foto_mes == rotas$foto_mes[k]),
        j = rotas$variable[k],
        value = NA)
  }

  dataset
}


# -----------------------------------------------------------------------------
# fe_eliminar()
# Elimina del dataset las variables indicadas. Como paso de feature
# engineering, y no como exclusión al entrenar, evita además que se generen
# sus derivadas (lags, deltas) y reduce el tamaño del dataset procesado.
# Las variables inexistentes se ignoran y se informan.
#
#   dataset    data.table
#   variables  nombres de las columnas a eliminar
# -----------------------------------------------------------------------------
fe_eliminar <- function(dataset, variables) {

  inexistentes <- setdiff(variables, names(dataset))
  if (length(inexistentes) > 0) {
    message("  variables inexistentes, se ignoran: ",
            paste(inexistentes, collapse = ", "))
  }

  presentes <- intersect(variables, names(dataset))
  message("  eliminación de ", length(presentes), " variables")

  if (length(presentes) > 0) dataset[, (presentes) := NULL]

  dataset
}


# -----------------------------------------------------------------------------
# fe_ranking_variables()
# Reemplaza las variables indicadas por su ranking con cero fijo, calculado
# dentro de cada foto_mes (ver rank_cero_fijo). A diferencia de
# fe_ranking_cero_fijo(), que selecciona las variables por un patrón de
# nombre, recibe la lista explícita de variables, de modo que la selección
# queda registrada en la especificación del dataset.
#
#   dataset    data.table con foto_mes
#   variables  nombres de las columnas numéricas a transformar
# -----------------------------------------------------------------------------
fe_ranking_variables <- function(dataset, variables) {

  inexistentes <- setdiff(variables, names(dataset))
  if (length(inexistentes) > 0) {
    message("  variables inexistentes, se ignoran: ",
            paste(inexistentes, collapse = ", "))
  }

  columnas <- intersect(variables, names(dataset))
  columnas <- columnas[sapply(dataset[, ..columnas], is.numeric)]

  message("  ranking con cero fijo sobre ", length(columnas), " atributos")

  # conversión previa a double: el ranking no es entero y data.table no
  # cambia el tipo de una columna al asignar por grupo
  for (col in columnas) set(dataset, j = col, value = as.numeric(dataset[[col]]))

  dataset[, (columnas) := lapply(.SD, rank_cero_fijo),
          by = foto_mes, .SDcols = columnas]

  dataset
}


# -----------------------------------------------------------------------------
# variables_no_monetarias_a_rankear()
# Devuelve, ordenadas alfabéticamente, las variables numéricas no monetarias
# del dataset base con al menos min_valores valores distintos, excluidas las
# indicadas (identificadores y variables de calendario). Es el criterio de
# selección del ranking de variables no monetarias; al
# calcularse siempre sobre el dataset base, devuelve la misma lista en todas
# las corridas.
#
#   min_valores  cantidad mínima de valores distintos
#   excluir      variables que no se consideran
#   base         archivo del dataset base en datasets/
# -----------------------------------------------------------------------------
variables_no_monetarias_a_rankear <- function(
    min_valores = 20,
    excluir = c("numero_de_cliente", "foto_mes",
                "Master_fultimo_cierre", "Visa_fultimo_cierre",
                "Master_Finiciomora", "Visa_Finiciomora",
                "Master_Fvencimiento", "Visa_Fvencimiento",
                "Master_fechaalta", "Visa_fechaalta",
                "cliente_antiguedad", "cliente_edad"),
    base = "competencia_01.csv.gz") {

  dataset <- fread(here("datasets", base))
  numericas <- names(dataset)[sapply(dataset, is.numeric)]
  no_monetarias <- setdiff(
    grep("^m|^(Visa|Master)_m", numericas, value = TRUE, invert = TRUE),
    excluir
  )
  valores_distintos <- sapply(no_monetarias, function(v) uniqueN(dataset[[v]]))

  sort(names(valores_distintos)[valores_distintos >= min_valores])
}


# -----------------------------------------------------------------------------
# nombres_combinaciones()
# Devuelve los nombres de las variables que genera fe_combinaciones() para
# las variables y operaciones indicadas, en el mismo orden.
# -----------------------------------------------------------------------------
nombres_combinaciones <- function(variables,
                                  operaciones = c("suma", "resta", "producto", "cociente"),
                                  prefijo = "comb_", excluir_pares = NULL) {
  pares <- pares_a_combinar(variables, excluir_pares)
  unlist(lapply(pares, function(p) {
    paste0(prefijo, p[1], "__", operaciones, "__", p[2])
  }))
}


# -----------------------------------------------------------------------------
# pares_a_combinar()
# Pares de variables a combinar: todos los pares posibles de la lista, salvo
# los indicados en excluir_pares (sin importar el orden). excluir_pares es un
# vector de claves "a|b".
# -----------------------------------------------------------------------------
pares_a_combinar <- function(variables, excluir_pares = NULL) {
  pares <- combn(variables, 2, simplify = FALSE)
  if (length(excluir_pares) > 0) {
    clave <- function(p) paste(sort(p), collapse = "|")
    excluir <- sapply(strsplit(excluir_pares, "|", fixed = TRUE), clave)
    pares <- pares[!sapply(pares, clave) %in% excluir]
  }
  pares
}


# -----------------------------------------------------------------------------
# fe_combinaciones()
# Agrega combinaciones de a pares de las variables indicadas, con las
# operaciones suma, resta, producto y cociente: primera generación del
# algoritmo genético de generación de atributos.
# Para el cociente se usa un único orden (a / b), porque para un árbol a / b
# y b / a son equivalentes; las divisiones por cero quedan en NA.
# Se calcula sobre los valores del dataset en el momento de aplicarse (en
# general, los originales, antes de cualquier ranking).
#
#   dataset      data.table
#   variables    variables a combinar
#   operaciones    subconjunto de c("suma", "resta", "producto", "cociente")
#   prefijo        prefijo de los nombres generados
#   excluir_pares  pares que no se combinan, como claves "a|b" (opcional;
#                  por ejemplo, pares ya combinados en una generación previa)
#
# Nombres generados: <prefijo><a>__<operacion>__<b>.
# -----------------------------------------------------------------------------
fe_combinaciones <- function(dataset, variables,
                             operaciones = c("suma", "resta", "producto", "cociente"),
                             prefijo = "comb_", excluir_pares = NULL) {

  faltantes <- setdiff(variables, names(dataset))
  if (length(faltantes) > 0) stop("Variables inexistentes: ", paste(faltantes, collapse = ", "))

  pares <- pares_a_combinar(variables, excluir_pares)
  message("  combinaciones: ", length(pares), " pares x ", length(operaciones),
          " operaciones = ", length(pares) * length(operaciones), " variables")

  for (p in pares) {
    a <- as.numeric(dataset[[p[1]]])
    b <- as.numeric(dataset[[p[2]]])
    for (op in operaciones) {
      valor <- switch(op,
        suma = a + b,
        resta = a - b,
        producto = a * b,
        cociente = ifelse(b == 0, NA_real_, a / b)
      )
      valor[!is.finite(valor)] <- NA_real_
      set(dataset, j = paste0(prefijo, p[1], "__", op, "__", p[2]), value = valor)
    }
  }

  dataset
}


# -----------------------------------------------------------------------------
# fe_negocio()
# Agrega cocientes con interpretación de negocio. Deben calcularse antes de
# eliminar los límites de tarjeta (fe_eliminar), porque los usan:
#   neg_visa_uso_limite       consumo del mes en Visa / límite de compra Visa
#   neg_master_uso_limite     consumo del mes en Master / límite de compra Master
#   neg_visa_saldo_limite     saldo en pesos de Visa / límite de compra Visa
#   neg_master_saldo_limite   saldo en pesos de Master / límite de compra Master
#   neg_saldo_payroll         saldo total de cuentas / acreditación de haberes
# Los límites cambian de valor nominal entre meses; los cocientes son
# relativos y no identifican el mes. Las divisiones por cero quedan en NA.
# -----------------------------------------------------------------------------
fe_negocio <- function(dataset) {

  cociente <- function(a, b) {
    a <- as.numeric(a)
    b <- as.numeric(b)
    r <- ifelse(b == 0, NA_real_, a / b)
    r[!is.finite(r)] <- NA_real_
    r
  }

  dataset[, neg_visa_uso_limite := cociente(mtarjeta_visa_consumo, Visa_mlimitecompra)]
  dataset[, neg_master_uso_limite := cociente(mtarjeta_master_consumo, Master_mlimitecompra)]
  dataset[, neg_visa_saldo_limite := cociente(Visa_msaldopesos, Visa_mlimitecompra)]
  dataset[, neg_master_saldo_limite := cociente(Master_msaldopesos, Master_mlimitecompra)]
  dataset[, neg_saldo_payroll := cociente(mcuentas_saldo, mpayroll)]

  message("  variables de negocio: 5")
  dataset
}


# -----------------------------------------------------------------------------
# fe_payroll_binaria()
# Reemplaza la cantidad de acreditaciones de haberes (cpayroll_trx) por un
# indicador de si el cliente tuvo alguna acreditación en el mes (0 / 1).
# En los meses de aguinaldo los asalariados reciben dos acreditaciones, lo
# que cambia el significado de los valores de la cantidad (notebook z701);
# el indicador no se ve afectado. Debe aplicarse antes de fe_lags.
# -----------------------------------------------------------------------------
fe_payroll_binaria <- function(dataset, variables = "cpayroll_trx") {
  for (v in intersect(variables, names(dataset))) {
    set(dataset, j = v, value = as.integer(dataset[[v]] > 0))
  }
  message("  payroll binaria: ", paste(intersect(variables, names(dataset)), collapse = ", "))
  dataset
}


# -----------------------------------------------------------------------------
# fe_historicas()
# Variables históricas (promedios, máximos y mínimos a distintos
# horizontes, ratio_avg, ratio_max, tendencias) para las variables
# indicadas, a partir de los registros previos del mismo cliente (hasta
# 'ventana' registros, los que existan):
#   <v>_avg<k>   promedio de los registros previos
#   <v>_max<k>   máximo de los registros previos
#   <v>_min<k>   mínimo de los registros previos
#   <v>_ratioavg<k>  valor actual / promedio previo
#   <v>_ratiomax<k>  valor actual / máximo previo
#   <v>_tend<k>  valor actual - promedio previo
# Se calcula sobre los valores del dataset en el momento de aplicarse (en
# general, ya rankeados dentro de cada mes). Cocientes con denominador 0
# quedan en NA.
# -----------------------------------------------------------------------------
fe_historicas <- function(dataset, variables, ventana = 3L) {

  faltantes <- setdiff(variables, names(dataset))
  if (length(faltantes) > 0) stop("Variables inexistentes: ", paste(faltantes, collapse = ", "))

  setorder(dataset, numero_de_cliente, foto_mes)
  cliente <- dataset$numero_de_cliente

  # para cada desplazamiento k, filas cuyo registro desplazado es de otro cliente
  invalidas <- lapply(seq_len(ventana), function(k) {
    otro <- cliente != shift(cliente, n = k)
    which(is.na(otro) | otro)
  })

  cociente <- function(a, b) {
    r <- ifelse(b == 0, NA_real_, a / b)
    r[!is.finite(r)] <- NA_real_
    r
  }

  for (v in variables) {
    x <- as.numeric(dataset[[v]])
    previos <- sapply(seq_len(ventana), function(k) {
      p <- shift(x, n = k)
      p[invalidas[[k]]] <- NA_real_
      p
    })
    hay <- rowSums(!is.na(previos)) > 0
    promedio <- ifelse(hay, rowMeans(previos, na.rm = TRUE), NA_real_)
    # pmax / pmin con na.rm devuelven NA solo si todos los previos son NA
    maximo <- do.call(pmax, c(lapply(seq_len(ventana), function(k) previos[, k]), na.rm = TRUE))
    minimo <- do.call(pmin, c(lapply(seq_len(ventana), function(k) previos[, k]), na.rm = TRUE))

    set(dataset, j = paste0(v, "_avg", ventana), value = promedio)
    set(dataset, j = paste0(v, "_max", ventana), value = maximo)
    set(dataset, j = paste0(v, "_min", ventana), value = minimo)
    set(dataset, j = paste0(v, "_ratioavg", ventana), value = cociente(x, promedio))
    set(dataset, j = paste0(v, "_ratiomax", ventana), value = cociente(x, maximo))
    set(dataset, j = paste0(v, "_tend", ventana), value = x - promedio)
  }

  message("  variables históricas: ", length(variables), " variables x 6 = ",
          6 * length(variables), " nuevas (ventana ", ventana, ")")
  dataset
}

# -----------------------------------------------------------------------------
# fe_mecanismos()
# Variables de trayectoria que apuntan a los caminos hacia la baja
# identificados en la segmentación de bajas propia (desarme, vínculo débil,
# activo sin sueldo, anclado): pérdida de productos y servicios, tarjetas en
# cierre, cancelación de préstamos, aparición de la comisión de
# mantenimiento, deterioro de variables clave, descubierto creciente,
# inactividad sostenida y salida de fondos por transferencias.
# Se calcula sobre los valores crudos (debe ser el primer paso, antes de la
# anti-imputación y los rankings), con los 'ventana' registros previos del
# mismo cliente. Las variables generadas empiezan con "tr_" para que no las
# tomen los rankings de variables monetarias; deben excluirse de fe_lags.
# No usa la clase.
# -----------------------------------------------------------------------------
fe_mecanismos <- function(dataset, ventana = 3L) {

  setorder(dataset, numero_de_cliente, foto_mes)
  cli <- dataset$numero_de_cliente
  n <- nrow(dataset)

  # valor del registro k meses antes del mismo cliente (NA si no existe)
  previo <- function(x, k) {
    p <- shift(x, n = k)
    otro <- shift(cli, n = k)
    p[is.na(otro) | otro != cli] <- NA
    p
  }
  num <- function(v) if (v %in% names(dataset)) as.numeric(dataset[[v]]) else rep(NA_real_, n)
  previos <- function(x) lapply(seq_len(ventana), function(k) previo(x, k))
  max_previo <- function(x) suppressWarnings(do.call(pmax, c(previos(x), na.rm = TRUE)))
  # valor más antiguo disponible dentro de la ventana
  mas_antiguo <- function(x) {
    r <- rep(NA_real_, n)
    for (k in seq_len(ventana)) { p <- previo(x, k); r <- ifelse(is.na(p), r, p) }
    r
  }
  # meses consecutivos (incluido el actual) en que se cumple la condición
  racha <- function(cond) {
    r <- as.integer(cond %in% TRUE)
    acum <- r
    for (k in seq_len(ventana)) {
      ck <- previo(as.numeric(cond %in% TRUE), k)
      seguir <- acum == k & ck %in% 1
      acum <- acum + as.integer(seguir)
    }
    acum
  }

  # desarme: productos perdidos y servicios dados de baja
  cp <- num("cproductos")
  set(dataset, j = "tr_productos_perdidos", value = pmax(0, max_previo(cp) - cp))
  servicios <- intersect(c("ctarjeta_visa", "ctarjeta_master", "cpayroll_trx", "ccuenta_debitos_automaticos",
                           "ctarjeta_visa_debitos_automaticos", "cplazo_fijo", "cprestamos_personales",
                           "cseguro_vida", "cseguro_auto", "cseguro_vivienda", "ccaja_seguridad"), names(dataset))
  bajas <- Reduce(`+`, lapply(servicios, function(v) {
    x <- num(v) > 0
    as.integer(x %in% FALSE & max_previo(as.numeric(x)) %in% 1)
  }))
  set(dataset, j = "tr_servicios_dados_de_baja", value = bajas)
  set(dataset, j = "tr_visa_en_cierre", value = as.integer(num("Visa_status") >= 6))
  set(dataset, j = "tr_master_en_cierre", value = as.integer(num("Master_status") >= 6))

  # préstamos: cancelación en la ventana
  pr <- num("cprestamos_personales")
  set(dataset, j = "tr_prestamo_cancelado", value = as.integer(pr %in% 0 & max_previo(pr) > 0))

  # comisión de mantenimiento: pago, aparición y antigüedad del pago
  paga <- num("ccomisiones_mantenimiento") > 0
  set(dataset, j = "tr_paga_comision", value = as.integer(paga))
  set(dataset, j = "tr_comision_nueva", value = as.integer(paga %in% TRUE & max_previo(as.numeric(paga)) %in% 0))
  set(dataset, j = "tr_meses_pagando_comision", value = racha(paga))

  # deterioro: variables clave que cayeron respecto del registro más antiguo de la ventana
  clave <- intersect(c("ctrx_quarter", "mcuentas_saldo", "mcaja_ahorro", "mtarjeta_visa_consumo",
                       "chomebanking_transacciones", "cproductos"), names(dataset))
  set(dataset, j = "tr_variables_en_caida",
      value = Reduce(`+`, lapply(clave, function(v) { x <- num(v); as.integer(x < mas_antiguo(x)) %in% 1 })))
  saldo <- num("mcuentas_saldo")
  set(dataset, j = "tr_meses_saldo_en_caida", value = racha(saldo < previo(saldo, 1)))
  trx <- num("ctrx_quarter")
  set(dataset, j = "tr_meses_trx_en_caida", value = racha(trx < previo(trx, 1)))

  # descubierto: cuenta corriente negativa y empeorando
  cc <- num("mcuenta_corriente")
  set(dataset, j = "tr_descubierto_creciente", value = as.integer(cc < 0 & cc < previo(cc, 1)))
  set(dataset, j = "tr_meses_descubierto", value = racha(cc < 0))

  # inactividad sostenida: sin consumo con tarjeta y saldo no positivo
  consumo <- num("mtarjeta_visa_consumo") + ifelse(is.na(num("mtarjeta_master_consumo")), 0, num("mtarjeta_master_consumo"))
  set(dataset, j = "tr_meses_sin_consumo", value = racha(consumo <= 0))
  set(dataset, j = "tr_meses_inactivo", value = racha(consumo <= 0 & saldo <= 0))

  # salida de fondos: transferencias emitidas sobre recibidas y su cambio
  emi <- num("mtransferencias_emitidas"); rec <- num("mtransferencias_recibidas")
  ratio <- emi / (pmax(rec, 0) + 1)
  set(dataset, j = "tr_ratio_emitidas_recibidas", value = ratio)
  set(dataset, j = "tr_delta_ratio_transferencias", value = ratio - previo(ratio, 1))

  message("  variables de mecanismo: ", sum(startsWith(names(dataset), "tr_")),
          " (ventana ", ventana, "; servicios considerados: ", length(servicios), ")")
  dataset
}

# nombres que genera fe_mecanismos(), en el mismo orden
nombres_mecanismos <- function() {
  c("tr_productos_perdidos", "tr_servicios_dados_de_baja", "tr_visa_en_cierre", "tr_master_en_cierre",
    "tr_prestamo_cancelado", "tr_paga_comision", "tr_comision_nueva", "tr_meses_pagando_comision",
    "tr_variables_en_caida", "tr_meses_saldo_en_caida", "tr_meses_trx_en_caida",
    "tr_descubierto_creciente", "tr_meses_descubierto", "tr_meses_sin_consumo", "tr_meses_inactivo",
    "tr_ratio_emitidas_recibidas", "tr_delta_ratio_transferencias")
}

# -----------------------------------------------------------------------------
# fe_lags_variables()
# Lags y deltas de los órdenes indicados solo para una lista de variables
# (fe_lags trabaja sobre todas las variables salvo las excluidas). Pensada
# para dar historia a las combinaciones del genético, que fe_lags excluye.
# El lag de orden k es el valor del registro k posiciones antes del mismo
# cliente (por foto_mes); el delta es el valor actual menos el lag.
# -----------------------------------------------------------------------------
fe_lags_variables <- function(dataset, variables, ordenes = 1L, deltas = TRUE) {

  faltantes <- setdiff(variables, names(dataset))
  if (length(faltantes) > 0) stop("Variables inexistentes: ", paste(faltantes, collapse = ", "))

  setorder(dataset, numero_de_cliente, foto_mes)
  for (k in ordenes) {
    nombres_lag <- paste0(variables, "_lag", k)
    dataset[, (nombres_lag) := shift(.SD, k, NA, "lag"), by = numero_de_cliente, .SDcols = variables]
    if (deltas) {
      for (v in variables) {
        set(dataset, j = paste0(v, "_delta", k), value = dataset[[v]] - dataset[[paste0(v, "_lag", k)]])
      }
    }
  }
  message("  lags", if (deltas) " y deltas", " de orden ", paste(ordenes, collapse = ", "),
          " para ", length(variables), " variables")
  dataset
}

# -----------------------------------------------------------------------------
# fe_lags_mes()
# Lags y deltas por mes calendario: el lag de orden k es el valor del mismo
# cliente exactamente k meses antes (si ese mes el cliente no está, queda NA).
# Corrige el caso de clientes que faltan un mes y vuelven, en el que fe_lags
# toma el registro anterior aunque no sea el del mes anterior.
# Se aplica a todas las variables numéricas salvo las excluidas.
# -----------------------------------------------------------------------------
fe_lags_mes <- function(dataset, ordenes = 1L, deltas = TRUE,
                        excluir = c("numero_de_cliente", "foto_mes", "clase_ternaria")) {

  variables <- setdiff(names(dataset)[sapply(dataset, is.numeric)], excluir)
  dataset[, periodo_tmp := as.integer(foto_mes %/% 100) * 12L + as.integer(foto_mes %% 100)]
  for (k in ordenes) {
    nombres_lag <- paste0(variables, "_lag", k)
    previo <- dataset[, c("numero_de_cliente", "periodo_tmp", variables), with = FALSE]
    previo[, periodo_tmp := periodo_tmp + k]
    setnames(previo, variables, nombres_lag)
    dataset[previo, on = c("numero_de_cliente", "periodo_tmp"), (nombres_lag) := mget(paste0("i.", nombres_lag))]
    rm(previo)
    if (deltas) {
      for (v in variables) set(dataset, j = paste0(v, "_delta", k), value = dataset[[v]] - dataset[[paste0(v, "_lag", k)]])
    }
  }
  dataset[, periodo_tmp := NULL]
  setorder(dataset, numero_de_cliente, foto_mes)
  message("  lags", if (deltas) " y deltas", " por mes calendario de orden ", paste(ordenes, collapse = ", "),
          " para ", length(variables), " variables")
  dataset
}

# -----------------------------------------------------------------------------
# fe_agregar_columnas()
# Agrega columnas de otro dataset ya construido (en datasets/), unidas por
# numero_de_cliente y foto_mes. Pensada para sumar al crudo las combinaciones
# del genético y las variables de negocio.
# -----------------------------------------------------------------------------
fe_agregar_columnas <- function(dataset, archivo, patron) {

  ruta <- here::here("datasets", archivo)
  if (!file.exists(ruta)) stop("No existe ", ruta, "; construirlo antes")
  todas <- names(fread(ruta, nrows = 0))
  columnas <- grep(patron, todas, value = TRUE)
  columnas <- setdiff(columnas, names(dataset))
  extra <- fread(ruta, select = c("numero_de_cliente", "foto_mes", columnas))
  n_antes <- nrow(dataset)
  dataset[extra, on = c("numero_de_cliente", "foto_mes"), (columnas) := mget(paste0("i.", columnas))]
  stopifnot(nrow(dataset) == n_antes)
  message("  columnas agregadas desde ", archivo, ": ", length(columnas))
  dataset
}
