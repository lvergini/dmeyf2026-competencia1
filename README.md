# Competencia 1 — Entrega reproducible

Predicción de bajas (BAJA+2) de 202108. La entrega es un modelo LightGBM
entrenado sobre el dataset FE026, en semillerío: el mismo modelo entrenado
con muchas semillas y promediado por cliente. Se envían los 11.750 clientes
con mayor probabilidad.

Todo lo necesario para reproducir el archivo final está en esta carpeta y
parte de `competencia_01.csv.gz`. Los comandos se ejecutan desde la carpeta
de la entrega.

Los nombres que aparecen en carpetas y archivos son identificadores de cada
paso: FE026 es el dataset, B018 la optimización bayesiana de los
hiperparámetros y P069 la producción (el entrenamiento final, semilla por
semilla).

## Contenido

```
README.md
renv.lock                 versiones de los paquetes
R/
  parametros.R            semilla, parámetros fijos de LightGBM, rangos de la bayesiana, exclusión de préstamos
  datasets.R              construcción y grabado de FE026
  produccion.R            entrenamiento final y predicción de 202108
01_datasets.R             graba datasets/FE026.csv.gz
modelo_A_FE026_B018/
  01_bayesiana.R          optimización bayesiana
  02_produccion.R         producción, una semilla por vez
  semillas_A.csv          semillas que entran en la entrega
  BO_log_corrido.txt      log de la optimización que se usó
02_prediccion_final.R     semillerío, corte y archivo final
evaluacion_junio/         evaluación en 202106 con la que se eligió el corte (no interviene en el archivo final)
```

## Dataset FE026

Se construye a partir de `competencia_01.csv.gz` (983.061 registros, 152
variables, meses 202103 a 202108) sin eliminar registros, meses ni
variables. La clase se toma como binaria: BAJA+1 y BAJA+2 son positivos.
FE026 tiene 763 columnas:

1. `ccajas_depositos` pasa a NA en 202105, donde el campo está roto, antes de
   calcular lags para que no hereden el valor erróneo.
2. Lag y delta lag de orden 1 y 2 de cada variable, por mes calendario: el
   lag de orden k es el valor del cliente k meses antes, y queda en NA si el
   cliente no estaba ese mes.
3. Ranking con cero fijo, por variable y dentro de cada mes, de todas las
   columnas salvo `numero_de_cliente`, `foto_mes` y `clase_ternaria`: los
   positivos pasan a su ranking normalizado en (0, 1], los negativos a
   [-1, 0), el cero queda en cero, sin entrar en ninguno de los dos
   rankings, y NA queda NA. El ranking reemplaza al valor original, lo que
   hace comparables las variables entre meses y atenúa el efecto de la
   inflación.

## Bayesiana

Los hiperparámetros salen de una optimización bayesiana con pocas
evaluaciones sobre un conjunto chico de hiperparámetros.

- Entrena con 202103 y 202104 completos y valida con 202106. 202105 no se
  usa, para dejar el mismo hueco de un mes que hay entre los datos de
  entrenamiento final y 202108.
- Sin undersampling. Métrica: AUC en validación.
- Optimización bayesiana con mlr3mbo (modelo sustituto kriging, mejora
  esperada), 10 evaluaciones pedidas; el diseño inicial al azar es de 12, así
  que el log tiene 12 evaluaciones.
- Parámetros fijos (definidos en `R/parametros.R`): gbdt, `max_bin` 31,
  `min_data_in_leaf` 0, `feature_fraction` 0,5, `feature_fraction_bynode`
  0,2, `learning_rate` 0,005, sin regularización ni bagging,
  `deterministic = TRUE`, 8 hilos.
- Hiperparámetros optimizados: `num_iterations` (64 a 4.096), `num_leaves`
  (16 a 2.048) y `min_sum_hessian_in_leaf` (1e-6 a 0,1).
- `mprestamos_personales`, `cprestamos_personales` y todos sus lags y deltas
  se excluyen en la optimización. En la producción se usan todas las
  variables.

Se usa la configuración de mayor AUC del log: evaluación 11, AUC 0,92064,
2.786 árboles, 1.080 hojas y `min_sum_hessian_in_leaf` 0,0473.

## Cómo se eligieron los hiperparámetros y el corte

Todas las decisiones se tomaron con modelos entrenados con 202103 y 202104 y
evaluados en 202106, un mes que no interviene en ese entrenamiento. 202105 no
se usa, para reproducir el hueco de un mes que hay entre los datos de
entrenamiento final y 202108.

- **Hiperparámetros:** la optimización bayesiana descripta arriba entrena con
  202103 y 202104 y mide el AUC en 202106.
- **Corte:** el modelo, con esos hiperparámetros, se entrenó con 202103 y
  202104 con 20 semillas, sin préstamos personales. Se calculó la ganancia en
  202106 para cada cantidad de envíos (+1.072.500 por cada BAJA+2 y −27.500
  por cada envío), tanto del semillerío como de cada semilla. Como la curva
  es irregular (cada baja suma de golpe), se usó la curva suavizada: el
  promedio de la ganancia en ±500 envíos alrededor de cada corte. El corte se
  ubicó en el centro de la zona más alta de esa curva.

El código de esta evaluación está en `evaluacion_junio/`. No interviene en la
producción del archivo final.

## Requisitos

- R 4.6.1 (versión con la que corrió el modelo) y los paquetes de
  `renv.lock` (entre otros, lightgbm 4.7.0, data.table 1.18.6.1, mlr3mbo
  1.2.1). El archivo registra R 4.6.0 porque se generó en otra máquina. Para
  instalar los paquetes:

  ```bash
  Rscript -e 'install.packages("renv"); renv::restore(lockfile = "renv.lock")'
  ```

- 8 hilos para LightGBM (fijo en el código).
- El modelo se entrenó en máquinas virtuales de Google Cloud e2-highmem-8
  (8 vCPU, 64 GB de memoria).
- `competencia_01.csv.gz` en `datasets/` dentro de esta carpeta:

  ```bash
  mkdir -p datasets && cp <ruta>/competencia_01.csv.gz datasets/
  ```

- Disco: unos 4,5 GB. `competencia_01.csv.gz` ocupa 0,15 GB y
  `FE026.csv.gz`, medido en una máquina virtual, 4,3 GB: se graba sin
  comprimir aunque termine en `.gz` (ver "1. Dataset"). Las predicciones por
  semilla ocupan poco.

## Orden de corrida

`00_pipeline.R` ejecuta toda la entrega en orden, cada paso en un proceso de
R aparte, y se detiene con un mensaje que indica qué paso falló; la salida de
cada paso queda en `exp/registros/`. Por defecto usa el log de la
optimización de la entrega (`BO_log_corrido.txt`) y corre las sublistas de
la producción una después de otra. Con `--paralelo` corre las sublistas a la
vez, cada una en su propio proceso, y con `--bayesiana` corre además la
optimización completa:

```bash
Rscript 00_pipeline.R
Rscript 00_pipeline.R --paralelo
Rscript 00_pipeline.R --bayesiana
```

Casi todo el tiempo es de la producción: entre 24 y 27 minutos por semilla
si las sublistas corren en secuencia; con `--paralelo`, el de la sublista
más larga, siempre que la máquina tenga hilos y memoria para todas a la vez
(cada sublista usa 8 hilos y carga el dataset completo). Para 100 semillas,
son unas 40 horas en secuencia y unas 10 con `--paralelo` y cuatro
sublistas, si cada sublista dispone de 8 hilos propios, como en la
producción original, donde cada sublista corrió en una máquina de 8 vCPU.
A eso se suman unos minutos de dataset y de predicción final, y entre 1 h
15 min y 1 h 22 min con `--bayesiana`. Los pasos siguientes describen lo que hace cada etapa.

Los tiempos son los medidos en las máquinas virtuales, a partir de los
registros de las corridas. Donde solo hay una cota, se indica.

### 1. Dataset

```bash
Rscript 01_datasets.R
```

Graba `datasets/FE026.csv.gz`, que lee la producción. La optimización
bayesiana no lo lee: arma FE026 en memoria, como en la corrida original. El
archivo queda sin comprimir aunque su nombre termine en `.gz`: se escribe
primero como `.parcial` (`fwrite` comprime solo si el nombre termina en
`.gz`) y después se renombra; `fread` lo lee igual. Así se grabó también en
la producción original. Tiempo: armar FE026 en memoria tomó menos de 7
minutos (cota que incluye además la primera evaluación de la bayesiana); la
grabación no se midió por separado.

### 2. Bayesiana

```bash
Rscript modelo_A_FE026_B018/01_bayesiana.R
```

Deja su log en `exp/B018/BO_log.txt` y un archivo `COMPLETA` al terminar.
Tiempo: entre 1 h 15 min y 1 h 22 min.

`BO_log_corrido.txt` es el log de la optimización que se usó para la
entrega. Para saltear este paso y producir con exactamente la misma
configuración:

```bash
mkdir -p exp/B018 && cp modelo_A_FE026_B018/BO_log_corrido.txt exp/B018/BO_log.txt && touch exp/B018/COMPLETA
```

### 3. Producción

El modelo se entrena con 202103 a 202106 y predice 202108 una vez por
semilla. Las semillas son los números primos de seis cifras en un orden
aleatorio fijo (`set.seed(173207)`), repartidos en cuatro sublistas: la
sublista k tiene las posiciones k, k + 4, k + 8, ... de esa lista. En la
producción original cada sublista corrió en su propio proceso de R, que arma
el dataset de LightGBM una vez y entrena sus semillas en orden, de la primera
en adelante. La entrega reproduce ese esquema: si LightGBM usara la semilla
del primer entrenamiento para construir los bins del dataset, el orden y la
separación en procesos cambiarían el resultado.

`semillas_A.csv` indica, para cada semilla que entra en la entrega, su
sublista, su posición y el proceso en que se entrenó (un número mayor que 1
indica que la producción se reinició en esa sublista). Las sublistas y
procesos presentes se listan con:

```bash
Rscript -e 'data.table::fread("modelo_A_FE026_B018/semillas_A.csv")[, .N, by = .(sublista, proceso)]'
```

Para cada par (sublista, proceso) de la lista se corre un `Rscript` aparte;
por ejemplo, sublista 1, proceso 1:

```bash
Rscript modelo_A_FE026_B018/02_produccion.R 1 1
```

Las sublistas son independientes entre sí y pueden correr en paralelo, en una
o varias máquinas (cada una necesita el dataset y el log de la bayesiana).
Cada semilla queda en `exp/P069/semillas/prob_<semilla>.csv.gz`; si un
proceso se interrumpe, al relanzarlo saltea las semillas ya grabadas.
Tiempo: entre 24 y 27 minutos por semilla. Con las cuatro sublistas en
paralelo, la producción completa lleva del orden de (cantidad de semillas
/ 4) × 25 minutos; por ejemplo, unas 10 horas para 90 semillas.

### 4. Predicción final

```bash
Rscript 02_prediccion_final.R
```

Lee todas las semillas, descarta las que tienen predicciones idénticas a
otra semilla (ocurre cuando dos semillas generan la misma semilla interna de
LightGBM), promedia el resto por cliente y ordena los clientes de mayor a
menor probabilidad. Tarda unos 3 minutos (medido en una computadora
personal, con 83 semillas).

## Archivo final

```
exp/entregas/<fecha_hora>_P069_prob/P069_prob_11750.csv
```

Tiene los `numero_de_cliente` de los 11.750 clientes con mayor probabilidad,
uno por línea, sin encabezado. En la misma carpeta quedan
`probabilidades.csv.gz` (probabilidad de cada semilla y del semillerío, por
cliente) y `resumen.txt` (semillas usadas y descartadas). La carpeta lleva la
fecha y la hora de la corrida y nunca se sobrescribe.

## Semillas incluidas

| Semillas en `semillas_A.csv` | Descartadas por predicciones idénticas | Usadas |
|---|---|---|
| 107 (4 sublistas, 7 procesos) | 1 (694327, idéntica a 316817) | 106 |

## Verificación

Se verificó:

- Dataset: FE026 construido con esta carpeta desde `competencia_01.csv.gz`
  tiene la misma huella md5 que el usado en la producción
  (175c5c33f47da426afeb25fdb29ce1c6).
- Producción: en la misma máquina virtual que produjo la sublista 1, las dos
  primeras semillas de esa sublista, entrenadas con esta carpeta y el log de
  la optimización, dieron probabilidades idénticas byte a byte a las de la
  producción original.
- Lista de semillas: con el entrenamiento reemplazado por una simulación, la
  producción entrena exactamente las semillas de cada sublista y proceso, en
  orden, y se detiene si la lista no coincide con la sublista.
- Predicción final: `02_prediccion_final.R` sobre las predicciones de la
  producción da un archivo idéntico byte a byte al generado con el
  procedimiento original.

LightGBM es determinístico con `deterministic = TRUE`, `force_row_wise = TRUE`
y una cantidad fija de hilos, pero en un procesador distinto puede dar
diferencias mínimas. La reproducción exacta se verificó en máquinas virtuales
e2-highmem-8.
