# Evaluación en junio

Código que midió el modelo en 202106: entrena el modelo de la entrega (FE026
con la configuración de mayor AUC de la optimización) con 202103 y 202104,
sin préstamos personales, con 20 semillas, y calcula la ganancia del
semillerío y de cada semilla para cada cantidad de envíos.
`curva_por_corte.R` calcula a partir de eso la ganancia promedio alrededor de
cada corte. E079 es el identificador de esta evaluación.

## Cómo se corre

Desde `evaluacion_junio/` (un archivo `.here` fija esta carpeta como raíz de
las rutas):

```bash
mkdir -p datasets exp/B018
cp <ruta>/competencia_01.csv.gz datasets/
cp ../modelo_A_FE026_B018/BO_log_corrido.txt exp/B018/BO_log.txt && touch exp/B018/COMPLETA
Rscript corridas/E079.R
Rscript curva_por_corte.R exp/E079/probabilidades_future.csv.gz exp/E079/curva_por_corte.csv
```

`corridas/E079.R` construye FE026 si no existe (`datasets/FE026.csv.gz`) y
deja en `exp/E079/` las ganancias por corte, la curva y las probabilidades
por semilla.

## Tiempo

Unos 12 minutos por semilla en una máquina virtual de 8 vCPU (20 semillas,
unas 4 horas), más la construcción de FE026.

## Archivos

| Archivo | Qué hace |
|---|---|
| `corridas/E079.R` | Lanza la evaluación: dataset FE026 y configuración de la optimización |
| `corridas/junio_fe026.R` | Construye FE026 si no existe y renderiza el Rmd con los parámetros de la evaluación |
| `corridas/junio_comun.R` | Esquema común: entrenamiento con 202103 y 202104, evaluación en 202106, 20 semillas, sin préstamos personales |
| `rmd/03_experimento.Rmd` | Entrena las 20 semillas y calcula la ganancia por corte del semillerío y de cada semilla |
| `R/evaluar_semillas_guardando.R`, `R/modelo.R`, `R/utils.R`, `R/experimentos.R` | Funciones que usa el Rmd |
| `R/pipeline.R`, `R/fe.R`, `R/fe_FE026.R` | Funciones que cargan los scripts de la evaluación; `R/fe.R` es una biblioteca general de transformaciones y contiene también funciones que este modelo no usa |
| `curva_por_corte.R` | Ganancia promedio alrededor de cada corte |
