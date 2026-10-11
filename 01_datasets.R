# =============================================================================
# 01_datasets.R
# uso, desde la carpeta de la entrega: Rscript 01_datasets.R
# Construye FE026 a partir de datasets/competencia_01.csv.gz y lo graba en
# datasets/FE026.csv.gz, que es el archivo que lee la producción. La
# optimización bayesiana no lo lee: arma FE026 en memoria, como en la corrida
# original.
# Si el archivo ya existe, no se vuelve a construir.
# =============================================================================

source(file.path("R", "datasets.R"))

asegurar_FE026()
