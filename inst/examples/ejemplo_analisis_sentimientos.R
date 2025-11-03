# ============================================================
# Ejemplo de uso del paquete DiccionariosEmocion
# ============================================================

# Este ejemplo realiza un análisis de sentimientos sobre tuits
# almacenados en archivos Excel, aplicando procesamiento de texto
# y varios métodos de análisis léxico.

# 1. Carga del paquete
library(DiccionariosEmocion)

# 2. Definición de los parámetros
ruta_base <- "Data/DocMoreR/BambasEn_"  # Ruta base sin número ni extensión
indice_inicio <- 1
indice_fin <- 1
idioma_origen <- "en"
idioma_destino <- "en"
idioma_limpieza <- "english"
porciones <- 10
usar_traduccion <- FALSE  # Cambiar a TRUE si los textos están en otro idioma

# 3. Ejecución del análisis
resultados <- UnirExcelITFF_Optimizada(
  nombreBase = ruta_base,
  IndIni = indice_inicio,
  IndFin = indice_fin,
  IdiomaIni = idioma_origen,
  IdiomaFin = idioma_destino,
  IdiomaLimpieza = idioma_limpieza,
  valorPor = porciones,
  SiTrad = usar_traduccion
)

# 4. Exploración de resultados básicos
print("Vista previa de los tuits procesados:")
head(resultados$SoloTuit, 3)

print("Valores NRC por palabra:")
head(resultados$MatrizSimplificadaTokensValorados$MatrizTranspuesta, 3)

print("Sentimiento total por tuit (NRC):")
head(resultados$VectorSTextoSentimientos, 3)

# 5. Visualización rápida con ggplot2 (requiere tidyverse)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
  df_nrc <- data.frame(sentimiento = resultados$VectorSTextoSentimientos)
  ggplot(df_nrc, aes(x = sentimiento)) +
    geom_histogram(binwidth = 0.25, fill = "steelblue", color = "black") +
    labs(title = "Distribución de sentimiento NRC en tuits",
         x = "Valor agregado de sentimiento",
         y = "Frecuencia")
}

# 6. Exportar los resultados a CSV (opcional)
# guardarDataFrameCSV(resultados$UnionXLSX, "Resultados_Tuits_Analizados.csv")
