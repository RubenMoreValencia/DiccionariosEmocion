resultados <- UnirExcelITFF_Optimizada(nombreBase = "Data/DocMoreR/GPT_BBC_",
                                       IndIni = 1,
                                       IndFin = 1,
                                       IdiomaIni = "en",
                                       IdiomaFin = "en",
                                       IdiomaLimpieza = "english",
                                       valorPor = 10,
                                       SiTrad = FALSE)
mvxm<-MatrizValoradaXMetodo(resultados$MatrizSimplificadaTokensValorados$MatrizCol0,FALSE)
dxM<-resultados$VectorTextoSentimientosNRC
dataR<-resultados$MatrizSimplificadaTokensValorados
dx<-dataR$MatrizTranspuestaSinDup
dxM<-as.matrix(dxM)
dxM<-as.data.frame(dxM)
dx<-as.matrix(dx)
dx<-as.data.frame(dx)
df1<-interseccionDataFrames(dxM,dx)
p<-TraduceA(df1$NombreFilas, idioma_destino = "español")
dfx<-modificarNombresFilas(df1$TablaCompletaNRC,p)
head(dfx)
dfx


# 3) Ejecuta de nuevo
dfc <- cbind(dfx[, c(10:14)], clase = factor(ifelse(dfx$positivo > 0, "pos", "no_pos")))
rmMC <- resModeloCluster(dfc, indiceClase = ncol(dfc), vectorClase = dfc$clase)
rmMC$graficaCluster
rmMC$graficaDend


rmMC$graficaCluster
CrearGraficoSentimiento(resultados$VectorSTextoSentimientos, "Behavioural Assessment for Comments")
tcos<-rmMC$modelo$ind$coord
tg<-GenerarDataframeT(tcos,"DimMax","DimMin")
tg[1]
tg[2]
tg[3]
tg[4]
tg[5]
dtf<-as.data.table(tg[1])
dtf
rmMC$graficaDend
rmMC$graficaInd
rmMC$graficaCompleta
rmMC$graficaBiplot2
rmMC$graficaBiplot

resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup
rmMC$graficaCluster

out <- visualizar_curvas_sentimiento(resultados, metodos = c("syuzhet","bing","afinn","nrc"))
print(out$grafico)
out$resumen_estadistico


# Básico (porcentaje)
out <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC, idioma = "es")
print(out$grafico_emociones)
out$tabla_emociones

# Mostrar también polaridades y top 6 emociones, en frecuencias
out2 <- visualizar_emociones_nrc(
  resultados$VectorTextoSentimientosNRC,
  normalizar = "frecuencia",
  top_n = 6,
  mostrar_polaridades = TRUE
)
print(out2$grafico_emociones)
print(out2$grafico_polaridades)

# Con función de traducción propia
traducir_emociones <- function(x, idioma="es") {
  dic <- c(anger="ira", anticipation="anticipación", disgust="disgusto", fear="miedo",
           joy="alegría", sadness="tristeza", surprise="sorpresa", trust="confianza",
           positive="positivo", negative="negativo")
  unname(ifelse(x %in% names(dic), dic[x], x))
}
out3 <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC,
                                 idioma="es", funcion_traduccion=traducir_emociones,
                                 normalizar="porcentaje")
print(out3$grafico_emociones)


mc <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup
# Básico, 10 por lado, todos los métodos numéricos
out <- analizar_tokens_extremos(mc)
print(out$grafico)
head(out$tabla_combinada)
# Solo algunos métodos + escalamiento z-score + facetas
out2 <- analizar_tokens_extremos(mc, n = 12, metodos = c("syuzhet","bing","afinn","nrc"),
                                 scale_method = "zscore", facet = TRUE)
print(out2$grafico)
# Con traducción de tokens al español (si tienes un traductor)
traducir <- function(x, idioma="es") x  # reemplaza por tu función
out3 <- analizar_tokens_extremos(mc, n = 8, funcion_traduccion = traducir_emociones, idioma = "es",
                                 scale_method = "range01", print_tables = FALSE)
print(out3$grafico)


traducir_emociones <- function(vec, idioma = "es") {
  # `idioma` puede ser "es" o "español"; lo reenviamos como lo espera TraduceA
  TraduceA(vec, idioma_destino = idioma)
}

out <- visualizar_emociones_nrc(
  resultados$VectorTextoSentimientosNRC,
  idioma = "es",
  funcion_traduccion = traducir_emociones,
  normalizar = "porcentaje",
  mostrar_polaridades = TRUE
)
print(out$grafico_emociones)

traducir_tokens <- function(vec, idioma = "es") TraduceA(vec, idioma_destino = idioma)

mc <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup

out2 <- analizar_tokens_extremos(
  mc,
  n = 12,
  idioma = "es",
  funcion_traduccion = traducir_tokens,
  metodos = c("syuzhet","bing","afinn","nrc"),
  scale_method = "zscore",
  facet = TRUE
)
print(out2$grafico)


# Heatmap normalizado por proporción y orden jerárquico
v1 <- visualizar_coocurrencia_nrc(cooc,
                                  tipo = "heatmap",
                                  normalizar = "prop",
                                  ordenar = "hclust")
print(v1$grafico)

# Red con filtro y top de aristas

v2 <- visualizar_coocurrencia_nrc(cooc,
                                  tipo = "red",
                                  normalizar = "max",
                                  min_edge = 0.05,
                                  top_edges = 50,
                                  layout_red = "fr")
print(v2$grafico)

v2c <- visualizar_coocurrencia_nrc_v2(
  cooc,
  tipo = "red",
  normalizar = "max",
  min_edge = 0.05,
  top_edges = 60,
  color_nodes = "community",
  community_method = "louvain",
  layout_red = "kk",
  seed = 123
)
print(v2c$grafico)


# Supongamos que quieres PCA solo de emociones NRC (columnas numéricas):
emo_cols <- c("ira","anticipación","disgusto","miedo","alegría","tristeza","sorpresa","confianza")
acdfx_emoc <- acdfx[, emo_cols]

# Entrenar PCA
pca <- prcomp(acdfx_emoc, center = TRUE, scale. = TRUE)

# Etiquetas por alguna categoría existente (p.ej., acdfx$categoria)
clases <- acdfx$categoria

# Graficar
g <- graficar_pca_emociones(
  pca = pca,
  data = acdfx_emoc,
  labels = clases,
  componentes = c(1, 2),
  titulo = "PCA de emociones",
  palette = "Dark2",
  label_points = TRUE,
  ellipse = TRUE,
  ellipse_type = "norm",
  ellipse_level = 0.95,
  top_n_loadings = 10,
  loading_scale = 1.2
)

print(g$scatter)
print(g$elipses)
print(g$cargas)     # si usaste 2 componentes
head(g$data_proyectada)


# 1) Con tu salida de agregación (ejemplo simple)
df_agru <- data.frame(positivo = 120, negativo = 80, activacion = 60)
res <- graficar_emociones_agrupadas(df_agru,
                                    normalizar = "percent",
                                    ordenar = "desc",
                                    titulo = "Perfil emocional (porcentaje)")
print(res$grafico)

# 2) Con vector con nombres
v <- c(positivo = 40, negativo = 25, activacion = 35)
print(graficar_emociones_agrupadas(v, normalizar="prop")$grafico)

pca <- prcomp(acdfx_emoc, center = TRUE, scale. = TRUE)

res <- graficar_pca_emociones_etiquetas(
  pca, acdfx_emoc,
  labels_cluster = acdfx$categoria,
  labels_texto = rownames(acdfx_emoc),
  palette = "Dark2",
  top_n_loadings = 10,
  loading_scale = 1.1,
  biplot = TRUE,          # ← activa el combinado
  biplot_alpha = 0.9      # ← controla el auto-escalado de cargas
)

print(res$scatter)
print(res$elipses)
print(res$cargas)
print(res$biplot)

# Selecciona solo emociones NRC (ajusta nombres según tu objeto)
emo_cols <- c("ira","anticipación","disgusto","miedo","alegría","tristeza","sorpresa","confianza")
acdfx_emoc <- acdfx[, emo_cols]

# PCA con escalado y centrado; omite filas con NA
res_pca <- generar_pca_emocional(acdfx_emoc, escalar = TRUE, center = TRUE, na_action = "omit")

# Varianza explicada
print(res_pca$var_exp)

# Integración con tus gráficos (función que ya tienes)
g <- graficar_pca_emociones_etiquetas(
  pca = res_pca$pca,
  data = acdfx_emoc,
  labels_cluster = acdfx$categoria,
  labels_texto = rownames(acdfx_emoc),
  palette = "Dark2",
  top_n_loadings = 10,
  biplot = TRUE
)
print(g$scatter); print(g$elipses); print(g$cargas); print(g$biplot)

comentarios <- resultados$TextoSentencias
lexicon     <- as.data.frame(resultados$VectorTextoSentimientosNRC)

out <- pipeline_emociones(
  comentarios = comentarios,
  lexicon = lexicon,
  k = 3,
  resumen_fun = "sum",
  seed = 42,
  palette = "Dark2"
)

# Inspecciones
out$metricas$oov_rate
head(out$matriz_emocional)
out$var_exp

# Gráficos
print(out$graficos$scatter)
print(out$graficos$elipses)
print(out$graficos$biplot)

viz <- visualizar_emociones_comentarios(
  pca = out$pca,
  matriz_emocional = out$matriz_emocional,
  etiquetas = rownames(out$matriz_emocional),
  cols_emociones = out$cols_emociones,
  palette = "Dark2",
  biplot = TRUE,
  barras_modo = "porcentaje",
  label_max_overlaps = 500  # si quieres menos avisos
)

print(viz$pca$scatter)
print(viz$pca$elipses)
print(viz$pca$biplot)
print(viz$barras)

# 1) Tokens por comentario (ya tienes esto)
# lista_de_tokens <- lapply(resultados$TextoSentencias, tokenizar_basico)

# 2) Matriz valorada: usa las filas = tokens y columnas = lexicones o emociones
matriz <- MVM$MatrizTranspuestaSinDup  # de tu salida MVM

# 3) Resumen “wide” y también “long”
out <- resumir_emociones_por_comentario(
  lista_tokens   = lista_de_tokens,
  matriz_valores = matriz,
  columnas       = c("syuzhet","bing","afinn","nrc"), # o deja NULL para auto
  normalizar     = "none",        # "none" | "row_sum" | "zscore"
  return_long    = TRUE
)

head(out$resumen)       # una fila por comentario, columnas = syuzhet/bing/afinn/nrc
head(out$resumen_long)  # Comentario, Variable, Valor (útil para ggplot)

# 1) Consistencia + emociones (global)
out <- analizar_consistencia_lexicos_v2(
  mc    = mc,
  acdfx = acdfx,
  metodos = c("syuzhet","bing","afinn","nrc"),
  segmentar_por = "categoria",   # o NULL si no quieres segmentar
  mostrar_pie = TRUE
)


# 1) Consistencia + emociones (global)
out <- analizar_consistencia_lexicos_v2(
  mc    = mc,
  acdfx = acdfx,
  metodos = c("syuzhet","bing","afinn","nrc"),
  segmentar_por = "categoria",   # o NULL si no quieres segmentar
  mostrar_pie = TRUE
)

# Indicadores y tablas
out$indicadores
out$resumen_categorias
out$perfil_emociones_por_consistencia

# Gráficos
print(out$grafico_barras_consistencia)
print(out$grafico_pie_consistencia)
print(out$grafico_emociones_vs_consistencia)

# Segmentación propositiva (si segmentar_por no es NULL)
out$segmentacion$indicadores_segmentados

# Asegura que los rownames sean iguales
rownames(acdfx) <- rownames(mc)

# Fusionar ambos data frames
acdfx_ext <- cbind(acdfx, CategoriaLexico = out_mc$clasificacion_por_token$categoria)
head(acdfx_ext)

library(dplyr)
emo_cols <- c("ira","anticipación","disgusto","miedo","alegría","tristeza","sorpresa","confianza")

res_emo <- acdfx_ext %>%
  group_by(CategoriaLexico) %>%
  summarise(across(all_of(emo_cols), mean, na.rm = TRUE)) %>%
  mutate(across(all_of(emo_cols), ~ round(.x * 100, 1)))

print(res_emo)





# Indicadores y tablas
out$indicadores
out$resumen_categorias
out$perfil_emociones_por_consistencia

# Gráficos
print(out$grafico_barras_consistencia)
print(out$grafico_pie_consistencia)
print(out$grafico_emociones_vs_consistencia)

# Segmentación propositiva (si segmentar_por no es NULL)
out$segmentacion$indicadores_segmentados

out_mc <- analizar_consistencia_lexicos(
  matriz = mc,
  metodos = c("syuzhet","bing","afinn","nrc"),
  umbral = 0,
  mostrar_pie = TRUE
)

