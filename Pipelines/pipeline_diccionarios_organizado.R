# ============================================================
# Pipeline DiccionariosEmocion — Ejecución, salidas y reporte
# ============================================================
# Propósito
# ---------
# Este pipeline orquesta la *EJECUCIÓN* de las funciones ya existentes en el
# proyecto (NO redefine funciones de análisis). Se orienta a:
#   1) Ingesta y persistencia mínima de insumos.
#   2) Generación de salidas (tablas/gráficos) en carpeta única.
#   3) Secuencialidad clara por pasos, con *comentarios explicativos*.
#   4) Guardado de un RDS maestro con todo lo relevante.
#   5) Emisión de un reporte HTML ligero si ya cuentas con el generador.
#
# Notas
# -----
# - Las funciones de análisis (p. ej., UnirExcelITFF_Optimizada, visualizar_curvas_sentimiento,
#   visualizar_emociones_nrc, analizar_tokens_extremos, resModeloCluster, MatrizValoradaXMetodo,
#   estrategia_p1_preparacion, estrategia_p2_metodos, estrategia_p3_ensamble, estrategia_p4_pca_cluster,
#   estrategia_p5_clusters, estrategia_p6_estabilidad_visual, p7_exportar_reporte_maestro,
#   clusterizar_lexico, analizar_coherencia_metodos, guardar_artefacto_modelos, validar_binario_con_split,
#   pca_tokens_plot, pca_tokens_biplot, analisis_componentes_general, etc.) SE ASUMEN DISPONIBLES
#   en tu proyecto. Aquí sólo se las llama en un orden y con persistencia organizada.
#
# - Este script es *idempotente* en cuanto a escrituras: crea directorios si faltan y
#   evita errores por existencia de archivos. No hace limpieza de salidas previas.
# - Ajusta la sección CONFIG según tus necesidades.
# ============================================================

suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(dplyr); library(tidyr)
})

# ------------------------------------------------------------
# CONFIG
# ------------------------------------------------------------
cfg <- list(
  nombreBase        = "Data/DocMoreR/ClasesVirtualEn_",  # prefijo de archivos
  IndIni            = 7,
  IndFin            = 7,
  IdiomaIni         = "en",
  IdiomaFin         = "en",
  IdiomaLimpieza    = "english",
  valorPor          = 10,
  SiTrad            = FALSE,          # evita traducción al vuelo (mejora performance)
  out_dir           = "salidas_pipeline",
  seed              = 42,
  top_n             = 12
)
dir.create(cfg$out_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(cfg$seed)

# ------------------------------------------------------------
# HELPERS DE ESCRITURA/EXPORTACIÓN (ligeros)
# ------------------------------------------------------------
safe_save_plot <- function(p, path, width=10, height=6, dpi=120) {
  if (inherits(p, "ggplot")) {
    dir.create(dirname(path), TRUE, TRUE)
    ggplot2::ggsave(path, p, width=width, height=height, dpi=dpi, limitsize = FALSE)
  }
  invisible(TRUE)
}
safe_write <- function(obj, path) {
  try({
    dir.create(dirname(path), TRUE, TRUE)
    if (is.data.frame(obj) || data.table::is.data.table(obj)) {
      data.table::fwrite(obj, path)
    } else if (is.list(obj)) {
      saveRDS(obj, sub("\\.csv$", ".rds", path))
    }
  }, silent = TRUE)
  invisible(TRUE)
}

# ------------------------------------------------------------
# 0) INGESTA — unifica fuentes y normaliza básicos
# ------------------------------------------------------------
message(">> [0] Ingesta…")
resultados <- UnirExcelITFF_Optimizada(
  nombreBase      = cfg$nombreBase,
  IndIni          = cfg$IndIni,
  IndFin          = cfg$IndFin,
  IdiomaIni       = cfg$IdiomaIni,
  IdiomaFin       = cfg$IdiomaFin,
  IdiomaLimpieza  = cfg$IdiomaLimpieza,
  valorPor        = cfg$valorPor,
  SiTrad          = cfg$SiTrad
)

# Persistimos insumos clave para trazabilidad
safe_write(as.data.frame(resultados$TextoSentencias), file.path(cfg$out_dir, "01_textos_sentencias.csv"))
safe_write(as.data.frame(resultados$TextoTokens_Rep), file.path(cfg$out_dir, "02_textos_tokens_rep.csv"))

# ------------------------------------------------------------
# 1) MATRICES LÉXICAS
# ------------------------------------------------------------
message(">> [1] Matrices léxicas…")
# Estructuras base provenientes de tu objeto resultados
mvxm_base <- MatrizValoradaXMetodo(resultados$MatrizSimplificadaTokensValorados$MatrizCol0, FALSE)
mc        <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup
acdfx     <- resultados$MatrizSimplificadaTokensValorados$MatrizCol0

safe_write(as.data.frame(mc),     file.path(cfg$out_dir, "03_matriz_valorada_mc.csv"))
safe_write(as.data.frame(acdfx),  file.path(cfg$out_dir, "04_matriz_valorada_acdfx.csv"))

# ------------------------------------------------------------
# 2) CURVAS DE SENTIMIENTO (Syuzhet, Bing, Afinn, NRC)
# ------------------------------------------------------------
message(">> [2] Curvas de sentimiento…")
curv <- visualizar_curvas_sentimiento(resultados, metodos = c("syuzhet","bing","afinn","NRC"))
safe_save_plot(curv$grafico, file.path(cfg$out_dir, "05_curvas_sentimiento.png"))
safe_write(curv$resumen_estadistico, file.path(cfg$out_dir, "05_curvas_resumen.csv"))

# ------------------------------------------------------------
# 3) EMOCIONES NRC (porcentaje y frecuencias)
# ------------------------------------------------------------
message(">> [3] Emociones NRC…")
emo1 <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC, idioma = "es")
safe_save_plot(emo1$grafico_emociones, file.path(cfg$out_dir, "06_nrc_porcentaje.png"))
safe_write(emo1$tabla_emociones, file.path(cfg$out_dir, "06_nrc_porcentaje_tabla.csv"))

emo2 <- visualizar_emociones_nrc(
  resultados$VectorTextoSentimientosNRC,
  normalizar = "frecuencia",
  top_n = 6,
  mostrar_polaridades = TRUE
)
safe_save_plot(emo2$grafico_emociones,  file.path(cfg$out_dir, "07_nrc_top6_frecuencias.png"))
safe_save_plot(emo2$grafico_polaridades, file.path(cfg$out_dir, "07_polaridades.png"))

# ------------------------------------------------------------
# 4) TOKENS EXTREMOS (top/bottom, z-score con facetas)
# ------------------------------------------------------------
message(">> [4] Tokens extremos…")
ext1 <- analizar_tokens_extremos(mc)
safe_save_plot(ext1$grafico, file.path(cfg$out_dir, "09_tokens_extremos_all.png"))
safe_write(ext1$tabla_combinada, file.path(cfg$out_dir, "09_tokens_extremos_tabla.csv"))

ext2 <- analizar_tokens_extremos(
  mc, n = cfg$top_n,
  metodos = c("syuzhet","bing","afinn","nrc"),
  scale_method = "zscore", facet = TRUE
)
safe_save_plot(ext2$grafico, file.path(cfg$out_dir, "10_tokens_extremos_zscore_facet.png"))

# ------------------------------------------------------------
# 5) CLUSTERING / FACTORIAL — construcción robusta
# ------------------------------------------------------------
message(">> [5] Clustering y análisis factorial…")
# Detecta insumos disponibles y delega a tu lógica (resModeloCluster/clusterizar_lexico)
# Puedes construir 'tokens' y 'feat' fuera de este script y luego ejecutar los bloques de PCA/PLS/MFA.
# Aquí conservamos solo la orquestación.


# ------------------------------------------------------------
# 6) ESTRATEGIAS P1..P7 (si ya existen en tu proyecto)
# ------------------------------------------------------------
message(">> [6] Estrategias P1..P7…")

# --- P1
method_cols <- c("syuzhet","bing","afinn","nrc")
nrc_cols <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
id_cols <- c("token_orig","token_idioma","token_idioma_unico")
pesos <- c(syuzhet=.35, bing=.15, afinn=.25, nrc=.25)

res_p1 <- estrategia_p1_preparacion(
  dfx = as.data.frame(resultados$TextoTokens_Rep),  # ajusta si tu función usa otra base
  method_cols = method_cols,
  nrc_cols = nrc_cols,
  id_cols = id_cols,
  weights = pesos,
  percentiles = c(0.25, 0.5, 0.75),
  out_dir = cfg$out_dir
)

# --- P2
res_p2 <- estrategia_p2_metodos(
  df = res_p1$data,
  method_cols = method_cols,
  nivel_col = "Nivel_sent_general",
  out_dir = cfg$out_dir
)

# --- P3
res_p3 <- estrategia_p3_ensamble(
  df = res_p1$data,
  method_cols = method_cols,
  ref = "syuzhet",
  out_dir = cfg$out_dir,
  robust = "auto",
  n_switch = 1000,
  sample_for_plots = 2000,
  seed = 1,
  percentiles = c(0.25, 0.5, 0.75)
)

# --- P4
res_p4 <- estrategia_p4_pca_cluster(
  df = res_p3$data,
  method_cols = method_cols,
  nrc_cols = nrc_cols,
  id_col = "token_idioma_unico",
  k_clust = 4,
  out_dir = cfg$out_dir
)

# --- P5
res_p5 <- estrategia_p5_clusters(
  df = res_p3$data,
  method_cols = method_cols,
  emotion_cols = nrc_cols,
  nivel_col = "Nivel_sent_compuesto",
  out_dir = cfg$out_dir,
  n_clusters = 4
)

# --- P6
res_p6 <- estrategia_p6_estabilidad_visual(
  df = res_p5$clusters |> dplyr::left_join(res_p3$data, by = "token_idioma_unico"),
  method_cols = method_cols,
  nivel_col = "Nivel_sent_compuesto",
  cluster_col = "cluster",
  out_dir = cfg$out_dir,
  label_fraction = 0.3
)

# --- P7 (reporte maestro de esas estrategias)
salida_p7 <- p7_exportar_reporte_maestro(
  out_dir        = cfg$out_dir,
  resultados_raw = resultados,
  res_p1 = res_p1,
  res_p2 = res_p2,
  res_p3 = res_p3,
  res_p4 = res_p4,
  res_p5 = res_p5,
  res_p6 = res_p6,
  cfg    = cfg
)

# ------------------------------------------------------------
# 7) VALIDACIÓN Y ARTEFACTO (si ya tienes las funciones)
# ------------------------------------------------------------
message(">> [7] Validación binaria + artefacto…")

# Preparación de target (ajusta a tu criterio de niveles)
tokens <- res_p1$data
tokens$Nivel_bin <- ifelse(tokens$Nivel_sent_general %in% c("N-2","N-3"), "Pos", "Neg")
tokens$Nivel_bin <- factor(tokens$Nivel_bin)

# Features del modelo (ajusta si tu función espera otra convención)
features_modelo <- c("anger","anticipation","disgust","fear","joy","sadness",
                     "surprise","trust","negative","positive",
                     "Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc")

res_val <- validar_binario_con_split(
  tokens   = tokens,
  target   = "Nivel_bin",
  features = features_modelo,
  modelos  = c("rf","svmRadial","glmnet"),
  p_train  = 0.8,
  seed     = 801,
  out_dir  = file.path(cfg$out_dir, "validacion_bin"),
  positive_label = "Pos",
  sampling = NULL,
  split_by = "token_idioma_unico",
  calibr_bins = 10
)

guardar_artefacto_modelos(res_val, features_modelo,
                          path_rds = file.path(cfg$out_dir, "modelos_emocion.rds"))

# ------------------------------------------------------------
# 8) PCA/BIPLOT CLÁSICOS (si tu proyecto los requiere para informe)
# ------------------------------------------------------------
message(">> [8] PCA/biplot clásicos (tokens)…")

# Reutiliza clusters si los tienes; si no, puedes saltar esta sección.
# tokens <- res_p1$data
# cl_res <- clusterizar_lexico(tokens, k=4, metodo="kmeans", features=seleccionar_features_tokens(tokens))
# tokens <- cl_res$data
# feat   <- seleccionar_features_tokens(tokens)
# p1 <- pca_tokens_plot(tokens, features = feat, color_by = "cluster",
#                       n_labels = 600, label_size = 2.1, palette = "OkabeIto")
# p2 <- pca_tokens_biplot(tokens, features = feat, color_by = "cluster",
#                         n_labels = 500, label_size = 2.0, palette = "Dark2",
#                         scale_arrows = 1.7, top_vars = min(10, length(feat)))

# ------------------------------------------------------------
# 9) SALIDA MAESTRA (RDS con lo más relevante del pipeline)
# ------------------------------------------------------------
message(">> [9] Exportando objeto maestro…")

salida <- list(
  config   = cfg,
  insumos  = list(
    sentencias = resultados$TextoSentencias,
    tokens_rep = resultados$TextoTokens_Rep
  ),
  matrices = list(
    mc   = mc,
    acdfx= acdfx,
    mvxm = mvxm_base
  ),
  curvas   = curv,
  nrc      = list(emo1=emo1, emo2=emo2),
  extremos = list(ext1=ext1, ext2=ext2),
  estrategias = list(res_p1=res_p1,res_p2=res_p2,res_p3=res_p3,res_p4=res_p4,res_p5=res_p5,res_p6=res_p6),
  validacion  = res_val
)
saveRDS(salida, file.path(cfg$out_dir, "00_salida_maestra.rds"))
message(">> Pipeline completado. Carpeta de salidas: ", normalizePath(cfg$out_dir, winslash = "/"))
