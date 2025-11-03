# ------------------------------------------------------------
# Pipeline DiccionariosEmocion - Ejecución integral y exportación
# ------------------------------------------------------------
suppressPackageStartupMessages({
  library(data.table); library(ggplot2); library(dplyr); library(tidyr)
})

# --- CONFIG --------------------------------------------------
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

# --- HELPERS -------------------------------------------------
safe_save_plot <- function(p, path, width=10, height=6, dpi=120) {
  if (inherits(p, "ggplot")) ggsave(path, p, width=width, height=height, dpi=dpi)
  invisible(TRUE)
}

safe_write <- function(obj, path) {
  try({
    if (is.data.frame(obj) || data.table::is.data.table(obj)) {
      data.table::fwrite(obj, path)
    } else if (is.list(obj)) {
      saveRDS(obj, sub("\\.csv$", ".rds", path))
    }
  }, silent = TRUE)
  invisible(TRUE)
}

ensure_cols <- function(df, cols) {
  add <- setdiff(cols, colnames(df))
  if (length(add)) df[add] <- 0
  df
}

traducir_emociones_local <- function(x, idioma="es"){
  dic <- c(anger="ira", anticipation="anticipación", disgust="disgusto", fear="miedo",
           joy="alegría", sadness="tristeza", surprise="sorpresa", trust="confianza",
           positive="positivo", negative="negativo")
  out <- ifelse(x %in% names(dic), dic[x], x)
  unname(out)
}

# --- 0) INGESTA ----------------------------------------------
message(">> Ingesta…")
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

# --- 1) MATRICES LÉXICAS ------------------------------------
message(">> Matrices léxicas…")
mvxm <- MatrizValoradaXMetodo(resultados$MatrizSimplificadaTokensValorados$MatrizCol0, FALSE)
mc   <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup
acdfx <- resultados$MatrizSimplificadaTokensValorados$MatrizCol0

# Guardamos bases
safe_write(as.data.frame(mc),     file.path(cfg$out_dir, "03_matriz_valorada_mc.csv"))
safe_write(as.data.frame(acdfx),  file.path(cfg$out_dir, "04_matriz_valorada_acdfx.csv"))

# --- 2) CURVAS DE SENTIMIENTO (Syuzhet, Bing, Afinn, NRC) ---
message(">> Curvas de sentimiento…")
curv <- visualizar_curvas_sentimiento(resultados, metodos = c("syuzhet","bing","afinn","NRC"))
safe_save_plot(curv$grafico, file.path(cfg$out_dir, "05_curvas_sentimiento.png"))
safe_write(curv$resumen_estadistico, file.path(cfg$out_dir, "05_curvas_resumen.csv"))

# --- 3) EMOCIONES NRC (porcentaje y frecuencias) -------------
message(">> Emociones NRC…")
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

# Variante con traductor local (sin llamadas externas)
emo3 <- visualizar_emociones_nrc(
  resultados$VectorTextoSentimientosNRC,
  idioma="es",
  funcion_traduccion=traducir_emociones_local,
  normalizar="porcentaje"
)
safe_save_plot(emo3$grafico_emociones, file.path(cfg$out_dir, "08_nrc_porcentaje_trad_local.png"))

# --- 4) TOKENS EXTREMOS (top/bottom, z-score con facetas) ---
message(">> Tokens extremos…")
ext1 <- analizar_tokens_extremos(mc)
safe_save_plot(ext1$grafico, file.path(cfg$out_dir, "09_tokens_extremos_all.png"))
safe_write(ext1$tabla_combinada, file.path(cfg$out_dir, "09_tokens_extremos_tabla.csv"))

ext2 <- analizar_tokens_extremos(
  mc, n = cfg$top_n,
  metodos = c("syuzhet","bing","afinn","nrc"),
  scale_method = "zscore", facet = TRUE
)
safe_save_plot(ext2$grafico, file.path(cfg$out_dir, "10_tokens_extremos_zscore_facet.png"))

# --- 5) CLUSTERING / FACTORIAL (robusto) --------------------
message(">> Clustering y PCA…")

var_ok <- function(x) {
  v <- suppressWarnings(sapply(x, function(v) stats::sd(v, na.rm=TRUE)))
  which(is.finite(v) & v > 0)
}

to_num_df <- function(df) {
  as.data.frame(lapply(df, function(col) suppressWarnings(as.numeric(col))))
}

build_cluster <- function(dfx_emoc = NULL, acdfx_metodos = NULL) {
  # 1) INTENTO A: emociones NRC (si tienes 'dfx' con columnas emocionales)
  if (!is.null(dfx_emoc)) {
    # nombres sin tildes por tu preferencia
    emo_cols <- c("ira","anticipacion","disgusto","miedo","alegria",
                  "tristeza","sorpresa","confianza")
    pol_cols <- c("positivo","negativo")

    # asegurar columnas (si faltan, crear 0)
    faltan <- setdiff(c(emo_cols, pol_cols), colnames(dfx_emoc))
    if (length(faltan)) dfx_emoc[faltan] <- 0

    dfc <- cbind(
      to_num_df(dfx_emoc[, emo_cols, drop=FALSE]),
      clase = factor(ifelse(dfx_emoc$positivo > dfx_emoc$negativo, "pos", "no_pos"))
    )

    keep <- var_ok(dfc[, emo_cols, drop=FALSE])
    if (length(keep) >= 2) {
      dfc_use <- cbind(dfc[, emo_cols[keep], drop=FALSE], clase = dfc$clase)
      rmMC <- resModeloCluster(dfc_use, indiceClase = ncol(dfc_use), vectorClase = dfc_use$clase)
      return(list(tipo="emociones", dfc=dfc_use, modelo=rmMC))
    }
  }

  # 2) INTENTO B: métodos (tokens x {syuzhet,bing,afinn,nrc})
  if (!is.null(acdfx_metodos)) {
    # si acdfx tiene filas = metodos y cols = tokens, transponer:
    # esperamos columnas con nombres: syuzhet, bing, afinn, nrc
    if (!all(c("syuzhet","bing","afinn","nrc") %in% rownames(acdfx_metodos)) &&
        all(c("syuzhet","bing","afinn","nrc") %in% colnames(acdfx_metodos))) {
      X <- acdfx_metodos
    } else {
      X <- as.data.frame(t(as.matrix(acdfx_metodos)))
    }

    # quedarnos con las columnas de método
    met_cols <- intersect(colnames(X), c("syuzhet","bing","afinn","nrc"))
    stopifnot(length(met_cols) >= 2)

    X_num <- to_num_df(X[, met_cols, drop=FALSE])
    # clase simple por polaridad combinada
    pol <- rowSums(cbind(
      if ("bing" %in% met_cols) X_num$bing else 0,
      if ("afinn" %in% met_cols) X_num$afinn else 0,
      if ("syuzhet" %in% met_cols) X_num$syuzhet else 0
    ), na.rm=TRUE)

    dfc2 <- cbind(X_num, clase = factor(ifelse(pol > 0, "pos", "no_pos")))
    keep2 <- var_ok(dfc2[, met_cols, drop=FALSE])

    if (length(keep2) >= 2) {
      dfc_use <- cbind(dfc2[, met_cols[keep2], drop=FALSE], clase = dfc2$clase)
      rmMC <- resModeloCluster(dfc_use, indiceClase = ncol(dfc_use), vectorClase = dfc_use$clase)
      return(list(tipo="metodos", dfc=dfc_use, modelo=rmMC))
    } else {
      stop("No hay al menos 2 variables con varianza distinta de cero para clustering (métodos).")
    }
  }

  stop("No se pudo construir una matriz válida para clustering.")
}

# --- Detectar insumos disponibles ----------------------------
# Opción emociones: usa tu 'dfx' si ya lo creaste en pasos previos (interseccion + modificarNombresFilas)
dfx_emoc_try <- if (exists("dfx")) dfx else NULL

# Opción métodos: usa la estructura que ya tienes en el pipeline
acdfx_raw <- resultados$MatrizSimplificadaTokensValorados$MatrizCol0
if (!is.data.frame(acdfx_raw) && !is.matrix(acdfx_raw) && is.list(acdfx_raw))
  acdfx_raw <- as.data.frame(acdfx_raw, stringsAsFactors = FALSE)

acdfx_metodos <- acdfx_raw  # esperado: filas=metodos, cols=tokens (o viceversa)

# --- Ejecutar construcción robusta ---------------------------
clu <- build_cluster(dfx_emoc = dfx_emoc_try, acdfx_metodos = acdfx_metodos)
rmMC <- clu$modelo
dfc  <- clu$dfc

message(">> Clustering construido a partir de: ", clu$tipo)

# Guardar gráficos si existen
if (!is.null(rmMC$graficaCluster)) safe_save_plot(rmMC$graficaCluster, file.path(cfg$out_dir, "11_cluster.png"))
if (!is.null(rmMC$graficaDend))    safe_save_plot(rmMC$graficaDend,    file.path(cfg$out_dir, "11_dendrograma.png"))
if (!is.null(rmMC$graficaInd))        safe_save_plot(rmMC$graficaInd,        file.path(cfg$out_dir, "11_individuos.png"))
if (!is.null(rmMC$graficaCompleta))   safe_save_plot(rmMC$graficaCompleta,   file.path(cfg$out_dir, "11_completa.png"))
if (!is.null(rmMC$graficaBiplot2))    safe_save_plot(rmMC$graficaBiplot2,    file.path(cfg$out_dir, "11_biplot2.png"))
if (!is.null(rmMC$graficaBiplot))     safe_save_plot(rmMC$graficaBiplot,     file.path(cfg$out_dir, "11_biplot.png"))

#----5B--- AQUÍ ESTRUCTURAS DE TABLAS EVALUADAS -----

# 1) Construir mvxm (con traducción al ES vía GPT, por ejemplo)
mvxm <- MatrizValoradaXMetodo(
  mdata          = resultados$MatrizSimplificadaTokensValorados$MatrizCol0,
  si_trad        = TRUE,
  idioma_destino = "espanol",      # o "portugues", "frances", etc.
  trad_fun       = "gpt",          # "auto" / "traducea" / "gpt"
  model          = "gpt-4o-mini"
)

# 2) Usar el diccionario en tu anotador simple (si quieres anotar la intersección NRC+léxicos):
dxM   <- as.data.frame(as.matrix(resultados$VectorTextoSentimientosNRC))
dx    <- as.data.frame(as.matrix(resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup))
inter <- interseccionDataFrames(dxM, dx)

dfx_es <- anotar_tokens_idioma_simple(inter, mvxm = mvxm, idioma_destino = "espanol", use_mvxm = "always")

head(dfx_es[, c("token_orig","token_idioma","token_idioma_unico")])
# token_idioma_unico es ideal para gráficos si hay colisiones de traducción.
resA <- categorizar_indices(
  df = dfx_es,
  idx = c("syuzhet","bing","afinn","nrc"),
  prefix = "Cat_",        # prefijo para las nuevas columnas
  mode = "per_column"     # una categoría por método
)

resB <- categorizar_indices(
  df = dfx_es,
  idx = c("syuzhet","bing","afinn","nrc"),
  mode = "aggregate",            # combina en un índice
  weights = c(0.4, 0.1, 0.3, 0.2), # pondera cada método
  scale_method = "zscore",       # normaliza escalas (importante)
  aggregate_name = "polaridad"   # nombre del índice base
)

resC <- categorizar_indices(
  df = dfx_es,
  idx = c("syuzhet","bing","afinn","nrc"),
  mode = "both",
  weights = c(0.25,0.25,0.25,0.25),
  scale_method = "robust",        # robusto a outliers
  aggregate_name = "polaridad"
)

names(resC$data)
# "syuzhet" "bing" "afinn" "nrc" "token_orig"
# "Cat_syuzhet" "Cat_bing" "Cat_afinn" "Cat_nrc"
# "polaridad" "Cat_polaridad"

### P1

# Define tus columnas
method_cols <- c("syuzhet","bing","afinn","nrc")
nrc_cols <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
id_cols <- c("token_orig","token_idioma","token_idioma_unico")

# Pesos sugeridos (ajusta si deseas)
pesos <- c(syuzhet=.35, bing=.15, afinn=.25, nrc=.25)

res_p1 <- estrategia_p1_preparacion(
  dfx = dfx_es,
  method_cols = method_cols,
  nrc_cols = nrc_cols,
  id_cols = id_cols,
  weights = pesos,
  percentiles = c(0.25, 0.5, 0.75),
  out_dir = "salidas_pipeline"
)

# Vistazos rápidos
head(res_p1$data[, c("token_idioma_unico","syuzhet","bing","afinn","nrc","sent_general","Nivel_sent_general")])
res_p1$auditoria$corr_metodos
res_p1$graficos$indice_cortes  # imprime el ggplot



#### P2
method_cols <- c("syuzhet","bing","afinn","nrc")

res_p2 <- estrategia_p2_metodos(
  df = res_p1$data,
  method_cols = method_cols,
  nivel_col = "Nivel_sent_general",
  out_dir = "salidas_pipeline"
)

# Tablas:
res_p2$tablas$cor_pearson
res_p2$tablas$ccc_pares

# Plots con leyendas enriquecidas:
res_p2$graficos$cor_pearson
res_p2$graficos$cor_spearman
res_p2$graficos$box_por_nivel
res_p2$graficos$violin_por_nivel
res_p2$graficos$bland_altman[[1]]
res_p2$graficos$pairs_z[[1]]

##### P3

method_cols <- c("syuzhet","bing","afinn","nrc")

res_p3 <- estrategia_p3_ensamble(
  df = res_p1$data,
  method_cols = method_cols,
  ref = "syuzhet",
  out_dir = "salidas_pipeline",
  robust = "auto",
  n_switch = 1000,
  sample_for_plots = 2000,
  seed = 1,
  percentiles = c(0.25, 0.5, 0.75)
)

res_p3$calibracion
res_p3$pesos
head(res_p3$data[, c("token_idioma_unico","sent_compuesto","Nivel_sent_compuesto")])


###### P4
method_cols <- c("syuzhet","bing","afinn","nrc")
nrc_cols <- c("anger","anticipation","disgust","fear","joy","sadness",
              "surprise","trust","negative","positive")

res_p4 <- estrategia_p4_pca_cluster(
  df = res_p3$data,
  method_cols = method_cols,
  nrc_cols = nrc_cols,
  id_col = "token_idioma_unico",
  k_clust = 4,
  out_dir = "salidas_pipeline"
)

# Visualizar gráficos
res_p4$graficos$scree
res_p4$graficos$biplot
res_p4$graficos$density
res_p4$graficos$loadings

# Examinar tablas
res_p4$tablas$varianza
head(res_p4$tablas$cargas)
table(res_p4$clusters$cluster)


##### P5
method_cols <- c("syuzhet","bing","afinn","nrc")
nrc_cols <- c("anger","anticipation","disgust","fear","joy","sadness",
              "surprise","trust","negative","positive")

res_p5 <- estrategia_p5_clusters(
  df = res_p3$data,
  method_cols = method_cols,
  emotion_cols = nrc_cols,
  nivel_col = "Nivel_sent_compuesto",
  out_dir = "salidas_pipeline",
  n_clusters = 4
)

#### P6
method_cols <- c("syuzhet","bing","afinn","nrc")

res_p6 <- estrategia_p6_estabilidad_visual(
  df = res_p5$clusters |> dplyr::left_join(res_p3$data, by = "token_idioma_unico"),
  method_cols = method_cols,
  nivel_col = "Nivel_sent_compuesto",
  cluster_col = "cluster",
  out_dir = "salidas_pipeline",
  label_fraction = 0.3   # 30% de tokens etiquetados
)

### P7

# Suponiendo que ya tienes res_p1..res_p6 construidos:
salida_p7 <- p7_exportar_reporte_maestro(
  out_dir        = "salidas_pipeline",
  resultados_raw = resultados,   # si lo tienes; si no, pasa NULL
  res_p1 = res_p1,
  res_p2 = res_p2,
  res_p3 = res_p3,
  res_p4 = res_p4,
  res_p5 = res_p5,
  res_p6 = res_p6,
  cfg    = cfg                   # si existe; si no, NULL
)

# Verifica el RDS maestro:
maestro <- readRDS(file.path("salidas_pipeline", "00_salida_maestra.rds"))
names(maestro)


# --- 6) CO-OCURRENCIA (heatmap + red) -----------------------
message(">> Co-ocurrencias…")
# Si no tienes 'cooc' preparado, usa tu función generadora (ajusta si ya existe en tu proyecto)
if (!exists("cooc")) {
  # Ejemplo: cooc <- generar_coocurrencias(resultados$TextoTokens_Rep, min_freq = 2, window = 5)
  cooc <- NULL
}
if (!is.null(cooc)) {
  v1 <- visualizar_coocurrencia_nrc(cooc, tipo="heatmap", normalizar="prop", ordenar="hclust")
  safe_save_plot(v1$grafico, file.path(cfg$out_dir, "13_cooc_heatmap.png"))

  v2 <- visualizar_coocurrencia_nrc(cooc, tipo="red", normalizar="max", min_edge=0.05, top_edges=50, layout_red="fr")
  safe_save_plot(v2$grafico, file.path(cfg$out_dir, "14_cooc_red_fr.png"))

  v2c <- visualizar_coocurrencia_nrc_v2(cooc, tipo="red", normalizar="max", min_edge=0.05, top_edges=60,
                                        color_nodes="community", community_method="louvain", layout_red="kk", seed=123)
  safe_save_plot(v2c$grafico, file.path(cfg$out_dir, "15_cooc_red_louvain.png"))
}

# --- 7) PIPELINE EMOCIONAL INTEGRADO ------------------------
message(">> Pipeline emocional integrado…")
comentarios <- resultados$TextoSentencias
lexicon     <- as.data.frame(resultados$VectorTextoSentimientosNRC)

pipe <- pipeline_emociones(
  comentarios = comentarios,
  lexicon = lexicon,
  k = 3, resumen_fun = "sum",
  seed = cfg$seed, palette = "Dark2"
)

# Métricas y variancias
safe_write(as.data.frame(pipe$metricas),      file.path(cfg$out_dir, "16_metricas_pipeline.csv"))
safe_write(as.data.frame(pipe$var_exp),       file.path(cfg$out_dir, "16_var_exp_pipeline.csv"))
safe_write(as.data.frame(pipe$matriz_emocional), file.path(cfg$out_dir, "16_matriz_emocional.csv"))

# Gráficos principales
safe_save_plot(pipe$graficos$scatter, file.path(cfg$out_dir, "16_scatter_pca.png"))
safe_save_plot(pipe$graficos$elipses, file.path(cfg$out_dir, "16_elipses.png"))
safe_save_plot(pipe$graficos$biplot,  file.path(cfg$out_dir, "16_biplot.png"))

# ============================================================
# Utilidades generales (etiquetas ES y persistencia)
# ============================================================
# Normaliza tokens para mapping
norm_tok <- function(s){
  s <- as.character(s)
  s <- iconv(s, to = "ASCII//TRANSLIT")
  s <- tolower(s)
  s <- gsub("[^a-z0-9_]+","", s)
  s
}

# Construye mapa: token_en_norm -> token_es (desde mvxm$TraduccTokens)
build_tok_map_es <- function(trad_table){
  stopifnot(is.data.frame(trad_table), all(c("Orig","Traduccion") %in% names(trad_table)))
  setNames(trad_table$Traduccion, nm = norm_tok(trad_table$Orig))
}

# Etiqueta sin forzar unicidad (para tablas donde no importan duplicados)
etiquetar_tokens_es <- function(tokens, tok_map_es){
  key <- norm_tok(tokens)
  lab <- unname(tok_map_es[key])
  ifelse(is.na(lab) | lab == "", tokens, lab)
}

# Etiqueta forzando unicidad (para ejes/rownames en objetos "de presentación")
etiquetar_tokens_es_unique <- function(tokens, tok_map_es){
  base <- etiquetar_tokens_es(tokens, tok_map_es)
  # hace unico respetando el orden
  make_unique <- function(x){
    seen <- list()
    out <- character(length(x))
    for (i in seq_along(x)){
      v <- x[i]
      if (is.null(seen[[v]])) {
        seen[[v]] <- 1L
        out[i] <- v
      } else {
        seen[[v]] <- seen[[v]] + 1L
        out[i] <- paste0(v, " (", seen[[v]], ")")
      }
    }
    out
  }
  make_unique(base)
}

# Agrega columna token_es (sin tocar rownames)
add_token_label_column <- function(df, tok_map_es, colname = "token_es"){
  stopifnot(!is.null(rownames(df)))
  df[[colname]] <- etiquetar_tokens_es(rownames(df), tok_map_es)
  df
}

# Persistencia
safe_write_if <- function(obj, path){
  if (is.null(obj)) return(invisible(FALSE))
  df <- tryCatch(as.data.frame(obj), error = function(e) NULL)
  if (is.null(df) || !ncol(df)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(df, path)
  TRUE
}

safe_save_plot <- function(p, path, width = 8, height = 5, dpi = 120){
  if (is.null(p)) return(invisible(FALSE))
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi, limitsize = FALSE)
  TRUE
}


# ============================================================
# PRE-PASO 8 — Integrar mvxm (sin duplicados y etiquetas ES)
# ============================================================
message(">> Matrices léxicas…")
mvxm <- MatrizValoradaXMetodo(
  resultados$MatrizSimplificadaTokensValorados$MatrizCol0,
  TRUE  # TRUE: sin duplicados + traducción ES (según tu implementación actual)
)

# Bases para cálculo (NO tocar rownames)
mc_raw <- mvxm$MatrizTranspuestaSinDup   # tokens x metodos
ac_raw <- mvxm$MatrizCol0                # metodos x tokens
stopifnot(!is.null(rownames(mc_raw)), !is.null(colnames(ac_raw)))

# Mapa ES para rotular (presentación)
tok_map_es <- build_tok_map_es(mvxm$TraduccTokens)

# ============================================================
# PASO 8 — Consistencia entre léxicos (alineación robusta + opciones NRC)
# ============================================================
message(">> Consistencia léxica… (paso 8)")

# Helpers locales de alineacion / numerico / varianza
norm_exact   <- function(x) x
norm_lower   <- function(x) tolower(x)
norm_trans   <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT"))
norm_strict  <- function(x){ x <- tolower(iconv(x, to = "ASCII//TRANSLIT")); gsub("[^a-z0-9_]+","", x) }
to_num_df <- function(df){ rn <- rownames(df); out <- as.data.frame(lapply(df, function(x) suppressWarnings(as.numeric(x)))); if(!is.null(rn)) rownames(out) <- rn; out }
sd_pos <- function(df){ if (is.null(df) || ncol(df) == 0) return(df); keep <- sapply(df, function(v){ s <- suppressWarnings(sd(v, na.rm=TRUE)); is.finite(s) && s>0 }); if(!any(keep)) return(df[,0,drop=FALSE]); df[,keep,drop=FALSE] }
aggregate_rows_by_name <- function(mat){ nms <- rownames(mat); if (anyDuplicated(nms)) { u <- unique(nms); out <- lapply(u, function(nm){ idx <- which(nms==nm); if (length(idx)==1) mat[idx,,drop=FALSE] else colSums(mat[idx,,drop=FALSE], na.rm=TRUE) }); out <- do.call(rbind,out); rownames(out) <- u; as.data.frame(out) } else as.data.frame(mat) }
aggregate_cols_by_name <- function(mat){ nms <- colnames(mat); if (anyDuplicated(nms)) { u <- unique(nms); out <- lapply(u, function(nm){ idx <- which(nms==nm); if (length(idx)==1) mat[,idx,drop=FALSE] else rowSums(mat[,idx,drop=FALSE,drop=FALSE], na.rm=TRUE) }); out <- do.call(cbind,out); colnames(out) <- u; as.data.frame(out) } else as.data.frame(mat) }
normalize_cols <- function(x){ x <- tolower(iconv(x, to = "ASCII//TRANSLIT")); gsub("[^a-z0-9_]+","", x) }

methods_whitelist <- c("syuzhet","bing","afinn","nrc")
emo_cols <- c("ira","anticipacion","disgusto","miedo","alegria","tristeza","sorpresa","confianza","positivo","negativo")

# 1) Alineacion multi-paso por tokens
tok_mc_base <- rownames(mc_raw)
tok_ac_base <- colnames(ac_raw)

passes <- list(
  list(fn="exact",  f_mc=norm_exact, f_ac=norm_exact),
  list(fn="lower",  f_mc=norm_lower, f_ac=norm_lower),
  list(fn="trans",  f_mc=norm_trans, f_ac=norm_trans),
  list(fn="strict", f_mc=norm_strict,f_ac=norm_strict)
)

tok_ok_norm <- character(0); pass_used <- NA_character_
mc2 <- NULL; ac2 <- NULL

for (ps in passes) {
  n_mc <- ps$f_mc(tok_mc_base)
  n_ac <- ps$f_ac(tok_ac_base)
  mc_norm <- aggregate_rows_by_name(`rownames<-`(as.matrix(mc_raw), n_mc))
  ac_norm <- aggregate_cols_by_name(`colnames<-`(as.matrix(ac_raw), n_ac))
  tok_ok  <- intersect(rownames(mc_norm), colnames(ac_norm))
  if (length(tok_ok) >= 1) {
    tok_ok_norm <- tok_ok
    mc2 <- as.data.frame(mc_norm[tok_ok_norm,,drop=FALSE])
    ac2 <- as.data.frame(t(ac_norm[, tok_ok_norm, drop=FALSE]))
    rownames(mc2) <- tok_ok_norm
    rownames(ac2) <- tok_ok_norm
    pass_used <- ps$fn
    break
  }
}
if (!length(tok_ok_norm)) stop("No se encontraron tokens comunes entre mc y ac tras los 4 niveles de normalizacion.")

# 2) Filtrar metodos y numerico + varianza
met_cols <- intersect(colnames(mc2), methods_whitelist)
if (!length(met_cols)) stop("mc (mvxm$MatrizTranspuestaSinDup) no contiene metodos tras la alineacion.")
mc2 <- to_num_df(mc2[, met_cols, drop=FALSE]); mc2 <- sd_pos(mc2)

metodos_ok <- intersect(colnames(mc2), methods_whitelist)
if (length(metodos_ok) < 2) {
  set.seed(1)
  rn <- rownames(mc2)
  mc2 <- as.data.frame(lapply(mc2, function(v){ if (all(is.na(v))) v <- rep(0,length(v)); v + rnorm(length(v), sd=1e-6) }))
  rownames(mc2) <- rn
  mc2 <- sd_pos(mc2)
  metodos_ok <- intersect(colnames(mc2), methods_whitelist)
  if (length(metodos_ok) < 2) stop("No hay >=2 metodos con varianza>0 para consistencia.")
}

# 3) Emociones (opcional) via dfx — NO tocar rownames; injertar columnas NRC si existen
usar_emociones <- FALSE
if (exists("dfx")) {
  dfx_emo <- tryCatch(dfx, error = function(e) NULL)
  if (!is.null(dfx_emo) && is.data.frame(dfx_emo)) {
    rownames(dfx_emo) <- norm_tok(rownames(dfx_emo))
    colnames(dfx_emo) <- normalize_cols(colnames(dfx_emo))
    faltan <- setdiff(emo_cols, colnames(dfx_emo)); if (length(faltan)) dfx_emo[faltan] <- 0
    dfx_emo_aligned <- dfx_emo[norm_tok(tok_ok_norm), emo_cols, drop = FALSE]
    dfx_emo_aligned[] <- lapply(dfx_emo_aligned, function(v){ v <- suppressWarnings(as.numeric(v)); v[is.na(v)] <- 0; v })
    # injerta si faltan en ac2
    for (cn in emo_cols) if (!cn %in% colnames(ac2)) ac2[[cn]] <- dfx_emo_aligned[[cn]]
    usar_emociones <- TRUE
  }
}

# 4) Segmentacion (opcional)
seg <- if ("categoria" %in% colnames(ac2)) "categoria" else NULL

# 5) Auditoria
message(sprintf(">> Consistencia: tokens=%d, metodos=%s, pase='%s', emociones=%s",
                nrow(mc2), paste(metodos_ok, collapse=", "),
                pass_used, if (usar_emociones) "SI" else "NO"))

# 6) Analisis de consistencia (v2 robusto que ya te pasé)
cons <- if (usar_emociones) {
  analizar_consistencia_lexicos_v2(
    mc    = mc2,
    acdfx = ac2,
    metodos = metodos_ok,
    emociones_cols = emo_cols,
    segmentar_por = seg,
    mostrar_pie = TRUE
  )
} else {
  analizar_consistencia_lexicos_v2(
    mc    = mc2,
    acdfx = ac2,
    metodos = metodos_ok,
    emociones_cols = NULL,
    segmentar_por = seg,
    mostrar_pie = TRUE
  )
}

# 7) Enriquecer salidas con etiquetas ES (SIN tocar rownames)
if (!is.null(cons$clasificacion_por_token) && "token" %in% names(cons$clasificacion_por_token)) {
  cons$clasificacion_por_token$token_es <- etiquetar_tokens_es(cons$clasificacion_por_token$token, tok_map_es)
  cons$clasificacion_por_token$token_es_unico <- etiquetar_tokens_es_unique(cons$clasificacion_por_token$token, tok_map_es)
}

# (opcional) copias SOLO para mostrar en tablas (no en calculos)
mc2_view <- add_token_label_column(mc2, tok_map_es, "token_es")
ac2_view <- add_token_label_column(ac2, tok_map_es, "token_es")

# 8) Persistencia (evita warnings vacios; parche de theme en pie)
safe_write_if(cons$indicadores,                        file.path(cfg$out_dir, "17_cons_indicadores.csv"))
safe_write_if(cons$resumen_categorias,                 file.path(cfg$out_dir, "17_cons_resumen_categorias.csv"))
safe_write_if(cons$perfil_emociones_por_consistencia,  file.path(cfg$out_dir, "17_cons_perfil_emociones.csv"))

# Pie: garantizar elementos en blanco para ejes
if (!is.null(cons$grafico_pie_consistencia)) {
  cons$grafico_pie_consistencia <- cons$grafico_pie_consistencia +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x  = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y  = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
}

safe_save_plot(cons$grafico_barras_consistencia,       file.path(cfg$out_dir, "17_cons_barras.png"))
safe_save_plot(cons$grafico_pie_consistencia,          file.path(cfg$out_dir, "17_cons_pie.png"))
safe_save_plot(cons$grafico_emociones_vs_consistencia, file.path(cfg$out_dir, "17_cons_emociones_vs_consistencia.png"))

# ============================================================
# PASO 9 — Salida maestra (incluye mapping ES y vistas)
# ============================================================
message(">> Listo. Exportando objeto maestro…")

salida <- list(
  config   = if (exists("cfg")) cfg else NULL,
  insumos  = list(
    sentencias = resultados$TextoSentencias,
    tokens_rep = resultados$TextoTokens_Rep
  ),
  matrices = list(
    mc2_calc  = mc2,      # para calculos (no tocar rownames)
    ac2_calc  = ac2,
    mc2_view  = mc2_view, # con columna token_es para reportes
    ac2_view  = ac2_view
  ),
  mapping  = list(
    traducciones = mvxm$TraduccTokens, # Orig, IdioOrig, Traduccion
    tok_map_es   = tok_map_es          # named vector (norm_orig -> es)
  ),
  consistencia = cons,
  # agrega aqui lo demas si existe en tu pipeline
  curvas   = if (exists("curv")) curv else NULL,
  nrc      = list(
    emo1 = if (exists("emo1")) emo1 else NULL,
    emo2 = if (exists("emo2")) emo2 else NULL,
    emo3 = if (exists("emo3")) emo3 else NULL
  ),
  extremos = list(
    ext1 = if (exists("ext1")) ext1 else NULL,
    ext2 = if (exists("ext2")) ext2 else NULL
  ),
  cluster  = if (exists("rmMC")) rmMC else NULL,
  pipeline = if (exists("pipe")) pipe else NULL
)

dir.create(cfg$out_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(salida, file.path(cfg$out_dir, "00_salida_maestra.rds"))
message(">> Pipeline completado. Revisa la carpeta: ", normalizePath(cfg$out_dir))


