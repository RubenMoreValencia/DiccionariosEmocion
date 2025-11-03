# ===================== PIPELINE COMPLETO =====================

# 0) Tu data token-level
tokens <- res_p1$data

# Asegura target binario para modelado (ajusta los niveles a tu criterio)
tokens$Nivel_bin <- ifelse(tokens$Nivel_sent_general %in% c("N-2","N-3"), "Pos", "Neg")
tokens$Nivel_bin <- factor(tokens$Nivel_bin)

# 1) Clustering (k-means) + PCA + perfiles
feat <- seleccionar_features_tokens(tokens)

cl_res <- clusterizar_lexico(
  tokens   = tokens,
  k        = 4,
  metodo   = "kmeans",                    # o "hclust"
  features = feat,
  escalar  = TRUE,
  out_dir  = "outputs_lexico"
)
cl_res$paths   # pca_clusters, perfiles, silhouette_or_dendro

# 2) Coherencia entre métodos de nivel (correlación)
coh <- analizar_coherencia_metodos(
  tokens,
  cols = c("Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc","Nivel_sent_general"),
  out_path = "outputs_lexico/coherencia_metodos/correlacion_metodos.png"
)
# coh$cor_mat  # matriz de correlación

# 3) Validación binaria con split robusto, umbral óptimo y calibración
features_modelo <- c("anger","anticipation","disgust","fear","joy","sadness",
                     "surprise","trust","negative","positive",
                     "Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc")
features_modelo1 <- c("anger","anticipation","disgust","fear","joy","sadness",
                     "surprise","trust","negative","positive",
                     "Nivel__sent_general")
res_val <- validar_binario_con_split(
  tokens   = tokens,
  target   = "Nivel_bin",
  features = features_modelo,
  modelos  = c("rf","svmRadial","glmnet"),
  p_train  = 0.8,
  seed     = 801,
  out_dir  = "outputs_lexico/validacion_bin",
  positive_label = "Pos",
  sampling = NULL,
  split_by = "token_idioma_unico",       # si no existe, usa NULL
  calibr_bins = 10
)

# 4) Guardar artefacto (modelos + receta de dummies + features)
guardar_artefacto_modelos(res_val, features_modelo,
                          path_rds = "outputs_lexico/modelos_emocion.rds")

# 5) Predicción y evaluación rápida sobre TEST (o cualquier df etiquetado)
tst_idx <- res_val$split_idx$test
df_tst  <- tokens[tst_idx, , drop = FALSE]

eval_rf <- evaluar_con_artefacto(df_tst, target = "Nivel_bin",
                                 artifact_rds = "outputs_lexico/modelos_emocion.rds",
                                 modelo = "rf", thr = 0.5)
# eval_rf$metrics ; eval_rf$confusion

# Asumiendo que ya tienes:
# tokens <- res_p1$data
# tokens$Nivel_bin <- ifelse(tokens$Nivel_sent_general %in% c("N-2","N-3"), "Pos", "Neg")
# y el clustering:
# cl_res <- clusterizar_lexico(tokens, k=4, metodo="kmeans", features=seleccionar_features_tokens(tokens))

feat <- seleccionar_features_tokens(tokens)

# Agregar cluster al data.frame para colorear (si usaste kmeans)
# tokens$cluster <- factor(NA)
# tokens$cluster[!is.na(cl_res$clusters)] <- factor(cl_res$clusters)
tokens<-cl_res$data
p1 <- pca_tokens_plot(tokens, features = feat, color_by = "cluster",
                      n_labels = 600, label_size = 2.1, palette = "OkabeIto")

p2 <- pca_tokens_biplot(tokens, features = feat, color_by = "cluster",
                        n_labels = 500, label_size = 2.0, palette = "Dark2",
                        scale_arrows = 1.7, top_vars = min(10, length(feat)))


# 6) Reporte HTML (métricas, CM, ROC, Calibración, VIP) con imágenes embebidas
generar_reporte_html(
  res_val,
  tokens = tokens,
  target = "Nivel_bin",
  titulo = "Validación – Niveles de Sentimiento (DiccionariosEmocion)",
  outfile = "outputs_lexico/reporte_validacion.html"
)
