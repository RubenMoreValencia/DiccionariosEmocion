seleccionar_features_tokens <- function(df) {
  cand <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust",
            "negative","positive","syuzhet","bing","afinn","nrc")
  cand[cand %in% names(df)]
}


clusterizar_lexico <- function(tokens,
                               k = 4,
                               metodo = "kmeans",   # "kmeans" o "hclust"
                               features = NULL,
                               escalar = TRUE,
                               out_dir = "outputs_lexico") {
  if (is.null(features)) features <- seleccionar_features_tokens(tokens)
  X0 <- tokens[, features, drop = FALSE]
  X0 <- X0[, sapply(X0, is.numeric), drop = FALSE]      # solo numéricas
  idx_keep <- stats::complete.cases(X0)                 # índice EXACTO usado
  X <- X0[idx_keep, , drop = FALSE]
  stopifnot(nrow(X) > k, ncol(X) > 1)

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  Xs <- if (escalar) scale(X) else as.matrix(X)

  # PCA
  pca <- stats::prcomp(Xs, center = FALSE, scale. = FALSE)
  pc  <- as.data.frame(pca$x[, 1:2]); names(pc) <- c("PC1","PC2")

  # Clustering
  if (tolower(metodo) == "kmeans") {
    set.seed(801); cl <- stats::kmeans(Xs, centers = k, nstart = 25)
    cl_id <- cl$cluster
  } else {
    d <- stats::dist(Xs, method = "euclidean"); hc <- stats::hclust(d, "ward.D2")
    cl_id <- stats::cutree(hc, k = k)
  }

  # Perfiles
  dfc <- data.frame(cluster = factor(cl_id), X)
  perfiles <- aggregate(. ~ cluster, dfc, mean)

  # Plots
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta ggplot2.")
  library(ggplot2)
  pc$cluster <- factor(cl_id)
  g_pca <- ggplot(pc, aes(PC1, PC2, color = cluster)) +
    geom_point(alpha = .7) +
    labs(title = "PCA de tokens (coloreado por cluster)") +
    theme_minimal(12)
  pca_path <- file.path(out_dir, "pca_clusters.png")
  ggplot2::ggsave(pca_path, g_pca, width = 7, height = 5, dpi = 300)

  if (!requireNamespace("reshape2", quietly = TRUE)) stop("Falta reshape2.")
  perf_long <- reshape2::melt(perfiles, id.vars = "cluster")
  g_perf <- ggplot(perf_long, aes(variable, value, fill = cluster)) +
    geom_col(position = position_dodge(.8)) + coord_flip() +
    labs(title = "Perfiles promedio por cluster", x = NULL, y = "Media") +
    theme_minimal(12)
  perf_path <- file.path(out_dir, "perfiles_clusters.png")
  ggplot2::ggsave(perf_path, g_perf, width = 8, height = 6, dpi = 300)

  # tokens alineado con cluster
  tokens_with_cluster <- tokens
  tokens_with_cluster$cluster <- NA_integer_
  tokens_with_cluster$cluster[idx_keep] <- cl_id
  tokens_with_cluster$cluster <- factor(tokens_with_cluster$cluster)

  list(
    clusters = cl_id,
    perfiles = perfiles,
    pca = pca,
    idx_keep = which(idx_keep),
    tokens_with_cluster = tokens_with_cluster,
    paths = list(pca_clusters = pca_path, perfiles = perf_path)
  )
}




# =====================================================================
# UTILIDADES COMUNES
# =====================================================================

#' Guardar plot usando save_plot() si existe; sino, ggsave()
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @keywords internal
#' @return (auto) Verificar y completar la descripción del valor retornado.
#' @family utilidades
#' @export
#' @param gg (auto) TODO: describir parámetro.
#' @param path (auto) TODO: describir parámetro.
#' @param width (auto) TODO: describir parámetro.
#' @param height (auto) TODO: describir parámetro.
#' @param dpi (auto) TODO: describir parámetro.
sp_save <- function(gg, path, width = 9, height = 6, dpi = 300) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (exists("save_plot", mode = "function")) {
    save_plot(gg, path, width = width, height = height)
  } else {
    ggplot2::ggsave(filename = path, plot = gg, width = width, height = height,
                    units = "in", dpi = dpi)
  }
}

#' Seleccionar columnas numericas relevantes por defecto para tokens
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param tokens data.frame con columnas numericas de metodos y derivadas
#' @param extra vector opcional con nombres adicionales
#' @return vector de nombres de columnas numericas a usar
#' @export
#' @param "valencia_minus" (auto) TODO: describir parámetro.
#' @param "activacion" (auto) TODO: describir parámetro.
seleccionar_features_tokens <- function(tokens,
                                        extra = c("valencia_plus","valencia_minus","activacion")) {
  cand <- c("syuzhet","bing","afinn","nrc",
            "negative","positive",
            "anger","anticipation","disgust","fear","joy","sadness","surprise","trust",
            "sent_general","consenso_metodos",
            extra)
  cand <- unique(cand)
  num_ok <- names(tokens)[sapply(tokens, is.numeric)]
  intersect(cand, num_ok)
}

# =====================================================================
# 1) CLUSTERING LEXICO-EMOCIONAL (K-means / Hclust) + PERFILES
# =====================================================================

#' Clusterizar tokens por perfil emocional y de polaridad
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Genera clusters a nivel **token** usando K-means u Hclust sobre
#' features numericas (metodos + NRC + derivados). Devuelve data con
#' cluster asignado, resúmenes por cluster y graficos: PCA con clusters,
#' dendrograma (si hclust) y perfiles de variables por cluster.
#'
#' @param tokens data.frame de nivel token.
#' @param k numero de clusters (si \code{metodo="kmeans"}).
#' @param metodo "kmeans" o "hclust".
#' @param features vector de nombres de columnas numericas a usar.
#'        Por defecto se infiere con \code{seleccionar_features_tokens()}.
#' @param escalar TRUE para escalar (recomendado).
#' @param seed semilla reproducible.
#' @param out_dir carpeta de salida para imagenes.
#' @return lista con: \code{data}, \code{centros_o_medias}, \code{perfiles},
#'         \code{pca}, \code{paths} (rutas a PNG generados).
#' @export
#' @param "hclust" (auto) TODO: describir parámetro.
clusterizar_lexico <- function(tokens,
                               k = 4,
                               metodo = c("kmeans","hclust"),
                               features = NULL,
                               escalar = TRUE,
                               seed = 801,
                               out_dir = "outputs_lexico") {
  metodo <- match.arg(metodo)
  if (is.null(features)) features <- seleccionar_features_tokens(tokens)
  stopifnot(length(features) > 1)

  X <- tokens[, features, drop = FALSE]
  X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < max(10, 2*k)) stop("Muy pocos tokens con datos completos para clusterizar.")
  if (escalar) Xs <- scale(X) else Xs <- as.matrix(X)

  set.seed(seed)
  if (metodo == "kmeans") {
    km <- stats::kmeans(Xs, centers = k, nstart = 25)
    cl <- factor(km$cluster)
    centros <- as.data.frame(km$centers)
    rownames(centros) <- paste0("C", seq_len(nrow(centros)))
  } else {
    # Distancia euclidiana + enlace Ward
    d  <- stats::dist(Xs)
    hc <- stats::hclust(d, method = "ward.D2")
    cl <- stats::cutree(hc, k = k)
    cl <- factor(cl)
    # "centros" como medias por cluster en el espacio escalado
    centros <- aggregate(Xs, by = list(cluster = cl), FUN = mean)
    rownames(centros) <- paste0("C", centros$cluster)
    centros$cluster <- NULL
  }

  # Adjuntar cluster al data original (alineado con X)
  tokens_cl <- tokens[rownames(X), , drop = FALSE]
  tokens_cl$cluster <- cl

  # Perfiles agregados por cluster (medias de cada feature)
  perfiles <- aggregate(X, by = list(cluster = cl), FUN = mean, na.rm = TRUE)
  names(perfiles)[1] <- "cluster"

  # ----------------- PCA para visualizacion -----------------
  pca <- stats::prcomp(Xs, center = FALSE, scale. = FALSE)
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  pc <- as.data.frame(pca$x[, 1:2])
  pc$cluster <- cl
  # etiquetas: intenta token_idioma_unico o token_orig si existen
  label_col <- if ("token_idioma_unico" %in% names(tokens_cl)) "token_idioma_unico" else
    if ("token_orig" %in% names(tokens_cl)) "token_orig" else NA_character_
  if (!is.na(label_col)) pc$label <- tokens_cl[[label_col]]

  # centroids en espacio PC
  cent_pcs <- aggregate(pc[, 1:2], by = list(cluster = pc$cluster), FUN = mean)
  names(cent_pcs)[1] <- "cluster"

  library(ggplot2)
  # Paleta por defecto (dejar que ggplot elija), tamaños y leyendas detalladas
  g_pca <- ggplot(pc, aes(x = PC1, y = PC2, color = cluster)) +
    geom_point(alpha = .7, size = 2) +
    geom_point(data = cent_pcs, aes(x = PC1, y = PC2, color = cluster),
               shape = 21, fill = NA, size = 4, stroke = 1.2, inherit.aes = FALSE) +
    geom_text(data = cent_pcs,
              aes(x = PC1, y = PC2, label = paste0("C", cluster)),
              fontface = "bold", size = 4, vjust = -1, show.legend = FALSE) +
    labs(
      title = "Mapa PCA de tokens por cluster",
      subtitle = sprintf("Var. explicada: PC1=%.1f%%, PC2=%.1f%% | n=%d | features=%d",
                         100*var_exp[1], 100*var_exp[2], nrow(pc), length(features)),
      x = sprintf("PC1 (%.1f%%)", 100*var_exp[1]),
      y = sprintf("PC2 (%.1f%%)", 100*var_exp[2]),
      color = "Cluster"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(face="bold"))

  # Lineas de referencia
  g_pca <- g_pca + geom_hline(yintercept = 0, linetype = 3, linewidth = .3) +
    geom_vline(xintercept = 0, linetype = 3, linewidth = .3)

  # Biplot (flechas de cargas) para top variables (claridad visual)
  cargas <- as.data.frame(pca$rotation[, 1:2, drop = FALSE])
  cargas$var <- rownames(cargas)
  # top 10 por norma
  cargas$norm <- sqrt(cargas$PC1^2 + cargas$PC2^2)
  cargas_top <- cargas[order(-cargas$norm), ][1:min(10, nrow(cargas)), ]

  g_pca <- g_pca +
    geom_segment(data = cargas_top,
                 aes(x = 0, y = 0, xend = PC1*max(abs(pc$PC1))*0.8,
                     yend = PC2*max(abs(pc$PC2))*0.8),
                 arrow = arrow(length = unit(0.15, "cm")),
                 color = "grey30", linewidth = .5, inherit.aes = FALSE) +
    geom_label(data = cargas_top,
               aes(x = PC1*max(abs(pc$PC1))*0.85,
                   y = PC2*max(abs(pc$PC2))*0.85,
                   label = var),
               fill = "white", alpha = .9, size = 3, label.size = 0,
               inherit.aes = FALSE)

  p_pca <- file.path(out_dir, "pca_clusters.png")
  sp_save(g_pca, p_pca, width = 10, height = 7)

  # ----------------- Dendrograma si hclust -----------------
  p_dend <- NULL
  if (metodo == "hclust") {
    d  <- stats::dist(Xs)
    hc <- stats::hclust(d, method = "ward.D2")
    # base plot guardado en dispositivo
    dend_path <- file.path(out_dir, "dendrograma_hclust.png")
    png(dend_path, width = 1200, height = 800, res = 150)
    plot(hc, labels = FALSE, main = "Dendrograma (Ward.D2)")
    rect.hclust(hc, k = k, border = "grey30")
    dev.off()
    p_dend <- dend_path
  }

  # ----------------- Barras de perfil por cluster -----------------
  perfiles_long <- reshape2::melt(perfiles, id.vars = "cluster",
                                  variable.name = "feature", value.name = "media")
  g_prof <- ggplot(perfiles_long, aes(x = reorder(feature, media), y = media, fill = cluster)) +
    geom_col(position = position_dodge(width = .8), width = .7) +
    coord_flip() +
    labs(title = "Perfiles de variables por cluster (medias)",
         x = NULL, y = "Media (escala original)") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(face="bold"))
  p_prof <- file.path(out_dir, "perfiles_clusters.png")
  sp_save(g_prof, p_prof, width = 10, height = 8)

  list(
    data = tokens_cl,
    centros_o_medias = centros,
    perfiles = perfiles,
    pca = pca,
    paths = list(pca_clusters = p_pca,
                 dendrograma = p_dend,
                 perfiles = p_prof)
  )
}

# =====================================================================
# 2) MAPA EMOCIONAL DETALLADO (PCA) CON ANOTACIONES/LEYENDAS
# =====================================================================

#' Visualizar mapa emocional (PCA) con detalles y anotaciones
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param tokens data.frame (nivel token).
#' @param features vector de columnas numericas a usar (por defecto se infiere).
#' @param color_by columna categorica para colorear (ej. "cluster" o "Nivel_sent_general").
#' @param size_by columna numerica para tamaño del punto (ej. "activacion" o "consenso_metodos").
#' @param label_col columna de texto para etiquetar puntos (ej. "token_idioma_unico").
#' @param top_n_labels maximo de etiquetas (prioriza los de mayor |PC1|+|PC2|).
#' @param out_path ruta del PNG a guardar.
#' @return lista con \code{plot}, \code{paths} y varianza explicada.
#' @family visualizacion
#' @export
visualizar_mapa_emocional <- function(tokens,
                                      features = NULL,
                                      color_by = "cluster",
                                      size_by = NULL,
                                      label_col = NULL,
                                      top_n_labels = 30,
                                      out_path = "outputs_lexico/mapa_emocional_pca.png") {
  if (is.null(features)) features <- seleccionar_features_tokens(tokens)
  X <- tokens[, features, drop = FALSE]
  X <- X[stats::complete.cases(X), , drop = FALSE]
  if (nrow(X) < 5) stop("Muy pocos tokens completos para PCA.")
  Xs <- scale(X)

  pca <- stats::prcomp(Xs, center = FALSE, scale. = FALSE)
  var_exp <- pca$sdev^2 / sum(pca$sdev^2)
  pc <- as.data.frame(pca$x[, 1:2])
  pc$id_row <- as.integer(rownames(pc))

  # unir meta (color/size/label)
  meta <- tokens[rownames(X), , drop = FALSE]
  if (!is.null(color_by) && color_by %in% names(meta)) pc$color_by <- meta[[color_by]]
  if (!is.null(size_by)  && size_by  %in% names(meta)) pc$size_by  <- meta[[size_by]]
  if (!is.null(label_col) && label_col %in% names(meta)) pc$label <- meta[[label_col]]

  library(ggplot2)
  aes_base <- aes(x = PC1, y = PC2,
                  color = if ("color_by" %in% names(pc)) color_by else NULL,
                  size  = if ("size_by"  %in% names(pc)) size_by  else NULL)

  g <- ggplot(pc, aes_base) +
    geom_point(alpha = .75, stroke = 0) +
    labs(
      title = "Mapa emocional PCA (nivel token)",
      subtitle = sprintf("PC1=%.1f%%, PC2=%.1f%% | n=%d | features=%d",
                         100*var_exp[1], 100*var_exp[2], nrow(pc), length(features)),
      x = sprintf("PC1 (%.1f%%)", 100*var_exp[1]),
      y = sprintf("PC2 (%.1f%%)", 100*var_exp[2]),
      color = if (!is.null(color_by) && color_by %in% names(meta)) color_by else NULL,
      size  = if (!is.null(size_by)  && size_by  %in% names(meta)) size_by  else NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(face="bold")) +
    geom_hline(yintercept = 0, linetype = 3, linewidth = .3) +
    geom_vline(xintercept = 0, linetype = 3, linewidth = .3)

  # Etiquetas: top_n por magnitud radial
  if ("label" %in% names(pc) && top_n_labels > 0) {
    pc$rad <- abs(pc$PC1) + abs(pc$PC2)
    lab_df <- pc[order(-pc$rad), ][1:min(top_n_labels, nrow(pc)), ]
    g <- g +
      geom_label(data = lab_df, aes(label = label),
                 size = 3, alpha = .85, label.size = 0,
                 fill = "white", label.padding = unit(0.12, "lines"),
                 show.legend = FALSE)
  }

  sp_save(g, out_path, width = 10, height = 7)
  list(plot = g,
       var_exp = var_exp[1:2],
       paths = list(pca = out_path))
}

# =====================================================================
# 3) CORRELACIONES ENTRE METODOS + HEATMAP + SCATTERS
# =====================================================================

#' Analizar correlaciones entre métodos de sentimiento
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Genera: (1) heatmap de correlaciones; (2) pares de dispersion con
#' recta de ajuste y R^2 en cada panel (cuando hay hasta 4 metodos).
#'
#' @param tokens data.frame (nivel token).
#' @param metodos vector, por defecto c("syuzhet","bing","afinn","nrc").
#' @param out_dir carpeta de salida.
#' @return lista con \code{corr}, \code{paths}.
#' @family utilidades
#' @export
#' @param "bing" (auto) TODO: describir parámetro.
#' @param "afinn" (auto) TODO: describir parámetro.
#' @param "nrc" (auto) TODO: describir parámetro.
analizar_correlaciones_metodos <- function(tokens,
                                           metodos = c("syuzhet","bing","afinn","nrc"),
                                           out_dir = "outputs_lexico") {
  metodos <- intersect(metodos, names(tokens))
  stopifnot(length(metodos) >= 2)
  M <- tokens[, metodos, drop = FALSE]
  M <- M[stats::complete.cases(M), , drop = FALSE]
  corr <- stats::cor(M)

  # Heatmap simple con ggplot
  library(ggplot2)
  library(reshape2)
  cm <- reshape2::melt(corr, varnames = c("x","y"), value.name = "r")
  g_hm <- ggplot(cm, aes(x, y, fill = r)) +
    geom_tile(color = "white") +
    geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
    scale_fill_gradient2(limits = c(-1,1), midpoint = 0) +
    labs(title = "Correlaciones entre metodos (nivel token)",
         x = NULL, y = NULL, fill = "r") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p_hm <- file.path(out_dir, "corr_heatmap_metodos.png")
  sp_save(g_hm, p_hm, width = 8, height = 6)

  # Pares de dispersion (solo si <=4 para legibilidad)
  p_pairs <- NULL
  if (length(metodos) <= 4) {
    # generar grid manual con facetting
    combs <- utils::combn(metodos, 2, simplify = FALSE)
    plots <- list()
    i <- 0
    for (c2 in combs) {
      i <- i + 1
      xnm <- c2[1]; ynm <- c2[2]
      dfp <- data.frame(x = M[[xnm]], y = M[[ynm]])
      mod <- stats::lm(y ~ x, dfp)
      r2  <- summary(mod)$r.squared
      p <- ggplot(dfp, aes(x, y)) +
        geom_point(alpha = .4) +
        geom_smooth(method = "lm", se = FALSE, linewidth = .7) +
        labs(title = sprintf("%s vs %s (R^2=%.2f)", ynm, xnm, r2),
             x = xnm, y = ynm) +
        theme_minimal(base_size = 11)
      plots[[i]] <- p
    }
    # Guardar cada panel y un indice
    p_idx <- file.path(out_dir, "pairs_scatter_index.txt")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    cat("Archivos generados:\n", file = p_idx)
    p_paths <- character(length(plots))
    for (j in seq_along(plots)) {
      p_paths[j] <- file.path(out_dir, sprintf("pair_%02d.png", j))
      sp_save(plots[[j]], p_paths[j], width = 6, height = 5)
      cat(basename(p_paths[j]), "\n", file = p_idx, append = TRUE)
    }
    p_pairs <- p_paths
  }

  list(corr = corr,
       paths = list(heatmap = p_hm, pairs = p_pairs))
}

# =====================================================================
# 4) CLASIFICACION INTERNA: PREDECIR NIVEL (OPCIONAL)
# =====================================================================

predecir_nivel_sentimiento <- function(tokens,
                                       target = "Nivel_sent_general",
                                       features = NULL,
                                       modelos = c("rf","svmRadial","glmnet"),
                                       seed = 801,
                                       out_dir = "outputs_lexico/clasificacion_nivel") {
  stopifnot(target %in% names(tokens))
  if (is.null(features)) features <- seleccionar_features_tokens(tokens)

  df <- tokens[, unique(c(features, target)), drop = FALSE]
  df <- df[stats::complete.cases(df), , drop = FALSE]

  # --- saneo de niveles: que sean nombres válidos ---
  y_raw <- as.character(df[[target]])
  y <- factor(make.names(y_raw))  # convierte "N-0" -> "N.0" (válido para caret)
  df[[target]] <- y

  X <- df[, features, drop = FALSE]
  # convertimos caracteres a factor
  for (nm in names(X)) if (is.character(X[[nm]])) X[[nm]] <- factor(X[[nm]])

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # --- detectar binaria vs multiclase ---
  is_binary <- nlevels(y) == 2

  # control de caret según caso
  if (is_binary) {
    # twoClassSummary necesita classProbs=TRUE y el 1er nivel es "positivo"
    # (si quieres otro positivo, rehace el orden con relevel)
    ctrl <- caret::trainControl(
      method = "repeatedcv", number = 5, repeats = 2,
      classProbs = TRUE, summaryFunction = caret::twoClassSummary,
      savePredictions = "final"
    )
    metric <- "ROC"
  } else {
    # multiclase: usar Accuracy/Kappa con defaultSummary
    ctrl <- caret::trainControl(
      method = "repeatedcv", number = 5, repeats = 2,
      classProbs = FALSE, summaryFunction = caret::defaultSummary,
      savePredictions = "final"
    )
    metric <- "Accuracy"
  }

  # grids
  grids <- list(
    glmnet = expand.grid(alpha = c(0, .5, 1),
                         lambda = 10^seq(-3, 1, length.out = 12)),
    rf     = expand.grid(mtry = pmax(1, round(sqrt(max(1, ncol(X)))))),
    svmRadial = NULL
  )

  have_save_plot <- exists("save_plot", mode = "function")
  sp_save <- function(gg, path, width=8, height=6) {
    if (have_save_plot) save_plot(gg, path, width=width, height=height)
    else ggplot2::ggsave(filename = path, plot = gg, width = width, height = height,
                         units = "in", dpi = 300)
  }

  set.seed(seed)
  fits <- list(); metrics <- list(); plots <- list()

  for (m in modelos) {
    message("Entrenando: ", m)
    grid <- if (!is.null(grids[[m]])) grids[[m]] else NULL

    fit <- caret::train(
      x = X, y = y, method = m, trControl = ctrl, tuneGrid = grid,
      preProcess = c("center","scale"), metric = metric
    )
    fits[[m]] <- fit

    # métricas
    pred_cls <- stats::predict(fit, newdata = X, type = "raw")
    cm <- caret::confusionMatrix(pred_cls, y)
    base_metrics <- data.frame(
      modelo  = m,
      Accuracy = cm$overall["Accuracy"],
      Kappa    = cm$overall["Kappa"]
    )

    if (is_binary) {
      # ROC solo binaria
      levs <- levels(y)
      # prob del nivel positivo (primer nivel)
      pred_prob <- try(stats::predict(fit, newdata = X, type = "prob"), silent = TRUE)
      if (!inherits(pred_prob, "try-error") && levs[1] %in% names(pred_prob)) {
        library(pROC)
        roc <- pROC::roc(response = y, predictor = pred_prob[[levs[1]]], quiet = TRUE)
        auc <- as.numeric(pROC::auc(roc))
        base_metrics$ROC_AUC <- auc

        roc_df <- data.frame(tpr = roc$sensitivities, fpr = 1 - roc$specificities)
        g <- ggplot2::ggplot(roc_df, ggplot2::aes(fpr, tpr)) +
          ggplot2::geom_line() + ggplot2::geom_abline(linetype = 2) +
          ggplot2::labs(title = paste0("ROC ", m, " (AUC=", round(auc,3),")"),
                        x = "1 - Especificidad", y = "Sensibilidad") +
          ggplot2::theme_minimal(base_size = 12)
        pth <- file.path(out_dir, paste0("roc_", m, ".png"))
        sp_save(g, pth)
        plots[[paste0("roc_", m)]] <- pth
      }
    }

    metrics[[m]] <- base_metrics

    # Importancia
    suppressWarnings({
      vi <- try(caret::varImp(fit, scale = TRUE), silent = TRUE)
      if (!inherits(vi, "try-error")) {
        dfvi <- as.data.frame(vi$importance)
        dfvi$feature <- rownames(dfvi); rownames(dfvi) <- NULL
        dfvi <- dfvi[order(-dfvi[[1]]), , drop = FALSE]
        top <- head(dfvi, 20)
        g <- ggplot2::ggplot(top, ggplot2::aes(x = stats::reorder(feature, !!as.name(names(top)[1])),
                                               y = !!as.name(names(top)[1]))) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::labs(title = paste0("Importancia de variables (", m, ")"),
                        x = NULL, y = "Importancia (esc.)") +
          ggplot2::theme_minimal(base_size = 12)
        pth <- file.path(out_dir, paste0("vip_", m, ".png"))
        sp_save(g, pth)
        plots[[paste0("vip_", m)]] <- pth
      }
    })
  }

  list(
    fits = fits,
    metrics = do.call(rbind, metrics),
    plots = plots
  )
}

#' Validación binaria con split (estratificado o por grupo) y calibración
#'
#' Realiza una validación basada en una partición \code{train/test} para un problema
#' binario a nivel token, entrenando varios modelos y comparando métricas. Incluye
#' umbralización, cálculo de \code{ROC AUC}, \code{Brier score} y, opcionalmente,
#' calibración por bins.
#'
#' @details
#' Si \code{split_by} no es \code{NULL}, se hace partición por grupos (por ejemplo,
#' por \code{"token_idioma_unico"}) para evitar fuga de información entre \code{train}
#' y \code{test}. El argumento \code{sampling} admite \code{"smote"}, \code{"up"} o
#' \code{"down"} para balanceo en \code{caret}. Cuando \code{out_dir} no es \code{NULL},
#' se guardan al disco las curvas ROC, calibración y matrices de confusión.
#'
#' @param tokens \code{data.frame}. Datos a nivel token que contienen \code{target} y \code{features}.
#' @param target \code{character}. Nombre de la columna binaria objetivo (factor con 2 niveles).
#' @param features \code{character} o \code{NULL}. Nombres de columnas predictoras a usar;
#'   si es \code{NULL}, se intenta seleccionarlas con \code{seleccionar_features_tokens()}.
#' @param modelos \code{character}. Métodos a entrenar (por ejemplo, \code{c("rf","svmRadial","glmnet")}).
#' @param p_train \code{numeric} en \code{(0,1)}. Proporción para el conjunto de entrenamiento.
#' @param seed \code{numeric}. Semilla para reproducibilidad.
#' @param out_dir \code{character} o \code{NULL}. Carpeta de salida para artefactos; si es \code{NULL},
#'   no se guardan gráficos ni tablas en disco.
#' @param positive_label \code{character} o \code{NULL}. Etiqueta positiva del \code{target} para métricas;
#'   si es \code{NULL}, se infiere con \code{make.names()} o se usa el nivel por defecto.
#' @param sampling \code{character} o \code{NULL}. Estrategia de muestreo en \code{caret} (\code{"smote"},
#'   \code{"up"}, \code{"down"}); si es \code{NULL}, no se aplica balanceo.
#' @param split_by \code{character} o \code{NULL}. Columna para split por grupos; si es \code{NULL},
#'   se realiza partición estratificada estándar.
#' @param calibr_bins \code{integer}. Número de bins para la evaluación de calibración.
#'
#' @return Una \code{list} con:
#'   \code{fits} (modelos \code{caret}),
#'   \code{metrics_cv} (métricas de CV),
#'   \code{metrics_tst_050} (métricas en \code{TEST} con umbral 0.5),
#'   \code{metrics_tst_thr} (métricas en \code{TEST} con umbral óptimo por modelo),
#'   \code{thresholds} (umbrales óptimos por modelo),
#'   \code{brier} (Brier score por modelo),
#'   \code{split_idx} (índices \code{train}/\code{test}),
#'   \code{paths} (rutas de los gráficos guardados, si \code{out_dir} no es \code{NULL}),
#'   \code{dummy_recipe} (objeto \code{caret::dummyVars} para reproducir el preprocesamiento).
#'
#' @examples
#' \dontrun{
#' # Selección explícita de features
#' features <- c("anger","anticipation","disgust","fear","joy","sadness",
#'               "surprise","trust","negative","positive",
#'               "Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc")
#'
#' res_val <- validar_binario_con_split(
#'   tokens  = res_p1$data,
#'   target  = "Nivel_bin",
#'   features = features,
#'   modelos = c("rf","svmRadial","glmnet"),
#'   p_train = 0.8, seed = 801,
#'   out_dir = "outputs_lexico/validacion_bin",
#'   positive_label = "Pos",
#'   sampling = NULL, split_by = NULL, calibr_bins = 10
#' )
#' res_val$metrics_tst_050
#' res_val$metrics_tst_thr
#' }
#'
#' @seealso \code{caret::train}, \code{caret::createDataPartition}, \code{caret::confusionMatrix},
#'   \code{pROC::roc}, \code{pROC::auc}
#' @importFrom stats complete.cases predict quantile
#' @export

validar_binario_con_split <- function(tokens,
                                      target,
                                      features = NULL,
                                      modelos = c("rf","svmRadial","glmnet"),
                                      p_train = 0.80,
                                      seed = 801,
                                      out_dir = "outputs_lexico/validacion_bin",
                                      positive_label = NULL,
                                      sampling = NULL,
                                      split_by = NULL,
                                      calibr_bins = 10) {
  stopifnot(is.data.frame(tokens), target %in% names(tokens))
  if (is.null(features)) features <- seleccionar_features_tokens(tokens)

  have_save_plot <- exists("save_plot", mode = "function")
  sp_save <- function(gg, path, width=8, height=6) {
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    if (have_save_plot) save_plot(gg, path, width=width, height=height)
    else ggplot2::ggsave(filename = path, plot = gg, width = width, height = height,
                         units = "in", dpi = 300)
  }

  # --- columnas a mantener (tolerante a split_by) ---
  cols_keep <- unique(c(features, target,
                        if (!is.null(split_by) && split_by %in% names(tokens)) split_by))
  df <- tokens[, cols_keep, drop = FALSE]
  df <- df[stats::complete.cases(df), , drop = FALSE]
  if (nrow(df) < 20) stop("Muy pocas observaciones completas para validar.")

  # --- target binario (sanear niveles) ---
  y_raw <- as.character(df[[target]])
  y_fac <- factor(make.names(y_raw))
  if (!is.null(positive_label)) {
    pos_mn <- make.names(positive_label)
    if (pos_mn %in% levels(y_fac)) y_fac <- stats::relevel(y_fac, ref = pos_mn)
  }
  df[[target]] <- y_fac
  if (nlevels(y_fac) != 2) stop("El target debe ser binario. Niveles: ", paste(levels(y_fac), collapse=", "))

  # --- separar X (antes de dummificar) e Y ---
  X0 <- df[, features, drop = FALSE]
  # Asegura factor para columnas no numéricas
  for (nm in names(X0)) if (!is.numeric(X0[[nm]])) X0[[nm]] <- as.factor(X0[[nm]])

  # --- one-hot encoding (fullRank) para TODOS los modelos ---
  dmy <- caret::dummyVars(~ ., data = X0, fullRank = TRUE)
  X_all <- as.data.frame(predict(dmy, newdata = X0))
  # Limpieza de infinitos y NAs residuales
  X_all[!is.finite(as.matrix(X_all))] <- NA
  keep <- stats::complete.cases(X_all, df[[target]])
  X_all <- X_all[keep, , drop = FALSE]
  y <- df[[target]][keep]
  if (!is.null(split_by) && split_by %in% names(df)) {
    grp <- df[[split_by]][keep]
  } else {
    grp <- NULL
  }

  # --- split: por grupo (si existe) o estratificado estándar ---
  set.seed(seed)
  if (!is.null(grp)) {
    grupos <- as.character(grp)
    gs <- unique(grupos)
    gs_trn <- sample(gs, size = ceiling(p_train * length(gs)))
    idx_trn <- which(grupos %in% gs_trn)
  } else {
    idx_trn <- caret::createDataPartition(y, p = p_train, list = FALSE)
  }
  idx_tst <- setdiff(seq_len(nrow(X_all)), idx_trn)

  trn <- X_all[idx_trn, , drop = FALSE]; y_trn <- y[idx_trn]
  tst <- X_all[idx_tst, , drop = FALSE]; y_tst <- y[idx_tst]

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # --- control CV interna ---
  ctrl <- caret::trainControl(
    method = "repeatedcv", number = 5, repeats = 2,
    classProbs = TRUE, summaryFunction = caret::twoClassSummary,
    savePredictions = "final", sampling = sampling
  )
  grids <- list(
    glmnet    = expand.grid(alpha = c(0, .5, 1),
                            lambda = 10^seq(-3, 1, length.out = 12)),
    rf        = expand.grid(mtry = pmax(1, round(sqrt(max(1, ncol(trn)))))),
    svmRadial = NULL
  )

  fits <- list(); metrics_cv <- list()
  metrics_tst_050 <- list(); metrics_tst_thr <- list()
  thresholds <- list(); brier <- list(); paths <- list()

  library(ggplot2); library(pROC)

  # --- helpers ---
  thr_opt <- function(resp, prob) {
    roc_obj <- pROC::roc(response = resp, predictor = prob, quiet = TRUE)
    youden <- roc_obj$sensitivities + (1 - roc_obj$specificities) - 1
    thr <- roc_obj$thresholds[which.max(youden)]
    if (!is.finite(thr) || is.na(thr)) thr <- 0.5
    list(threshold = thr, roc = roc_obj)
  }
  calibrar <- function(resp, prob, bins = 10) {
    resp01 <- as.numeric(resp == levels(resp)[1])
    brier_score <- mean((prob - resp01)^2, na.rm = TRUE)
    qs <- stats::quantile(prob, probs = seq(0, 1, length.out = bins + 1), na.rm = TRUE, names = FALSE)
    brks <- unique(qs)
    if (length(brks) < 3) brks <- unique(c(min(prob, na.rm=TRUE), median(prob, na.rm=TRUE), max(prob, na.rm=TRUE)))
    bin  <- cut(prob, breaks = brks, include.lowest = TRUE)
    obs_rate <- tapply(resp01, bin, mean, na.rm = TRUE)
    est_prob <- tapply(prob,   bin, mean, na.rm = TRUE)
    data.frame(est_prob = as.numeric(est_prob),
               obs_rate = as.numeric(obs_rate),
               bin = names(est_prob),
               brier = brier_score)
  }
  eval_tst <- function(modelo_tag, fit, tstX, tstY) {
    # Probabilidades; si vienen NAs, filtrar
    pred_prob <- try(stats::predict(fit, newdata = tstX, type = "prob"), silent = TRUE)
    if (inherits(pred_prob, "try-error")) {
      stop("El modelo ", modelo_tag, " no devolvió probabilidades. Revisa 'classProbs=TRUE'.")
    }
    p <- pred_prob[, levels(y_trn)[1]]
    ok <- is.finite(p)
    if (!all(ok)) {
      warning("Se filtraron ", sum(!ok), " filas en TEST por prob NA/Inf para ", modelo_tag, ".")
      p <- p[ok]; tstY <- tstY[ok]; tstX <- tstX[ok, , drop = FALSE]
    }

    pred_cls0 <- stats::predict(fit, newdata = tstX, type = "raw")

    # ROC & AUC
    roc_obj <- pROC::roc(response = tstY, predictor = p, quiet = TRUE)
    auc_val <- as.numeric(pROC::auc(roc_obj))

    # CM a 0.5
    cm0 <- caret::confusionMatrix(pred_cls0, tstY)
    met_050 <- data.frame(modelo = modelo_tag,
                          Accuracy = cm0$overall["Accuracy"],
                          Kappa    = cm0$overall["Kappa"],
                          ROC_AUC  = auc_val)

    # Umbral óptimo
    topt <- thr_opt(tstY, p); thr <- topt$threshold
    pred_thr <- factor(ifelse(p >= thr, levels(y_trn)[1], levels(y_trn)[2]),
                       levels = levels(y_trn))
    cmT <- caret::confusionMatrix(pred_thr, tstY)
    met_thr <- data.frame(modelo = modelo_tag, thr = thr,
                          Accuracy = cmT$overall["Accuracy"],
                          Kappa    = cmT$overall["Kappa"],
                          ROC_AUC  = auc_val)

    # Gráficos
    cm_df0 <- as.data.frame(cm0$table)
    g_cm0 <- ggplot(cm_df0, aes(Prediction, Reference, fill = Freq)) +
      geom_tile(color = "white") + geom_text(aes(label = Freq)) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = paste0("CM (TEST, thr=0.5) - ", modelo_tag),
           subtitle = sprintf("Acc=%.3f | Kappa=%.3f", cm0$overall["Accuracy"], cm0$overall["Kappa"]),
           x = "Predicho", y = "Real", fill = "Frecuencia") +
      theme_minimal(base_size = 12)
    p_cm0 <- file.path(out_dir, paste0("cm_tst_050_", modelo_tag, ".png")); sp_save(g_cm0, p_cm0)

    cm_dfT <- as.data.frame(cmT$table)
    g_cmT <- ggplot(cm_dfT, aes(Prediction, Reference, fill = Freq)) +
      geom_tile(color = "white") + geom_text(aes(label = Freq)) +
      scale_fill_gradient(low = "white", high = "steelblue") +
      labs(title = paste0("CM (TEST, thr=", round(thr,3), ") - ", modelo_tag),
           subtitle = sprintf("Acc=%.3f | Kappa=%.3f", cmT$overall["Accuracy"], cmT$overall["Kappa"]),
           x = "Predicho", y = "Real", fill = "Frecuencia") +
      theme_minimal(base_size = 12)
    p_cmT <- file.path(out_dir, paste0("cm_tst_thr_", modelo_tag, ".png")); sp_save(g_cmT, p_cmT)

    roc_df <- data.frame(tpr = topt$roc$sensitivities, fpr = 1 - topt$roc$specificities)
    g_roc <- ggplot(roc_df, aes(fpr, tpr)) +
      geom_line() + geom_abline(linetype = 2) +
      labs(title = paste0("ROC (TEST) - ", modelo_tag, " | AUC=", round(auc_val, 3)),
           x = "1 - Especificidad", y = "Sensibilidad") +
      theme_minimal(base_size = 12)
    p_roc <- file.path(out_dir, paste0("roc_tst_", modelo_tag, ".png")); sp_save(g_roc, p_roc)

    cal_df <- calibrar(tstY, p, bins = calibr_bins)
    g_cal <- ggplot(cal_df, aes(est_prob, obs_rate)) +
      geom_point() + geom_line() + geom_abline(linetype = 2) +
      labs(title = paste0("Calibración (TEST) - ", modelo_tag,
                          " | Brier=", sprintf("%.4f", unique(cal_df$brier))),
           x = "Prob. estimada (promedio por bin)", y = "Tasa observada") +
      theme_minimal(base_size = 12)
    p_cal <- file.path(out_dir, paste0("calibracion_tst_", modelo_tag, ".png")); sp_save(g_cal, p_cal)

    list(
      met_050 = met_050,
      met_thr = met_thr,
      thr     = thr,
      brier   = unique(cal_df$brier),
      paths   = list(cm_050 = p_cm0, cm_thr = p_cmT, roc = p_roc, cal = p_cal)
    )
  }

  # --- CV + entrenamiento + evaluación ---
  ctrl <- ctrl  # explícito
  grids <- grids

  fits <- list(); metrics_cv <- list()
  metrics_tst_050 <- list(); metrics_tst_thr <- list()
  thresholds <- list(); brier <- list(); paths <- list()

  for (m in modelos) {
    message("Entrenando: ", m)
    grid <- if (!is.null(grids[[m]])) grids[[m]] else NULL
    fit <- caret::train(
      x = trn, y = y_trn, method = m,
      trControl = ctrl, tuneGrid = grid,
      preProcess = c("center","scale"), metric = "ROC"
    )
    fits[[m]] <- fit
    metrics_cv[[m]] <- fit$results

    # importancia
    suppressWarnings({
      vi <- try(caret::varImp(fit, scale = TRUE), silent = TRUE)
      if (!inherits(vi, "try-error")) {
        dfvi <- as.data.frame(vi$importance)
        dfvi$feature <- rownames(dfvi); rownames(dfvi) <- NULL
        dfvi <- dfvi[order(-dfvi[[1]]), , drop = FALSE]
        top <- head(dfvi, 20)
        g_vi <- ggplot2::ggplot(top, ggplot2::aes(x = stats::reorder(feature, !!as.name(names(top)[1])),
                                                  y = !!as.name(names(top)[1]))) +
          ggplot2::geom_col() + ggplot2::coord_flip() +
          ggplot2::labs(title = paste0("Importancia de variables (", m, ")"),
                        x = NULL, y = "Importancia (esc.)") +
          ggplot2::theme_minimal(base_size = 12)
        p_vi <- file.path(out_dir, paste0("vip_", m, ".png")); sp_save(g_vi, p_vi)
        paths[[m]] <- list(vip = p_vi)
      }
    })

    ev <- eval_tst(m, fit, tst, y_tst)
    metrics_tst_050[[m]] <- ev$met_050
    metrics_tst_thr[[m]] <- ev$met_thr
    thresholds[[m]]     <- ev$thr
    brier[[m]]          <- ev$brier
    paths[[m]]          <- c(paths[[m]], ev$paths)
  }

  list(
    fits = fits,
    metrics_cv = metrics_cv,
    metrics_tst_050 = do.call(rbind, metrics_tst_050),
    metrics_tst_thr = do.call(rbind, metrics_tst_thr),
    thresholds = thresholds,
    brier = brier,
    split_idx = list(train = idx_trn, test = idx_tst),
    paths = paths,
    dummy_recipe = dmy  # útil si luego quieres predecir fuera de muestra
  )
}


#' Predecir Nivel_bin con modelo entrenado y receta de dummies
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param new_df data.frame con las mismas features originales (no dummificadas)
#' @param artefacto_rds path al .rds guardado con (fits + dummies + features)
#' @param modelo uno de c("rf","svmRadial","glmnet")
#' @param devolver c("clase","prob") para clase o probabilidad del positivo
#' @family utilidades
#' @export
#' @return (auto) Verificar y completar la descripción del valor retornado.
#' @param "prob" (auto) TODO: describir parámetro.
predecir_nivel_bin <- function(new_df, artefacto_rds = "outputs_lexico/modelos_emocion.rds",
                               modelo = "rf", devolver = c("clase","prob")) {
  devolver <- match.arg(devolver)
  art <- readRDS(artefacto_rds)
  stopifnot(modelo %in% names(art$fit_rf %>% list(rf=., svmRadial=art$fit_svm, glmnet=art$fit_glm)))
  # asegurar columnas y tipos
  X0 <- new_df[, art$features, drop = FALSE]
  for (nm in names(X0)) if (!is.numeric(X0[[nm]])) X0[[nm]] <- as.factor(X0[[nm]])
  X <- as.data.frame(predict(art$dummies, newdata = X0))
  fit <- switch(modelo, rf = art$fit_rf, svmRadial = art$fit_svm, glmnet = art$fit_glm)
  if (devolver == "clase") {
    stats::predict(fit, newdata = X, type = "raw")
  } else {
    pp <- stats::predict(fit, newdata = X, type = "prob")
    pp[, levels(fit$trainingData$.outcome)[1]]  # prob del positivo
  }
}

# ================== UTILIDADES ESTABLES PARA PRODUCCION ==================

# Asegura que el objeto de validación guarde las 'features' usadas
.attach_features_to_res <- function(res_obj, features) {
  if (is.null(res_obj$features)) res_obj$features <- features
  res_obj
}

# 1) Empaquetar artefacto (modelos + receta + features + etiqueta positiva)
#    Guarda todo lo necesario para predecir en el futuro sin ambigüedad.
guardar_artefacto_modelos <- function(res_val, features, path_rds = "outputs_lexico/modelos_emocion.rds") {
  dir.create(dirname(path_rds), showWarnings = FALSE, recursive = TRUE)
  res_val <- .attach_features_to_res(res_val, features)
  positive <- levels(res_val$fits$rf$trainingData$.outcome)[1]
  art <- list(
    fits     = res_val$fits,
    dummies  = res_val$dummy_recipe,
    features = res_val$features,
    positive = positive
  )
  saveRDS(art, path_rds)
  invisible(path_rds)
}

# 2) Construir matriz X con la MISMA receta de dummies usada al entrenar
#    new_df: datos crudos que contengan las columnas 'features' originales (no dummies)
build_matrix_from_artifact <- function(new_df, artifact) {
  stopifnot(is.list(artifact), !is.null(artifact$dummies), !is.null(artifact$features))
  X0 <- new_df[, artifact$features, drop = FALSE]
  # Forzar tipos factor para no-numéricas
  for (nm in names(X0)) if (!is.numeric(X0[[nm]])) X0[[nm]] <- as.factor(X0[[nm]])
  X <- as.data.frame(predict(artifact$dummies, newdata = X0))
  # Alinear columnas con el modelo principal (tomamos RF por defecto para xNames)
  need <- artifact$fits$rf$finalModel$xNames
  miss <- setdiff(need, names(X))
  if (length(miss)) X[, miss] <- 0
  X <- X[, need, drop = FALSE]
  X
}

# 3) Predecir (clase o prob) con cualquier modelo del artefacto
predecir_con_artefacto <- function(new_df, artifact_rds = "outputs_lexico/modelos_emocion.rds",
                                   modelo = c("rf","svmRadial","glmnet"),
                                   devolver = c("prob","clase")) {
  modelo   <- match.arg(modelo)
  devolver <- match.arg(devolver)
  art <- readRDS(artifact_rds)

  # Construir X consistente
  X <- build_matrix_from_artifact(new_df, art)

  fit <- switch(modelo,
                rf        = art$fits$rf,
                svmRadial = art$fits$svmRadial,
                glmnet    = art$fits$glmnet)

  if (devolver == "clase") {
    return(stats::predict(fit, newdata = X, type = "raw"))
  } else {
    pp <- stats::predict(fit, newdata = X, type = "prob")
    pos <- levels(fit$trainingData$.outcome)[1]
    return(pp[, pos])
  }
}

# 4) Evaluar rápidamente en un conjunto etiquetado (hold-out u otro)
evaluar_con_artefacto <- function(df_labeled, target, artifact_rds = "outputs_lexico/modelos_emocion.rds",
                                  modelo = c("rf","svmRadial","glmnet"), thr = 0.5) {
  modelo <- match.arg(modelo)
  art <- readRDS(artifact_rds)
  stopifnot(target %in% names(df_labeled))
  y_raw <- as.character(df_labeled[[target]])
  y_fac <- factor(make.names(y_raw))
  # Alinear niveles con el modelo (positivo es el primer nivel en entrenamiento)
  pos <- levels(art$fits$rf$trainingData$.outcome)[1]
  if (!(pos %in% levels(y_fac))) y_fac <- stats::relevel(y_fac, ref = levels(y_fac)[1])

  X <- build_matrix_from_artifact(df_labeled, art)
  pp <- predecir_con_artefacto(df_labeled, artifact_rds = artifact_rds, modelo = modelo, devolver = "prob")

  # ROC / AUC
  roc_obj <- pROC::roc(response = y_fac, predictor = pp, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))

  # CM @ thr
  pred_thr <- factor(ifelse(pp >= thr, levels(y_fac)[1], levels(y_fac)[2]),
                     levels = levels(y_fac))
  cm <- caret::confusionMatrix(pred_thr, y_fac)

  # Brier
  y01 <- as.integer(y_fac == levels(y_fac)[1])
  brier <- mean((pp - y01)^2)

  list(
    metrics = c(Accuracy = cm$overall["Accuracy"], Kappa = cm$overall["Kappa"], AUC = auc_val, Brier = brier),
    roc = roc_obj,
    confusion = cm
  )
}

#' Coherencia entre métodos de etiquetado de sentimiento (matriz y gráfico)
#'
#' Calcula la coherencia entre métodos de etiquetado de sentimiento a partir de
#' columnas con niveles categóricos (p. ej., \code{Nivel_syuzhet}, \code{Nivel_bing},
#' \code{Nivel_afinn}, \code{Nivel_nrc}, \code{Nivel_sent_general}). Convierte los
#' niveles a códigos numéricos y estima una matriz de correlación; opcionalmente
#' guarda un heatmap en disco.
#'
#' @details
#' Los niveles con prefijos \code{N-} y \code{N+} se transforman a valores numéricos
#' simples (por ejemplo, \code{"N-3"} \eqn{\rightarrow} \code{-3}, \code{"N+2"} \eqn{\rightarrow} \code{2})
#' para calcular la correlación entre métodos (\code{stats::cor} con \code{use = "pairwise.complete.obs"}).
#' Si \code{out_path} no es \code{NULL}, se genera y guarda un gráfico de
#' correlaciones usando \code{ggcorrplot::ggcorrplot}.
#'
#' @param df \code{data.frame} que contiene las columnas de niveles por token.
#' @param cols \code{character} o \code{NULL}. Vector con los nombres de columnas a evaluar.
#'   Si es \code{NULL}, se usa el conjunto canónico
#'   \code{c("Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc","Nivel_sent_general")}.
#' @param out_path \code{character} o \code{NULL}. Ruta del archivo para guardar la figura.
#'   Si es \code{NULL}, no se guarda imagen.
#'
#' @return Una \code{list} con:
#'   \code{cor_mat} (matriz de correlación numérica) y
#'   \code{plot_path} (ruta del archivo escrito si \code{out_path} no es \code{NULL}; de lo contrario \code{NULL}).
#'
#' @examples
#' \dontrun{
#' df_tokens <- res_p1$data
#' coh <- analizar_coherencia_metodos(
#'   df_tokens,
#'   cols = NULL,
#'   out_path = "outputs_lexico/coherencia_metodos/cor_metodos.png"
#' )
#' coh$cor_mat
#' }
#'
#' @seealso \code{ggcorrplot::ggcorrplot}, \code{ggplot2::ggsave}, \code{stats::cor}
#' @importFrom stats cor complete.cases
#' @importFrom ggplot2 theme element_text ggsave
#' @family utilidades
#' @export
analizar_coherencia_metodos <- function(
    df,
    cols = NULL,
    out_path = NULL
) {
  # Default canónico para 'cols' (fuera de la firma para no confundir al auditor)
  if (is.null(cols)) {
    cols <- c("Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc","Nivel_sent_general")
  }

  # Validación de columnas
  faltan <- setdiff(cols, names(df))
  if (length(faltan)) {
    stop("Columnas no encontradas en 'df': ", paste(faltan, collapse = ", "))
  }

  # Crear carpeta si corresponde
  if (!is.null(out_path)) {
    dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  }

  # Conversión de niveles a numérico
  niv_to_num <- function(x) {
    suppressWarnings(as.numeric(gsub("N-", "-", gsub("N\\+", "", as.character(x)))))
  }
  df_num <- as.data.frame(lapply(df[, cols, drop = FALSE], niv_to_num))
  df_num <- df_num[stats::complete.cases(df_num), ]

  cor_mat <- stats::cor(df_num, use = "pairwise.complete.obs")

  # Gráfico
  g <- ggcorrplot::ggcorrplot(
    cor_mat,
    lab = TRUE, lab_size = 3,
    colors = c("red", "white", "blue"),
    title = "Correlación entre métodos de nivel de sentimiento"
  ) + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  plot_path <- NULL
  if (!is.null(out_path)) {
    if (exists("save_plot", mode = "function")) {
      save_plot(g, out_path, 8, 6)
    } else {
      ggplot2::ggsave(out_path, g, width = 8, height = 6, dpi = 300)
    }
    plot_path <- out_path
  }

  list(cor_mat = cor_mat, plot_path = plot_path)
}


#' Entrenamiento de clasificadores de nivel de sentimiento (token-level)
#'
#' Entrena modelos supervisados (p. ej., \code{rf}, \code{svmRadial}, \code{glmnet})
#' para predecir el nivel objetivo de sentimiento. Soporta partición estratificada
#' o por grupo con \code{split_by}.
#'
#' @details
#' Construye \code{Nivel_bin} desde \code{target} (por defecto considera \code{"N-2"} y \code{"N-3"} como positivos)
#' y delega en \code{validar_binario_con_split()} para el pipeline de train/test, métricas y exportación.
#'
#' @param df \code{data.frame}. Datos a nivel token con \code{target} y predictores potenciales.
#' @param target \code{character}. Columna objetivo ordinal para derivar \code{Nivel_bin}.
#' @param out_dir \code{character}. Carpeta de salida para artefactos del modelo.
#' @param modelos \code{character} o \code{NULL}. Métodos a entrenar; si \code{NULL}, usa
#'   \code{c("rf","svmRadial","glmnet")}.
#' @param positive_label \code{character}. Etiqueta positiva del binario (\code{"Pos"} por defecto).
#' @param split_by \code{character} o \code{NULL}. Columna para split por grupos.
#' @param seed \code{numeric}. Semilla para reproducibilidad.
#'
#' @return La lista retornada por \code{validar_binario_con_split()}.
#'
#' @examples
#' \dontrun{
#' res <- entrenar_modelo_nivel(
#'   df = res_p1$data,
#'   target = "Nivel_sent_general",
#'   out_dir = "outputs_lexico/modelo_niveles_metodos",
#'   modelos = NULL,
#'   positive_label = "Pos",
#'   split_by = "token_idioma_unico",
#'   seed = 801
#' )
#' }
#'
#' @seealso validar_binario_con_split
#' @importFrom stats complete.cases
#' @family modelado_supervisado
#' @export
entrenar_modelo_nivel <- function(df,
                                  target = "Nivel_sent_general",
                                  out_dir = "outputs_lexico/modelo_niveles_metodos",
                                  modelos = NULL,
                                  positive_label = "Pos",
                                  split_by = NULL,
                                  seed = 801) {
  if (is.null(modelos)) modelos <- c("rf","svmRadial","glmnet")
  cols_base <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust",
                 "negative","positive","Nivel_syuzhet","Nivel_bing","Nivel_afinn","Nivel_nrc",target)
  cols_exist <- cols_base[cols_base %in% names(df)]
  df_modelo <- df[, cols_exist, drop = FALSE]
  df_modelo <- df_modelo[complete.cases(df_modelo), ]

  # --- definir binario ---
  # Puedes ajustar los niveles según tu escala (N-2 y N-3 = positivo)
  df_modelo$Nivel_bin <- ifelse(df_modelo[[target]] %in% c("N-2","N-3"), "Pos", "Neg")

  # --- definir features ---
  features <- setdiff(cols_exist, c(target))

  # --- entrenar ---
  res_val <- validar_binario_con_split(
    tokens  = df_modelo,
    target  = "Nivel_bin",
    features = features,
    modelos = modelos,
    p_train = 0.8,
    seed    = seed,
    out_dir = out_dir,
    positive_label = positive_label,
    sampling = NULL,
    split_by = split_by,
    calibr_bins = 10
  )

  return(res_val)
}







#' Extraer features emocionales multimetodo (token -> nivel documento/categoria)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Integra una matriz global (dfx) con un detalle tokenizado (tokens) para
#' construir features agregadas explicativas por grupo (documento, categoria, etc.).
#'
#' dfx (ej. head):
#'   ira, anticipacion, disgusto, miedo, alegria, tristeza, sorpresa, confianza,
#'   negativo, positivo, syuzhet, bing, afinn, nrc   (una fila por termino/entrada)
#'
#' tokens (ej. res_p1$data, head):
#'   token_orig, token_idioma, token_idioma_unico, syuzhet, bing, afinn, nrc,
#'   anger, anticipation, disgust, fear, joy, sadness, surprise, trust,
#'   negative, positive, sent_general, Nivel_* , consenso_metodos,
#'   valencia_plus, valencia_minus, activacion, <columna_grupo>
#'
#' @param dfx data.frame matriz global de emociones y metodos.
#' @param tokens data.frame detalle tokenizado (res_p1$data).
#' @param group_col nombre de la columna en \code{tokens} que define el nivel
#'        de agregacion (p.ej. "documento_id", "categoria", "fuente").
#'        Si \code{NULL}, agrega a un solo grupo global.
#' @param join_by columna para unir dfx ~ tokens (por defecto "token_idioma_unico",
#'        usando nombres de fila de dfx si existen; si no, merge por interseccion
#'        de columnas de tokens y dfx que sean texto).
#' @return data.frame con una fila por grupo y columnas de features.
#' @family utilidades
#' @export
extraer_features_emocionales <- function(dfx, tokens,
                                         group_col = NULL,
                                         join_by = "token_idioma_unico") {
  stopifnot(is.data.frame(dfx), is.data.frame(tokens))

  # normalizar nombres (sin tildes/espacios)
  std_names <- function(x) {
    x <- gsub("\\s+", "_", x)
    x <- tolower(x)
    x
  }
  names(dfx)    <- std_names(names(dfx))
  names(tokens) <- std_names(names(tokens))
  if (!is.null(group_col)) group_col <- std_names(group_col)
  join_by <- std_names(join_by)

  # si dfx tiene rownames utiles, exponlos como columna para poder unir
  if (is.null(dfx[[join_by]]) && !is.null(rownames(dfx))) {
    dfx[[join_by]] <- rownames(dfx)
  }

  # columnas numericas candidatas en dfx y tokens
  emo_cols_dfx <- intersect(
    c("ira","anticipacion","disgusto","miedo","alegria","tristeza","sorpresa","confianza",
      "negativo","positivo","syuzhet","bing","afinn","nrc"),
    names(dfx)
  )
  emo_cols_tok <- intersect(
    c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust",
      "negative","positive","syuzhet","bing","afinn","nrc",
      "sent_general","consenso_metodos","valencia_plus","valencia_minus","activacion"),
    names(tokens)
  )

  # merge robusto
  can_join <- join_by %in% names(dfx) && join_by %in% names(tokens)
  if (!can_join) {
    # fallback: intenta unir por interseccion de columnas de texto
    cand <- intersect(names(dfx)[sapply(dfx, is.character)],
                      names(tokens)[sapply(tokens, is.character)])
    if (length(cand) == 0) stop("No hay llave de union valida entre dfx y tokens.")
    join_by <- cand[1]
  }

  dfm <- merge(dfx[, unique(c(emo_cols_dfx, join_by)), drop=FALSE],
               tokens[, unique(c(emo_cols_tok, join_by, group_col)), drop=FALSE],
               by = join_by, all.x = TRUE)

  # helpers
  qnum <- function(x, p) if (length(x)) stats::quantile(x, p, na.rm = TRUE, names = FALSE) else NA_real_

  # si no hay grupo, crealo global
  if (is.null(group_col) || !(group_col %in% names(dfm))) {
    dfm$.__grupo__ <- "global"
    group_col <- ".__grupo__"
  }

  # construir features por grupo
  split_g <- split(dfm, dfm[[group_col]])
  out <- lapply(names(split_g), function(g) {
    dd <- split_g[[g]]

    # consenso entre metodos por fila (sd) y polaridad media
    met_cols <- intersect(c("syuzhet","bing","afinn","nrc"), names(dd))
    if (length(met_cols) > 0) {
      mtx <- as.matrix(dd[, met_cols])
      row_sd   <- apply(mtx, 1, stats::sd, na.rm = TRUE)
      row_mean <- rowMeans(mtx, na.rm = TRUE)
    } else {
      row_sd <- row_mean <- rep(NA_real_, nrow(dd))
    }

    # balances emociones NRC en dfx y en tokens (si existen)
    pos_nrc_dfx <- rowSums(dd[, intersect(c("alegria","confianza","anticipacion","sorpresa"), names(dd)), drop=FALSE], na.rm = TRUE)
    neg_nrc_dfx <- rowSums(dd[, intersect(c("ira","miedo","disgusto","tristeza"), names(dd)), drop=FALSE], na.rm = TRUE)
    bal_nrc_dfx <- pos_nrc_dfx - neg_nrc_dfx

    pos_nrc_tok <- rowSums(dd[, intersect(c("joy","trust","anticipation","surprise"), names(dd)), drop=FALSE], na.rm = TRUE)
    neg_nrc_tok <- rowSums(dd[, intersect(c("anger","fear","disgust","sadness"), names(dd)), drop=FALSE], na.rm = TRUE)
    bal_nrc_tok <- pos_nrc_tok - neg_nrc_tok

    # agregados estadisticos (se pueden ampliar)
    num_cols <- intersect(c(
      met_cols,
      "negative","positive","negativo","positivo",
      "sent_general","consenso_metodos","valencia_plus","valencia_minus","activacion"
    ), names(dd))

    agg_mean <- if (length(num_cols)) sapply(dd[, num_cols, drop=FALSE], function(x) mean(x, na.rm = TRUE)) else numeric()
    agg_sd   <- if (length(num_cols)) sapply(dd[, num_cols, drop=FALSE], function(x) stats::sd(x, na.rm = TRUE)) else numeric()
    names(agg_mean) <- paste0(names(agg_mean), "_mean")
    names(agg_sd)   <- paste0(names(agg_sd),   "_sd")

    # resumenes de consenso y polaridad media a nivel grupo
    feats <- c(
      grupo = g,
      n_tokens = nrow(dd),
      met_polaridad_mean = mean(row_mean, na.rm = TRUE),
      met_polaridad_sd   = stats::sd(row_mean, na.rm = TRUE),
      met_consenso_sd_mean = mean(row_sd, na.rm = TRUE),
      met_consenso_sd_q75  = qnum(row_sd, .75),
      nrc_balance_dfx_mean = mean(bal_nrc_dfx, na.rm = TRUE),
      nrc_balance_tok_mean = mean(bal_nrc_tok, na.rm = TRUE),
      agg_mean, agg_sd
    )

    as.data.frame(as.list(feats), check.names = FALSE)
  })

  res <- do.call(rbind, out)
  rownames(res) <- NULL
  # coercion numericos
  for (nm in names(res)) {
    if (nm != "grupo") {
      suppressWarnings({
        res[[nm]] <- as.numeric(res[[nm]])
      })
    }
  }
  res
}

#' Entrenar y comparar modelos usando features emocionales
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Entrena modelos (glmnet, rf, gbm, svmRadial) con CV repetida, calcula métricas
#' y guarda gráficos diagnósticos. Usa `save_plot()` si existe; si no, `ggplot2::ggsave()`.
#'
#' @param data data.frame de features (una fila por grupo/documento).
#' @param target nombre de la variable objetivo en \code{data}.
#' @param tipo "regresion" o "clasificacion".
#' @param modelos vector de métodos caret (por defecto c("glmnet","rf","gbm")).
#' @param seed semilla reproducible.
#' @param out_dir carpeta para guardar resultados/figuras.
#' @return lista con \code{fits}, \code{metrics}, \code{plots}.
#' @family modelado_supervisado
#' @export
#' @param "clasificacion" (auto) TODO: describir parámetro.
entrenar_modelos_emocionales <- function(data, target,
                                         tipo = c("regresion","clasificacion"),
                                         modelos = c("glmnet","rf","gbm"),
                                         seed = 801,
                                         out_dir = "outputs_modelos") {
  stopifnot(is.data.frame(data), target %in% names(data))
  tipo <- match.arg(tipo)

  # utils de guardado
  have_save_plot <- exists("save_plot", mode = "function")
  sp_save <- function(gg, path, width=8, height=5) {
    if (have_save_plot) {
      save_plot(gg, path, width = width, height = height)
    } else {
      ggplot2::ggsave(filename = path, plot = gg, width = width, height = height, units = "in", dpi = 300)
    }
  }

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # separar X/y
  y <- data[[target]]
  x <- data[, setdiff(names(data), target), drop=FALSE]
  # quitar columnas no numericas salvo que caret las acepte como factor
  # (caret maneja factores; mantenlos)
  # aseguramos que los caracteres no-target en x no den problemas
  for (nm in names(x)) {
    if (is.character(x[[nm]])) x[[nm]] <- factor(x[[nm]])
  }

  # setup caret
  set.seed(seed)
  ctrl <- caret::trainControl(
    method = "repeatedcv", number = 5, repeats = 3,
    classProbs = (tipo == "clasificacion"),
    summaryFunction = if (tipo == "clasificacion") caret::twoClassSummary else caret::defaultSummary,
    savePredictions = "final"
  )

  # grids pequeños razonables
  grids <- list(
    glmnet = expand.grid(alpha = c(0, .5, 1), lambda = 10^seq(-3, 1, length.out = 12)),
    rf     = expand.grid(mtry  = pmax(1, round(sqrt(max(1, ncol(x)))))),
    gbm    = expand.grid(interaction.depth = c(1, 3, 5),
                         n.trees = c(100, 300, 600),
                         shrinkage = c(0.05, 0.1),
                         n.minobsinnode = c(5, 10))
  )

  fits <- list()
  metrics <- list()
  plots <- list()

  for (m in modelos) {
    message("Entrenando: ", m)
    grid <- if (!is.null(grids[[m]])) grids[[m]] else NULL

    fit <- caret::train(
      x = x, y = y,
      method = m,
      trControl = ctrl,
      tuneGrid = grid,
      preProcess = c("center","scale")
    )
    fits[[m]] <- fit

    # métricas base
    if (tipo == "regresion") {
      pred <- stats::predict(fit, newdata = x)
      rmse <- Metrics::rmse(y, pred)
      mae  <- Metrics::mae (y, pred)
      r2   <- 1 - sum((y - pred)^2, na.rm=TRUE) / sum((y - mean(y,na.rm=TRUE))^2, na.rm=TRUE)
      metrics[[m]] <- data.frame(modelo = m, RMSE = rmse, MAE = mae, R2 = r2)

      # plot residuales
      dfp <- data.frame(obs = y, pred = pred, res = y - pred)
      gg <- ggplot2::ggplot(dfp, ggplot2::aes(pred, res)) +
        ggplot2::geom_hline(yintercept = 0, linetype = 2) +
        ggplot2::geom_point(alpha = .6) +
        ggplot2::labs(title = paste0("Residuos vs Predicho (", m, ")"), x = "Predicho", y = "Residual")
      pth <- file.path(out_dir, paste0("residuos_", m, ".png"))
      sp_save(gg, pth)
      plots[[paste0("residuos_", m)]] <- pth

    } else {
      # clasificacion binaria: y debe ser factor con niveles (Pos, Neg) o similar
      if (!is.factor(y)) y <- factor(y)
      # caret twoClassSummary espera nivel positivo en y: usa el primero como positivo
      levs <- levels(y)
      pred_prob <- stats::predict(fit, newdata = x, type = "prob")
      pred_cls  <- stats::predict(fit, newdata = x, type = "raw")

      # ROC-AUC si hay columna del positivo
      pos <- levs[1]
      if (!is.null(pred_prob[[pos]])) {
        roc <- pROC::roc(response = y, predictor = pred_prob[[pos]], quiet = TRUE)
        auc <- as.numeric(pROC::auc(roc))
      } else {
        auc <- NA_real_
      }
      cm <- caret::confusionMatrix(pred_cls, y)
      metrics[[m]] <- data.frame(
        modelo = m,
        Accuracy = cm$overall["Accuracy"],
        Kappa    = cm$overall["Kappa"],
        ROC_AUC  = auc,
        Sens     = cm$byClass["Sensitivity"],
        Spec     = cm$byClass["Specificity"]
      )

      # ROC plot (si aplica)
      if (!is.na(auc) && !is.null(pred_prob[[pos]])) {
        roc_df <- data.frame(
          tpr = roc$sensitivities,
          fpr = 1 - roc$specificities
        )
        gg <- ggplot2::ggplot(roc_df, ggplot2::aes(fpr, tpr)) +
          ggplot2::geom_line() +
          ggplot2::geom_abline(linetype = 2) +
          ggplot2::labs(title = paste0("ROC ", m, " (AUC=", round(auc,3),")"),
                        x = "1 - Especificidad", y = "Sensibilidad")
        pth <- file.path(out_dir, paste0("roc_", m, ".png"))
        sp_save(gg, pth)
        plots[[paste0("roc_", m)]] <- pth
      }
    }

    # importancia de variables (cuando disponible)
    suppressWarnings({
      vi <- try(caret::varImp(fit, scale = TRUE), silent = TRUE)
      if (!inherits(vi, "try-error")) {
        dfvi <- as.data.frame(vi$importance)
        dfvi$feature <- rownames(dfvi)
        rownames(dfvi) <- NULL
        dfvi <- dfvi[order(-dfvi[[1]]), , drop=FALSE]
        top <- head(dfvi, 20)
        gg <- ggplot2::ggplot(top, ggplot2::aes(x = stats::reorder(feature, !!as.name(names(top)[1])),
                                                y = !!as.name(names(top)[1]))) +
          ggplot2::geom_col() +
          ggplot2::coord_flip() +
          ggplot2::labs(title = paste0("Importancia de variables (", m, ")"),
                        x = NULL, y = "Importancia (esc.)")
        pth <- file.path(out_dir, paste0("vip_", m, ".png"))
        sp_save(gg, pth)
        plots[[paste0("vip_", m)]] <- pth
      }
    })
  }

  list(
    fits = fits,
    metrics = do.call(rbind, metrics),
    plots = plots
  )
}


#' Ajuste de modelo de regresión logística multinomial
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función ajusta un modelo de regresión logística multinomial utilizando la función `multinom`
#' del paquete `nnet`. Divide los datos en entrenamiento y prueba de forma estratificada y calcula
#' métricas de evaluación incluyendo F1, precisión, recall, balanced accuracy, AUC-ROC y AUC-PR.
#' @param datos Data frame con los datos.
#' @param predictoras Vector con los nombres de las variables predictoras.
#' @param clase Nombre de la variable de clase (factor).
#' @param prop_test Proporción de datos para prueba (por defecto 0.3).
#'
#' @importFrom caret createDataPartition
#' @importFrom nnet multinom
#' @importFrom caret confusionMatrix
#' @return Una lista con:
#' \item{modelo}{Modelo ajustado con multinom.}
#' \item{predicciones}{Vector de predicciones del modelo.}
#' \item{probabilidades}{Probabilidades estimadas por clase.}
#' \item{matriz_confusion}{Matriz de confusión.}
#' \item{macro_f1}{F1 macro promedio.}
#' \item{micro_f1}{F1 micro.}
#' \item{balanced_accuracy}{Precisión balanceada promedio.}
#' \item{roc_obj}{Lista de curvas ROC por clase.}
#' \item{pr_obj}{Lista de curvas PR por clase.}
#' \item{tabla_metricas}{Data frame con métricas por clase.}
#' @examples
#' \dontrun{
#'   ajustar_regresion_multinomial(acdfx, c("ira", "miedo", "alegría"), "categoria")
#' }
#' @family utilidades
#' @export
ajustar_regresion_multinomial <- function(datos, predictoras, clase, prop_test = 0.3) {
  # Filtrar clases con menos de 2 observaciones
  tabla_clases <- table(datos[[clase]])
  datos <- datos[datos[[clase]] %in% names(tabla_clases[tabla_clases > 1]), ]
  datos[[clase]] <- droplevels(factor(datos[[clase]]))

  # Validar columnas
  if (!all(c(predictoras, clase) %in% colnames(datos))) {
    stop("Las columnas especificadas no existen en el data frame.")
  }

  # División estratificada
  set.seed(123)
  idx <- createDataPartition(datos[[clase]], p = 1 - prop_test, list = FALSE)
  datos_entrenamiento <- datos[idx, ]
  datos_prueba <- datos[-idx, ]

  # Fórmula y modelo
  formula <- as.formula(paste(clase, "~", paste(predictoras, collapse = "+")))
  modelo <- multinom(formula, data = datos_entrenamiento)

  # Predicción
  predicciones <- predict(modelo, newdata = datos_prueba, type = "class")
  probabilidades <- predict(modelo, newdata = datos_prueba, type = "probs")
  clases <- levels(datos[[clase]])

  # Matriz de confusión general
  matriz_confusion <- confusionMatrix(predicciones, datos_prueba[[clase]])

  # Inicializar métricas
  f1_scores <- precision_scores <- recall_scores <- balanced_scores <- auc_roc_scores <- auc_pr_scores <- numeric(length(clases))
  rocs <- prs <- list()

  for (i in seq_along(clases)) {
    cl <- clases[i]
    y_true_bin <- ifelse(datos_prueba[[clase]] == cl, 1, 0)
    y_pred_bin <- ifelse(predicciones == cl, 1, 0)

    # Calcular métricas
    precision_scores[i] <- tryCatch(Precision(y_pred_bin, y_true_bin), error = function(e) NA)
    recall_scores[i]    <- tryCatch(Recall(y_pred_bin, y_true_bin), error = function(e) NA)
    f1_scores[i]        <- tryCatch(F1_Score(y_pred_bin, y_true_bin), error = function(e) NA)
    balanced_scores[i]  <- tryCatch(matriz_confusion$byClass[i, "Balanced Accuracy"], error = function(e) NA)

    if (!is.null(dim(probabilidades))) {
      rocs[[cl]] <- tryCatch(roc(y_true_bin, probabilidades[, cl]), error = function(e) NULL)
      auc_roc_scores[i] <- tryCatch(if (!is.null(rocs[[cl]])) auc(rocs[[cl]]) else NA, error = function(e) NA)

      prs[[cl]] <- tryCatch(pr.curve(scores.class0 = probabilidades[, cl], weights.class0 = y_true_bin, curve = TRUE), error = function(e) NULL)
      auc_pr_scores[i] <- tryCatch(if (!is.null(prs[[cl]])) prs[[cl]]$auc.integral else NA, error = function(e) NA)
    }
  }

  # Macro y micro F1
  macro_f1 <- mean(f1_scores, na.rm = TRUE)
  micro_f1 <- sum(predicciones == datos_prueba[[clase]]) / length(predicciones)
  balanced_acc_prom <- mean(balanced_scores, na.rm = TRUE)

  tabla_metricas <- data.frame(
    Clase = clases,
    Precision = round(precision_scores, 3),
    Recall = round(recall_scores, 3),
    F1_Score = round(f1_scores, 3),
    Balanced_Accuracy = round(balanced_scores, 3),
    AUC_ROC = round(auc_roc_scores, 3),
    AUC_PR = round(auc_pr_scores, 3)
  )

  return(list(
    modelo = modelo,
    predicciones = predicciones,
    probabilidades = probabilidades,
    matriz_confusion = matriz_confusion,
    macro_f1 = macro_f1,
    micro_f1 = micro_f1,
    balanced_accuracy = balanced_acc_prom,
    roc_obj = rocs,
    pr_obj = prs,
    tabla_metricas = tabla_metricas
  ))
}

#' Ajuste de modelos basados en árboles para clasificación de sentimiento
#'
#' Ajusta un modelo de árbol de decisión (\code{rpart}) o un bosque aleatorio
#' (\code{randomForest}) para clasificar una variable objetivo a partir de un
#' conjunto de predictores a nivel token. Divide los datos en \code{train/test}
#' según \code{prop_test}, entrena el modelo y calcula métricas por clase (F1,
#' Precision, Recall, Balanced Accuracy) y áreas bajo curva ROC y PR cuando es posible.
#'
#' @details
#' Argumento \code{metodo} admite \code{"arbol"} (vía \code{rpart}) o \code{"rf"}
#' (vía \code{randomForest}). Para \code{"arbol"} se grafica el árbol con
#' \code{rpart.plot}. Las probabilidades de clase se usan para estimar curvas
#' ROC/PR por clase cuando el método lo provee.
#'
#' Paquetes requeridos en tiempo de ejecución según el método y métricas:
#' \code{rpart}, \code{rpart.plot}, \code{randomForest}, \code{caret},
#' \code{MLmetrics}, \code{pROC}, \code{PRROC}.
#'
#' @param datos \code{data.frame}. Datos a nivel token con las columnas de
#'   \code{predictoras} y la columna objetivo \code{clase}.
#' @param predictoras \code{character}. Vector con los nombres de las columnas
#'   predictoras a utilizar.
#' @param clase \code{character}. Nombre de la columna objetivo (se forzará a \code{factor}).
#' @param prop_test \code{numeric} en \code{(0,1)}. Proporción destinada a \code{TEST}
#'   (el resto se usa para \code{TRAIN}). Por defecto \code{0.3}.
#' @param metodo \code{character}. Uno de \code{"arbol"} (usa \code{rpart}) o
#'   \code{"rf"} (usa \code{randomForest}). Por defecto \code{"arbol"}.
#'
#' @return Una \code{list} con los elementos:
#'   \code{modelo} (objeto del modelo entrenado),
#'   \code{predicciones} (factor en \code{TEST}),
#'   \code{probabilidades} (matriz o \code{NULL}, según método),
#'   \code{matriz_confusion} (objeto \code{caret::confusionMatrix}),
#'   \code{macro_f1}, \code{micro_f1}, \code{balanced_accuracy} (numéricos),
#'   \code{roc_obj}, \code{pr_obj} (listas por clase; pueden contener \code{NULL}),
#'   \code{tabla_metricas} (data frame por clase con métricas y AUCs).
#'
#' @examples
#' \dontrun{
#' # Suponga que 'datos' contiene columnas de entrada y una columna objetivo 'Nivel_bin'
#' set.seed(123)
#' fit_res <- ajustar_modelo_arboles(
#'   datos       = datos,
#'   predictoras = c("anger","fear","joy","sadness","Nivel_syuzhet","Nivel_bing"),
#'   clase       = "Nivel_bin",
#'   prop_test   = 0.3,
#'   metodo      = "arbol"   # o "rf"
#' )
#' fit_res$tabla_metricas
#' }
#'
#' @seealso \code{rpart}, \code{rpart.plot}, \code{randomForest},
#'   \code{caret::createDataPartition}, \code{caret::confusionMatrix},
#'   \code{pROC::roc}, \code{pROC::auc}, \code{PRROC::pr.curve}
#'
#' @importFrom stats as.formula predict
#' @importFrom caret createDataPartition confusionMatrix
#' @export


ajustar_modelo_arboles <- function(datos, predictoras, clase, prop_test = 0.3, metodo = "arbol") {
  if (!all(c(predictoras, clase) %in% colnames(datos))) {
    stop("Las columnas especificadas no existen en el data frame.")
  }

  datos[[clase]] <- as.factor(datos[[clase]])

  set.seed(123)
  idx_train <- createDataPartition(datos[[clase]], p = 1 - prop_test, list = FALSE)
  datos_train <- datos[idx_train, ]
  datos_test <- datos[-idx_train, ]

  datos_train[[clase]] <- droplevels(datos_train[[clase]])
  datos_test[[clase]] <- factor(datos_test[[clase]], levels = levels(datos_train[[clase]]))

  formula_modelo <- as.formula(paste(clase, "~", paste(predictoras, collapse = "+")))
  print(formula_modelo)

  if (metodo == "arbol") {
    modelo <- rpart(formula_modelo, data = datos_train, method = "class")
    rpart.plot(modelo, type = 3, extra = 101, under = TRUE, fallen.leaves = TRUE, cex = 0.6)
    pred_clase <- predict(modelo, newdata = datos_test, type = "class")
    pred_prob <- predict(modelo, newdata = datos_test, type = "prob")
  } else if (metodo == "rf") {
    modelo <- randomForest(formula_modelo, data = datos_train, importance = TRUE)
    pred_clase <- predict(modelo, newdata = datos_test, type = "response")
    pred_prob <- predict(modelo, newdata = datos_test, type = "prob")
  } else {
    stop("Método no válido. Usa 'arbol' o 'rf'.")
  }

  conf_matrix <- confusionMatrix(pred_clase, datos_test[[clase]])

  clases <- levels(datos_train[[clase]])
  f1_scores <- numeric(length(clases))
  precision_scores <- numeric(length(clases))
  recall_scores <- numeric(length(clases))
  balanced_scores <- numeric(length(clases))
  auc_roc_scores <- numeric(length(clases))
  auc_pr_scores <- numeric(length(clases))

  rocs <- list()
  prs <- list()

  for (i in seq_along(clases)) {
    cl <- clases[i]
    y_true_bin <- as.numeric(datos_test[[clase]] == cl)

    f1_scores[i] <- MLmetrics::F1_Score(y_pred = pred_clase, y_true = datos_test[[clase]], positive = cl)
    precision_scores[i] <- MLmetrics::Precision(y_pred = pred_clase, y_true = datos_test[[clase]], positive = cl)
    recall_scores[i] <- MLmetrics::Recall(y_pred = pred_clase, y_true = datos_test[[clase]], positive = cl)
    balanced_scores[i] <- conf_matrix$byClass[i, "Balanced Accuracy"]

    if (!is.null(dim(pred_prob))) {
      rocs[[cl]] <- tryCatch(
        roc(y_true_bin, pred_prob[, cl]),
        error = function(e) NULL
      )
      auc_roc_scores[i] <- tryCatch(
        if (!is.null(rocs[[cl]])) as.numeric(auc(rocs[[cl]])) else NA,
        error = function(e) NA
      )
      prs[[cl]] <- tryCatch(
        pr.curve(scores.class0 = pred_prob[, cl], weights.class0 = y_true_bin, curve = TRUE),
        error = function(e) NULL
      )
      auc_pr_scores[i] <- tryCatch(
        if (!is.null(prs[[cl]])) prs[[cl]]$auc.integral else NA,
        error = function(e) NA
      )
    }
  }

  macro_f1 <- mean(f1_scores, na.rm = TRUE)
  micro_f1 <- sum(pred_clase == datos_test[[clase]]) / length(pred_clase)
  balanced_acc_prom <- mean(balanced_scores, na.rm = TRUE)

  tabla_metricas <- data.frame(
    Clase = clases,
    Precision = round(precision_scores, 3),
    Recall = round(recall_scores, 3),
    F1_Score = round(f1_scores, 3),
    Balanced_Accuracy = round(balanced_scores, 3),
    AUC_ROC = round(auc_roc_scores, 3),
    AUC_PR = round(auc_pr_scores, 3)
  )

  return(list(
    modelo = modelo,
    predicciones = pred_clase,
    probabilidades = pred_prob,
    matriz_confusion = conf_matrix,
    macro_f1 = macro_f1,
    micro_f1 = micro_f1,
    balanced_accuracy = balanced_acc_prom,
    roc_obj = rocs,
    pr_obj = prs,
    tabla_metricas = tabla_metricas
  ))
}

#' Ajustar una red neuronal densa unificada para clasificación multiclase
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función ajusta una red neuronal simple usando `keras` para un problema de clasificación multiclase.
#' Calcula métricas por clase, curvas ROC y PR, y devuelve un resumen del modelo.
#'
#' @param datos Data frame con las variables predictoras y la variable de clase.
#' @param predictoras Vector de nombres de columnas a usar como variables predictoras.
#' @param clase Nombre de la variable de clase (factor).
#' @param prop_test Proporción de los datos que se usarán como conjunto de prueba. Default es 0.3.
#' @param epochs Número de épocas para entrenar el modelo. Default es 50.
#'
#' @return Una lista con:
#' \describe{
#'   \item{modelo}{Objeto del modelo keras entrenado.}
#'   \item{historial}{Historial del entrenamiento del modelo.}
#'   \item{predicciones}{Clases predichas en el conjunto de prueba.}
#'   \item{matriz_confusion}{Matriz de confusión del modelo.}
#'   \item{macro_f1}{F1 macro promedio.}
#'   \item{micro_f1}{F1 micro (exactitud general).}
#'   \item{balanced_accuracy}{Precisión balanceada promedio.}
#'   \item{roc_obj}{Lista de objetos ROC por clase.}
#'   \item{pr_obj}{Lista de objetos PR por clase.}
#'   \item{tabla_metricas}{Data frame con métricas por clase.}
#' }
#'
#' @examples
#' \dontrun{
#' resultado <- ajustar_red_nn_unificada(acdfx, vPred, "categoria", epochs = 30)
#' print(resultado$tabla_metricas)
#' }
#' @family utilidades
#' @export
ajustar_red_nn_unificada <- function(datos, predictoras, clase, prop_test = 0.3, epochs = 50) {
  if (!all(c(predictoras, clase) %in% colnames(datos))) {
    stop("Las columnas especificadas no existen en el data frame.")
  }

  datos[[clase]] <- as.factor(datos[[clase]])

  set.seed(123)
  idx_train <- createDataPartition(datos[[clase]], p = 1 - prop_test, list = FALSE)
  datos_train <- datos[idx_train, ]
  datos_test <- datos[-idx_train, ]

  datos_train[[clase]] <- droplevels(datos_train[[clase]])
  datos_test[[clase]] <- factor(datos_test[[clase]], levels = levels(datos_train[[clase]]))

  x_train <- as.matrix(datos_train[, predictoras])
  x_test <- as.matrix(datos_test[, predictoras])

  clase_levels <- levels(datos_train[[clase]])
  y_train_int <- as.integer(datos_train[[clase]]) - 1
  y_test_int <- as.integer(datos_test[[clase]]) - 1
  y_train_cat <- to_categorical(y_train_int)
  y_test_cat <- to_categorical(y_test_int)

  model <- keras_model_sequential() %>%
    layer_dense(units = 128, activation = 'relu', input_shape = ncol(x_train)) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = length(clase_levels), activation = 'softmax')

  model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = 'adam',
    metrics = c('accuracy')
  )

  history <- model %>% fit(
    x_train, y_train_cat,
    epochs = epochs,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 0
  )

  pred_probs <- model %>% predict(x_test)
  pred_int <- apply(pred_probs, 1, which.max) - 1
  pred_factor <- factor(clase_levels[pred_int + 1], levels = clase_levels)

  conf_matrix <- confusionMatrix(pred_factor, datos_test[[clase]])

  clases <- clase_levels
  f1_scores <- numeric(length(clases))
  precision_scores <- numeric(length(clases))
  recall_scores <- numeric(length(clases))
  balanced_scores <- numeric(length(clases))
  auc_roc_scores <- numeric(length(clases))
  auc_pr_scores <- numeric(length(clases))
  rocs <- list()
  prs <- list()

  for (i in seq_along(clases)) {
    cl <- clases[i]
    y_true_bin <- factor(ifelse(datos_test[[clase]] == cl, cl, paste0("no_", cl)))
    y_pred_bin <- factor(ifelse(pred_factor == cl, cl, paste0("no_", cl)))
    y_prob_cl <- pred_probs[, i]

    f1_scores[i] <- tryCatch(F1_Score(y_true = y_true_bin, y_pred = y_pred_bin, positive = cl), error = function(e) NA)
    precision_scores[i] <- tryCatch(Precision(y_true = y_true_bin, y_pred = y_pred_bin, positive = cl), error = function(e) NA)
    recall_scores[i] <- tryCatch(Recall(y_true = y_true_bin, y_pred = y_pred_bin, positive = cl), error = function(e) NA)
    balanced_scores[i] <- conf_matrix$byClass[i, "Balanced Accuracy"]

    rocs[[cl]] <- tryCatch(roc(response = y_true_bin, predictor = y_prob_cl), error = function(e) NULL)
    auc_roc_scores[i] <- tryCatch(if (!is.null(rocs[[cl]])) as.numeric(auc(rocs[[cl]])) else NA, error = function(e) NA)
    prs[[cl]] <- tryCatch(pr.curve(scores.class0 = y_prob_cl, weights.class0 = (y_true_bin == cl), curve = TRUE), error = function(e) NULL)
    auc_pr_scores[i] <- tryCatch(if (!is.null(prs[[cl]])) prs[[cl]]$auc.integral else NA, error = function(e) NA)
  }

  macro_f1 <- mean(f1_scores, na.rm = TRUE)
  micro_f1 <- mean(pred_factor == datos_test[[clase]])
  balanced_acc_prom <- mean(balanced_scores, na.rm = TRUE)

  tabla_metricas <- data.frame(
    Clase = clases,
    Precision = round(precision_scores, 3),
    Recall = round(recall_scores, 3),
    F1_Score = round(f1_scores, 3),
    Balanced_Accuracy = round(balanced_scores, 3),
    AUC_ROC = round(auc_roc_scores, 3),
    AUC_PR = round(auc_pr_scores, 3)
  )

  return(list(
    modelo = model,
    historial = history,
    predicciones = pred_factor,
    matriz_confusion = conf_matrix,
    macro_f1 = macro_f1,
    micro_f1 = micro_f1,
    balanced_accuracy = balanced_acc_prom,
    roc_obj = rocs,
    pr_obj = prs,
    tabla_metricas = tabla_metricas
  ))
}

#' Ajustar un modelo de regresión RNN con Keras
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función entrena una red neuronal recurrente (RNN) para realizar regresión sobre un conjunto de datos.
#' Utiliza `keras` para definir y entrenar el modelo, y retorna métricas comunes de regresión.
#' @import Metrics
#' @param df Data frame que contiene las variables predictoras y la variable objetivo.
#' @param columnas_predictoras Vector de nombres de columnas a usar como variables de entrada.
#' @param columna_clase Nombre de la variable objetivo (de salida).
#' @param prop_test Proporción de datos reservados para validación (por defecto 0.3).
#' @param epochs Número de épocas de entrenamiento (por defecto 50).
#'
#' @return Una lista con los siguientes elementos:
#' \describe{
#'   \item{modelo}{El modelo keras entrenado.}
#'   \item{historial}{Historial del entrenamiento keras.}
#'   \item{mae}{Error absoluto medio.}
#'   \item{rmse}{Raíz del error cuadrático medio.}
#'   \item{r2}{Coeficiente de determinación R².}
#'   \item{predicciones}{Predicciones realizadas sobre el conjunto de prueba.}
#'   \item{comparacion}{Data frame con valores reales y predichos.}
#' }
#'
#' @examples
#' \dontrun{
#' resultado <- ajustar_rnn_regresion(acdfx, vPred, "ValorS", epochs = 30)
#' resultado$rmse
#' }
#' @family utilidades
#' @export
ajustar_rnn_regresion <- function(df, columnas_predictoras, columna_clase, prop_test = 0.3, epochs = 50) {
  # Validación inicial
  if (!all(columnas_predictoras %in% colnames(df))) {
    stop("Una o más columnas predictoras no existen en el data frame.")
  }
  if (!columna_clase %in% colnames(df)) {
    stop("La columna de salida no existe en el data frame.")
  }
  if (any(is.na(df[, c(columnas_predictoras, columna_clase)]))) {
    stop("El data frame contiene valores NA en columnas predictoras o en la salida.")
  }

  # Partición aleatoria
  set.seed(123)
  n <- nrow(df)
  idx_train <- sample(seq_len(n), size = round((1 - prop_test) * n))
  df_train <- df[idx_train, ]
  df_test  <- df[-idx_train, ]

  # Preparar datos en formato array 3D para RNN
  x_train <- array(as.matrix(df_train[, columnas_predictoras]),
                   dim = c(nrow(df_train), length(columnas_predictoras), 1))
  x_test  <- array(as.matrix(df_test[, columnas_predictoras]),
                   dim = c(nrow(df_test), length(columnas_predictoras), 1))

  y_train <- as.numeric(df_train[[columna_clase]])
  y_test  <- as.numeric(df_test[[columna_clase]])

  # Validaciones finales
  if (length(unique(y_train)) < 2) stop("La variable objetivo no tiene suficiente variabilidad.")

  # Definición del modelo RNN
  model <- keras_model_sequential() %>%
    layer_simple_rnn(units = 50, activation = 'tanh', input_shape = c(length(columnas_predictoras), 1)) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 1, activation = 'linear')

  model %>% compile(
    loss = 'mse',
    optimizer = 'adam',
    metrics = c('mae')
  )

  # Entrenamiento
  history <- model %>% fit(
    x = x_train,
    y = y_train,
    epochs = epochs,
    batch_size = 32,
    validation_split = 0.2,
    verbose = 0
  )

  # Predicción
  y_pred <- model %>% predict(x_test)

  # Evaluación
  mae_val <- mae(y_test, y_pred)
  rmse_val <- rmse(y_test, y_pred)
  r2_val <- 1 - sum((y_test - y_pred)^2) / sum((y_test - mean(y_test))^2)

  # Visualización
  df_pred <- data.frame(Real = y_test, Predicho = y_pred)
  p <- ggplot(df_pred, aes(x = Real, y = Predicho)) +
    geom_point(color = "steelblue") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(title = "Predicción vs Real", x = "Valor Real", y = "Predicción")
  print(p)

  return(list(
    modelo = model,
    historial = history,
    mae = mae_val,
    rmse = rmse_val,
    r2 = r2_val,
    predicciones = y_pred,
    comparacion = df_pred
  ))
}

#' Ajustar un modelo de regresión RNN usando Torch
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función ajusta una red neuronal recurrente (RNN) para un problema de regresión utilizando la librería `torch`.
#' @import torch
#' @param df Data frame con las variables predictoras y de salida.
#' @param columnas_predictoras Vector con los nombres de columnas a usar como variables predictoras.
#' @param columna_clase Nombre de la variable objetivo (numérica).
#' @param prop_test Proporción de datos reservados para validación (por defecto 0.3).
#' @param epochs Número de épocas de entrenamiento (por defecto 50).
#'
#' @return Una lista con:
#' \describe{
#'   \item{modelo}{Modelo torch entrenado.}
#'   \item{mae}{Error absoluto medio.}
#'   \item{rmse}{Raíz del error cuadrático medio.}
#'   \item{r2}{Coeficiente de determinación R².}
#'   \item{predicciones}{Vector de predicciones.}
#'   \item{comparacion}{Data frame con valores reales y predichos.}
#' }
#'
#' @examples
#' \dontrun{
#' resultado <- ajustar_rnn_regresion_torch(acdfx, vPred, "ValorS", epochs = 30)
#' resultado$r2
#' }
#' @family utilidades
#' @export
ajustar_rnn_regresion_torch <- function(df, columnas_predictoras, columna_clase, prop_test = 0.3, epochs = 50) {

  if (!all(columnas_predictoras %in% colnames(df))) {
    stop("Una o más columnas predictoras no existen.")
  }
  if (!columna_clase %in% colnames(df)) {
    stop("La columna de salida no existe.")
  }
  if (any(is.na(df[, c(columnas_predictoras, columna_clase)]))) {
    stop("El dataset contiene NA.")
  }

  # Partición de datos
  set.seed(123)
  idx_train <- sample(seq_len(nrow(df)), size = round((1 - prop_test) * nrow(df)))
  df_train <- df[idx_train, ]
  df_test  <- df[-idx_train, ]

  # Tensores
  x_train <- torch_tensor(as.matrix(df_train[, columnas_predictoras]), dtype = torch_float())
  x_test  <- torch_tensor(as.matrix(df_test[, columnas_predictoras]), dtype = torch_float())
  y_train <- torch_tensor(as.numeric(df_train[[columna_clase]]), dtype = torch_float())
  y_test  <- torch_tensor(as.numeric(df_test[[columna_clase]]), dtype = torch_float())

  if (length(unique(as.numeric(y_train))) < 2) stop("La variable de salida tiene un solo valor único.")

  # Ajustar dimensiones para RNN (batch, seq_len, input_size)
  x_train <- x_train$unsqueeze(2)
  x_test  <- x_test$unsqueeze(2)

  # Definición del modelo
  model <- nn_module(
    initialize = function(input_size, hidden_size, output_size) {
      self$rnn <- nn_rnn(input_size = input_size, hidden_size = hidden_size, batch_first = TRUE)
      self$fc <- nn_linear(hidden_size, output_size)
    },
    forward = function(x) {
      out <- self$rnn(x)[[1]]
      self$fc(out[ , dim(out)[2], ])
    }
  )

  net <- model(length(columnas_predictoras), 50, 1)
  optimizer <- optim_adam(net$parameters, lr = 0.001)
  loss_fn <- nn_mse_loss()

  # Entrenamiento
  for (epoch in 1:epochs) {
    net$train()
    optimizer$zero_grad()
    y_pred <- net(x_train)
    loss <- loss_fn(y_pred, y_train$unsqueeze(2))
    loss$backward()
    optimizer$step()

    if (epoch %% 10 == 0) {
      cat("Epoch", epoch, "- Loss:", round(loss$item(), 5), "\n")
    }
  }

  # Evaluación
  net$eval()
  y_pred_tensor <- net(x_test)$squeeze()$cpu()
  y_pred <- as.numeric(y_pred_tensor)
  y_real <- as.numeric(y_test$cpu())

  # Métricas
  rmse <- sqrt(mean((y_real - y_pred)^2))
  mae <- mean(abs(y_real - y_pred))
  r2 <- 1 - sum((y_real - y_pred)^2) / sum((y_real - mean(y_real))^2)

  # Visualización
  df_pred <- data.frame(Real = y_real, Predicho = y_pred)
  p <- ggplot(df_pred, aes(x = Real, y = Predicho)) +
    geom_point(color = "darkgreen") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    theme_minimal() +
    labs(title = "Predicción vs Real (Torch)", x = "Valor Real", y = "Predicción")
  print(p)

  return(list(
    modelo = net,
    mae = mae,
    rmse = rmse,
    r2 = r2,
    predicciones = y_pred,
    comparacion = df_pred
  ))
}

#' Ajustar modelo SVM multiclase y calcular métricas de evaluación
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función entrena un modelo SVM para clasificación multiclase utilizando un kernel especificado,
#' y devuelve métricas detalladas por clase, incluyendo AUC-ROC y AUC-PR.
#'
#' @param df Data frame con las variables predictoras y la variable de clase.
#' @param columnas_predictoras Vector de nombres de columnas predictoras.
#' @param columna_clase Nombre de la variable de salida (factor multiclase).
#' @param prop_test Proporción de los datos reservada para el conjunto de prueba (por defecto 0.3).
#' @param kernel Tipo de kernel a utilizar en el modelo SVM (`"radial"` por defecto).
#'
#' @return Una lista con:
#' \describe{
#'   \item{modelo}{Modelo SVM entrenado.}
#'   \item{predicciones}{Predicciones del conjunto de prueba.}
#'   \item{reales}{Clases reales del conjunto de prueba.}
#'   \item{confusion_matrix}{Matriz de confusión.}
#'   \item{tabla_metricas}{Tabla con métricas por clase.}
#'   \item{macro_f1}{F1 Score macro promedio.}
#'   \item{micro_f1}{F1 Score micro global.}
#' }
#'
#' @examples
#' \dontrun{
#' resultado <- ajustar_svm_multiclase(acdfx, vPred, "categoria")
#' print(resultado$tabla_metricas)
#' }
#' @family modelado_supervisado
#' @export
ajustar_svm_multiclase <- function(df, columnas_predictoras, columna_clase, prop_test = 0.3, kernel = "radial") {
  if (!all(c(columnas_predictoras, columna_clase) %in% colnames(df))) {
    stop("Las columnas especificadas no existen.")
  }

  if (!is.factor(df[[columna_clase]])) {
    df[[columna_clase]] <- as.factor(df[[columna_clase]])
  }

  set.seed(123)
  idx <- createDataPartition(df[[columna_clase]], p = 1 - prop_test, list = FALSE)
  df_train <- df[idx, ]
  df_test  <- df[-idx, ]

  formula_modelo <- as.formula(paste(columna_clase, "~", paste(columnas_predictoras, collapse = " + ")))

  modelo <- svm(formula_modelo, data = df_train, kernel = kernel, probability = TRUE)

  pred_clase <- predict(modelo, df_test[, columnas_predictoras])
  pred_prob  <- attr(predict(modelo, df_test[, columnas_predictoras], probability = TRUE), "probabilities")

  pred_clase <- factor(pred_clase, levels = levels(df[[columna_clase]]))
  real <- factor(df_test[[columna_clase]], levels = levels(df[[columna_clase]]))

  cm <- confusionMatrix(pred_clase, real)

  clases <- levels(real)
  tabla_metricas <- data.frame(
    Clase = clases,
    Precision = NA,
    Recall = NA,
    F1_Score = NA,
    Balanced_Accuracy = NA,
    AUC_ROC = NA,
    AUC_PR = NA
  )

  for (i in seq_along(clases)) {
    cl <- clases[i]
    bin_real <- factor(ifelse(real == cl, cl, paste0("no_", cl)))
    bin_pred <- factor(ifelse(pred_clase == cl, cl, paste0("no_", cl)))

    if (length(unique(bin_real)) >= 2 && length(unique(bin_pred)) >= 2) {
      m <- confusionMatrix(bin_pred, bin_real, positive = cl)
      tabla_metricas$Precision[i] <- m$byClass["Precision"]
      tabla_metricas$Recall[i] <- m$byClass["Recall"]
      tabla_metricas$F1_Score[i] <- m$byClass["F1"]
      tabla_metricas$Balanced_Accuracy[i] <- m$byClass["Balanced Accuracy"]

      if (!is.null(pred_prob) && cl %in% colnames(pred_prob)) {
        probs <- pred_prob[, cl]
        try({
          roc_obj <- roc(response = real == cl, predictor = probs)
          tabla_metricas$AUC_ROC[i] <- as.numeric(auc(roc_obj))
          pr_obj <- pr.curve(scores.class0 = probs, weights.class0 = real == cl)
          tabla_metricas$AUC_PR[i] <- pr_obj$auc.integral
        }, silent = TRUE)
      }
    }
  }

  macro_f1 <- mean(tabla_metricas$F1_Score, na.rm = TRUE)

  cm_tab <- table(real, pred_clase)
  TP <- sum(diag(cm_tab))
  FP <- sum(colSums(cm_tab)) - TP
  FN <- sum(rowSums(cm_tab)) - TP

  precision_micro <- TP / (TP + FP)
  recall_micro <- TP / (TP + FN)

  micro_f1 <- if ((precision_micro + recall_micro) > 0) {
    2 * precision_micro * recall_micro / (precision_micro + recall_micro)
  } else {
    NA
  }

  return(list(
    modelo = modelo,
    predicciones = pred_clase,
    reales = real,
    confusion_matrix = cm,
    tabla_metricas = tabla_metricas,
    macro_f1 = macro_f1,
    micro_f1 = micro_f1
  ))
}

#' Ajustar un modelo PCA + SVM con evaluación multiclase
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función entrena un modelo SVM sobre componentes principales extraídos por PCA
#' y devuelve métricas detalladas por clase, incluyendo curvas ROC y PR, además de visualización
#' si hay al menos dos componentes.
#'
#' @param df Data frame con las columnas predictoras y la clase.
#' @param columnas_predictoras Vector de nombres de columnas numéricas predictoras.
#' @param columna_clase Nombre de la variable categórica objetivo.
#' @param prop_test Proporción de datos reservados para prueba (por defecto 0.3).
#' @param n_comp Número de componentes principales a usar. Si es NULL, se seleccionan los necesarios para explicar al menos 95% de la varianza.
#'
#' @return Una lista con:
#' \describe{
#'   \item{modelo}{Modelo SVM entrenado.}
#'   \item{predicciones}{Clases predichas sobre test.}
#'   \item{reales}{Clases reales sobre test.}
#'   \item{tabla_metricas}{Métricas por clase.}
#'   \item{macro_f1}{Promedio macro del F1.}
#'   \item{micro_f1}{Promedio micro del F1.}
#'   \item{confusion_matrix}{Matriz de confusión.}
#'   \item{pca}{Modelo PCA entrenado.}
#'   \item{n_comp}{Número de componentes utilizados.}
#'   \item{escalado}{Lista con parámetros de escalado usados.}
#'   \item{plot}{Gráfico de dispersión PCA si n_comp >= 2.}
#' }
#'
#' @examples
#' \dontrun{
#' resultado <- ajustar_pca_svm(acdfx, vPred, "categoria")
#' resultado$plot
#' print(resultado$tabla_metricas)
#' }
#' @family reduccion_dimensional
#' @export
ajustar_pca_svm <- function(df, columnas_predictoras, columna_clase, prop_test = 0.3, n_comp = NULL) {

  if (!is.factor(df[[columna_clase]])) {
    df[[columna_clase]] <- as.factor(df[[columna_clase]])
  }

  set.seed(123)
  idx <- createDataPartition(df[[columna_clase]], p = 1 - prop_test, list = FALSE)
  df_train <- df[idx, ]
  df_test  <- df[-idx, ]

  train_scaled <- scale(df_train[, columnas_predictoras])
  test_scaled  <- scale(df_test[, columnas_predictoras],
                        center = attr(train_scaled, "scaled:center"),
                        scale  = attr(train_scaled, "scaled:scale"))

  pca <- prcomp(train_scaled, center = FALSE, scale. = FALSE)

  if (is.null(n_comp)) {
    var_exp <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
    n_comp <- which(var_exp >= 0.95)[1]
  }

  x_train_pca <- as.data.frame(pca$x[, 1:n_comp])
  x_test_pca  <- as.data.frame(predict(pca, test_scaled)[, 1:n_comp])

  x_train_pca[[columna_clase]] <- df_train[[columna_clase]]
  x_test_pca[[columna_clase]]  <- df_test[[columna_clase]]

  formula <- as.formula(paste(columna_clase, "~ ."))
  modelo <- e1071::svm(formula, data = x_train_pca, probability = TRUE)

  pred_clase <- predict(modelo, x_test_pca[, -ncol(x_test_pca)])
  probas <- attr(predict(modelo, x_test_pca[, -ncol(x_test_pca)], probability = TRUE), "probabilities")
  real <- factor(x_test_pca[[columna_clase]], levels = levels(df[[columna_clase]]))

  clases <- levels(real)
  tabla_metricas <- data.frame(Clase = clases, Precision = NA, Recall = NA, F1_Score = NA,
                               Balanced_Accuracy = NA, AUC_ROC = NA, AUC_PR = NA)
  for (i in seq_along(clases)) {
    cl <- clases[i]
    bin_real <- factor(ifelse(real == cl, cl, paste0("no_", cl)))
    bin_pred <- factor(ifelse(pred_clase == cl, cl, paste0("no_", cl)))

    if (length(unique(bin_real)) >= 2 && length(unique(bin_pred)) >= 2) {
      cm <- confusionMatrix(bin_pred, bin_real, positive = cl)
      tabla_metricas[i, 2:5] <- cm$byClass[c("Precision", "Recall", "F1", "Balanced Accuracy")]

      if (!is.null(probas) && cl %in% colnames(probas)) {
        prob_cl <- probas[, cl]
        bin_cl <- as.factor(real == cl)

        if (length(unique(bin_cl)) == 2) {
          try({
            roc_obj <- roc(response = bin_cl, predictor = prob_cl, quiet = TRUE)
            if (inherits(roc_obj, "roc")) {
              tabla_metricas$AUC_ROC[i] <- as.numeric(auc(roc_obj))
              pr_obj <- pr.curve(scores.class0 = prob_cl, weights.class0 = bin_cl == TRUE)
              tabla_metricas$AUC_PR[i] <- pr_obj$auc.integral
            }
          }, silent = TRUE)
        }
      }
    }
  }

  cm_full <- table(real, pred_clase)
  TP <- sum(diag(cm_full))
  FP <- sum(colSums(cm_full)) - TP
  FN <- sum(rowSums(cm_full)) - TP
  prec_micro <- TP / (TP + FP)
  rec_micro  <- TP / (TP + FN)
  micro_f1 <- if ((prec_micro + rec_micro) > 0) {
    2 * prec_micro * rec_micro / (prec_micro + rec_micro)
  } else { NA }
  macro_f1 <- mean(tabla_metricas$F1_Score, na.rm = TRUE)

  plot <- NULL
  if (n_comp >= 2) {
    plot <- ggplot(x_test_pca, aes(x = PC1, y = PC2, color = !!as.name(columna_clase))) +
      geom_point(alpha = 0.7) +
      labs(title = "PCA + SVM: espacio reducido") +
      theme_minimal()
  }

  return(list(
    modelo = modelo,
    predicciones = pred_clase,
    reales = real,
    tabla_metricas = tabla_metricas,
    macro_f1 = macro_f1,
    micro_f1 = micro_f1,
    confusion_matrix = confusionMatrix(pred_clase, real),
    pca = pca,
    n_comp = n_comp,
    escalado = list(center = attr(train_scaled, "scaled:center"),
                    scale  = attr(train_scaled, "scaled:scale")),
    plot = plot
  ))
}

#' Ajustar modelo de Gradient Boosting con opción de reducción PCA
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función permite entrenar un modelo de Gradient Boosting para regresión o clasificación,
#' con opción de aplicar reducción de dimensionalidad mediante Análisis de Componentes Principales (PCA).
#'
#' @param df Data frame que contiene los datos de entrada.
#' @param variables_independientes Vector de nombres de columnas predictoras.
#' @param variable_dependiente Nombre de la columna objetivo.
#' @param n_trees Número de árboles (por defecto 100).
#' @param learning_rate Tasa de aprendizaje (por defecto 0.1).
#' @param interaction_depth Profundidad de interacción (por defecto 3).
#' @param prop_test Proporción de datos para prueba (por defecto 0.3).
#' @param bag_fraction Fracción de datos para muestreo (por defecto 0.5).
#' @param n_minobsinnode Número mínimo de observaciones por nodo (por defecto 10).
#' @param usar_pca Booleano que indica si se debe aplicar PCA antes del entrenamiento.
#' @param varianza_explicada Porcentaje de varianza explicada para seleccionar componentes en PCA (por defecto 0.95).
#'
#' @return Una lista con los siguientes elementos si es regresión:
#' \item{modelo}{Modelo ajustado con `gbm`.}
#' \item{predicciones}{Predicciones sobre el conjunto de prueba.}
#' \item{tabla_metricas}{Data frame con métricas de evaluación (RMSE, MAE, R2).}
#' \item{importancia}{Importancia de variables según el modelo.}
#' \item{grafico}{Gráfico ggplot2 con comparación Real vs Predicho.}
#' \item{pca_modelo}{Objeto PCA si se usó, en caso contrario NULL.}
#' \item{pca_center}{Vector de centrado si se usó PCA.}
#' \item{pca_scale}{Vector de escalado si se usó PCA.}
#' \item{n_comp}{Número de componentes seleccionados por PCA.}
#'
#' Para problemas de clasificación, se retorna el modelo y un mensaje de advertencia.
#'
#' @family utilidades
#' @export
#'
#' @examples
#' # Usar sin PCA
#' res_gbm <- ajustar_gradient_boosting(acdfx, vPred_expandido, "diferencia")
#' print(res_gbm$tabla_metricas)
#' print(res_gbm$grafico)
#' head(res_gbm$importancia)
#'
#' # Usar con PCA
#' res_gbm_pca <- ajustar_gradient_boosting(acdfx, vPred_expandido, "diferencia", usar_pca = TRUE)
#' print(res_gbm_pca$tabla_metricas)
#' print(res_gbm_pca$grafico)
#' head(res_gbm_pca$importancia)
ajustar_gradient_boosting <- function(df, variables_independientes, variable_dependiente,
                                      n_trees = 100, learning_rate = 0.1, interaction_depth = 3,
                                      prop_test = 0.3, bag_fraction = 0.5, n_minobsinnode = 10,
                                      usar_pca = FALSE, varianza_explicada = 0.95) {
  library(gbm)
  library(caret)
  library(ggplot2)

  set.seed(123)
  y_var <- df[[variable_dependiente]]
  indices_entrenamiento <- if (is.factor(y_var)) {
    createDataPartition(y_var, p = 1 - prop_test, list = FALSE)
  } else {
    sample(seq_len(nrow(df)), size = round((1 - prop_test) * nrow(df)))
  }

  df_entrenamiento <- df[indices_entrenamiento, ]
  df_prueba <- df[-indices_entrenamiento, ]

  x_train <- df_entrenamiento[, variables_independientes, drop = FALSE]
  x_test  <- df_prueba[, variables_independientes, drop = FALSE]
  y_train <- df_entrenamiento[[variable_dependiente]]
  y_test  <- df_prueba[[variable_dependiente]]

  if (usar_pca) {
    escala_train <- scale(x_train)
    pca <- prcomp(escala_train, center = FALSE, scale. = FALSE)
    var_exp <- cumsum(pca$sdev^2 / sum(pca$sdev^2))
    n_comp <- which(var_exp >= varianza_explicada)[1]
    x_train <- as.data.frame(pca$x[, 1:n_comp])
    escala_test <- scale(x_test, center = attr(escala_train, "scaled:center"), scale = attr(escala_train, "scaled:scale"))
    x_test <- as.data.frame(predict(pca, escala_test)[, 1:n_comp])
  }

  distribucion <- if (is.factor(y_var)) {
    if (length(levels(y_var)) == 2) "bernoulli" else "multinomial"
  } else {
    "gaussian"
  }

  formula <- as.formula(paste("y_train ~", paste(colnames(x_train), collapse = "+")))

  modelo <- gbm(
    formula,
    data = cbind(x_train, y_train),
    distribution = distribucion,
    n.trees = n_trees,
    interaction.depth = interaction_depth,
    shrinkage = learning_rate,
    bag.fraction = bag_fraction,
    n.minobsinnode = n_minobsinnode,
    cv.folds = 5,
    verbose = FALSE
  )

  predicciones <- predict(modelo, newdata = x_test, n.trees = n_trees, type = "response")

  if (distribucion == "gaussian") {
    mse <- mean((predicciones - y_test)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(predicciones - y_test))
    r2 <- cor(predicciones, y_test)^2

    tabla_metricas <- data.frame(
      Modelo = if (usar_pca) "Gradient Boosting + PCA" else "Gradient Boosting",
      RMSE = rmse,
      MAE = mae,
      R2 = r2
    )

    grafico <- ggplot(data.frame(Real = y_test, Pred = predicciones), aes(x = Real, y = Pred)) +
      geom_point(alpha = 0.6) + geom_abline(intercept = 0, slope = 1, color = "blue") +
      theme_minimal() + labs(title = "Prediccion vs Real", x = "Real", y = "Predicho")

    return(list(
      modelo = modelo,
      predicciones = predicciones,
      tabla_metricas = tabla_metricas,
      importancia = summary(modelo, plotit = FALSE),
      grafico = grafico,
      pca_modelo = if (usar_pca) pca else NULL,
      pca_center = if (usar_pca) attr(escala_train, "scaled:center") else NULL,
      pca_scale = if (usar_pca) attr(escala_train, "scaled:scale") else NULL,
      n_comp = if (usar_pca) n_comp else NULL
    ))

  } else {
    return(list(
      modelo = modelo,
      mensaje = "Este modelo es de clasificacion. Para evaluacion usar AUC u otras metricas clasificatorias.",
      pca_modelo = if (usar_pca) pca else NULL
    ))
  }
}

#' Ajustar un modelo de regresión polinomial de grado especificado
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función ajusta un modelo lineal con transformaciones polinomiales sobre las variables predictoras,
#' evaluando el ajuste mediante métricas estándar como MSE, RMSE y R².
#'
#' @param df Data frame que contiene los datos.
#' @param columnas_predictoras Vector de nombres de columnas numéricas utilizadas como variables independientes.
#' @param columna_dependiente Nombre de la columna dependiente (numérica).
#' @param grado Grado del polinomio para cada predictor (por ejemplo, 2 para cuadrático).
#'
#' @return Una lista con:
#' \describe{
#'   \item{\code{resumen_modelo}}{Resumen del modelo ajustado (output de \code{summary(lm)})}
#'   \item{\code{modelo}}{El modelo ajustado (\code{lm})}
#'   \item{\code{predicciones}}{Vector de predicciones sobre el mismo conjunto de datos}
#'   \item{\code{mse}}{Error cuadrático medio}
#'   \item{\code{rmse}}{Raíz del error cuadrático medio}
#'   \item{\code{r2}}{Coeficiente de determinación R²}
#' }
#' @family modelado_supervisado
#' @export
#'
#' @examples
#' # Usar con un conjunto de emociones como predictores
#' resultado <- ajustar_modelo_polinomial(acdfx, c("ira", "alegría", "tristeza"), "diferencia", grado = 2)
#' print(resultado$resumen_modelo)
#' print(resultado$rmse)
ajustar_modelo_polinomial <- function(df, columnas_predictoras, columna_dependiente, grado) {
  # Validaciones
  if (!is.numeric(df[[columna_dependiente]])) {
    stop("La columna dependiente debe ser numérica.")
  }
  for (col in columnas_predictoras) {
    if (!is.numeric(df[[col]])) {
      stop(paste("La columna predictora", col, "debe ser numérica."))
    }
  }

  # Construcción de la fórmula polinomial
  terminos_polinomiales <- sapply(columnas_predictoras, function(col) {
    paste0("poly(", col, ", ", grado, ", raw=TRUE)")
  })
  formula_modelo <- as.formula(paste(columna_dependiente, "~", paste(terminos_polinomiales, collapse = " + ")))

  # Ajuste del modelo
  modelo_polinomial <- lm(formula_modelo, data = df)

  # Predicciones
  predicciones <- predict(modelo_polinomial, newdata = df)

  # Evaluación
  errores <- df[[columna_dependiente]] - predicciones
  mse <- mean(errores^2)
  rmse <- sqrt(mse)
  r2 <- summary(modelo_polinomial)$r.squared

  return(list(
    resumen_modelo = summary(modelo_polinomial),
    modelo = modelo_polinomial,
    predicciones = predicciones,
    mse = mse,
    rmse = rmse,
    r2 = r2
  ))
}


# ================================================================
# REPORTE HTML DE VALIDACION: métricas, CM, ROC, Calibración, VIP
# ================================================================

#' Generar reporte HTML de una validación binaria
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param res_val objeto devuelto por validar_binario_con_split()
#' @param tokens data.frame opcional con la columna objetivo para balance en TEST
#' @param target nombre de la variable objetivo en tokens (p.ej. "Nivel_bin"); opcional
#' @param titulo título del reporte
#' @param outfile ruta de salida .html (se crean carpetas si no existen)
#' @return ruta del archivo HTML generado (invisible)
#' @family utilidades
#' @export
generar_reporte_html <- function(res_val,
                                 tokens = NULL,
                                 target = NULL,
                                 titulo = "Reporte de Validación de Modelos (DiccionariosEmocion)",
                                 outfile = "outputs_lexico/reporte_validacion.html") {
  # deps livianas
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Falta 'htmltools'. Instálalo con install.packages('htmltools')")
  if (!requireNamespace("base64enc", quietly = TRUE))
    stop("Falta 'base64enc'. Instálalo con install.packages('base64enc')")

  dir.create(dirname(outfile), showWarnings = FALSE, recursive = TRUE)

  # helpers ----------
  esc <- function(x) htmltools::htmlEscape(as.character(x), attribute = FALSE)

  # mini tabla HTML
  make_table <- function(df, caption = NULL) {
    if (is.null(df) || NROW(df) == 0) return(htmltools::HTML("<em>(sin datos)</em>"))
    thead <- htmltools::tags$thead(
      htmltools::tags$tr(lapply(colnames(df), function(h) htmltools::tags$th(esc(h))))
    )
    tbody <- htmltools::tags$tbody(
      lapply(seq_len(nrow(df)), function(i) {
        htmltools::tags$tr(lapply(df[i, ], function(v) htmltools::tags$td(esc(v))))
      })
    )
    htmltools::tags$div(
      if (!is.null(caption)) htmltools::tags$div(class = "tblcap", esc(caption)),
      htmltools::tags$table(class = "tbl", thead, tbody)
    )
  }

  # incrustar imagen si existe
  embed_img <- function(path, alt = "", width = "560px") {
    if (!is.null(path) && length(path) == 1 && file.exists(path)) {
      uri <- base64enc::dataURI(file = path, mime = "image/png")
      htmltools::tags$img(src = uri, alt = alt, style = paste0("max-width:", width, ";height:auto;border:1px solid #ddd;border-radius:6px;"))
    } else {
      htmltools::tags$div(style="color:#999;font-style:italic;", paste("(No se encontró la imagen:", path, ")"))
    }
  }

  # bloques del reporte ----------
  hdr <- htmltools::tags$div(
    htmltools::tags$h1(titulo),
    htmltools::tags$p(sprintf("Generado: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
    htmltools::tags$hr()
  )

  # Métricas principales
  mt050 <- res_val$metrics_tst_050
  mtthr <- res_val$metrics_tst_thr
  thrs  <- tryCatch({
    data.frame(modelo = names(res_val$thresholds),
               thr    = unlist(res_val$thresholds), row.names = NULL)
  }, error = function(e) NULL)
  brier <- tryCatch({
    data.frame(modelo = names(res_val$brier),
               brier  = unlist(res_val$brier), row.names = NULL)
  }, error = function(e) NULL)

  # Balance de clases en TEST (si se provee tokens + target)
  bal_test <- NULL
  if (!is.null(tokens) && !is.null(target) && !is.null(res_val$split_idx$test)) {
    if (target %in% names(tokens)) {
      y_tst <- tokens[[target]][res_val$split_idx$test]
      tb <- table(y_tst)
      bal_test <- data.frame(Clase = names(tb), Frecuencia = as.integer(tb),
                             Proporcion = round(as.integer(tb) / sum(tb), 4))
    }
  }

  sec_metrics <- htmltools::tags$div(
    htmltools::tags$h2("Métricas en TEST"),
    make_table(mt050, "Umbral 0.5"),
    htmltools::tags$br(),
    make_table(mtthr, "Umbral óptimo (Youden)"),
    htmltools::tags$br(),
    make_table(thrs, "Umbrales óptimos por modelo"),
    htmltools::tags$br(),
    make_table(brier, "Brier score (calibración, menor es mejor)"),
    if (!is.null(bal_test)) {
      htmltools::tagList(
        htmltools::tags$h3("Balance de clases en TEST"),
        make_table(bal_test)
      )
    }
  )

  # Gráficos por modelo (si existen en res_val$paths)
  modelos <- names(res_val$paths)
  sec_plots <- htmltools::tagList(htmltools::tags$h2("Gráficos"))
  for (m in modelos) {
    pths <- res_val$paths[[m]]
    cm050 <- if (!is.null(pths$cm_050)) pths$cm_050 else NULL
    cmthr <- if (!is.null(pths$cm_thr)) pths$cm_thr else NULL
    roc   <- if (!is.null(pths$roc)) pths$roc else NULL
    cal   <- if (!is.null(pths$cal)) pths$cal else NULL
    vip   <- if (!is.null(pths$vip)) pths$vip else NULL

    sec_plots <- htmltools::tagList(
      sec_plots,
      htmltools::tags$h3(sprintf("Modelo: %s", m)),
      htmltools::tags$div(class="grid",
                          htmltools::tags$div(class="card", htmltools::tags$h4("Matriz de confusión (thr=0.5)"), embed_img(cm050)),
                          htmltools::tags$div(class="card", htmltools::tags$h4("Matriz de confusión (thr óptimo)"), embed_img(cmthr)),
                          htmltools::tags$div(class="card", htmltools::tags$h4("ROC (AUC)"), embed_img(roc)),
                          htmltools::tags$div(class="card", htmltools::tags$h4("Calibración (Brier)"), embed_img(cal)),
                          htmltools::tags$div(class="card", htmltools::tags$h4("Importancia de variables"), embed_img(vip))
      ),
      htmltools::tags$hr()
    )
  }

  # CSS simple
  css <- htmltools::tags$style(htmltools::HTML("
  body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif; line-height: 1.45; color: #222; }
  h1 { margin-bottom: .2rem; }
  h2 { margin-top: 1.8rem; border-bottom: 2px solid #eee; padding-bottom: .2rem; }
  h3 { margin-top: 1.2rem; }
  .tbl { border-collapse: collapse; width: 100%; margin: .3rem 0 1rem; }
  .tbl th, .tbl td { border: 1px solid #ddd; padding: 6px 8px; text-align: right; }
  .tbl th:first-child, .tbl td:first-child { text-align: left; }
  .tblcap { font-weight: 600; margin: .5rem 0 .3rem; }
  .grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 14px; }
  .card { background: #fafafa; border: 1px solid #eee; border-radius: 8px; padding: 10px; }
  hr { border: 0; border-top: 1px solid #eee; margin: 1.2rem 0; }
  .footer { color:#777; font-size: 12px; margin-top: 2rem; }
"))


  # ensamblar y guardar
  doc <- htmltools::tags$html(
    htmltools::tags$head(htmltools::tags$title(titulo), css),
    htmltools::tags$body(
      hdr, sec_metrics, sec_plots,
      htmltools::tags$div(class="footer",
                          "Generado por modelos_aprendizaje.R · ",
                          sprintf("R %s | %s", getRversion(), paste(R.version$platform, R.version$arch)))
    )
  )


  htmltools::save_html(doc, file = outfile, background = "white")
  invisible(outfile)
}

#' Normaliza columnas de emociones a nombres canónicos (inglés)
#'
#' Crea columnas canónicas (anger, anticipation, ...) copiando desde equivalentes
#' en español cuando falten (p. ej., "alegria" -> "joy"). No elimina las columnas originales.
#'
#' @param df data.frame con columnas de emociones en español e/o inglés.
#' @return data.frame con las columnas canónicas añadidas si faltaban.
#' @examples
#' df <- data.frame(alegria = 1:3, tristeza = 3:1)
#' head(normalizar_emociones(df))
#' @family DiccionariosEmocion
#' @export
normalizar_emociones <- function(df) {
  alias <- list(
    anger        = c("ira"),
    anticipation = c("anticipación","anticipacion"),
    disgust      = c("disgusto"),
    fear         = c("miedo"),
    joy          = c("alegría","alegria"),
    sadness      = c("tristeza"),
    surprise     = c("sorpresa"),
    trust        = c("confianza"),
    negative     = c("negativo"),
    positive     = c("positivo")
  )
  for (canon in names(alias)) {
    if (!canon %in% names(df)) {
      for (es in alias[[canon]]) {
        if (es %in% names(df)) { df[[canon]] <- df[[es]]; break }
      }
    }
  }
  df
}

#' Devuelve el set canónico de features a nivel token
#' @return character vector con nombres de columnas
#' @examples
#' features_canonicas()
#' @family DiccionariosEmocion
#' @export
features_canonicas <- function() {
  c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust",
    "negative","positive","syuzhet","bing","afinn","nrc")
}

#' Prepara el data.frame y filtra features numéricas existentes (>= 2)
#'
#' @param tokens data.frame con columnas de emociones y métricas.
#' @param preferidas vector de nombres de columnas preferidas (por defecto canónicas).
#' @param estricto si TRUE y faltan preferidas, lanza error (solo informativo aquí).
#' @return lista con: `df`, `features` (numéricas halladas), `faltan` (preferidas que no están).
#' @examples
#' # preparar_features_tokens(tokens)
#' @family DiccionariosEmocion
#' @export
preparar_features_tokens <- function(tokens, preferidas = features_canonicas(), estricto = FALSE) {
  df <- normalizar_emociones(tokens)
  cand <- intersect(preferidas, names(df))
  if (!length(cand)) {
    stop("No se encontraron columnas candidatas en el data.frame.")
  }
  # solo numéricas
  is_num <- sapply(df[, cand, drop = FALSE], is.numeric)
  cand <- cand[is_num]
  faltan <- setdiff(preferidas, names(df))
  if (length(cand) < 2) {
    msg <- sprintf("No hay suficientes features numéricas para PCA/modelo. Faltan: %s",
                   paste(faltan, collapse = ", "))
    stop(msg)
  }
  list(df = df, features = cand, faltan = faltan)
}


####
# Paletas de alto contraste
.scale_discrete_contraste <- function(palette = c("Dark2","Set1","OkabeIto","viridis")) {
  palette <- match.arg(palette)
  if (palette %in% c("Dark2","Set1")) {
    return(ggplot2::scale_color_brewer(palette = palette))
  }
  if (palette == "viridis") {
    if (!requireNamespace("viridisLite", quietly = TRUE)) stop("Falta 'viridisLite'.")
    return(ggplot2::scale_color_viridis_d(option = "D"))
  }
  # Okabe–Ito (8 colores, friendly a daltónicos)
  okabe <- c("#000000","#E69F00","#56B4E9","#009E73",
             "#F0E442","#0072B2","#D55E00","#CC79A7")
  ggplot2::scale_color_manual(values = okabe)
}

#' PCA de tokens con etiquetas (contraste)
#' @inheritParams pca_tokens_plot
#' @param label_size tamaño de texto para etiquetas (default 2.3).
#' @param palette paleta discreta de alto contraste: "Dark2","Set1","OkabeIto","viridis".
#' @family reduccion_dimensional
#' @export
pca_tokens_plot <- function(tokens,
                            features = NULL,
                            label_col = "token_idioma_unico",
                            color_by  = NULL,
                            escalar   = TRUE,
                            n_labels  = 500,
                            alpha_opacidad = 0.5,
                            label_size = 2.3,
                            palette = c("Dark2","Set1","OkabeIto","viridis"),
                            out_path  = "outputs_lexico/pca_tokens.png") {

  if (!is.null(features)) {
    stopifnot(all(features %in% names(tokens)))
    prep <- list(df = tokens, features = features, faltan = setdiff(features, names(tokens)))
  } else {
    prep <- preparar_features_tokens(tokens, features_canonicas())
  }
  df <- prep$df; feat <- prep$features

  stopifnot(label_col %in% names(df))
  if (!is.null(color_by)) stopifnot(color_by %in% names(df))

  X <- df[, feat, drop = FALSE]
  X <- X[, sapply(X, is.numeric), drop = FALSE]

  keep <- stats::complete.cases(X)
  if (!any(keep)) stop("No hay filas completas para PCA.")
  X <- X[keep, , drop = FALSE]
  lab <- df[[label_col]][keep]
  colv <- if (!is.null(color_by)) df[[color_by]][keep] else NULL
  if (nrow(X) < 2) stop("Muy pocos tokens completos para PCA.")

  M <- if (escalar) scale(X) else as.matrix(X)
  pca <- stats::prcomp(M, center = FALSE, scale. = FALSE)

  pcs <- as.data.frame(pca$x[, 1:2, drop = FALSE]); names(pcs) <- c("PC1","PC2")
  pcs[[label_col]] <- lab; if (!is.null(colv)) pcs[[color_by]] <- colv

  idx_lab <- seq_len(nrow(pcs))
  if (!is.na(n_labels) && n_labels < nrow(pcs)) {
    idx_lab <- order(-(pcs$PC1^2 + pcs$PC2^2))[seq_len(n_labels)]
  }
  pcs$to_label <- FALSE; pcs$to_label[idx_lab] <- TRUE

  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Falta 'ggrepel'.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")

  var_exp <- summary(pca)$importance[2, 1:2] * 100

  p <- ggplot2::ggplot(pcs, ggplot2::aes(PC1, PC2))
  if (is.null(color_by)) {
    p <- p + ggplot2::geom_point(alpha = alpha_opacidad, size = 1.8)
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color_by]]),
                                 alpha = alpha_opacidad, size = 1.8) +
      ggplot2::guides(color = ggplot2::guide_legend(title = color_by)) +
      .scale_discrete_contraste(match.arg(palette))
  }
  p <- p +
    ggrepel::geom_text_repel(
      data = subset(pcs, to_label),
      ggplot2::aes(label = .data[[label_col]]),
      size = label_size, max.overlaps = Inf,
      box.padding = 0.25, point.padding = 0.15, min.segment.length = 0
    ) +
    ggplot2::labs(
      title = "PCA de tokens con etiquetas",
      subtitle = sprintf("Features usadas: %d%s", length(feat),
                         if (length(prep$faltan)) paste0(" · omitidas: ", paste(prep$faltan, collapse=", ")) else ""),
      x = sprintf("PC1 (%.1f%% var)", var_exp[1]),
      y = sprintf("PC2 (%.1f%% var)", var_exp[2])
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  dir.create(dirname(out_path), TRUE, TRUE)
  if (exists("save_plot", mode = "function")) save_plot(p, out_path, 8, 6) else
    ggplot2::ggsave(out_path, p, width = 8, height = 6, dpi = 300)

  list(pca = pca, df_plot = pcs, features_usadas = feat, omitidas = prep$faltan, path = out_path)
}

#' PCA biplot con etiquetas de tokens y flechas (contraste)
#' @inheritParams pca_tokens_biplot
#' @param label_size tamaño de texto para etiquetas (default 2.3).
#' @param palette paleta discreta de alto contraste: "Dark2","Set1","OkabeIto","viridis".
#' @family reduccion_dimensional
#' @export
pca_tokens_biplot <- function(tokens, features,
                              label_col = "token_idioma_unico",
                              color_by = NULL, escalar = TRUE,
                              n_labels = 500, scale_arrows = 1.6, top_vars = 10,
                              label_size = 2.3,
                              palette = c("Dark2","Set1","OkabeIto","viridis"),
                              out_path = "outputs_lexico/pca_tokens_biplot.png") {

  stopifnot(all(features %in% names(tokens)), label_col %in% names(tokens))
  if (!is.null(color_by)) stopifnot(color_by %in% names(tokens))

  X <- tokens[, features, drop = FALSE]
  X <- X[, sapply(X, is.numeric), drop = FALSE]

  keep <- stats::complete.cases(X)
  if (!any(keep)) stop("No hay filas completas para PCA.")
  X <- X[keep, , drop = FALSE]
  lab <- tokens[[label_col]][keep]
  colv <- if (!is.null(color_by)) tokens[[color_by]][keep] else NULL
  if (nrow(X) < 2) stop("Muy pocos tokens completos para PCA.")

  M <- if (escalar) scale(X) else as.matrix(X)
  pca <- stats::prcomp(M, center = FALSE, scale. = FALSE)

  pcs <- as.data.frame(pca$x[, 1:2, drop = FALSE]); names(pcs) <- c("PC1","PC2")
  pcs[[label_col]] <- lab; if (!is.null(colv)) pcs[[color_by]] <- colv

  idx_lab <- seq_len(nrow(pcs))
  if (!is.na(n_labels) && n_labels < nrow(pcs)) {
    idx_lab <- order(-(pcs$PC1^2 + pcs$PC2^2))[seq_len(n_labels)]
  }
  pcs$to_label <- FALSE; pcs$to_label[idx_lab] <- TRUE

  L <- as.data.frame(pca$rotation[, 1:2, drop = FALSE]); L$var <- rownames(L)
  L$mag <- sqrt(L$PC1^2 + L$PC2^2); L <- L[order(-L$mag), ]
  Ls <- head(L, top_vars)
  Ls$PC1 <- Ls$PC1 * scale_arrows; Ls$PC2 <- Ls$PC2 * scale_arrows

  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Falta 'ggrepel'.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("grid", quietly = TRUE)) stop("Falta 'grid'.")
  if (!requireNamespace("viridisLite", quietly = TRUE)) { } # opcional si usas "viridis"

  var_exp <- summary(pca)$importance[2, 1:2] * 100

  p <- ggplot2::ggplot(pcs, ggplot2::aes(PC1, PC2))
  if (is.null(color_by)) {
    p <- p + ggplot2::geom_point(alpha = 0.35, size = 1.8)
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(color = .data[[color_by]]), alpha = 0.35, size = 1.8) +
      ggplot2::guides(color = ggplot2::guide_legend(title = color_by)) +
      .scale_discrete_contraste(match.arg(palette))
  }
  p <- p +
    ggrepel::geom_text_repel(data = subset(pcs, to_label),
                             ggplot2::aes(label = .data[[label_col]]),
                             size = label_size, max.overlaps = Inf,
                             box.padding = 0.25, point.padding = 0.15, min.segment.length = 0) +
    ggplot2::geom_segment(data = Ls,
                          ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
                          arrow = grid::arrow(length = grid::unit(0.18, "cm")),
                          linewidth = 0.5) +
    ggrepel::geom_text_repel(data = Ls,
                             ggplot2::aes(x = PC1, y = PC2, label = var),
                             size = 2.8, box.padding = 0.25, point.padding = 0.15,
                             min.segment.length = 0) +
    ggplot2::labs(title = "PCA biplot con etiquetas de tokens",
                  subtitle = sprintf("Flechas: loadings (top %d) · escala = %.1f", top_vars, scale_arrows),
                  x = sprintf("PC1 (%.1f%% var)", var_exp[1]),
                  y = sprintf("PC2 (%.1f%% var)", var_exp[2])) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  dir.create(dirname(out_path), TRUE, TRUE)
  if (exists("save_plot", mode = "function")) save_plot(p, out_path, 9, 6) else
    ggplot2::ggsave(out_path, p, width = 9, height = 6, dpi = 300)

  list(pca = pca, df_plot = pcs, df_loadings = Ls, path = out_path)
}

#' Paletas discretas de alto contraste (helper interno)
#'
#' Devuelve una escala de color discreta de alto contraste para ggplot2.
#' Soporta "Dark2", "Set1", "OkabeIto" (paleta daltónico-friendly)
#' y "viridis" (discreta).
#'
#' @param palette Cadena: "Dark2","Set1","OkabeIto","viridis".
#' @return Escala ggplot2 para color discreto.
#' @keywords internal
.scale_discrete_contraste <- function(palette = c("Dark2","Set1","OkabeIto","viridis")) {
  palette <- match.arg(palette)
  if (palette %in% c("Dark2","Set1")) {
    return(ggplot2::scale_color_brewer(palette = palette))
  }
  if (palette == "viridis") {
    if (!requireNamespace("viridisLite", quietly = TRUE))
      stop("Falta 'viridisLite'.")
    return(ggplot2::scale_color_viridis_d(option = "D"))
  }
  # Okabe–Ito (8 colores)
  okabe <- c("#000000","#E69F00","#56B4E9","#009E73",
             "#F0E442","#0072B2","#D55E00","#CC79A7")
  ggplot2::scale_color_manual(values = okabe)
}

#' Análisis de Componentes General (PCA, MFA, PLS) con visualización ggplot2
#'
#' Unifica PCA, MFA y PLS bajo una sola interfaz. Permite definir variables
#' independientes (\code{vars_independientes}), opcionalmente variables dependientes
#' (\code{vars_dependientes}) y, en el caso de MFA, bloques/tipos de variables
#' (\code{bloques}, \code{tipos_bloques}). Devuelve coordenadas de individuos y
#' variables, más una figura \pkg{ggplot2} en un solo lienzo.
#'
#' @param data \code{data.frame}. Conjunto de datos (p.ej., \code{tokens}).
#' @param metodo \code{character}. Uno de \code{"PCA"}, \code{"MFA"}, \code{"PLS"}.
#' @param vars_independientes \code{character}. Nombres de columnas numéricas usadas como X.
#' @param vars_dependientes \code{character} o \code{NULL}. Para \code{PLS} (Y) o como
#'   variables suplementarias en \code{PCA}/\code{MFA}. En \code{PLS}, si Y es factor binaria,
#'   se convertirá a 0/1 (primera categoría = 1).
#' @param bloques \code{list} o \code{NULL}. Solo para \code{MFA}. Lista de vectores de
#'   nombres de columnas por bloque (p.ej., \code{list(lex=c(...), emo=c(...))}). Alternativa:
#'   vector de tamaños (enteros) en el orden de \code{vars_independientes}.
#' @param tipos_bloques \code{character} o \code{NULL}. Solo para \code{MFA}. Tipos de cada
#'   bloque, p.ej., \code{c("s","s","n")} (ver \code{FactoMineR::MFA}).
#' @param escalar \code{logical}. Si \code{TRUE}, estandariza X.
#' @param n_comp \code{integer}. Nº de componentes/ejes a calcular (min 2; se grafican PC1 y PC2).
#' @param color_by \code{character} o \code{NULL}. Columna categórica para color (no entra en el ajuste).
#' @param label_col \code{character} o \code{NULL}. Columna con etiquetas de puntos (p.ej., \code{"token_orig"}).
#' @param idioma_col \code{character} o \code{NULL}. Columna categórica que puede mapearse a \emph{shape}.
#' @param n_labels \code{integer} o \code{NA}. Máximo de etiquetas de puntos (\code{NA} para todas).
#' @param label_min_r2 \code{numeric}. Umbral mínimo de \eqn{PC1^2+PC2^2} para etiquetar.
#' @param point_size \code{numeric}. Tamaño de puntos.
#' @param label_size \code{numeric}. Tamaño del texto de etiquetas de individuos.
#' @param label_size_vars \code{numeric}. Tamaño del texto de etiquetas de variables.
#' @param alpha_opacidad \code{numeric} en \code{[0,1]}. Opacidad de puntos.
#' @param palette \code{character}. \code{"Dark2"}, \code{"Set1"}, \code{"OkabeIto"}, \code{"viridis"}.
#' @param show_ellipses \code{logical}. Dibuja elipses por \code{color_by} si hay suficientes puntos.
#' @param ellipse_min_n \code{integer}. Mínimo de puntos por grupo para elipse.
#' @param ellipse_level \code{numeric}. Nivel de confianza de elipse (p.ej., 0.9).
#' @param shape_mode \code{character}. \code{"auto"}, \code{"none"} o \code{"topk"} para manejar muchas categorías.
#' @param shape_top_k \code{integer}. Si \code{shape_mode="topk"}, nº máximo de shapes y el resto se agrupa en "Otros".
#' @param top_vars \code{integer}. Nº de variables (loadings) a mostrar (flechas) en el plano.
#' @param scale_arrows \code{numeric}. Factor de escala para las flechas de variables (loadings).
#' @param out_path \code{character} o \code{NULL}. Si no es \code{NULL}, guarda PNG en esa ruta.
#'
#' @return \code{list} con:
#' \itemize{
#'   \item \code{metodo}: método usado.
#'   \item \code{modelo}: objeto del modelo subyacente (\code{prcomp}, \code{MFA}, \code{mvr}).
#'   \item \code{coords_ind}: \code{data.frame} con PC1/PC2 de individuos (+ columnas de color/shape/label).
#'   \item \code{coords_var}: \code{data.frame} con loadings de variables (PC1/PC2 y magnitud).
#'   \item \code{plot}: objeto \pkg{ggplot2}.
#'   \item \code{path}: ruta del PNG si se guardó.
#' }
#'
#' @details
#' \strong{PCA}: usa \code{stats::prcomp}. \cr
#' \strong{MFA}: usa \code{FactoMineR::MFA}; requiere \code{bloques} y \code{tipos_bloques}. \cr
#' \strong{PLS}: usa \code{pls::plsr}. Si \code{vars_dependientes} es factor binaria se transforma a 0/1.
#'
#' @examples
#' \dontrun{
#' # Ejemplo con tokens:
#' tokens <- cl_res$data
#' feat <- seleccionar_features_tokens(tokens)
#'
#' # 1) PCA puro (exploratorio)
#' res_pca <- analisis_componentes_general(
#'   data = tokens,
#'   metodo = "PCA",
#'   vars_independientes = feat,
#'   color_by = "cluster",
#'   label_col = "token_orig",
#'   idioma_col = "token_idioma_unico",
#'   shape_mode = "none",     # evita 1000 shapes
#'   n_labels = 200,
#'   palette = "OkabeIto",
#'   out_path = "outputs_lexico/pca_general.png"
#' )
#' print(res_pca$plot)
#'
#' # 2) MFA por bloques (ejemplo: 4 léxicos + 10 emocionales)
#' bloques <- list(
#'   lex = c("syuzhet","bing","afinn","nrc"),
#'   emo = c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
#' )
#' tipos <- c("s","s")  # ambas cuantitativas estandarizadas
#' res_mfa <- analisis_componentes_general(
#'   data = tokens,
#'   metodo = "MFA",
#'   vars_independientes = unlist(bloques),
#'   bloques = bloques,
#'   tipos_bloques = tipos,
#'   color_by = "cluster",
#'   label_col = "token_orig",
#'   palette = "Dark2",
#'   out_path = "outputs_lexico/mfa_general.png"
#' )
#' print(res_mfa$plot)
#'
#' # 3) PLS (ej.: predecir valencia numérica o binaria)
#' # Supón que 'Nivel_bin' es factor con 2 niveles o tienes una métrica continua 'valencia_plus'
#' res_pls <- analisis_componentes_general(
#'   data = tokens,
#'   metodo = "PLS",
#'   vars_independientes = feat,
#'   vars_dependientes = "valencia_plus",  # o "Nivel_bin" (binaria)
#'   color_by = "cluster",
#'   label_col = "token_orig",
#'   palette = "Set1",
#'   out_path = "outputs_lexico/pls_general.png"
#' )
#' print(res_pls$plot)
#' }
#'
#' @family DiccionariosEmocion
#' @export
analisis_componentes_general <- function(
    data,
    metodo = c("PCA","MFA","PLS"),
    vars_independientes,
    vars_dependientes = NULL,
    bloques = NULL,
    tipos_bloques = NULL,
    escalar = TRUE,
    n_comp = 2,
    color_by = NULL,
    label_col = NULL,
    idioma_col = NULL,
    n_labels = 200,
    label_min_r2 = 0,
    point_size = 2.2,
    label_size = 2.2,
    label_size_vars = 2.3,
    alpha_opacidad = 0.7,
    palette = c("Dark2","Set1","OkabeIto","viridis"),
    show_ellipses = TRUE,
    ellipse_min_n = 3L,
    ellipse_level = 0.9,
    shape_mode = c("auto","none","topk"),
    shape_top_k = 6L,
    top_vars = 10,
    scale_arrows = 1.6,
    out_path = NULL
){
  stopifnot(is.data.frame(data))
  metodo <- match.arg(metodo)
  shape_mode <- match.arg(shape_mode)
  palette <- match.arg(palette)

  if (!is.null(color_by))  stopifnot(color_by %in% names(data))
  if (!is.null(label_col)) stopifnot(label_col %in% names(data))
  if (!is.null(idioma_col)) stopifnot(idioma_col %in% names(data))

  # --- Helpers internos -------------------------------------------------------
  .pal <- function(pal) {
    switch(pal,
           "Dark2"   = ggplot2::scale_color_brewer(palette = "Dark2"),
           "Set1"    = ggplot2::scale_color_brewer(palette = "Set1"),
           "viridis" = { if (requireNamespace("viridisLite", quietly=TRUE))
             ggplot2::scale_color_viridis_d(option = "D") else ggplot2::scale_color_brewer(palette = "Dark2") },
           # Okabe-Ito:
           ggplot2::scale_color_manual(values = c("#000000","#E69F00","#56B4E9","#009E73",
                                                  "#F0E442","#0072B2","#D55E00","#CC79A7"))
    )
  }

  .shape_prepare <- function(df, idioma_col, shape_mode, shape_top_k) {
    add_shape <- FALSE
    if (!is.null(idioma_col)) {
      n_lv <- length(unique(na.omit(df[[idioma_col]])))
      if (shape_mode == "none") {
        add_shape <- FALSE
      } else if (shape_mode == "topk") {
        add_shape <- TRUE
        frec <- sort(table(df[[idioma_col]]), decreasing = TRUE)
        lv_keep <- names(frec)[seq_len(min(shape_top_k, length(frec)))]
        df[[idioma_col]] <- ifelse(df[[idioma_col]] %in% lv_keep, df[[idioma_col]], "Otros")
        df[[idioma_col]] <- factor(df[[idioma_col]], levels = c(lv_keep, "Otros"))
      } else { # auto
        if (n_lv <= min(shape_top_k, 6L)) add_shape <- TRUE else add_shape <- FALSE
      }
    }
    list(df = df, add_shape = add_shape)
  }

  # --- Preparar X (independientes) -------------------------------------------
  stopifnot(all(vars_independientes %in% names(data)))
  X0 <- data[, vars_independientes, drop = FALSE]
  num_mask <- vapply(X0, is.numeric, logical(1))
  if (!all(num_mask)) {
    # intenta coerción segura
    for (nm in names(X0)[!num_mask]) {
      if (is.factor(X0[[nm]]) || is.character(X0[[nm]])) {
        sup <- suppressWarnings(as.numeric(as.character(X0[[nm]])))
        if (all(is.finite(sup) | is.na(sup))) X0[[nm]] <- sup
      }
    }
  }
  X <- as.data.frame(X0)
  num_mask2 <- vapply(X, is.numeric, logical(1))
  if (!all(num_mask2)) {
    stop("Hay variables independientes no numéricas que no se pudieron convertir a numérico.")
  }

  keep <- stats::complete.cases(X)
  if (!any(keep)) stop("No hay filas completas en X.")
  X <- X[keep, , drop = FALSE]

  # --- Variables de color/shape/labels alineadas ------------------------------
  colv <- if (!is.null(color_by))  data[[color_by]][keep]  else NULL
  lab  <- if (!is.null(label_col)) data[[label_col]][keep] else NULL
  idi  <- if (!is.null(idioma_col)) data[[idioma_col]][keep] else NULL

  # --- Ajustar según método ----------------------------------------------------
  coords_ind <- coords_var <- NULL
  model_obj <- NULL
  var_exp <- c(NA_real_, NA_real_)

  if (metodo == "PCA") {
    M <- if (escalar) base::scale(X) else as.matrix(X)
    model_obj <- stats::prcomp(M, center = FALSE, scale. = FALSE, rank. = max(2L, n_comp))
    sco <- as.data.frame(model_obj$x[, 1:2, drop = FALSE]); names(sco) <- c("PC1","PC2")
    lod <- as.data.frame(model_obj$rotation[, 1:2, drop = FALSE]); names(lod) <- c("PC1","PC2")
    lod$var <- rownames(lod); lod$mag <- sqrt(lod$PC1^2 + lod$PC2^2)
    var_exp <- summary(model_obj)$importance[2, 1:2] * 100

    coords_ind <- sco
    coords_var <- lod

  } else if (metodo == "MFA") {
    if (!requireNamespace("FactoMineR", quietly = TRUE)) {
      stop("Falta 'FactoMineR' para ejecutar MFA.")
    }

    # --- construir inputs group/type
    if (is.null(bloques)) stop("Para MFA debes proporcionar 'bloques'.")

    if (is.list(bloques)) {
      group_sizes <- vapply(bloques, length, integer(1))
      all_names <- unlist(bloques, use.names = FALSE)
      stopifnot(all(all_names %in% names(data)))
      X_mfa <- data[keep, all_names, drop = FALSE]
    } else if (is.numeric(bloques)) {
      group_sizes <- as.integer(bloques)
      stopifnot(sum(group_sizes) == ncol(X))
      X_mfa <- X
    } else stop("'bloques' debe ser lista de nombres de columnas o vector de tamaños.")

    if (is.null(tipos_bloques)) {
      tipos_bloques <- rep("s", length(group_sizes)) # por defecto: cuantitativas estandarizadas
    }
    stopifnot(length(tipos_bloques) == length(group_sizes))

    model_obj <- FactoMineR::MFA(X_mfa, group = group_sizes, type = tipos_bloques,
                                 ncp = max(2L, n_comp), graph = FALSE)

    ind <- as.data.frame(model_obj$ind$coord[, 1:2, drop = FALSE])
    names(ind) <- c("PC1","PC2")

    # --- extraer variables: var$coord si existe; si no, quanti.var/quali.var
    .extract_mfa_vars <- function(mfa_obj) {
      out <- list()
      if (!is.null(mfa_obj$var) && !is.null(mfa_obj$var$coord) && nrow(mfa_obj$var$coord) > 0) {
        vc <- as.data.frame(mfa_obj$var$coord[, 1:2, drop = FALSE])
        vc$var <- rownames(mfa_obj$var$coord)
        out <- vc
      } else {
        parts <- list()
        if (!is.null(mfa_obj$quanti.var) && !is.null(mfa_obj$quanti.var$coord) &&
            nrow(mfa_obj$quanti.var$coord) > 0) {
          qv <- as.data.frame(mfa_obj$quanti.var$coord[, 1:2, drop = FALSE])
          qv$var <- rownames(mfa_obj$quanti.var$coord)
          parts <- c(parts, list(qv))
        }
        if (!is.null(mfa_obj$quali.var) && !is.null(mfa_obj$quali.var$coord) &&
            nrow(mfa_obj$quali.var$coord) > 0) {
          lv <- as.data.frame(mfa_obj$quali.var$coord[, 1:2, drop = FALSE])
          lv$var <- rownames(mfa_obj$quali.var$coord)
          parts <- c(parts, list(lv))
        }
        if (length(parts)) out <- do.call(rbind, parts)
      }
      out
    }

    var <- .extract_mfa_vars(model_obj)
    # puede ocurrir que no haya variables geométricas (poco habitual pero válido)
    if (NROW(var)) {
      names(var)[1:2] <- c("PC1","PC2")
      var$mag <- sqrt(var$PC1^2 + var$PC2^2)
    } else {
      var <- data.frame(PC1 = numeric(0), PC2 = numeric(0), var = character(0), mag = numeric(0))
    }

    eig <- model_obj$eig
    if (NROW(eig) >= 2) var_exp <- eig[1:2, 2] else var_exp <- c(NA_real_, NA_real_)

    coords_ind <- ind
    coords_var <- var


  } else if (metodo == "PLS") {
    if (is.null(vars_dependientes) || length(vars_dependientes) != 1L) {
      stop("PLS requiere una sola variable dependiente en 'vars_dependientes'.")
    }
    if (!requireNamespace("pls", quietly = TRUE)) {
      stop("Falta 'pls' para ejecutar PLS.")
    }
    y <- data[[vars_dependientes]][keep]
    # Soporte binario (factor) o continuo
    if (is.factor(y) || is.character(y)) {
      y <- as.factor(y)
      lv <- levels(y)
      if (length(lv) != 2L) stop("Para PLS, si Y es factor debe ser binaria (2 niveles).")
      y_num <- as.numeric(y == lv[1L]) # primera categoría = 1
    } else {
      y_num <- suppressWarnings(as.numeric(y))
      if (!all(is.finite(y_num) | is.na(y_num))) stop("Y no puede convertirse a numérico para PLS.")
    }
    df_pls <- data.frame(y = y_num, X)
    form <- stats::as.formula(paste("y ~", paste(colnames(X), collapse = "+")))
    model_obj <- pls::plsr(form, data = df_pls, scale = escalar, ncomp = max(2L, n_comp))

    sco <- as.data.frame(pls::scores(model_obj)[, 1:2, drop = FALSE]); names(sco) <- c("PC1","PC2")
    lod <- as.data.frame(pls::loadings(model_obj)[, 1:2, drop = FALSE]); names(lod) <- c("PC1","PC2")
    lod$var <- rownames(lod); lod$mag <- sqrt(lod$PC1^2 + lod$PC2^2)

    # var_exp aproximada: proporción de varianza explicada en X por componente
    explx <- try(pls::explvar(model_obj), silent = TRUE)
    if (!inherits(explx, "try-error") && length(explx) >= 2) var_exp <- explx[1:2] else var_exp <- c(NA_real_, NA_real_)

    coords_ind <- sco
    coords_var <- lod
  }

  # --- Armar DF de plotting ---------------------------------------------------
  if (!is.null(color_by))  coords_ind[[color_by]]   <- colv
  if (!is.null(label_col)) coords_ind[[label_col]]  <- lab
  if (!is.null(idioma_col)) coords_ind[[idioma_col]] <- idi

  coords_ind <- coords_ind[is.finite(coords_ind$PC1) & is.finite(coords_ind$PC2), , drop = FALSE]

  # Etiquetado por distancia al origen
  r2 <- rowSums(coords_ind[, c("PC1","PC2"), drop = FALSE]^2)
  idx <- which(r2 >= label_min_r2)
  if (!is.na(n_labels) && length(idx) > n_labels) {
    idx <- idx[order(r2[idx], decreasing = TRUE)][seq_len(n_labels)]
  }
  coords_ind$to_label <- FALSE
  if (length(idx)) coords_ind$to_label[idx] <- TRUE

  # Selección de top variables (flechas)
  coords_var <- coords_var[order(-coords_var$mag), , drop = FALSE]
  coords_var_top <- utils::head(coords_var, top_vars)
  coords_var_top$PC1 <- coords_var_top$PC1 * scale_arrows
  coords_var_top$PC2 <- coords_var_top$PC2 * scale_arrows

  # Shapes seguros
  shp <- .shape_prepare(coords_ind, idioma_col, shape_mode, shape_top_k)
  coords_ind <- shp$df; add_shape <- shp$add_shape

  # --- Plot ggplot2 -----------------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Falta 'ggrepel'.")

  p <- ggplot2::ggplot(coords_ind, ggplot2::aes(PC1, PC2))
  aes_point <- if (!is.null(color_by))
    ggplot2::aes(color = .data[[color_by]]) else ggplot2::aes()
  p <- p + ggplot2::geom_point(aes_point, alpha = alpha_opacidad, size = point_size, na.rm = TRUE)

  if (!is.null(color_by)) {
    p <- p + .pal(palette) + ggplot2::guides(color = ggplot2::guide_legend(title = color_by))
  }
  if (add_shape) {
    p <- p + ggplot2::geom_point(ggplot2::aes(shape = .data[[idioma_col]]),
                                 alpha = 0, size = point_size, na.rm = TRUE) +
      ggplot2::guides(shape = ggplot2::guide_legend(title = idioma_col))
  }

  # Elipses por grupo si aplica
  if (show_ellipses && !is.null(color_by)) {
    cnt <- stats::ave(rep(1L, nrow(coords_ind)), coords_ind[[color_by]], FUN = sum)
    ind_ell <- coords_ind[cnt >= ellipse_min_n, , drop = FALSE]
    if (nrow(ind_ell) > 2) {
      p <- p + ggplot2::stat_ellipse(ggplot2::aes(color = .data[[color_by]]),
                                     data = ind_ell, level = ellipse_level,
                                     linewidth = 0.5, show.legend = FALSE, na.rm = TRUE)
    }
  }

  # Flechas de variables (loadings)
  if (NROW(coords_var_top) > 0) {
    p <- p + ggplot2::geom_segment(
      data = coords_var_top,
      ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
      arrow = grid::arrow(length = grid::unit(0.18, "cm")),
      linewidth = 0.5, na.rm = TRUE
    ) +
      ggrepel::geom_text_repel(
        data = coords_var_top,
        ggplot2::aes(x = PC1, y = PC2, label = .data$var),
        size = label_size_vars,
        box.padding = 0.25, point.padding = 0.15,
        min.segment.length = 0, na.rm = TRUE
      )
  }

  title_txt <- switch(metodo,
                      "PCA" = "PCA (plano 1–2)",
                      "MFA" = "MFA (plano 1–2)",
                      "PLS" = "PLS (Scores 1–2)")

  p <- p + ggplot2::labs(
    title = title_txt,
    subtitle = sprintf("PC1 (%.1f%%) · PC2 (%.1f%%) · Método: %s", var_exp[1], var_exp[2], metodo),
    x = "Componente 1", y = "Componente 2"
  ) + ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank())

  if (any(coords_ind$to_label) && !is.null(label_col)) {
    p <- p + ggrepel::geom_text_repel(
      data = subset(coords_ind, to_label),
      ggplot2::aes(label = .data[[label_col]]),
      size = label_size, max.overlaps = Inf,
      box.padding = 0.25, point.padding = 0.15,
      min.segment.length = 0, na.rm = TRUE
    )
  }

  # Guardar si se solicita
  out_path_final <- NULL
  if (!is.null(out_path)) {
    dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
    try(ggplot2::ggsave(out_path, p, width = 9, height = 6, dpi = 300), silent = TRUE)
    out_path_final <- out_path
  }

  list(
    metodo = metodo,
    modelo = model_obj,
    coords_ind = coords_ind,
    coords_var = coords_var,
    plot = p,
    path = out_path_final
  )
}


#' Correlación (Spearman) entre métodos de nivel y heatmap anotado
#'
#' Convierte niveles ordinales (N-3..N+3, Pos/Neg, etc.) a escala numérica y
#' calcula la matriz de correlaciones de Spearman, graficando un heatmap con anotaciones.
#'
#' @param df data.frame con columnas a correlacionar.
#' @param cols vector de nombres de columnas a usar.
#' @param out_path archivo de salida (PNG).
#'
#' @return lista con `cor_mat` y `path`.
#' @examples
#' # analizar_coherencia_metodos(df, c("Nivel_bing","Nivel_afinn","Nivel_nrc"))
#' @family DiccionariosEmocion
#' @export
analizar_coherencia_metodos <- function(df, cols, out_path = "outputs_lexico/coherencia_metodos/correlacion_metodos.png") {
  stopifnot(all(cols %in% names(df)))

  map_level <- function(v) {
    if (is.numeric(v)) return(v)
    x <- as.character(v)
    x <- sub("^N\\+", "P", x)
    m <- c("N-3"=-3,"N-2"=-2,"N-1"=-1,"N-0"=0,"N+1"=1,"N+2"=2,"N+3"=3,
           "P1"=1,"P2"=2,"P3"=3,"Pos"=1,"Neg"=-1)
    as.numeric(ifelse(x %in% names(m), m[x], NA))
  }

  M <- as.data.frame(lapply(df[, cols, drop = FALSE], map_level))
  keep <- stats::complete.cases(M)
  if (!any(keep)) stop("No hay filas completas para correlación.")
  M <- M[keep, , drop = FALSE]

  cor_mat <- stats::cor(M, use = "pairwise.complete.obs", method = "spearman")

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Falta 'ggplot2'.")
  if (!requireNamespace("reshape2", quietly = TRUE)) stop("Falta 'reshape2'.")

  dir.create(dirname(out_path), TRUE, TRUE)
  mlong <- reshape2::melt(cor_mat, varnames = c("Var1","Var2"), value.name = "rho")
  g <- ggplot2::ggplot(mlong, ggplot2::aes(Var1, Var2, fill = rho)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", rho)), size = 3) +
    ggplot2::scale_fill_gradient2(low = "#b2182b", mid = "white", high = "#2166ac", midpoint = 0) +
    ggplot2::labs(title = "Correlación (Spearman) entre métodos de nivel") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle=45, hjust=1))

  ggplot2::ggsave(out_path, g, width = 7.5, height = 6, dpi = 300)
  list(cor_mat = cor_mat, path = out_path)
}

#' Guarda artefactos de modelos (fits, receta de dummies y metadatos)
#'
#' @param res_val lista con `fits` (e.g., rf/svm/glmnet), `dummy_recipe`.
#' @param features vector de nombres de features originales.
#' @param path_rds ruta del archivo RDS a crear.
#' @return ruta creada (invisible).
#' @examples
#' # guardar_artefacto_modelos(res_val, features)
#' @family DiccionariosEmocion
#' @export
guardar_artefacto_modelos <- function(res_val, features, path_rds = "outputs_lexico/modelos_emocion.rds") {
  dir.create(dirname(path_rds), TRUE, TRUE)
  positive <- levels(res_val$fits$rf$trainingData$.outcome)[1]
  saveRDS(list(fits = res_val$fits, dummies = res_val$dummy_recipe,
               features = features, positive = positive), path_rds)
  invisible(path_rds)
}

#' Construye matriz de predictores desde un artefacto
#'
#' @param new_df data.frame nuevo con columnas originales (antes de dummies).
#' @param artifact lista cargada desde `guardar_artefacto_modelos()`.
#' @return data.frame X con columnas alineadas a las del modelo.
#' @examples
#' # X <- build_matrix_from_artifact(new_df, readRDS("...rds"))
#' @family DiccionariosEmocion
#' @export
build_matrix_from_artifact <- function(new_df, artifact) {
  X0 <- new_df[, artifact$features, drop = FALSE]
  for (nm in names(X0)) if (!is.numeric(X0[[nm]])) X0[[nm]] <- as.factor(X0[[nm]])
  X <- as.data.frame(predict(artifact$dummies, newdata = X0))
  need <- artifact$fits$rf$finalModel$xNames
  miss <- setdiff(need, names(X)); if (length(miss)) X[, miss] <- 0
  X[, need, drop = FALSE]
}

#' Predice usando un artefacto entrenado (rf/svmRadial/glmnet)
#'
#' @param new_df data.frame nuevo.
#' @param artifact_rds ruta del artefacto RDS.
#' @param modelo uno de `c("rf","svmRadial","glmnet")`.
#' @param devolver `c("prob","clase")`.
#' @return vector de probabilidades (clase positiva) o factor de clases.
#' @examples
#' # predecir_con_artefacto(new_df, "outputs_lexico/modelos_emocion.rds", "rf", "prob")
#' @family DiccionariosEmocion
#' @export
predecir_con_artefacto <- function(new_df, artifact_rds = "outputs_lexico/modelos_emocion.rds",
                                   modelo = c("rf","svmRadial","glmnet"),
                                   devolver = c("prob","clase")) {
  modelo <- match.arg(modelo); devolver <- match.arg(devolver)
  art <- readRDS(artifact_rds); X <- build_matrix_from_artifact(new_df, art)
  fit <- switch(modelo, rf=art$fits$rf, svmRadial=art$fits$svmRadial, glmnet=art$fits$glmnet)
  if (devolver == "clase") {
    stats::predict(fit, newdata = X, type = "raw")
  } else {
    pp <- stats::predict(fit, newdata = X, type = "prob")
    pos <- levels(fit$trainingData$.outcome)[1]; pp[, pos]
  }
}

#' Evalúa desempeño con un artefacto y un dataset etiquetado
#'
#' Calcula AUC (pROC), Accuracy/Kappa (caret), Brier score y retorna la matriz de confusión.
#'
#' @param df_labeled data.frame con verdad-terreno en `target`.
#' @param target nombre de columna objetivo (factor o coercible a factor).
#' @param artifact_rds ruta del artefacto RDS.
#' @param modelo `c("rf","svmRadial","glmnet")`.
#' @param thr umbral para convertir probabilidades a clase positiva.
#' @return lista con `metrics`, `roc` y `confusion`.
#' @examples
#' # evaluar_con_artefacto(df, "clase", "outputs_lexico/modelos_emocion.rds", "rf")
#' @family DiccionariosEmocion
#' @export
evaluar_con_artefacto <- function(df_labeled, target, artifact_rds = "outputs_lexico/modelos_emocion.rds",
                                  modelo = c("rf","svmRadial","glmnet"), thr = 0.5) {
  if (!requireNamespace("pROC", quietly = TRUE)) stop("Falta 'pROC'.")
  if (!requireNamespace("caret", quietly = TRUE)) stop("Falta 'caret'.")

  modelo <- match.arg(modelo); art <- readRDS(artifact_rds)
  stopifnot(target %in% names(df_labeled))

  y_fac <- factor(make.names(as.character(df_labeled[[target]])))
  X <- build_matrix_from_artifact(df_labeled, art)
  pp <- predecir_con_artefacto(df_labeled, artifact_rds, modelo, "prob")

  roc_obj <- pROC::roc(response = y_fac, predictor = pp, quiet = TRUE)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  pred_thr <- factor(ifelse(pp >= thr, levels(y_fac)[1], levels(y_fac)[2]), levels = levels(y_fac))
  cm <- caret::confusionMatrix(pred_thr, y_fac)

  y01 <- as.integer(y_fac == levels(y_fac)[1])
  brier <- mean((pp - y01)^2)

  list(metrics = c(Accuracy = cm$overall["Accuracy"],
                   Kappa = cm$overall["Kappa"],
                   AUC = auc_val,
                   Brier = brier),
       roc = roc_obj, confusion = cm)
}

