# ============================================================
# analisis_emociones.R
# Funciones utilitarias para el analisis comparado de metodos
# (Syuzhet, Bing, AFINN, NRC) + NRC emociones + indices/KPIs
# ============================================================

#' prep_emotions_data
#'
#' @description
#' (Auto-generado) Documentación roxygen2 mejorada a partir del reporte de auditoría.
#' @param dfx Descripción del parámetro dfx.
#' @param method_cols Descripción del parámetro method_cols.
#' @param nrc_cols Descripción del parámetro nrc_cols.
#' @param id_cols Descripción del parámetro id_cols.
#' @return Objeto resultante de la función.
#' @examples
#' # prep_emotions_data(dfx, method_cols, nrc_cols, id_cols)
#' @seealso \code{?prep_emotions_data}
#' @family DiccionariosEmocion
#' @export
prep_emotions_data <- function(dfx, method_cols, nrc_cols, id_cols = character(0)) {
  stopifnot(is.data.frame(dfx))
  # Subconjuntos
  miss_methods <- setdiff(method_cols, names(dfx))
  miss_nrc     <- setdiff(nrc_cols, names(dfx))
  if (length(miss_methods)) stop("Faltan columnas de metodos: ", paste(miss_methods, collapse = ", "))
  if (length(miss_nrc))     stop("Faltan columnas NRC: ", paste(miss_nrc, collapse = ", "))

  df <- dfx

  # Forzar numerico en metodos y NRC
  for (nm in c(method_cols, nrc_cols)) {
    if (!is.numeric(df[[nm]])) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
  }

  # Z-score por metodo
  methods_raw <- df[, method_cols, drop = FALSE]
  methods_z   <- as.data.frame(scale(methods_raw), check.names = FALSE)
  nrc_mat     <- df[, nrc_cols, drop = FALSE]

  # Resumen NA
  na_rate <- vapply(df[, c(method_cols, nrc_cols), drop = FALSE],
                    function(v) mean(is.na(v)), numeric(1))
  # Duplicados por token_idioma_unico si existe
  dupe_key <- if ("token_idioma_unico" %in% names(df)) "token_idioma_unico" else NA_character_
  n_dup    <- if (!is.na(dupe_key)) sum(duplicated(df[[dupe_key]])) else 0L

  out <- list(
    data        = df[, unique(c(id_cols, method_cols, nrc_cols)), drop = FALSE],
    methods_raw = methods_raw,
    methods_z   = methods_z,
    nrc_mat     = nrc_mat,
    meta        = list(na_rate = na_rate, duplicate_key = dupe_key, n_duplicates = n_dup)
  )
  out
}

#' compute_correlations
#'
#' @description
#' (Auto-generado) Documentación roxygen2 mejorada a partir del reporte de auditoría.
#' @param df Descripción del parámetro df.
#' @param cols Descripción del parámetro cols.
#' @param method Descripción del parámetro method.
#' @param "spearman" Descripción del parámetro "spearman".
#' @return Objeto resultante de la función.
#' @examples
#' # compute_correlations(df, cols, method, "spearman")
#' @seealso \code{?compute_correlations}
#' @family DiccionariosEmocion
#' @export
compute_correlations <- function(df, cols, method = c("pearson","spearman")) {
  method <- match.arg(method)
  mat <- as.matrix(df[, cols, drop = FALSE])
  suppressWarnings(cor(mat, use = "pairwise.complete.obs", method = method))
}

#' compute_kappa
#'
#' @description
#' (Auto-generado) Documentación roxygen2 mejorada a partir del reporte de auditoría.
#' @param df Descripción del parámetro df.
#' @param level_cols Descripción del parámetro level_cols.
#' @return Objeto resultante de la función.
#' @examples
#' # compute_kappa(df, level_cols)
#' @seealso \code{?compute_kappa}
#' @family DiccionariosEmocion
#' @export
compute_kappa <- function(df, level_cols) {
  # Utiliza irr::kappa2 y irr::kappam.fleiss si existen; si no, calcula Cohen basico
  have_irr <- requireNamespace("irr", quietly = TRUE)

  # normalizar a factor con mismos niveles
  lvls <- unique(unlist(lapply(df[level_cols], levels)))
  if (length(lvls)) {
    for (nm in level_cols) df[[nm]] <- factor(df[[nm]], levels = lvls)
  }

  # Pares
  pairs <- utils::combn(level_cols, 2, simplify = FALSE)
  kp_rows <- lapply(pairs, function(p) {
    a <- df[[p[1]]]; b <- df[[p[2]]]
    a <- droplevels(a); b <- droplevels(b)
    if (have_irr) {
      k <- irr::kappa2(cbind(a, b))$value
    } else {
      # kappa simple (Cohen) manual
      tab <- table(a, b)
      n <- sum(tab)
      p0 <- sum(diag(tab)) / n
      pa <- rowSums(tab) / n
      pb <- colSums(tab) / n
      pe <- sum(pa * pb)
      k <- (p0 - pe) / (1 - pe)
    }
    data.frame(par = paste(p, collapse = " ~ "), kappa = as.numeric(k))
  })
  kappa_pairs <- do.call(rbind, kp_rows)

  # Fleiss
  fleiss <- NA_real_
  if (have_irr && length(level_cols) >= 2) {
    # irr::kappam.fleiss espera matriz de codificaciones (n_items x n_raters) como factores
    fleiss <- tryCatch(irr::kappam.fleiss(df[level_cols])$value, error = function(e) NA_real_)
  }

  list(kappa_pairs = kappa_pairs, fleiss_kappa = fleiss, details = list(level_cols = level_cols))
}

#' cronbach_alpha
#'
#' @description
#' (Auto-generado) Documentación roxygen2 mejorada a partir del reporte de auditoría.
#' @param df Descripción del parámetro df.
#' @param cols Descripción del parámetro cols.
#' @return Objeto resultante de la función.
#' @examples
#' # cronbach_alpha(df, cols)
#' @seealso \code{?cronbach_alpha}
#' @family DiccionariosEmocion
#' @export
cronbach_alpha <- function(df, cols) {
  X <- as.matrix(df[, cols, drop = FALSE])
  # quitar filas con NA en todas las cols
  ok <- rowSums(is.na(X)) < ncol(X)
  X <- X[ok, , drop = FALSE]
  # alpha clasico
  k <- ncol(X)
  vars <- apply(X, 2, stats::var, na.rm = TRUE)
  total <- stats::var(rowSums(X, na.rm = TRUE), na.rm = TRUE)
  alpha <- (k / (k - 1)) * (1 - sum(vars, na.rm = TRUE) / total)
  list(alpha = as.numeric(alpha), k = k, var_items = vars, var_total = total)
}

#' PCA (prcomp) con salida ordenada
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param df data.frame
#' @param cols columnas numericas a incluir
#' @param scale TRUE para escalar internamente (recomendado si no pasaste z-score)
#' @return list con objeto prcomp, var_exp, scores, loadings
#' @family reduccion_dimensional
#' @export
run_pca <- function(df, cols, scale = TRUE) {
  X <- as.matrix(df[, cols, drop = FALSE])
  fit <- stats::prcomp(X, scale. = scale, center = TRUE)
  var_exp <- fit$sdev^2 / sum(fit$sdev^2)
  scores  <- as.data.frame(fit$x, check.names = FALSE)
  loadings <- as.data.frame(fit$rotation, check.names = FALSE)
  list(model = fit, var_exp = var_exp, scores = scores, loadings = loadings)
}

#' Analisis factorial exploratorio (si psych esta disponible)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param df data.frame
#' @param cols columnas numericas
#' @param nfactors numero de factores
#' @param rotation "promax","oblimin","varimax", etc.
#' @return objeto psych::fa o NULL
#' @family utilidades
#' @export
run_fa <- function(df, cols, nfactors = 2, rotation = "promax") {
  if (!requireNamespace("psych", quietly = TRUE)) {
    message("Package 'psych' no disponible; FA omitido (retornando NULL).")
    return(NULL)
  }
  X <- as.matrix(df[, cols, drop = FALSE])
  psych::fa(X, nfactors = nfactors, rotate = rotation, fm = "minres")
}

#' K-means con seleccion automatica de k por silhouette (si cluster esta disponible)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param df data.frame
#' @param cols columnas numericas (recomendado z-score)
#' @param k_range enteros a evaluar (por defecto 2:8)
#' @param nstart nstart de kmeans
#' @return list con best_k, best_model, silhouette_avg (por k), asignacion
#' @family utilidades
#' @export
cluster_kmeans_auto <- function(df, cols, k_range = 2:8, nstart = 25) {
  X <- as.matrix(df[, cols, drop = FALSE])
  have_cluster <- requireNamespace("cluster", quietly = TRUE)
  sil_avg <- numeric(length(k_range))
  models  <- vector("list", length(k_range))

  for (i in seq_along(k_range)) {
    km <- stats::kmeans(X, centers = k_range[i], nstart = nstart)
    models[[i]] <- km
    if (have_cluster) {
      sil <- tryCatch(cluster::silhouette(km$cluster, dist(X)), error = function(e) NULL)
      sil_avg[i] <- if (!is.null(sil)) mean(sil[, "sil_width"]) else NA_real_
    } else {
      # fallback: usamos -tot.withinss como proxy (mayor es mejor)
      sil_avg[i] <- -km$tot.withinss
    }
  }

  idx <- which.max(sil_avg)
  best <- models[[idx]]
  list(best_k = k_range[idx], best_model = best, silhouette_avg = data.frame(k = k_range, score = sil_avg),
       assignment = best$cluster, centers = best$centers)
}

#' Clustering jerarquico (ward.D2) + corte por k
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param df data.frame
#' @param cols columnas numericas (recomendado z-score)
#' @param k numero de clusters
#' @param method aglomerativo (default "ward.D2")
#' @return list con hclust, cutree, heights
#' @family utilidades
#' @export
cluster_hclust_cut <- function(df, cols, k = 4, method = "ward.D2") {
  X <- as.matrix(df[, cols, drop = FALSE])
  hc <- stats::hclust(stats::dist(X), method = method)
  cut <- stats::cutree(hc, k = k)
  list(hclust = hc, cluster = cut, heights = hc$height)
}

#' Construir indices: polaridad compuesta, consenso, subindices NRC y niveles por percentiles
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param dfx data.frame base
#' @param method_cols columnas de metodos (continuas)
#' @param nrc_cols columnas NRC (ang/ant/.../positive/negative)
#' @param weights named numeric con pesos de metodos (suman 1). Si NULL usa uniforme.
#' @param percentiles vector de probs para cortes, p.ej. c(0.25,0.5,0.75)
#' @return list con data (dfx + columnas agregadas) y metadata (pesos, cortes)
#' @details Requiere que exista la funcion \code{categorizar_por_percentiles}.
#' @family utilidades
#' @export
#' @param 0.5 (auto) TODO: describir parámetro.
#' @param 0.75 (auto) TODO: describir parámetro.
build_emotion_indices <- function(dfx, method_cols, nrc_cols,
                                  weights = NULL,
                                  percentiles = c(0.25, 0.5, 0.75)) {
  df <- dfx
  # pesos
  if (is.null(weights)) {
    weights <- rep(1/length(method_cols), length(method_cols))
    names(weights) <- method_cols
  } else {
    if (abs(sum(weights) - 1) > 1e-8) weights <- weights / sum(weights)
    if (!all(names(weights) %in% method_cols))
      stop("Los nombres de 'weights' deben ser subset de method_cols.")
  }
  # asegurar numericos
  for (nm in method_cols) if (!is.numeric(df[[nm]])) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))

  # indice compuesto (sent_general)
  M <- as.matrix(df[, method_cols, drop = FALSE])
  w <- as.numeric(weights[method_cols])
  sent <- as.numeric(M %*% w)
  df$sent_general <- sent

  # niveles por percentiles del indice
  pc <- categorizar_por_percentiles(df$sent_general, probs = percentiles)
  df$Nivel_sent_general <- pc$categoria

  # niveles por metodo (percentiles independientes por cada escala)
  for (m in method_cols) {
    pc_m <- categorizar_por_percentiles(df[[m]], probs = percentiles)
    df[[paste0("Nivel_", m)]] <- pc_m$categoria
  }

  # consenso (proporcion de metodos con signo concordante con el indice)
  # definimos signo metodo y signo del indice (>=0 positivo)
  sign_idx <- ifelse(df$sent_general >= 0, 1L, -1L)
  signs_m  <- sapply(method_cols, function(m) ifelse(df[[m]] >= 0, 1L, -1L))
  df$consenso_metodos <- rowMeans(t(t(signs_m) == sign_idx)) # proporcion de coincidencia con el indice

  # subindices NRC: valencia_plus, valencia_minus, activacion (definicion basica)
  need <- c("joy","trust","positive","anger","disgust","fear","negative","surprise")
  miss <- setdiff(need, nrc_cols)
  if (length(miss)) {
    # crea columnas faltantes en 0 para no romper
    for (nm in miss) df[[nm]] <- 0
  }
  df$valencia_plus  <- rowMeans(df[, intersect(c("joy","trust","positive"), names(df)), drop = FALSE])
  df$valencia_minus <- rowMeans(df[, intersect(c("anger","disgust","fear","negative"), names(df)), drop = FALSE])
  df$activacion     <- rowMeans(df[, intersect(c("surprise","fear","anger"), names(df)), drop = FALSE])

  list(
    data = df,
    metadata = list(weights = weights, percentiles = percentiles,
                    cortes_indice = pc$breaks)
  )
}

#' Resumen integral de concordancia y reduccion dimensional
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Calcula correlaciones, kappa, alpha, PCA de metodos y de NRC.
#' @param dfx data.frame preparado (usar salida de prep_emotions_data()$data o build_emotion_indices()$data)
#' @param method_cols columnas de metodos
#' @param nrc_cols columnas de NRC
#' @return list con: corr_pearson, corr_spearman, kappa, alpha, pca_methods, pca_nrc
#' @family utilidades
#' @export
summarize_emotion_space <- function(dfx, method_cols, nrc_cols) {
  corr_p <- compute_correlations(dfx, method_cols, "pearson")
  corr_s <- compute_correlations(dfx, method_cols, "spearman")
  # niveles si existen
  level_cols <- paste0("Nivel_", method_cols)
  level_cols <- level_cols[level_cols %in% names(dfx)]
  kap <- if (length(level_cols) >= 2) compute_kappa(dfx, level_cols) else NULL
  # alpha en z-score de metodos
  z <- as.data.frame(scale(dfx[, method_cols, drop = FALSE]))
  alpha <- cronbach_alpha(z, colnames(z))
  # PCA metodos y NRC
  pca_m <- run_pca(z, colnames(z), scale = FALSE)
  pca_n <- run_pca(dfx, nrc_cols, scale = TRUE)
  list(
    corr_pearson = corr_p,
    corr_spearman = corr_s,
    kappa = kap,
    alpha = alpha,
    pca_methods = pca_m,
    pca_nrc = pca_n
  )
}

#' Clustering en espacios de metodos y/o NRC
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param dfx data.frame
#' @param method_cols columnas de metodos (usara z-score)
#' @param nrc_cols columnas NRC (usara estandarizacion interna del algoritmo)
#' @param k_range rango de k para k-means
#' @return list con kmeans_methods, kmeans_nrc, hclust_methods, hclust_nrc
#' @family utilidades
#' @export
cluster_emotion_spaces <- function(dfx, method_cols, nrc_cols, k_range = 2:8) {
  z <- as.data.frame(scale(dfx[, method_cols, drop = FALSE]))
  km_m  <- cluster_kmeans_auto(z, colnames(z), k_range = k_range)
  km_n  <- cluster_kmeans_auto(dfx, nrc_cols, k_range = k_range)
  hc_m  <- cluster_hclust_cut(z, colnames(z), k = km_m$best_k)
  hc_n  <- cluster_hclust_cut(dfx, nrc_cols, k = km_n$best_k)
  list(
    kmeans_methods = km_m,
    kmeans_nrc     = km_n,
    hclust_methods = hc_m,
    hclust_nrc     = hc_n
  )
}

# ============================================================
# EJEMPLO MINIMO DE USO (asumiendo objeto dfx_es ya construido)
# (Pon este bloque en un script de ejemplo o test, no en produccion)
# ============================================================
# if (FALSE) {
#   method_cols <- c("syuzhet","bing","afinn","nrc")
#   nrc_cols <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
#
#   # 1) Preparacion
#   prep <- prep_emotions_data(dfx_es, method_cols, nrc_cols,
#                              id_cols = c("token_orig","token_idioma","token_idioma_unico"))
#
#   # 2) Indices (pesos de ejemplo; ajusta a tu definicion)
#   pesos <- c(syuzhet=.35, bing=.15, afinn=.25, nrc=.25)
#   idx   <- build_emotion_indices(prep$data, method_cols, nrc_cols, weights = pesos,
#                                  percentiles = c(0.25, 0.5, 0.75))
#   dfi <- idx$data
#
#   # 3) Resumen integral
#   sumry <- summarize_emotion_space(dfi, method_cols, nrc_cols)
#
#   # 4) Clustering en espacios
#   clus <- cluster_emotion_spaces(dfi, method_cols, nrc_cols, k_range = 2:8)
#
#   # Accesos rapidos
#   sumry$corr_pearson
#   sumry$kappa$kappa_pairs
#   sumry$alpha$alpha
#   head(sumry$pca_methods$scores)
#   table(clus$kmeans_methods$assignment)
# }

# ============================================================
# PUNTO 1 — Preparacion e higiene de datos + auditoria
# - Estandariza, categoriza por percentiles, arma indice compuesto
# - Genera auditoria (NA, duplicados, rangos, cuantiles, correl)
# - Exporta CSV y graficas diagnosticas
# ============================================================

# ---------- helpers seguros (por si no existen en el proyecto) ----------
safe_write <- function(obj, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (inherits(obj, "data.frame")) {
    suppressWarnings(utils::write.csv(obj, path, row.names = FALSE, fileEncoding = "UTF-8"))
  } else {
    saveRDS(obj, path)
  }
}

safe_save_plot <- function(gg, path, width = 9, height = 6, dpi = 150) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  if (inherits(gg, "ggplot")) {
    ggplot2::ggsave(filename = path, plot = gg, width = width, height = height, dpi = dpi, limitsize = FALSE)
  }
}


# ---------- funciones core si aun no las tienes ----------

#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @export
#' @return (auto) Verificar y completar la descripción del valor retornado.
#' @param dfx (auto) TODO: describir parámetro.
#' @param method_cols (auto) TODO: describir parámetro.
#' @param nrc_cols (auto) TODO: describir parámetro.
#' @param id_cols (auto) TODO: describir parámetro.
  prep_emotions_data <- function(dfx, method_cols, nrc_cols, id_cols = character(0)) {
    stopifnot(is.data.frame(dfx))
    miss_methods <- setdiff(method_cols, names(dfx))
    miss_nrc     <- setdiff(nrc_cols, names(dfx))
    if (length(miss_methods)) stop("Faltan columnas de metodos: ", paste(miss_methods, collapse=", "))
    if (length(miss_nrc))     stop("Faltan columnas NRC: ", paste(miss_nrc, collapse=", "))
    df <- dfx
    for (nm in c(method_cols, nrc_cols)) if (!is.numeric(df[[nm]])) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    methods_raw <- df[, method_cols, drop = FALSE]
    methods_z   <- as.data.frame(scale(methods_raw), check.names = FALSE)
    nrc_mat     <- df[, nrc_cols, drop = FALSE]
    na_rate <- vapply(df[, c(method_cols, nrc_cols), drop = FALSE], function(v) mean(is.na(v)), numeric(1))
    dupe_key <- if ("token_idioma_unico" %in% names(df)) "token_idioma_unico" else NA_character_
    n_dup    <- if (!is.na(dupe_key)) sum(duplicated(df[[dupe_key]])) else 0L
    list(
      data        = df[, unique(c(id_cols, method_cols, nrc_cols)), drop = FALSE],
      methods_raw = methods_raw,
      methods_z   = methods_z,
      nrc_mat     = nrc_mat,
      meta        = list(na_rate = na_rate, duplicate_key = dupe_key, n_duplicates = n_dup)
    )
  }

#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @export
#' @return (auto) Verificar y completar la descripción del valor retornado.
#' @param dfx (auto) TODO: describir parámetro.
#' @param method_cols (auto) TODO: describir parámetro.
#' @param nrc_cols (auto) TODO: describir parámetro.
#' @param weights (auto) TODO: describir parámetro.
#' @param percentiles (auto) TODO: describir parámetro.
#' @param 0.5 (auto) TODO: describir parámetro.
#' @param 0.75 (auto) TODO: describir parámetro.
  build_emotion_indices <- function(dfx, method_cols, nrc_cols, weights = NULL, percentiles = c(0.25,0.5,0.75)) {
    df <- dfx
    if (is.null(weights)) {
      weights <- rep(1/length(method_cols), length(method_cols)); names(weights) <- method_cols
    } else {
      if (abs(sum(weights) - 1) > 1e-8) weights <- weights / sum(weights)
      if (!all(names(weights) %in% method_cols))
        stop("Los nombres de 'weights' deben estar en method_cols.")
    }
    for (nm in method_cols) if (!is.numeric(df[[nm]])) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    M <- as.matrix(df[, method_cols, drop = FALSE]); w <- as.numeric(weights[method_cols])
    df$sent_general <- as.numeric(M %*% w)
    pc <- categorizar_por_percentiles(df$sent_general, probs = percentiles)
    df$Nivel_sent_general <- pc$categoria
    for (m in method_cols) {
      pc_m <- categorizar_por_percentiles(df[[m]], probs = percentiles)
      df[[paste0("Nivel_", m)]] <- pc_m$categoria
    }
    sign_idx <- ifelse(df$sent_general >= 0, 1L, -1L)
    signs_m  <- sapply(method_cols, function(m) ifelse(df[[m]] >= 0, 1L, -1L))
    df$consenso_metodos <- rowMeans(t(t(signs_m) == sign_idx))
    need <- c("joy","trust","positive","anger","disgust","fear","negative","surprise")
    miss <- setdiff(need, nrc_cols)
    for (nm in miss) df[[nm]] <- 0
    df$valencia_plus  <- rowMeans(df[, intersect(c("joy","trust","positive"), names(df)), drop = FALSE])
    df$valencia_minus <- rowMeans(df[, intersect(c("anger","disgust","fear","negative"), names(df)), drop = FALSE])
    df$activacion     <- rowMeans(df[, intersect(c("surprise","fear","anger"), names(df)), drop = FALSE])
    list(data = df,
         metadata = list(weights = weights, percentiles = percentiles, cortes_indice = pc$breaks))
  }


# ---------- graficas diagnosticas (detalladas con leyendas) ----------
# Distribuciones de metodos
# --- FIX 1: Distribuciones por método (usa after_stat) ---
plot_metodos_distrib <- function(df, method_cols) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE),
            requireNamespace("tidyr", quietly = TRUE))
  d <- df[, method_cols, drop = FALSE]
  d$.__id__ <- seq_len(nrow(d))
  long <- tidyr::pivot_longer(d, cols = all_of(method_cols),
                              names_to = "metodo", values_to = "valor")
  ggplot2::ggplot(long, ggplot2::aes(x = valor, fill = metodo)) +
    ggplot2::geom_histogram(alpha = 0.5, bins = 30, position = "identity") +
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(density)), linewidth = 0.6) +
    ggplot2::facet_wrap(~ metodo, scales = "free") +
    ggplot2::labs(title = "Distribución por método",
                  subtitle = "Histogramas y densidades; escalas libres por método",
                  x = "Valor método", y = "Frecuencia / Densidad", fill = "Método") +
    ggplot2::theme_minimal()
}

# --- FIX 2: Índice compuesto con cortes (usa data=breaks, no recicla linetype) ---
# Reemplaza tu función por esta versión a prueba de balas
plot_indice_con_cortes <- function(df, percentiles = c(0.25, 0.5, 0.75)) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))

  # 1) Categorías + cortes
  pc   <- categorizar_por_percentiles(df$sent_general, probs = percentiles)
  brks <- as.numeric(pc$breaks)  # p.ej., c(-Inf, q1, q2, q3, Inf)

  # 2) Tabla de cortes con linetype por fila (misma longitud que 'brks')
  #    Bordes (-Inf, +Inf) = "borde" (dashed); cuantiles intermedios = "cuantil" (solid)
  tipo <- ifelse(seq_along(brks) %in% c(1, length(brks)), "borde", "cuantil")
  df_breaks <- data.frame(
    x    = brks,
    tipo = factor(tipo, levels = c("borde", "cuantil"))
  )

  # 3) Altura para etiquetas (máximo de la densidad)
  dens  <- stats::density(df$sent_general)
  y_top <- max(dens$y, na.rm = TRUE)

  # 4) Gráfico: usa after_stat(density) (sin ..density.. deprecado)
  ggplot2::ggplot(df, ggplot2::aes(x = sent_general)) +
    ggplot2::geom_histogram(bins = 30, fill = "grey70", color = "grey30") +
    ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(density)), linewidth = 0.7) +
    ggplot2::geom_vline(
      data = df_breaks,
      ggplot2::aes(xintercept = x, linetype = tipo),
      linewidth = 0.7, show.legend = TRUE
    ) +
    ggplot2::geom_text(
      data = df_breaks,
      ggplot2::aes(x = x, y = y_top, label = paste0("Corte: ", signif(x, 3))),
      angle = 90, vjust = -0.3, size = 3.2
    ) +
    ggplot2::scale_linetype_manual(
      name   = "Tipo de corte",
      values = c(borde = "dashed", cuantil = "solid")
    ) +
    ggplot2::labs(
      title    = "Índice compuesto (sent_general)",
      subtitle = "Cortes por percentiles (líneas) y distribución",
      x = "sent_general", y = "Frecuencia / Densidad"
    ) +
    ggplot2::theme_minimal()
}


# Mapa de calor de correlaciones
plot_cor_metodos <- function(df, method_cols) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE), requireNamespace("reshape2", quietly = TRUE))
  C <- suppressWarnings(cor(df[, method_cols, drop = FALSE], use = "pairwise.complete.obs"))
  m <- reshape2::melt(C, varnames = c("Var1","Var2"), value.name = "corr")
  ggplot2::ggplot(m, ggplot2::aes(Var1, Var2, fill = corr)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", corr)), color = "black", size = 3) +
    ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0) +
    ggplot2::labs(title = "Correlacion entre metodos", x = NULL, y = NULL, fill = "r") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

# ---------- Funcion principal del Punto 1 ----------
#' Punto 1: Preparacion e higiene + indice y auditoria
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param dfx data.frame con metodos, NRC e identificadores de token.
#' @param method_cols character, p.ej. c("syuzhet","bing","afinn","nrc")
#' @param nrc_cols character, p.ej. c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
#' @param id_cols character, p.ej. c("token_orig","token_idioma","token_idioma_unico")
#' @param weights named numeric con pesos para indice compuesto (suman 1). Si NULL, uniforme.
#' @param percentiles vector de probs para cortes del indice y de cada metodo.
#' @param out_dir carpeta de salida (CSV/Graficos). Por defecto "salidas_pipeline".
#' @return list con data (df_preparado), auditoria, graficos (ggplot)
#' @family utilidades
#' @export
#' @param "bing" (auto) TODO: describir parámetro.
#' @param "afinn" (auto) TODO: describir parámetro.
#' @param "nrc" (auto) TODO: describir parámetro.
estrategia_p1_preparacion <- function(
    dfx,
    method_cols = c("syuzhet","bing","afinn","nrc"),
    nrc_cols = c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive"),
    id_cols = c("token_orig","token_idioma","token_idioma_unico"),
    weights = NULL,
    percentiles = c(0.25, 0.5, 0.75),
    out_dir = "salidas_pipeline"
) {
  stopifnot(is.data.frame(dfx))

  # 1) Preparacion
  prep <- prep_emotions_data(dfx, method_cols, nrc_cols, id_cols)
  df0  <- prep$data

  # 2) Indice compuesto + niveles
  idx  <- build_emotion_indices(df0, method_cols, nrc_cols, weights = weights, percentiles = percentiles)
  dfi  <- idx$data

  # 3) Auditoria
  # Rangos y cuantiles por metodo
  rango <- lapply(method_cols, function(m) range(dfi[[m]], na.rm = TRUE))
  names(rango) <- method_cols
  quants <- lapply(method_cols, function(m) stats::quantile(dfi[[m]], probs = c(.01,.25,.5,.75,.99), na.rm = TRUE))
  names(quants) <- method_cols

  # Correlaciones Pearson
  corr_m <- suppressWarnings(cor(dfi[, method_cols, drop = FALSE], use = "pairwise.complete.obs"))

  # Duplicados por token_idioma_unico y token_orig
  dup_unico <- if ("token_idioma_unico" %in% names(dfi)) sum(duplicated(dfi$token_idioma_unico)) else NA_integer_
  dup_orig  <- if ("token_orig" %in% names(dfi)) sum(duplicated(dfi$token_orig)) else NA_integer_

  auditoria <- list(
    meta      = prep$meta,
    weights   = idx$metadata$weights,
    percentiles = idx$metadata$percentiles,
    cortes_indice = idx$metadata$cortes_indice,
    rango     = rango,
    cuantiles = quants,
    corr_metodos = corr_m,
    duplicados = list(token_idioma_unico = dup_unico, token_orig = dup_orig),
    n_tokens  = nrow(dfi)
  )

  # 4) Graficas
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    g_metodos  <- plot_metodos_distrib(dfi, method_cols)
    g_indice   <- plot_indice_con_cortes(dfi, percentiles = percentiles)
    g_corr     <- plot_cor_metodos(dfi, method_cols)
  } else {
    g_metodos <- g_indice <- g_corr <- NULL
  }

  # 5) Persistencia basica
  safe_write(dfi, file.path(out_dir, "p1_datos_preparados.csv"))
  # Tablas resumen
  safe_write(as.data.frame(prep$meta$na_rate), file.path(out_dir, "p1_na_rate.csv"))
  # Correlaciones
  safe_write(as.data.frame(corr_m), file.path(out_dir, "p1_cor_metodos.csv"))
  # Graficas
  if (!is.null(g_metodos)) safe_save_plot(g_metodos, file.path(out_dir, "p1_distrib_metodos.png"))
  if (!is.null(g_indice))  safe_save_plot(g_indice,  file.path(out_dir, "p1_indice_cortes.png"))
  if (!is.null(g_corr))    safe_save_plot(g_corr,    file.path(out_dir, "p1_cor_metodos_heatmap.png"))

  list(
    data     = dfi,
    auditoria = auditoria,
    graficos = list(distrib_metodos = g_metodos,
                    indice_cortes   = g_indice,
                    cor_metodos     = g_corr)
  )
}

# ==== Utils ====

scale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) return(rep(0.5, length(x)))
  (x - rng[1]) / diff(rng)
}

zscore <- function(x) {
  mu <- mean(x, na.rm = TRUE); sdv <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(sdv) || sdv == 0) return(rep(0, length(x)))
  (x - mu)/sdv
}

# Lin's Concordance Correlation Coefficient (CCC)
ccc_lins <- function(x, y) {
  x <- as.numeric(x); y <- as.numeric(y)
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  vx <- stats::var(x); vy <- stats::var(y)
  r  <- suppressWarnings(stats::cor(x, y, method = "pearson"))
  if (!is.finite(r)) return(NA_real_)
  (2 * r * sqrt(vx) * sqrt(vy)) / (vx + vy + (mx - my)^2)
}

# Bland–Altman (datos + limites)
bland_altman_df <- function(a, b) {
  a <- as.numeric(a); b <- as.numeric(b)
  ok <- is.finite(a) & is.finite(b)
  a <- a[ok]; b <- b[ok]
  m <- (a + b)/2
  d <- a - b
  md <- mean(d); sdv <- stats::sd(d)
  lim_low  <- md - 1.96*sdv
  lim_high <- md + 1.96*sdv
  list(
    data = data.frame(mean = m, diff = d),
    md = md, lim_low = lim_low, lim_high = lim_high
  )
}

# ---- Helpers de anotaciones y estadísticos ----
# sig_stars <- function(p) {
#   if (is.na(p)) return("")
#   if (p < 0.001) "***" else if (p < 0.01) "**" else if (p < 0.05) "*" else ""
# }

pairwise_cor_info <- function(df, cols, method = "pearson") {
  cols <- cols[cols %in% names(df)]
  comb <- t(combn(cols, 2))
  out <- lapply(seq_len(nrow(comb)), function(i){
    a <- comb[i,1]; b <- comb[i,2]
    x <- df[[a]]; y <- df[[b]]
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    n <- length(x)
    if (n < 3) return(data.frame(a=a,b=b,r=NA,p=NA,n=n, ccc=NA))
    ct <- suppressWarnings(stats::cor.test(x, y, method = method))
    data.frame(a=a,b=b, r=unname(ct$estimate), p=ct$p.value, n=n, ccc=ccc_lins(x,y))
  })
  do.call(rbind, out)
}

lm_eqn <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(list(eq="y = a + b x", r2=NA))
  m <- lm(y ~ x)
  a <- coef(m)[1]; b <- coef(m)[2]
  r2 <- summary(m)$r.squared
  list(eq = sprintf("y = %.3f %s %.3f x", a, ifelse(b>=0,"+","-"), abs(b)), r2 = r2)
}

group_counts <- function(df, group_col) {
  as.data.frame(table(df[[group_col]]), stringsAsFactors = FALSE) |>
    stats::setNames(c(group_col, "n"))
}

# --- Helpers seguros (añadir/actualizar) ---
`%||%` <- function(x, y) if (is.null(x) || (length(x)==1 && is.na(x))) y else x

# Vectorizado: acepta escalar o vector de p-values
sig_stars <- function(p) {
  p <- as.numeric(p)
  out <- character(length(p))
  out[!is.na(p) & p < 0.001] <- "***"
  out[!is.na(p) & p >= 0.001 & p < 0.01] <- "**"
  out[!is.na(p) & p >= 0.01  & p < 0.05] <- "*"
  out[is.na(p)] <- ""
  out
}




diag_cor_metodos <- function(df, method_cols, method = c("pearson","spearman","kendall")) {
  method <- match.arg(method)
  M <- as.matrix(df[, method_cols, drop = FALSE])
  ok <- apply(M, 2, function(x) is.numeric(x))
  M <- M[, ok, drop = FALSE]
  if (ncol(M) < 2) stop("Se requieren >=2 columnas numericas en method_cols.")
  suppressWarnings(stats::cor(M, use = "pairwise.complete.obs", method = method))
}

# --- Heatmap de correlaciones con r, p, N y estrellas (reemplaza tu versión) ---
plot_cor_heatmap <- function(df, method_cols, method = "pearson") {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  R <- diag_cor_metodos(df, method_cols, method = method)

  # tabla r, p, n (pareja a pareja)
  info <- pairwise_cor_info(df, method_cols, method = method)

  # diagonal (r=1, p=0, n=NA)
  diag_df <- data.frame(a = method_cols, b = method_cols, r = 1, p = 0, n = NA_real_, ccc = NA_real_)

  info_full <- rbind(info, diag_df)

  # grid completo Var1 x Var2
  grid <- expand.grid(Var1 = method_cols, Var2 = method_cols, stringsAsFactors = FALSE)
  grid <- merge(grid, transform(info_full, Var1 = a, Var2 = b),
                by = c("Var1","Var2"), all.x = TRUE, sort = FALSE)

  # ordenar para una matriz bien presentada
  grid$Var1 <- factor(grid$Var1, levels = method_cols)
  grid$Var2 <- factor(grid$Var2, levels = method_cols)
  grid <- grid[order(grid$Var1, grid$Var2), ]

  # etiquetas vectorizadas
  stars <- sig_stars(grid$p)
  n_lab <- ifelse(is.na(grid$n), "–", as.character(grid$n))
  r_lab <- ifelse(is.na(grid$r), "", sprintf("r=%.2f", grid$r))
  grid$label <- ifelse(r_lab == "", "",
                       sprintf("%s%s\nN=%s", r_lab, stars, n_lab))

  ggplot2::ggplot(grid, ggplot2::aes(Var1, Var2, fill = r)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label), size = 3, lineheight = 0.95) +
    ggplot2::scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                                  midpoint = 0, na.value = "grey90", limits = c(-1,1)) +
    ggplot2::labs(
      title = sprintf("Mapa de calor de correlaciones (%s)", method),
      subtitle = "Etiqueta: r + estrellas(p) y N pareado por celda  (*** <.001, ** <.01, * <.05)",
      x = NULL, y = NULL, fill = "r"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
# ---------- Helpers seguros ----------
zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

ccc_lins <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NA_real_)
  mx <- mean(x); my <- mean(y)
  vx <- stats::var(x); vy <- stats::var(y)
  sxy <- stats::cov(x, y)
  (2 * sxy) / (vx + vy + (mx - my)^2)
}

plot_pairs_simple <- function(df, method_cols) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  Z <- as.data.frame(lapply(df[, method_cols, drop = FALSE], zscore))
  comb <- t(combn(method_cols, 2))
  lapply(seq_len(nrow(comb)), function(i){
    a <- comb[i,1]; b <- comb[i,2]
    x <- Z[[a]]; y <- Z[[b]]

    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 3) return(NULL)

    m <- lm(y ~ x)
    a0 <- unname(coef(m)[1]); b1 <- unname(coef(m)[2])
    r  <- suppressWarnings(stats::cor(x, y, method = "pearson"))
    r2 <- summary(m)$r.squared
    ccc <- ccc_lins(df[[a]][ok], df[[b]][ok])

    subt <- sprintf("r = %.3f | R² = %.3f | CCC = %.3f | y = %.3f + %.3fx",
                    r, r2, ccc, a0, b1)

    ggplot2::ggplot(data.frame(x, y), ggplot2::aes(x = x, y = y)) +
      ggplot2::stat_density_2d_filled(alpha = 0.35, show.legend = FALSE) +
      ggplot2::geom_point(alpha = 0.55, size = 1.6) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.9) +
      ggplot2::labs(
        title = sprintf("Comparación z-%s vs z-%s", a, b),
        subtitle = subt,
        x = paste0("z-", a),
        y = paste0("z-", b)
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        panel.grid.minor = ggplot2::element_blank()
      )
  })
}

make_corr_heatmap <- function(M, title = "Correlación (Pearson)") {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  dfm <- as.data.frame(as.table(M))
  names(dfm) <- c("Var1","Var2","value")
  ggplot2::ggplot(dfm, ggplot2::aes(Var1, Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", value)), size = 3) +
    ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
    ggplot2::labs(title = title, x = NULL, y = NULL, fill = "r") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    )
}

make_bland_altman <- function(x, y, a = "A", b = "B") {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]; y <- y[ok]
  if (length(x) < 3) return(NULL)
  m <- (x + y) / 2
  d <- y - x
  md <- mean(d); sd_d <- stats::sd(d)
  loA <- md - 1.96 * sd_d
  upA <- md + 1.96 * sd_d
  subt <- sprintf("Mean diff = %.3f | LoA = [%.3f, %.3f]", md, loA, upA)

  ggplot2::ggplot(data.frame(m, d), ggplot2::aes(m, d)) +
    ggplot2::geom_hline(yintercept = md, linetype = 1) +
    ggplot2::geom_hline(yintercept = c(loA, upA), linetype = 2) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::labs(
      title = sprintf("Bland–Altman: %s vs %s", a, b),
      subtitle = subt,
      x = "Media de métodos",
      y = sprintf("%s - %s", b, a)
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

# ---------- FUNCIÓN PRINCIPAL (patch) ----------
estrategia_p2_metodos <- function(
    df,
    method_cols = c("syuzhet","bing","afinn","nrc"),
    nivel_col   = "Nivel_sent_general",
    out_dir     = NULL
){
  stopifnot(is.data.frame(df))
  method_cols <- intersect(method_cols, names(df))
  if (length(method_cols) < 2) stop("Se requieren ≥2 columnas de métodos.")

  # Matrices de correlación (evita NAs)
  M <- as.matrix(df[, method_cols, drop = FALSE])
  M <- apply(M, 2, function(v) as.numeric(v))
  cor_p  <- suppressWarnings(stats::cor(M, use = "pairwise.complete.obs", method = "pearson"))
  cor_s  <- suppressWarnings(stats::cor(M, use = "pairwise.complete.obs", method = "spearman"))

  # Concordancia par-a-par
  comb <- t(combn(method_cols, 2))
  ccc_tab <- lapply(seq_len(nrow(comb)), function(i){
    a <- comb[i,1]; b <- comb[i,2]
    data.frame(
      metodo_a = a,
      metodo_b = b,
      ccc = ccc_lins(df[[a]], df[[b]]),
      stringsAsFactors = FALSE
    )
  })
  ccc_tab <- do.call(rbind, ccc_tab)

  # Gráficos
  g_cor_p <- make_corr_heatmap(cor_p, "Correlación (Pearson)")
  g_cor_s <- make_corr_heatmap(cor_s, "Correlación (Spearman)")

  # Distribuciones por nivel (si existe)
  g_box <- g_violin <- NULL
  if (!is.null(nivel_col) && nivel_col %in% names(df)) {
    long <- utils::stack(df[method_cols])
    long$Nivel <- df[[nivel_col]]
    names(long) <- c("valor","metodo","Nivel")

    g_box <- ggplot2::ggplot(long, ggplot2::aes(x = Nivel, y = valor, fill = metodo)) +
      ggplot2::geom_boxplot(alpha = 0.6, outlier.alpha = 0.25) +
      ggplot2::facet_wrap(~ metodo, scales = "free_y") +
      ggplot2::labs(title = "Distribución por nivel de sentimiento", x = "Nivel", y = "Valor") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))

    g_violin <- ggplot2::ggplot(long, ggplot2::aes(x = Nivel, y = valor, fill = metodo)) +
      ggplot2::geom_violin(alpha = 0.5, trim = FALSE) +
      ggplot2::geom_boxplot(width = 0.1, outlier.shape = NA, alpha = 0.5) +
      ggplot2::facet_wrap(~ metodo, scales = "free_y") +
      ggplot2::labs(title = "Violin + Box por nivel", x = "Nivel", y = "Valor") +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1))
  }

  # Pares (z) y Bland–Altman
  g_pairs  <- plot_pairs_simple(df, method_cols)
  g_ba_list <- lapply(seq_len(nrow(comb)), function(i){
    a <- comb[i,1]; b <- comb[i,2]
    make_bland_altman(df[[a]], df[[b]], a = a, b = b)
  })

  # Guardado opcional
  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ggplot2::ggsave(file.path(out_dir, "p2_cor_pearson.png"), g_cor_p, width = 6.5, height = 5.2, dpi = 150)
    ggplot2::ggsave(file.path(out_dir, "p2_cor_spearman.png"), g_cor_s, width = 6.5, height = 5.2, dpi = 150)
    if (!is.null(g_box))    ggplot2::ggsave(file.path(out_dir, "p2_box_por_nivel.png"), g_box, width = 7.5, height = 5.2, dpi = 150)
    if (!is.null(g_violin)) ggplot2::ggsave(file.path(out_dir, "p2_violin_por_nivel.png"), g_violin, width = 7.5, height = 5.2, dpi = 150)
    # Guarda los 1ros 3 pares y 3 BA para no saturar
    for (i in seq_len(min(3, length(g_pairs)))) if (!is.null(g_pairs[[i]]))
      ggplot2::ggsave(file.path(out_dir, sprintf("p2_pairs_z_%02d.png", i)), g_pairs[[i]], width = 6.2, height = 5.2, dpi = 150)
    for (i in seq_len(min(3, length(g_ba_list)))) if (!is.null(g_ba_list[[i]]))
      ggplot2::ggsave(file.path(out_dir, sprintf("p2_bland_altman_%02d.png", i)), g_ba_list[[i]], width = 6.2, height = 5.2, dpi = 150)
  }

  list(
    tablas = list(
      cor_pearson = cor_p,
      cor_spearman = cor_s,
      ccc_pares = ccc_tab
    ),
    graficos = list(
      cor_pearson = g_cor_p,
      cor_spearman = g_cor_s,
      box_por_nivel = g_box,
      violin_por_nivel = g_violin,
      pairs_z = g_pairs,
      bland_altman = g_ba_list
    )
  )
}


plot_bland_altman <- function(df, col_a, col_b) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  ba <- bland_altman_df(df[[col_a]], df[[col_b]])
  dentro <- mean(ba$data$diff >= ba$lim_low & ba$data$diff <= ba$lim_high) * 100
  subt <- sprintf("Media dif = %.3f | LI=%.3f, LS=%.3f | %% dentro=%.1f%%",
                  ba$md, ba$lim_low, ba$lim_high, dentro)

  ggplot2::ggplot(ba$data, ggplot2::aes(x = mean, y = diff)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.6) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ba$lim_low, ymax = ba$lim_high),
                         fill = "grey85", alpha = 0.5) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::geom_hline(yintercept = ba$md, linetype = "solid") +
    ggplot2::geom_hline(yintercept = ba$lim_low, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = ba$lim_high, linetype = "dashed") +
    ggplot2::labs(
      title = sprintf("Bland–Altman: %s vs %s", col_a, col_b),
      subtitle = subt,
      x = "Media de los dos metodos", y = sprintf("Diferencia (%s - %s)", col_a, col_b)
    ) +
    ggplot2::theme_minimal()
}

ba_todos_los_pares <- function(df, method_cols) {
  comb <- t(combn(method_cols, 2))
  lapply(seq_len(nrow(comb)), function(i){
    a <- comb[i,1]; b <- comb[i,2]
    list(pair = c(a,b),
         plot = plot_bland_altman(df, a, b),
         ccc  = ccc_lins(df[[a]], df[[b]]))
  })
}

plot_metodos_por_nivel <- function(df, method_cols, nivel_col = "Nivel_sent_general", tipo = c("box","violin")) {
  tipo <- match.arg(tipo)
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  long <- tidyr::pivot_longer(df, cols = all_of(method_cols), names_to = "metodo", values_to = "valor")
  n_tab <- group_counts(long, nivel_col)

  p <- ggplot2::ggplot(long, ggplot2::aes_string(x = nivel_col, y = "valor", fill = "metodo"))
  if (tipo == "box") {
    p <- p + ggplot2::geom_boxplot(outlier.alpha = 0.25, position = ggplot2::position_dodge2(preserve = "single"))
  } else {
    p <- p + ggplot2::geom_violin(trim = TRUE, alpha = 0.7, position = ggplot2::position_dodge(width = 0.8))
  }

  p +
    ggplot2::stat_summary(fun = median, geom = "point", shape = 21, color = "black",
                          position = ggplot2::position_dodge(width = 0.8), size = 1.8) +
    ggplot2::stat_summary(fun = mean, geom = "point", shape = 23, color = "black",
                          position = ggplot2::position_dodge(width = 0.8), size = 1.8, fill = "white") +
    ggplot2::geom_text(
      data = aggregate(valor ~ get(nivel_col), long, mean),
      ggplot2::aes_string(x = "get(nivel_col)", y = Inf,
                          label = sprintf("N==%s", n_tab$n)),
      inherit.aes = FALSE, vjust = 1.25, parse = TRUE, size = 3.5
    ) +
    ggplot2::labs(title = "Distribucion por nivel de sent_general",
                  subtitle = "Puntos: ● mediana (negro), ◆ media (blanco); N por nivel en el encabezado",
                  x = "Nivel_sent_general", y = "Valor metodo") +
    ggplot2::theme_minimal()
}


#####. P3 del Análisis

#' Calibrar métodos de sentimiento contra una referencia
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Ajusta cada método usando regresión robusta (Theil–Sen) o lineal simple
#' para escalarlo a la misma métrica que el método de referencia.
#'
#' @param df Data.frame con columnas de métodos de sentimiento.
#' @param method_cols Vector de nombres de columnas numéricas a calibrar.
#' @param ref Nombre del método referencia.
#' @param robust Logical; si TRUE usa Theil–Sen (resistente a outliers).
#'
#' @return Lista con:
#'   \item{data}{data.frame con columnas calibradas}
#'   \item{param}{tabla de coeficientes por método (a0,b1,R2,CCC)}
#' @family utilidades
#' @export
calibrar_metodo_referencia <- function(df, method_cols, ref = "syuzhet", robust = TRUE) {
  stopifnot(ref %in% method_cols)
  refv <- df[[ref]]
  res <- list()
  tab <- data.frame(metodo = character(), a0 = numeric(), b1 = numeric(),
                    R2 = numeric(), CCC = numeric(), stringsAsFactors = FALSE)
  for (m in method_cols) {
    if (m == ref) next
    y <- df[[m]]; ok <- is.finite(y) & is.finite(refv)
    if (robust && requireNamespace("mblm", quietly = TRUE)) {
      fit <- mblm::mblm(y ~ refv, repeated = FALSE)
    } else fit <- lm(y ~ refv)
    a0 <- coef(fit)[1]; b1 <- coef(fit)[2]
    y_pred <- a0 + b1 * refv
    r2 <- summary(lm(y ~ refv))$r.squared
    ccc <- (2 * cov(refv, y, use = "pairwise.complete.obs")) /
      (var(refv, na.rm = TRUE) + var(y, na.rm = TRUE) + (mean(refv, na.rm = TRUE) - mean(y, na.rm = TRUE))^2)
    df[[m]] <- (y - a0) / b1
    tab <- rbind(tab, data.frame(metodo = m, a0 = a0, b1 = b1, R2 = r2, CCC = ccc))
  }
  res$data <- df
  res$param <- tab
  res
}

#' Ensamble ponderado de métodos calibrados
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Combina métodos de sentimiento calibrados ponderando por su correlación
#' promedio con los demás (peso = mean(|r|) / sum).
#'
#' @param df Data.frame con columnas numéricas (métodos calibrados).
#' @param method_cols Vector de nombres de columnas a combinar.
#'
#' @return Lista con:
#'   \item{indice}{vector de puntajes combinados}
#'   \item{pesos}{data.frame con pesos usados}
#'   \item{alpha}{fiabilidad interna (alfa Cronbach)}
#' @family utilidades
#' @export
calcular_ensamble_ponderado <- function(df, method_cols) {
  M <- as.matrix(df[, method_cols, drop = FALSE])
  cor_m <- suppressWarnings(cor(M, use = "pairwise.complete.obs"))
  w <- rowMeans(abs(cor_m))
  w <- w / sum(w, na.rm = TRUE)
  indice <- as.vector(M %*% w)
  # alfa de Cronbach
  k <- ncol(M)
  v_total <- var(rowMeans(M, na.rm = TRUE))
  v_medios <- mean(diag(var(M, na.rm = TRUE)))
  alpha <- (k / (k - 1)) * (1 - v_medios / v_total)
  list(indice = indice, pesos = data.frame(metodo = method_cols, peso = w),
       alpha = alpha)
}

# ---- helpers ----
.fit_calibracion <- function(refv, y, robust = c("auto","mblm","rq","rlm","lm"), n_switch = 5000) {
  robust <- match.arg(robust)
  n <- length(y)

  # limpieza y checks
  ok <- is.finite(refv) & is.finite(y)
  refv <- refv[ok]; y <- y[ok]
  if (length(y) < 3L || sd(refv) == 0 || sd(y) == 0) {
    return(list(a0 = NA_real_, b1 = NA_real_, R2 = NA_real_))
  }

  use_method <- robust
  if (robust == "auto") use_method <- if (length(y) > n_switch) "rq" else "mblm"

  a0 <- b1 <- R2 <- NA_real_

  if (use_method == "mblm") {
    fit <- mblm::mblm(y ~ refv, repeated = FALSE)
    a0  <- unname(coef(fit)[1]); b1 <- unname(coef(fit)[2])
    R2  <- max(0, 1 - sum(resid(fit)^2)/sum((y - mean(y))^2))
  } else if (use_method == "rq") {
    fit <- quantreg::rq(y ~ refv, tau = 0.5)
    cf  <- coef(fit); a0 <- unname(cf[1]); b1 <- unname(cf[2])
    R2  <- tryCatch({
      1 - fit$rho / quantreg::rq(y ~ 1, tau = 0.5)$rho
    }, error = function(e) NA_real_)
  } else if (use_method == "rlm") {
    fit <- MASS::rlm(y ~ refv, psi = MASS::psi.huber, maxit = 20)
    a0  <- unname(coef(fit)[1]); b1 <- unname(coef(fit)[2])
    R2  <- max(0, 1 - sum(resid(fit)^2)/sum((y - mean(y))^2))
  } else { # lm
    fit <- stats::lm(y ~ refv)
    a0  <- unname(coef(fit)[1]); b1 <- unname(coef(fit)[2])
    R2  <- summary(fit)$r.squared
  }

  list(a0 = a0, b1 = b1, R2 = R2)
}

#' Ensamble robusto de métodos de sentimiento sobre una escala de referencia
#'
#' @description
#' Calibra métodos \code{method_cols} hacia la escala del método \code{ref}
#' mediante regresión robusta (\code{rq} si \code{robust="rq"} o si \code{n >= n_switch},
#' \code{rlm} si \code{robust="rlm"}, \code{mblm} si \code{robust="mblm"}).
#' Proyecta cada método a la escala del ref y calcula un índice compuesto ponderando
#' por la concordancia de Lin (CCC). Devuelve tabla con resultados, calibración, pesos,
#' y gráficos diagnósticos (opcionalmente muestreados para ser livianos).
#'
#' @param df data.frame con columnas de métodos numéricas y (opcional) identificadores.
#' @param method_cols character, nombres de columnas de métodos (incluye \code{ref}).
#' @param ref character, nombre del método de referencia (debe estar en \code{method_cols}).
#' @param out_dir character, carpeta para guardar PNG (si existe); si \code{NULL}, no guarda.
#' @param robust "auto"|"rq"|"rlm"|"mblm". En "auto": usa "rq" si \code{n >= n_switch}, si no "rlm".
#' @param n_switch integer, umbral de n para usar \code{rq} en modo "auto".
#' @param sample_for_plots integer o \code{NULL}. Si no \code{NULL}, tamaño de muestra para gráficos.
#' @param seed integer o \code{NULL}. Semilla para muestrear gráficos.
#' @param percentiles numeric en \code{[0,1]}, p.ej. \code{c(0.25,0.5,0.75)} para categorizar el ensamble.
#'
#' @return list con:
#' \itemize{
#'   \item \code{data}: \code{df} con columnas \code{sent_compuesto} y \code{Nivel_sent_compuesto}.
#'   \item \code{calibracion}: data.frame con \code{metodo}, \code{a0}, \code{b1}, \code{R2}, \code{CCC}.
#'   \item \code{pesos}: data.frame con \code{metodo}, \code{peso} (normalizados a 1).
#'   \item \code{alpha}: intercepto global del ensamble (0; se absorbe por-método en proyección).
#'   \item \code{graficos}: lista de ggplots (\code{pesos}, \code{densidad_ensamble}, \code{scatter_ref_vs_metodo}).
#'   \item \code{auditoria}: lista con \code{n}, \code{robust_final}, \code{muestra_graficos}, \code{tiempos}.
#' }
#'
#' @importFrom stats lm coef predict quantile
#' @family utilidades
#' @export
#' @param "rq" (auto) TODO: describir parámetro.
#' @param "rlm" (auto) TODO: describir parámetro.
#' @param "mblm" (auto) TODO: describir parámetro.
estrategia_p3_ensamble <- function(
    df,
    method_cols,
    ref,
    out_dir = NULL,
    robust = c("auto","rq","rlm","mblm"),
    n_switch = 1000L,
    sample_for_plots = 2000L,
    seed = 1L,
    percentiles = c(0.25, 0.5, 0.75)
){
  t0 <- proc.time()

  robust <- match.arg(robust)
  stopifnot(is.data.frame(df))
  stopifnot(length(method_cols) >= 2L, ref %in% method_cols)
  for (m in method_cols) if (!is.numeric(df[[m]])) stop("Columna no numérica: ", m)

  ccc_lin <- function(x, y) {
    x <- as.numeric(x); y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 2L) return(NA_real_)
    mx <- mean(x); my <- mean(y)
    vx <- var(x);  vy <- var(y)
    sxy <- stats::cov(x, y)
    denom <- vx + vy + (mx - my)^2
    if (!is.finite(denom) || denom == 0) return(NA_real_)
    (2 * sxy) / denom
  }

  r2_pred <- function(y, yhat) {
    ok <- is.finite(y) & is.finite(yhat)
    y <- y[ok]; yhat <- yhat[ok]
    if (length(y) < 2L) return(NA_real_)
    1 - sum((y - yhat)^2) / sum((y - mean(y))^2)
  }

  categorizar_por_percentiles_full <- function(
    x,
    probs = c(0.25, 0.5, 0.75),
    labels = NULL,
    include_lowest = TRUE,
    right = FALSE
  ){
    n <- length(x)
    xf <- x[is.finite(x)]
    if (length(xf) < 2L) {
      return(list(
        categoria = factor(rep("SinDato", n), levels = "SinDato"),
        breaks = NA_real_, probs_usados = numeric(0)
      ))
    }
    fallback_grid <- list(
      c(0.25,0.5,0.75),
      c(0.33,0.66),
      0.5,
      numeric(0)
    )
    probs_used <- NULL; brks <- NULL
    for (pg in fallback_grid) {
      qv <- if (length(pg)) unique(stats::quantile(xf, probs = pg, na.rm = TRUE, type = 7)) else numeric(0)
      brks_try <- sort(unique(c(min(xf), qv, max(xf))))
      if (length(brks_try) >= 2L) { probs_used <- pg; brks <- brks_try; break }
    }
    n_intervals <- length(brks) - 1L
    if (is.null(labels) || length(labels) != n_intervals)
      labels <- paste0("N-", seq_len(n_intervals) - 1L)
    cat_x <- cut(x, breaks = brks, labels = labels, include_lowest = include_lowest, right = right)
    if (anyNA(cat_x)) {
      lv <- c("SinDato", levels(cat_x))
      cat_chr <- as.character(cat_x)
      cat_chr[!is.finite(x)] <- "SinDato"
      cat_x <- factor(cat_chr, levels = lv)
    }
    list(categoria = cat_x, breaks = brks, probs_usados = probs_used)
  }

  n <- nrow(df)
  robust_final <- switch(match.arg(robust),
                         auto = if (n >= n_switch) "rq" else "rlm",
                         rq   = "rq",
                         rlm  = "rlm",
                         mblm = "mblm"
  )

  fit_model <- function(y, x, engine) {
    ok <- is.finite(y) & is.finite(x)
    y <- y[ok]; x <- x[ok]
    if (length(y) < 3L) return(list(a0 = NA_real_, b1 = NA_real_, pred = rep(NA_real_, length(x)), R2 = NA_real_))
    if (engine == "rq"  && !requireNamespace("quantreg", quietly = TRUE)) engine <- "rlm"
    if (engine == "rlm" && !requireNamespace("MASS", quietly = TRUE))      engine <- "mblm"
    if (engine == "mblm"&& !requireNamespace("mblm", quietly = TRUE))      engine <- "lm"

    if (engine == "rq") {
      mod <- quantreg::rq(y ~ x, tau = 0.5)
    } else if (engine == "rlm") {
      mod <- MASS::rlm(y ~ x, maxit = 50)
    } else if (engine == "mblm") {
      mod <- mblm::mblm(y ~ x, repeated = FALSE)
    } else {
      mod <- stats::lm(y ~ x)
    }
    co   <- stats::coef(mod); a0 <- unname(co[1]); b1 <- unname(co[2])
    yhat <- a0 + b1 * x
    list(a0 = a0, b1 = b1, pred = yhat, R2 = r2_pred(y, yhat))
  }

  y_ref <- df[[ref]]
  calib <- vector("list", length(method_cols)); names(calib) <- method_cols
  ccc_vals <- setNames(rep(NA_real_, length(method_cols)), method_cols); ccc_vals[ref] <- 1

  for (m in method_cols) {
    x_m <- df[[m]]
    if (m == ref) { calib[[m]] <- list(a0 = 0, b1 = 1, R2 = 1, CCC = 1); next }
    ccc_vals[m] <- ccc_lin(y_ref, x_m)
    fm <- fit_model(y = y_ref, x = x_m, engine = robust_final)
    calib[[m]] <- list(a0 = fm$a0, b1 = fm$b1, R2 = fm$R2, CCC = ccc_vals[m])
  }

  preds <- setNames(vector("list", length(method_cols)), method_cols)
  for (m in method_cols) {
    a0 <- calib[[m]]$a0; b1 <- calib[[m]]$b1
    preds[[m]] <- a0 + b1 * df[[m]]
  }

  w_raw <- ccc_vals
  w_raw[!is.finite(w_raw)] <- 0
  w_raw[w_raw < 0] <- 0
  if (sum(w_raw) == 0) w_raw[ref] <- 1
  pesos <- w_raw / sum(w_raw)

  mat_pred <- do.call(cbind, preds)
  sent_compuesto <- as.numeric(mat_pred %*% as.numeric(pesos[colnames(mat_pred)]))
  alpha <- 0

  cat_out <- categorizar_por_percentiles_full(sent_compuesto, probs = percentiles)
  df$sent_compuesto <- sent_compuesto
  df$Nivel_sent_compuesto <- cat_out$categoria

  calibracion <- data.frame(
    metodo = method_cols,
    a0     = vapply(method_cols, function(m) calib[[m]]$a0, numeric(1)),
    b1     = vapply(method_cols, function(m) calib[[m]]$b1, numeric(1)),
    R2     = vapply(method_cols, function(m) calib[[m]]$R2, numeric(1)),
    CCC    = vapply(method_cols, function(m) calib[[m]]$CCC, numeric(1)),
    row.names = NULL
  )
  pesos_df <- data.frame(metodo = names(pesos), peso = as.numeric(pesos), row.names = NULL)

  plot_df <- df
  if (is.numeric(sample_for_plots) && nrow(plot_df) > sample_for_plots) {
    if (!is.null(seed)) set.seed(seed)
    idx <- sample.int(nrow(plot_df), sample_for_plots)
    plot_df <- plot_df[idx, , drop = FALSE]
  }

  plots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    library(ggplot2)

    g_pesos <- ggplot2::ggplot(pesos_df, ggplot2::aes(x = reorder(metodo, -peso), y = peso, fill = metodo)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.3f", peso)), vjust = -0.3, size = 3) +
      ggplot2::labs(title = "Pesos del ensamble por CCC (normalizados a 1)",
                    x = "Método", y = "Peso (CCC normalizado)") +
      ggplot2::guides(fill = "none") +
      ggplot2::theme_minimal()

    g_dens <- ggplot2::ggplot(plot_df, ggplot2::aes(x = sent_compuesto)) +
      ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = 40, alpha = 0.2) +
      ggplot2::geom_density(linewidth = 0.9) +
      ggplot2::geom_vline(xintercept = cat_out$breaks,
                          linetype = c("dashed","solid","dashed","solid")[seq_along(cat_out$breaks)],
                          linewidth = 0.6) +
      ggplot2::annotate("text", x = cat_out$breaks, y = Inf, vjust = 1.5,
                        label = c("min", paste0("p=", cat_out$probs_usados), "max")[seq_along(cat_out$breaks)],
                        size = 3) +
      ggplot2::labs(title = "Distribución del sentimiento compuesto",
                    x = "Sentimiento compuesto (escala ref)", y = "Densidad") +
      ggplot2::theme_minimal()

    g_sc_list <- vector("list", length(method_cols))
    names(g_sc_list) <- method_cols
    for (m in method_cols) {
      g_sc_list[[m]] <- ggplot2::ggplot(plot_df, ggplot2::aes_string(x = m, y = ref)) +  # <— usa aes_string
        ggplot2::geom_point(alpha = 0.5) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dotted") +
        ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
        ggplot2::labs(title = sprintf("Ref (%s) vs %s", ref, m), x = m, y = ref) +
        ggplot2::theme_minimal()
    }

    plots <- list(
      pesos = g_pesos,
      densidad_ensamble = g_dens,
      scatter_ref_vs_metodo = g_sc_list
    )

    if (!is.null(out_dir)) {
      if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      ggplot2::ggsave(file.path(out_dir, "p3_pesos.png"), g_pesos, width = 7, height = 4, dpi = 150)
      ggplot2::ggsave(file.path(out_dir, "p3_densidad_ensamble.png"), g_dens, width = 7, height = 4, dpi = 150)
      for (m in method_cols) {
        ggplot2::ggsave(file.path(out_dir, sprintf("p3_scatter_%s_vs_%s.png", ref, m)),
                        plots$scatter_ref_vs_metodo[[m]], width = 6, height = 5, dpi = 150)
      }
    }
  } else {
    warning("ggplot2 no disponible; no se generan gráficos.")
  }

  t1 <- proc.time()
  auditoria <- list(
    n = n,
    robust_final = robust_final,
    muestra_graficos = nrow(plot_df),
    tiempos = (t1 - t0)[3]
  )

  list(
    data = df,
    calibracion = calibracion,
    pesos = pesos_df,
    alpha = alpha,
    graficos = plots,
    auditoria = auditoria
  )
}

##### Análisis nivel P4...

# ========================= PUNTO 4: PCA + CLUSTERING ========================= #

#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Estrategia P4: PCA y Clústeres de Emociones
#' @description
#' Ejecuta un análisis factorial (PCA) y un agrupamiento (k-means jerárquico)
#' sobre los métodos calibrados y emociones NRC. Genera gráficos detallados
#' con colores, leyendas y etiquetas informativas.
#'
#' @param df Data.frame proveniente de P3 (contiene sent_compuesto y columnas NRC).
#' @param method_cols Vector de columnas numéricas de métodos calibrados.
#' @param nrc_cols Vector de columnas NRC (emociones: anger, joy, fear, etc.).
#' @param id_col Nombre de columna identificadora de tokens (e.g. "token_idioma_unico").
#' @param k_clust Número de clústeres a probar (default = 4).
#' @param out_dir Carpeta de salida opcional.
#'
#' @return list con:
#'   \item{pca}{objeto prcomp}
#'   \item{clusters}{data.frame con tokens y su clúster}
#'   \item{graficos}{lista de gráficos}
#'   \item{tablas}{cargas, varianza, silhouette, centroides}
#' @family reduccion_dimensional
#' @export
estrategia_p4_pca_cluster <- function(df, method_cols, nrc_cols, id_col = "token_idioma_unico",
                                      k_clust = 4, out_dir = NULL) {
  stopifnot(requireNamespace("ggplot2"), requireNamespace("ggrepel"), requireNamespace("scales"))

  # --- Preparar matriz ----
  vars <- c(method_cols, nrc_cols)
  X <- df[, vars]
  X <- as.data.frame(lapply(X, as.numeric))
  X <- X[, apply(X, 2, sd, na.rm = TRUE) > 0, drop = FALSE]
  X <- scale(X)
  pca <- prcomp(X, center = FALSE, scale. = FALSE)

  # --- Varianza explicada ---
  var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
  scree <- data.frame(
    PC = paste0("PC", seq_along(var_exp)),
    Varianza = var_exp,
    Acumulada = cumsum(var_exp)
  )

  # --- Scores y cargas ---
  scores <- as.data.frame(pca$x[, 1:2])
  scores[[id_col]] <- df[[id_col]]
  loadings <- as.data.frame(pca$rotation[, 1:2])
  loadings$variable <- rownames(loadings)

  # --- Clustering (K-means) ---
  set.seed(999)
  km <- stats::kmeans(scores[, c("PC1","PC2")], centers = k_clust, nstart = 25)
  scores$cluster <- as.factor(km$cluster)

  # --- Métricas de clúster ---
  sil <- tryCatch({
    if (requireNamespace("cluster", quietly = TRUE))
      cluster::silhouette(km$cluster, dist(scores[, c("PC1","PC2")]))
    else NULL
  }, error = function(e) NULL)
  sil_df <- if (!is.null(sil)) data.frame(cluster = km$cluster, silhouette = sil[, 3]) else NULL

  # --- Gráfico 1: Scree plot ---
  g_scree <- ggplot2::ggplot(scree, ggplot2::aes(x = PC, y = Varianza)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::geom_line(ggplot2::aes(y = Acumulada), group = 1, color = "firebrick") +
    ggplot2::geom_point(ggplot2::aes(y = Acumulada), color = "firebrick") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(Varianza, 1)),
                       vjust = -0.5, size = 3) +
    ggplot2::labs(title = "Varianza explicada por componente",
                  y = "Proporción", x = "Componente") +
    ggplot2::theme_minimal(base_size = 11)

  # --- Gráfico 2: Biplot PCA enriquecido ---
  g_biplot <- ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2, color = cluster)) +
    ggplot2::geom_point(size = 3, alpha = 0.8) +
    ggrepel::geom_text_repel(ggplot2::aes(label = !!sym(id_col)), size = 3, alpha = 0.7) +
    ggplot2::geom_segment(data = loadings,
                          ggplot2::aes(xend = PC1 * 5, yend = PC2 * 5),
                          arrow = ggplot2::arrow(length = unit(0.1, "inches")),
                          color = "gray30") +
    ggrepel::geom_text_repel(data = loadings,
                             ggplot2::aes(x = PC1 * 5, y = PC2 * 5, label = variable),
                             color = "black", size = 3, segment.color = "gray60") +
    ggplot2::labs(title = "Mapa factorial de métodos y emociones",
                  subtitle = "Tokens coloreados por clúster | vectores = variables",
                  x = sprintf("PC1 (%.1f%%)", 100*var_exp[1]),
                  y = sprintf("PC2 (%.1f%%)", 100*var_exp[2])) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::scale_color_brewer(palette = "Set1")

  # --- Gráfico 3: Densidad 2D de clústeres ---
  g_density <- ggplot2::ggplot(scores, ggplot2::aes(PC1, PC2, fill = cluster)) +
    ggplot2::stat_density_2d(ggplot2::aes(alpha = after_stat(level)), geom = "polygon") +
    ggplot2::geom_point(color = "black", size = 1.5, alpha = 0.7) +
    ggplot2::scale_fill_brewer(palette = "cividis") +
    ggplot2::labs(title = "Distribución 2D por clúster (densidad de tokens)") +
    ggplot2::theme_minimal(base_size = 11)

  # --- Gráfico 4: Cargas principales (PC1 y PC2) ---
  top_loads <- loadings[order(abs(loadings$PC1) + abs(loadings$PC2), decreasing = TRUE), ][1:10, ]
  g_loads <- ggplot2::ggplot(top_loads, ggplot2::aes(x = reorder(variable, PC1), y = PC1, fill = PC2)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_gradient2(low = "red", mid = "white", high = "blue") +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Variables con mayor carga en PC1 y PC2",
                  y = "Carga PC1", x = NULL, fill = "Carga PC2") +
    ggplot2::theme_minimal(base_size = 11)

  # --- Salidas ---
  res <- list(
    pca = pca,
    clusters = scores,
    graficos = list(
      scree = g_scree,
      biplot = g_biplot,
      density = g_density,
      loadings = g_loads
    ),
    tablas = list(
      varianza = scree,
      cargas = loadings,
      silhouette = sil_df,
      centroides = as.data.frame(km$centers)
    )
  )

  # Guardar si se especifica
  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    ggplot2::ggsave(file.path(out_dir, "p4_scree.png"), g_scree, width = 6, height = 4.5, dpi = 150)
    ggplot2::ggsave(file.path(out_dir, "p4_biplot.png"), g_biplot, width = 7, height = 6, dpi = 150)
    ggplot2::ggsave(file.path(out_dir, "p4_density.png"), g_density, width = 7, height = 6, dpi = 150)
    ggplot2::ggsave(file.path(out_dir, "p4_loads.png"), g_loads, width = 7, height = 6, dpi = 150)
  }

  return(res)
}

# =============================================================
# PUNTO 5: Análisis Multivariado y Clústeres
# =============================================================

#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Análisis multivariado y de clústeres de emociones
#' @description
#' Ejecuta PCA, correlaciones y agrupamiento jerárquico y k-means
#' sobre los métodos de sentimiento y emociones NRC. Genera
#' tablas y gráficos con indicadores factoriales y de clúster.
#'
#' @param df Data frame con columnas numéricas de métodos (syuzhet, bing, afinn, nrc)
#'   y opcionalmente emociones NRC (anger, joy, etc.).
#' @param method_cols vector con nombres de columnas de métodos.
#' @param emotion_cols vector con nombres de columnas NRC (opcional).
#' @param nivel_col nombre de columna categórica con nivel (p.ej. Nivel_sent_compuesto)
#' @param out_dir carpeta para guardar gráficos (NULL = no guardar).
#' @param n_clusters número de clústeres a generar (por defecto 4).
#'
#' @return lista con resultados PCA, clústeres, centroides y gráficos.
#' @family utilidades
#' @export
#' @param "bing" (auto) TODO: describir parámetro.
#' @param "afinn" (auto) TODO: describir parámetro.
#' @param "nrc" (auto) TODO: describir parámetro.
estrategia_p5_clusters <- function(
    df,
    method_cols = c("syuzhet","bing","afinn","nrc"),
    emotion_cols = NULL,
    nivel_col = "Nivel_sent_compuesto",
    out_dir = "salidas_pipeline",
    n_clusters = 4
){
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Requiere ggplot2")
  if (!requireNamespace("FactoMineR", quietly = TRUE)) stop("Requiere FactoMineR")
  if (!requireNamespace("factoextra", quietly = TRUE)) stop("Requiere factoextra")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Requiere dplyr")

  library(ggplot2)
  library(FactoMineR)
  library(factoextra)
  library(dplyr)

  # =======================
  # 1. Datos base
  # =======================
  vars <- method_cols
  if (!is.null(emotion_cols)) vars <- unique(c(vars, emotion_cols))
  dnum <- df[, vars]
  dnum <- dnum %>% mutate_all(as.numeric)
  rownames(dnum) <- df$token_idioma_unico

  # =======================
  # 2. PCA básico
  # =======================
  pca_res <- FactoMineR::PCA(dnum, scale.unit = TRUE, graph = FALSE)

  g_pca_var <- factoextra::fviz_pca_var(pca_res,
                                        col.var = "contrib",
                                        gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                        repel = TRUE) +
    labs(title = "Contribución de variables (PCA)")

  g_pca_ind <- factoextra::fviz_pca_ind(pca_res,
                                        label = "none",
                                        col.ind = df[[nivel_col]],
                                        palette = "Set2",
                                        addEllipses = TRUE,
                                        legend.title = "Nivel") +
    labs(title = "Proyección de tokens según métodos y nivel")

  # =======================
  # 3. Clústeres (k-means + jerárquico)
  # =======================
  set.seed(123)
  k_res <- stats::kmeans(scale(dnum), centers = n_clusters, nstart = 25)

  df$cluster <- factor(k_res$cluster)
  g_cluster_k <- factoextra::fviz_cluster(k_res, data = scale(dnum),
                                          geom = "point",
                                          ellipse.type = "norm",
                                          repel = TRUE,
                                          show.clust.cent = TRUE,
                                          palette = "Set1") +
    labs(title = sprintf("Agrupamiento k-means (%d clústeres)", n_clusters))

  hc <- stats::hclust(dist(scale(dnum)), method = "ward.D2")
  g_cluster_h <- factoextra::fviz_dend(hc, k = n_clusters,
                                       rect = TRUE, rect_fill = TRUE,
                                       rect_border = "Set1",
                                       main = sprintf("Dendrograma jerárquico (%d grupos)", n_clusters))

  # =======================
  # 4. Centroides y resumen
  # =======================
  centroides <- df %>%
    group_by(cluster) %>%
    summarise(across(all_of(method_cols), mean, na.rm = TRUE)) %>%
    arrange(cluster)

  # =======================
  # 5. Gráficos comparativos
  # =======================
  g_centroides <- centroides %>%
    tidyr::pivot_longer(-cluster, names_to = "metodo", values_to = "media") %>%
    ggplot(aes(x = metodo, y = media, fill = metodo)) +
    geom_col(position = "dodge") +
    facet_wrap(~cluster) +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Medias de métodos por clúster", y = "Promedio", x = "Método") +
    theme_minimal()

  g_biplot <- factoextra::fviz_pca_biplot(pca_res,
                                          label = "var",
                                          habillage = df[[nivel_col]],
                                          palette = "Set3",
                                          addEllipses = TRUE,
                                          title = "Biplot PCA: Métodos vs Niveles")

  # =======================
  # 6. Guardado opcional
  # =======================
  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(out_dir, "p5_pca_var.png"), g_pca_var, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p5_pca_ind.png"), g_pca_ind, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p5_kmeans.png"), g_cluster_k, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p5_dendrograma.png"), g_cluster_h, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p5_centroides.png"), g_centroides, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p5_biplot.png"), g_biplot, width = 7, height = 5, dpi = 150)
  }

  list(
    pca = pca_res,
    centroides = centroides,
    clusters = df[, c("token_idioma_unico","cluster",nivel_col)],
    graficos = list(
      pca_var = g_pca_var,
      pca_ind = g_pca_ind,
      cluster_k = g_cluster_k,
      cluster_h = g_cluster_h,
      centroides = g_centroides,
      biplot = g_biplot
    )
  )
}



# =============================================================
# PUNTO 6: Evaluación de estabilidad y visualización analítica
# =============================================================

#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Evaluación de estabilidad y visualización de tokens en PCA/clústeres
#' @description
#' Evalúa la calidad de agrupamientos mediante índices (silhouette, gap, WSS)
#' y genera representaciones PCA con etiquetas de tokens para análisis interpretativo.
#'
#' @param df Data frame con columnas numéricas de métodos y clúster asignado.
#' @param method_cols vector con nombres de columnas de métodos.
#' @param nivel_col nombre de columna categórica (p.ej. Nivel_sent_compuesto).
#' @param cluster_col nombre de columna con el clúster asignado (por defecto "cluster").
#' @param out_dir carpeta para guardar gráficos (opcional).
#' @param label_fraction proporción de tokens etiquetados (0–1), útil para no saturar el gráfico.
#'
#' @return lista con métricas de estabilidad, mejor número de clústeres, y gráficos.
#' @family visualizacion
#' @export
#' @param "bing" (auto) TODO: describir parámetro.
#' @param "afinn" (auto) TODO: describir parámetro.
#' @param "nrc" (auto) TODO: describir parámetro.
estrategia_p6_estabilidad_visual <- function(
    df,
    method_cols = c("syuzhet","bing","afinn","nrc"),
    nivel_col = "Nivel_sent_compuesto",
    cluster_col = "cluster",
    out_dir = "salidas_pipeline",
    label_fraction = 0.25
){
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Requiere ggplot2")
  if (!requireNamespace("factoextra", quietly = TRUE)) stop("Requiere factoextra")
  if (!requireNamespace("cluster", quietly = TRUE)) stop("Requiere cluster")

  library(ggplot2)
  library(factoextra)
  library(cluster)

  dnum <- df[, method_cols]
  rownames(dnum) <- df$token_idioma_unico
  dnum <- scale(dnum)

  # =========================
  # 1. Silhouette promedio
  # =========================
  sil <- cluster::silhouette(as.integer(df[[cluster_col]]), dist(dnum))
  sil_avg <- mean(sil[, 3], na.rm = TRUE)

  g_sil <- factoextra::fviz_silhouette(sil) +
    labs(title = sprintf("Análisis de Silhouette (media = %.3f)", sil_avg))

  # =========================
  # 2. Gap Statistic (n óptimo)
  # =========================
  set.seed(1)
  gap_res <- cluster::clusGap(dnum, FUN = stats::kmeans, nstart = 25, K.max = 10, B = 50)
  g_gap <- factoextra::fviz_gap_stat(gap_res) +
    labs(title = "Gap Statistic para número óptimo de clústeres")

  best_k <- which.max(gap_res$Tab[, "gap"])

  # =========================
  # 3. Elbow / WSS
  # =========================
  wss_vals <- sapply(1:10, function(k) {
    stats::kmeans(dnum, centers = k, nstart = 20)$tot.withinss
  })
  df_wss <- data.frame(k = 1:10, WSS = wss_vals)
  g_wss <- ggplot(df_wss, aes(x = k, y = WSS)) +
    geom_line(linewidth = 1, color = "#0073C2FF") +
    geom_point(size = 2) +
    geom_vline(xintercept = best_k, color = "red", linetype = "dashed") +
    labs(title = "Curva Elbow (WSS)",
         subtitle = sprintf("Número óptimo (Gap) = %d", best_k),
         x = "Número de clústeres (k)", y = "Suma intra-clúster (WSS)") +
    theme_minimal()

  # =========================
  # 4. Visualización PCA con etiquetas
  # =========================
  pca_res <- FactoMineR::PCA(dnum, graph = FALSE)
  ind_df <- as.data.frame(pca_res$ind$coord)
  ind_df$token <- rownames(dnum)
  ind_df$cluster <- df[[cluster_col]]
  ind_df$nivel <- df[[nivel_col]]

  # Submuestreo de etiquetas para legibilidad
  set.seed(123)
  n_show <- ceiling(nrow(ind_df) * label_fraction)
  label_idx <- sample(seq_len(nrow(ind_df)), n_show)
  ind_df$label <- ifelse(seq_len(nrow(ind_df)) %in% label_idx, ind_df$token, "")

  g_pca_tokens <- ggplot(ind_df, aes(x = Dim.1, y = Dim.2, color = factor(cluster))) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = label), size = 3, vjust = 1.2, check_overlap = TRUE) +
    scale_color_brewer(palette = "Set1") +
    labs(title = "PCA con etiquetas de tokens y clústeres",
         subtitle = sprintf("Mostrando %.0f%% de tokens", label_fraction * 100),
         x = "Componente 1", y = "Componente 2", color = "Clúster") +
    theme_minimal()

  # =========================
  # 5. Guardado opcional
  # =========================
  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    ggsave(file.path(out_dir, "p6_silhouette.png"), g_sil, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p6_gap_stat.png"), g_gap, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p6_elbow_wss.png"), g_wss, width = 7, height = 5, dpi = 150)
    ggsave(file.path(out_dir, "p6_pca_tokens.png"), g_pca_tokens, width = 8, height = 6, dpi = 150)
  }

  list(
    metrics = list(
      silhouette_mean = sil_avg,
      best_k_gap = best_k,
      gap_table = gap_res$Tab
    ),
    graficos = list(
      silhouette = g_sil,
      gap = g_gap,
      elbow = g_wss,
      pca_tokens = g_pca_tokens
    )
  )
}

###### Parte 7 de Análisis

# ============================================================
# PUNTO 7 — REPORTE MAESTRO Y EXPORTACIÓN (robusto y namespaced)
# ============================================================

# --------- Helpers seguros (sin conflictos) -----------------

#' Guardado seguro a CSV (no escribe si el objeto no tiene columnas)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param obj data.frame/data.table
#' @param path ruta destino .csv
#' @return (invisible) ruta o NULL si no escribió
#' @family utilidades
#' @export
safe_write_csv <- function(obj, path) {
  if (is.null(obj)) return(invisible(NULL))
  if (!is.data.frame(obj)) obj <- try(as.data.frame(obj), silent = TRUE)
  if (inherits(obj, "try-error")) return(invisible(NULL))
  if (ncol(obj) == 0L) return(invisible(NULL))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(obj, path)
  invisible(path)
}

#' Guardado seguro de ggplot/recordedplot
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param p objeto ggplot2 o recordedplot
#' @param path ruta .png/.pdf
#' @param width alto en pulgadas
#' @param height ancho en pulgadas
#' @param dpi resolución
#' @return (invisible) ruta o NULL
#' @export
safe_save_plot <- function(p, path, width = 9, height = 5.5, dpi = 300) {
  if (is.null(p)) return(invisible(NULL))
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ext <- tolower(tools::file_ext(path))
  if (inherits(p, "ggplot")) {
    ggplot2::ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi, limitsize = FALSE)
  } else if (inherits(p, "recordedplot")) {
    grDevices::png(path, width = width, height = height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    replayPlot(p)
  } else {
    # último recurso: try-catch con ggsave por si es un objeto compatible
    try(ggplot2::ggsave(filename = path, plot = p, width = width, height = height, dpi = dpi, limitsize = FALSE), silent = TRUE)
  }
  invisible(path)
}

#' Coerción segura a data.frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param x objeto
#' @return data.frame o NULL si no se pudo
#' @family utilidades
#' @export
as_df_safe <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  out <- try(as.data.frame(x), silent = TRUE)
  if (inherits(out, "try-error")) return(NULL)
  out
}

#' Compacta lista de gráficos (si existen) en disco con prefijo
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param gl lista de ggplots
#' @param out_dir carpeta
#' @param prefix prefijo de archivo
#' @return (auto) Verificar y completar la descripción del valor retornado.
#' @family visualizacion
#' @export
save_plot_list <- function(gl, out_dir, prefix) {
  if (is.null(gl)) return(invisible(NULL))
  if (!is.list(gl)) return(invisible(NULL))
  i <- 0L
  for (nm in names(gl)) {
    i <- i + 1L
    p <- gl[[nm]]
    fname <- if (!is.null(nm) && nzchar(nm)) {
      sprintf("%s_%02d_%s.png", prefix, i, nm)
    } else {
      sprintf("%s_%02d.png", prefix, i)
    }
    safe_save_plot(p, file.path(out_dir, fname))
  }
  invisible(NULL)
}

# --------- Punto 7: Exportar Reporte Maestro ----------------

#' Exporta tablas, gráficos y objeto maestro (Punto 7)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Reúne las salidas de los puntos previos (P1..P6), exporta CSV/PNG y
#' persiste un `00_salida_maestra.rds` con todo lo relevante.
#'
#' @param out_dir Carpeta de salida (se crea si no existe).
#' @param resultados_raw (opcional) objeto original `resultados` del pipeline.
#' @param res_p1 Lista devuelta por estrategia_p1_preparacion().
#' @param res_p2 Lista devuelta por estrategia_p2_metodos().
#' @param res_p3 Lista devuelta por estrategia_p3_ensamble().
#' @param res_p4 Lista devuelta por estrategia_p4_perfil_emocional().
#' @param res_p5 Lista devuelta por estrategia_p5_factores().
#' @param res_p6 Lista devuelta por estrategia_p6_clustering().
#' @param cfg (opcional) lista de configuración del pipeline.
#'
#' @return Lista con rutas escritas y el objeto maestro en memoria.
#' @family utilidades
#' @export
p7_exportar_reporte_maestro <- function(
    out_dir = "salidas_pipeline",
    resultados_raw = NULL,
    res_p1 = NULL,
    res_p2 = NULL,
    res_p3 = NULL,
    res_p4 = NULL,
    res_p5 = NULL,
    res_p6 = NULL,
    cfg = NULL
) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # ---------- 1) Tablas a CSV ----------
  # P1
  safe_write_csv(as_df_safe(res_p1$data),               file.path(out_dir, "p1_tabla_base.csv"))
  safe_write_csv(as_df_safe(res_p1$auditoria$corr_metodos), file.path(out_dir, "p1_corr_metodos.csv"))

  # P2
  safe_write_csv(as_df_safe(res_p2$tablas$cor_pearson),  file.path(out_dir, "p2_cor_pearson.csv"))
  safe_write_csv(as_df_safe(res_p2$tablas$cor_spearman), file.path(out_dir, "p2_cor_spearman.csv"))
  safe_write_csv(as_df_safe(res_p2$tablas$ccc_pares),    file.path(out_dir, "p2_ccc_pares.csv"))

  # P3
  safe_write_csv(as_df_safe(res_p3$calibracion),         file.path(out_dir, "p3_calibracion.csv"))
  safe_write_csv(as_df_safe(res_p3$pesos),               file.path(out_dir, "p3_pesos.csv"))
  if (!is.null(res_p3$alpha)) {
    data.table::fwrite(data.frame(alpha = res_p3$alpha), file.path(out_dir, "p3_alpha.csv"))
  }
  safe_write_csv(as_df_safe(res_p3$data),                file.path(out_dir, "p3_tabla_ensamble.csv"))

  # P4
  safe_write_csv(as_df_safe(res_p4$indicadores),         file.path(out_dir, "p4_indicadores.csv"))
  safe_write_csv(as_df_safe(res_p4$long),                file.path(out_dir, "p4_largo_perfiles.csv"))

  # P5
  safe_write_csv(as_df_safe(res_p5$resultados),          file.path(out_dir, "p5_factores_componentes.csv"))
  safe_write_csv(as_df_safe(res_p5$cargas),              file.path(out_dir, "p5_cargas_rotadas.csv"))
  safe_write_csv(as_df_safe(res_p5$var_explicada),       file.path(out_dir, "p5_var_explicada.csv"))

  # P6
  safe_write_csv(as_df_safe(res_p6$clusters),            file.path(out_dir, "p6_clusters.csv"))
  safe_write_csv(as_df_safe(res_p6$centroides),          file.path(out_dir, "p6_centroids.csv"))
  safe_write_csv(as_df_safe(res_p6$metrics$gap_table),   file.path(out_dir, "p6_gap_table.csv"))

  # ---------- 2) Gráficos a PNG ----------
  # P1
  safe_save_plot(res_p1$graficos$indice_cortes,          file.path(out_dir, "p1_indice_cortes.png"))
  safe_save_plot(res_p1$graficos$densidad_indice,        file.path(out_dir, "p1_densidad_indice.png"))
  save_plot_list(res_p1$graficos$hist_por_metodo,        out_dir, "p1_hist_metodo")

  # P2
  safe_save_plot(res_p2$graficos$cor_pearson,            file.path(out_dir, "p2_cor_pearson.png"))
  safe_save_plot(res_p2$graficos$cor_spearman,           file.path(out_dir, "p2_cor_spearman.png"))
  safe_save_plot(res_p2$graficos$box_por_nivel,          file.path(out_dir, "p2_box_por_nivel.png"))
  safe_save_plot(res_p2$graficos$violin_por_nivel,       file.path(out_dir, "p2_violin_por_nivel.png"))
  if (is.list(res_p2$graficos$bland_altman)) {
    save_plot_list(res_p2$graficos$bland_altman,         out_dir, "p2_bland_altman")
  }
  if (is.list(res_p2$graficos$pairs_z)) {
    save_plot_list(res_p2$graficos$pairs_z,              out_dir, "p2_pairs_z")
  }

  # P3
  safe_save_plot(res_p3$graficos$pesos,                  file.path(out_dir, "p3_pesos.png"))
  safe_save_plot(res_p3$graficos$qq_ref,                 file.path(out_dir, "p3_qq_ref.png"))
  safe_save_plot(res_p3$graficos$ba_ref,                 file.path(out_dir, "p3_bland_altman_ref.png"))
  safe_save_plot(res_p3$graficos$density_compuesto,      file.path(out_dir, "p3_densidad_compuesto.png"))
  safe_save_plot(res_p3$graficos$scatter_compuesto_ref,  file.path(out_dir, "p3_scatter_compuesto_vs_ref.png"))

  # P4
  safe_save_plot(res_p4$graficos$perfil_promedio,        file.path(out_dir, "p4_perfil_promedio.png"))
  safe_save_plot(res_p4$graficos$heatmap,                file.path(out_dir, "p4_heatmap.png"))
  safe_save_plot(res_p4$graficos$radar,                  file.path(out_dir, "p4_radar.png"))

  # P5
  safe_save_plot(res_p5$graficos$pca_scree,              file.path(out_dir, "p5_pca_scree.png"))
  safe_save_plot(res_p5$graficos$pca_biplot,             file.path(out_dir, "p5_pca_biplot.png"))
  safe_save_plot(res_p5$graficos$fa_loadings,            file.path(out_dir, "p5_fa_loadings.png"))

  # P6
  safe_save_plot(res_p6$graficos$silhouette,             file.path(out_dir, "p6_silhouette.png"))
  safe_save_plot(res_p6$graficos$gap,                     file.path(out_dir, "p6_gap.png"))
  safe_save_plot(res_p6$graficos$elbow,                   file.path(out_dir, "p6_elbow.png"))
  safe_save_plot(res_p6$graficos$pca_tokens,              file.path(out_dir, "p6_pca_tokens.png"))

  # ---------- 3) Objeto maestro (RDS) ----------
  objeto_maestro <- list(
    meta = list(
      timestamp = Sys.time(),
      session   = utils::sessionInfo(),
      cfg       = cfg
    ),
    raw = list(
      resultados = resultados_raw
    ),
    p1 = res_p1,
    p2 = res_p2,
    p3 = res_p3,
    p4 = res_p4,
    p5 = res_p5,
    p6 = res_p6
  )

  rds_path <- file.path(out_dir, "00_salida_maestra.rds")
  saveRDS(objeto_maestro, rds_path)

  invisible(list(
    out_dir   = out_dir,
    rds_path  = rds_path,
    ok        = TRUE
  ))
}
