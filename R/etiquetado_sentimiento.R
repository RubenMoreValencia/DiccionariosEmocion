#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Categorizar por percentiles con fallback robusto
#' @description
#' Dada una serie numérica, calcula cortes por percentiles y crea una
#' variable categórica. Si los cuantiles solicitados coinciden (valores repetidos)
#' se degrada automáticamente a una malla de cortes más simple hasta obtener
#' límites únicos. Soporta etiquetas personalizadas.
#'
#' Categoriza un vector numérico según percentiles, con robustez ante empates
#'
#' @param x Vector numérico.
#' @param probs Numeric vector de percentiles (ej. c(0.25, 0.5, 0.75)).
#' @param prefix Prefijo para etiquetas (default "N-").
#' @return list con $categoria (factor) y $breaks (vector de cortes usados).
#' @family utilidades
#' @export
#' @param 0.5 (auto) TODO: describir parámetro.
#' @param 0.75 (auto) TODO: describir parámetro.
categorizar_por_percentiles <- function(x, probs = c(0.25, 0.5, 0.75), prefix = "N-") {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) {
    return(list(categoria = factor(rep(paste0(prefix, "0"), length(x))),
                breaks = NA_real_))
  }

  qs <- stats::quantile(x, probs = probs, na.rm = TRUE)
  qs <- unique(qs)  # eliminar duplicados

  # Cortes siempre comienzan y terminan en rango extendido
  brks <- sort(unique(c(min(x, na.rm = TRUE), qs, max(x, na.rm = TRUE))))
  n_intervals <- length(brks) - 1
  labels <- paste0(prefix, seq_len(n_intervals) - 1)

  # Seguridad: si hay menos breaks, ajustar etiquetas
  if (length(labels) != n_intervals) {
    labels <- paste0(prefix, seq_len(n_intervals))
  }

  cat_factor <- cut(
    x, breaks = brks, labels = labels,
    include.lowest = TRUE, right = TRUE
  )

  list(categoria = cat_factor, breaks = brks)
}

#' Categoriza por percentiles manteniendo longitud (robusto a empates/NA)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#' @param x numeric vector (se mantiene longitud)
#' @param probs percentiles deseados (default c(0.25,0.5,0.75))
#' @param labels etiquetas a usar; si NULL se autogeneran N-0..N-k
#' @param include_lowest, right como en cut()
#' @return list(categoria= factor(len(x)), breaks= numeric, probs_usados=numeric)
#' @family utilidades
#' @export
#' @param 0.5 (auto) TODO: describir parámetro.
#' @param 0.75 (auto) TODO: describir parámetro.
categorizar_por_percentiles_full <- function(
    x,
    probs = c(0.25, 0.5, 0.75),
    labels = NULL,
    include_lowest = TRUE,
    right = FALSE
){
  n <- length(x)
  if (!is.numeric(x)) stop("`x` debe ser numerico.")
  if (n < 1L) return(list(
    categoria = factor(integer(0)),
    breaks = numeric(0),
    probs_usados = numeric(0)
  ))

  xf <- x[is.finite(x)]
  if (length(xf) < 2L) {
    # todo NA o constante, una sola categoría, conserva longitud
    cat_all <- factor(rep("SinDato", n), levels = "SinDato")
    return(list(categoria = cat_all, breaks = NA_real_, probs_usados = numeric(0)))
  }

  # intento de cuantiles con fallback si colapsan
  fallback_grid <- list(
    c(0.25, 0.5, 0.75),
    c(0.33, 0.66),
    0.5,
    numeric(0)
  )
  probs_used <- NULL
  qs <- NULL
  for (pg in fallback_grid) {
    if (length(pg)) {
      qv <- stats::quantile(xf, probs = pg, na.rm = TRUE, type = 7)
      qv <- unique(as.numeric(qv))
    } else {
      qv <- numeric(0)
    }
    brks <- sort(unique(c(min(xf), qv, max(xf))))
    if (length(brks) >= 2L) {
      probs_used <- pg
      qs <- qv
      break
    }
  }

  n_intervals <- length(brks) - 1L
  if (is.null(labels)) labels <- paste0("N-", seq_len(n_intervals) - 1L)
  if (length(labels) != n_intervals) labels <- paste0("N-", seq_len(n_intervals) - 1L)

  # categorizamos TODO x (incluyendo NA), y luego mapeamos NA -> "SinDato"
  cat_x <- cut(x, breaks = brks, labels = labels,
               include_lowest = include_lowest, right = right)
  if (anyNA(cat_x)) {
    lv <- c("SinDato", levels(cat_x))
    cat_x <- as.character(cat_x)
    cat_x[!is.finite(x)] <- "SinDato"
    cat_x <- factor(cat_x, levels = lv)
  }
  list(categoria = cat_x, breaks = brks, probs_usados = probs_used)
}



#' Categorización por percentiles de columnas (y/o índice agregado) con soporte de pesos y escalado
#'
#' @description
#' Dadas columnas numéricas de un data.frame, categoriza por percentiles cada columna
#' (modo "per_column") y opcionalmente construye un **índice agregado** con pesos
#' y escalado (modo "aggregate"). Usa internamente `categorizar_por_percentiles()`.
#'
#' @param df data.frame con columnas numéricas a procesar.
#' @param idx Integer o character. Columnas a categorizar (por nombre o posición).
#' @param prefix Character. Prefijo para las nuevas columnas categóricas. Default `"Cat_"`.
#' @param mode Character. `"per_column"` (default) para categorizar cada columna por separado;
#'   `"aggregate"` para crear un índice agregado y categorizarlo; `"both"` para hacer ambos.
#' @param weights Numeric o NULL. Pesos para las columnas cuando `mode` incluye `"aggregate"`.
#'   Si `NULL`, se asume iguales. Debe coincidir con `length(idx)`.
#' @param scale_method Character. Escalado previo al agregado: `"auto"` (default, z-score),
#'   `"zscore"`, `"range"` (min-max), `"robust"` (mediana/MAD), `"none"`.
#' @param aggregate_name Character. Nombre de la columna del índice agregado (sin prefijo).
#'   Default `"indice"`.
#' @param probs,labels,include_lowest,right,na.rm Pasados a `categorizar_por_percentiles()`.
#'
#' @return list con:
#' \itemize{
#'   \item \code{data}: data.frame original + columnas categorizadas añadidas.
#'   \item \code{meta}: lista con \code{breaks} y \code{probs_usados} por cada salida creada.
#' }
#'
#' @examples
#' # Suponiendo dfx_es con métodos léxicos en columnas 11:14 (syuzhet, bing, afinn, nrc):
#' # 1) Categorizar cada método por separado
#' res1 <- categorizar_indices(
#'   df = dfx_es, idx = 11:14, prefix = "Cat_", mode = "per_column"
#' )
#' names(res1$data)
#'
#' # 2) Índice agregado (zscore + pesos) y categorización
#' res2 <- categorizar_indices(
#'   df = dfx_es, idx = c("syuzhet","bing","afinn","nrc"),
#'   mode = "aggregate", weights = c(0.25,0.25,0.25,0.25),
#'   scale_method = "zscore", aggregate_name = "polaridad"
#' )
#' table(res2$data$Cat_polaridad)
#'
#' # 3) Ambos: por columna + índice agregado
#' res3 <- categorizar_indices(
#'   df = dfx_es, idx = 11:14, mode = "both",
#'   weights = c(0.4,0.1,0.3,0.2), scale_method = "robust",
#'   aggregate_name = "mix"
#' )
#' str(res3$meta)
#'
#' @family utilidades
#' @export
#' @param "aggregate" (auto) TODO: describir parámetro.
#' @param "both" (auto) TODO: describir parámetro.
categorizar_indices <- function(
    df,
    idx,
    prefix = "Cat_",
    mode = c("per_column", "aggregate", "both"),
    weights = NULL,
    scale_method = c("auto", "zscore", "range", "robust", "none"),
    aggregate_name = "indice",
    probs = c(0.25, 0.5, 0.75),
    labels = NULL,
    include_lowest = TRUE,
    right = FALSE,
    na.rm = TRUE
) {
  # ---- validación básica ----
  if (!is.data.frame(df)) stop("`df` debe ser data.frame.")
  mode <- match.arg(mode)
  scale_method <- match.arg(scale_method)

  # resolver columnas
  if (is.character(idx)) {
    if (!all(idx %in% colnames(df))) stop("`idx` (nombres) no están en df.")
    cols <- idx
  } else if (is.numeric(idx)) {
    if (any(idx < 1 | idx > ncol(df))) stop("`idx` (posiciones) fuera de rango.")
    cols <- colnames(df)[idx]
  } else {
    stop("`idx` debe ser character (nombres) o numeric (posiciones).")
  }

  # comprobar numéricas
  if (!all(vapply(df[ , cols, drop = FALSE], is.numeric, TRUE))) {
    stop("Todas las columnas en `idx` deben ser numéricas.")
  }

  out_df <- df
  meta   <- list()

  # helper: escalado de columnas a matriz escalada
  scale_cols <- function(mat, method) {
    mat <- as.matrix(mat)
    if (method %in% c("auto","zscore")) {
      # z-score: (x - mean)/sd (si sd=0 => 0)
      mu <- colMeans(mat, na.rm = TRUE)
      sdv <- apply(mat, 2, stats::sd, na.rm = TRUE)
      sdv[!is.finite(sdv) | sdv == 0] <- 1
      sweep(sweep(mat, 2, mu, FUN = "-"), 2, sdv, FUN = "/")
    } else if (method == "range") {
      # min-max a [0,1] (si rango=0 => 0)
      mn <- apply(mat, 2, min, na.rm = TRUE)
      mx <- apply(mat, 2, max, na.rm = TRUE)
      rg <- mx - mn
      rg[!is.finite(rg) | rg == 0] <- 1
      sweep(sweep(mat, 2, mn, FUN = "-"), 2, rg, FUN = "/")
    } else if (method == "robust") {
      # robusto: (x - mediana) / MAD (si MAD=0 => 0)
      med <- apply(mat, 2, stats::median, na.rm = TRUE)
      madv <- apply(mat, 2, stats::mad, na.rm = TRUE, constant = 1.4826)
      madv[!is.finite(madv) | madv == 0] <- 1
      sweep(sweep(mat, 2, med, FUN = "-"), 2, madv, FUN = "/")
    } else if (method == "none") {
      mat
    } else {
      mat
    }
  }

  # ---- modo "per_column": categorizar cada columna por separado ----
  if (mode %in% c("per_column","both")) {
    for (cn in cols) {
      res <- categorizar_por_percentiles(
        x = out_df[[cn]],
        probs = probs, labels = labels,
        include_lowest = include_lowest, right = right, na.rm = na.rm
      )
      new_name <- paste0(prefix, cn)
      out_df[[new_name]] <- res$categoria
      meta[[new_name]] <- list(
        origen = cn,
        breaks = res$breaks,
        probs_usados = res$probs_usados
      )
    }
  }

  # ---- modo "aggregate": índice combinado + categorización ----
  if (mode %in% c("aggregate","both")) {
    mat <- out_df[, cols, drop = FALSE]

    # pesos
    if (is.null(weights)) {
      weights <- rep(1/length(cols), length(cols))
    } else {
      if (length(weights) != length(cols)) {
        stop("`weights` debe tener la misma longitud que `idx`.")
      }
      if (any(!is.finite(weights))) stop("`weights` contiene valores no finitos.")
      s <- sum(weights)
      if (s == 0) stop("La suma de `weights` no puede ser 0.")
      weights <- weights / s
    }

    # escalado para compatibilizar escalas
    mat_scaled <- scale_cols(mat, scale_method)

    # índice ponderado
    ind_vec <- as.numeric(as.matrix(mat_scaled) %*% weights)

    # categorización del índice
    res_ind <- categorizar_por_percentiles(
      x = ind_vec,
      probs = probs, labels = labels,
      include_lowest = include_lowest, right = right, na.rm = na.rm
    )

    ind_name <- paste0(prefix, aggregate_name)
    out_df[[aggregate_name]] <- ind_vec
    out_df[[ind_name]]       <- res_ind$categoria

    meta[[ind_name]] <- list(
      origen = cols,
      weights = weights,
      scale_method = scale_method,
      breaks = res_ind$breaks,
      probs_usados = res_ind$probs_usados
    )
  }

  list(data = out_df, meta = meta)
}






#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Diferencia positivo-negativo e indexación flexible
#' @description
#' Calcula una columna \code{diferencia = positivo - negativo} a partir de un
#' data frame con columnas referenciadas por nombre o índice. Luego categoriza
#' esa diferencia por percentiles usando \code{\link{categorizar_por_percentiles}}.
#'
#' @param datos Data frame.
#' @param indice_positivo Nombre o índice (1..p) de la columna positiva.
#' @param indice_negativo Nombre o índice (1..p) de la columna negativa.
#' @param probs Vector de percentiles para la categorización. Por defecto \code{c(0.25, 0.5, 0.75)}.
#' @param labels Etiquetas para las categorías. Por defecto
#'   \code{c("muy negativo","negativo","positivo","muy positivo")}.
#' @param nombre_col_diff Nombre de la columna resultado de diferencia. Por defecto \code{"diferencia"}.
#'
#' @return \code{datos} con dos columnas nuevas: \code{diferencia} (o el nombre indicado)
#' y \code{categoria}.
#'
#' @examples
#' df <- data.frame(pos = c(0.2,0.6,0.9), neg = c(0.7,0.2,0.1))
#' analizar_comentarios(df, "pos", "neg")
#' @family utilidades
#' @export
#' @param 0.5 (auto) TODO: describir parámetro.
#' @param 0.75 (auto) TODO: describir parámetro.
analizar_comentarios <- function(
    datos,
    indice_positivo,
    indice_negativo,
    probs  = c(0.25, 0.5, 0.75),
    labels = c("muy negativo", "negativo", "positivo", "muy positivo"),
    nombre_col_diff = "diferencia"
) {
  if (!is.data.frame(datos)) stop("`datos` debe ser data.frame.")
  # resolver nombres
  get_col <- function(d, i) {
    if (is.numeric(i)) {
      if (i < 1 || i > ncol(d)) stop("Indice fuera de rango.")
      names(d)[i]
    } else {
      if (!i %in% names(d)) stop("Nombre de columna no existe: ", i)
      i
    }
  }
  cpos <- get_col(datos, indice_positivo)
  cneg <- get_col(datos, indice_negativo)

  # diferencia y categorizacion
  dif <- datos[[cpos]] - datos[[cneg]]
  datos[[nombre_col_diff]] <- dif

  out <- categorizar_por_percentiles(dif, probs = probs, labels = labels)
  datos$categoria <- out$categoria
  attr(datos$categoria, "breaks") <- out$breaks
  attr(datos$categoria, "probs")  <- out$probs_usados
  datos
}


#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Reescalar a mínimo cero y categorizar por percentiles
#' @description
#' Resta el mínimo de la serie para llevarla a \code{[0, +inf)} (min=0) y luego
#' categoriza por percentiles. Útil cuando solo interesan **relativos** y no la
#' polaridad negativa original.
#'
#' @param datos Data frame.
#' @param indice_positivo Nombre o índice de la columna positiva (si aplica).
#' @param indice_negativo Nombre o índice de la columna negativa (si aplica).
#' @param columna_valor Alternativa a los dos anteriores: nombre/índice de una
#'   columna ya calculada (p.ej. \code{"syuzhet"}). Si se provee, ignora los otros dos.
#' @param nombre_col_resultado Nombre para la columna reescalada. Por defecto \code{"valor_ajustado"}.
#' @param probs Vector de percentiles. Por defecto \code{c(0.2, 0.4, 0.6, 0.8)}.
#' @param labels Etiquetas. Por defecto
#' \code{c("muy negativo","negativo","Ni negativo Ni positivo","positivo","muy positivo")}.
#'
#' @return \code{datos} con columnas nuevas: \code{nombre_col_resultado}, \code{categoria} y
#'   atributos \code{breaks}/\code{probs} en \code{categoria}.
#'
#' @examples
#' set.seed(1); df <- data.frame(pos = runif(10), neg = runif(10))
#' analizar_comentarios_ReEscalado(df, "pos", "neg")
#' @family utilidades
#' @export
#' @param 0.4 (auto) TODO: describir parámetro.
#' @param 0.6 (auto) TODO: describir parámetro.
#' @param 0.8 (auto) TODO: describir parámetro.
analizar_comentarios_ReEscalado <- function(
    datos,
    indice_positivo = NULL,
    indice_negativo = NULL,
    columna_valor   = NULL,
    nombre_col_resultado = "valor_ajustado",
    probs  = c(0.2, 0.4, 0.6, 0.8),
    labels = c("muy negativo", "negativo", "Ni negativo Ni positivo", "positivo", "muy positivo")
) {
  if (!is.data.frame(datos)) stop("`datos` debe ser data.frame.")

  # resolver serie base
  resolve_col <- function(d, i) {
    if (is.null(i)) return(NULL)
    if (is.numeric(i)) {
      if (i < 1 || i > ncol(d)) stop("Indice fuera de rango.")
      names(d)[i]
    } else {
      if (!i %in% names(d)) stop("Nombre de columna no existe: ", i)
      i
    }
  }
  if (!is.null(columna_valor)) {
    cname <- resolve_col(datos, columna_valor)
    basev <- datos[[cname]]
  } else {
    cpos <- resolve_col(datos, indice_positivo)
    cneg <- resolve_col(datos, indice_negativo)
    if (is.null(cpos) || is.null(cneg)) stop("Especifique `columna_valor` o ambas `indice_positivo` y `indice_negativo`.")
    basev <- datos[[cpos]] - datos[[cneg]]
  }
  if (!is.numeric(basev)) stop("La serie base debe ser numerica.")

  vmin <- min(basev, na.rm = TRUE)
  aj   <- basev - vmin
  datos[[nombre_col_resultado]] <- aj

  out <- categorizar_por_percentiles(aj, probs = probs, labels = labels)
  datos$categoria <- out$categoria
  attr(datos$categoria, "breaks") <- out$breaks
  attr(datos$categoria, "probs")  <- out$probs_usados
  datos
}


#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Version automatica: intenta varios esquemas de cortes
#' @description
#' Variante que prueba esquemas de percentiles de mayor a menor complejidad
#' hasta encontrar cortes únicos. Si ninguno funciona, usa el percentil 50\% (binario).
#'
#' @param datos Data frame.
#' @param columna_valor Nombre o índice de la columna base (numérica).
#' @param nombre_columna_resultado Nombre de la columna de salida. Por defecto \code{"diferencia"}.
#'
#' @return \code{datos} con columnas \code{nombre_columna_resultado}, \code{categoria} y \code{percentil}.
#'
#' @examples
#' set.seed(1); df <- data.frame(syuzhet = rnorm(12))
#' analizar_comentarios_ReEscalado_v3(df, "syuzhet")
#' @family utilidades
#' @export
analizar_comentarios_ReEscalado_v3 <- function(
    datos,
    columna_valor,
    nombre_columna_resultado = "diferencia"
) {
  if (!is.data.frame(datos)) stop("`datos` debe ser data.frame.")
  # resolver columna
  colname <- if (is.numeric(columna_valor)) {
    if (columna_valor < 1 || columna_valor > ncol(datos)) stop("Indice de columna fuera de rango.")
    names(datos)[columna_valor]
  } else {
    if (!columna_valor %in% names(datos)) stop("La columna especificada no existe.")
    columna_valor
  }

  v <- datos[[colname]]
  if (!is.numeric(v)) stop("La columna debe ser numerica.")

  # reescala a min 0
  vmin <- min(v, na.rm = TRUE)
  adj  <- v - vmin
  datos[[nombre_columna_resultado]] <- adj

  # intentos de cortes
  tries <- list(c(0.2, 0.4, 0.6, 0.8),
                c(0.25, 0.5, 0.75),
                c(0.33, 0.66),
                0.5,
                numeric(0))
  labs_list <- list(
    c("muy negativo", "negativo", "Ni negativo Ni positivo", "positivo", "muy positivo"),
    c("muy negativo", "negativo", "positivo", "muy positivo"),
    c("negativo", "Ni negativo Ni positivo", "positivo"),
    c("negativo", "positivo"),
    c("todos")
  )

  percentiles <- numeric(0); etiquetas <- NULL
  for (i in seq_along(tries)) {
    p <- tries[[i]]
    if (length(p) == 0) {  # sin cortes
      percentiles <- numeric(0)
      etiquetas   <- labs_list[[i]]
      break
    }
    q <- unique(stats::quantile(adj, probs = p, na.rm = TRUE, type = 7))
    if (length(q) == length(p)) {
      percentiles <- q
      etiquetas   <- labs_list[[i]]
      break
    }
  }
  if (length(etiquetas) == 1L && etiquetas == "todos") {
    # sin cortes: todo a una sola clase
    datos$categoria <- factor(rep("todos", length(adj)))
  } else {
    datos$categoria <- cut(adj, breaks = c(-Inf, percentiles, Inf),
                           labels = etiquetas, right = FALSE, include_lowest = TRUE)
  }

  # percentil asociado a cada registro (aprox por corte siguiente)
  if (length(percentiles)) {
    datos$percentil <- sapply(adj, function(x) {
      p <- percentiles[percentiles >= x][1]
      if (is.na(p)) p <- percentiles[length(percentiles)]
      p
    })
  } else {
    datos$percentil <- NA_real_
  }
  datos
}


#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Categorizar una columna numérica (por nombre) en 4 rangos
#' @description
#' Categorización tipo cuartiles (Q1, Q2, Q3) con etiquetas sentimentales por defecto.
#'
#' @param datos Data frame.
#' @param columna_evaluar Nombre de la columna numérica a categorizar.
#'
#' @return \code{datos} con columna \code{categoria}.
#'
#' @examples
#' set.seed(1); df <- data.frame(x = rnorm(15))
#' analizar_percentiles_columna(df, "x")
#' @family utilidades
#' @export
analizar_percentiles_columna <- function(datos, columna_evaluar) {
  if (!is.data.frame(datos)) stop("`datos` debe ser data.frame.")
  if (!columna_evaluar %in% names(datos)) stop("Columna no existe.")
  x <- datos[[columna_evaluar]]
  out <- categorizar_por_percentiles(
    x,
    probs  = c(0.25, 0.50, 0.75),
    labels = c("muy negativo", "negativo", "positivo", "muy positivo")
  )
  datos$categoria <- out$categoria
  attr(datos$categoria, "breaks") <- out$breaks
  attr(datos$categoria, "probs")  <- out$probs_usados
  datos
}


#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Categorización binaria por mediana
#' @description
#' Divide una columna numérica en dos categorías usando el percentil 50\%.
#'
#' @param datos Data frame.
#' @param columna_evaluar Nombre de la columna numérica a binarizar.
#'
#' @return \code{datos} con columna \code{categoriaBinaria}.
#'
#' @examples
#' set.seed(1); df <- data.frame(x = rnorm(15))
#' analizar_percentiles_columnaBinaria(df, "x")
#' @family utilidades
#' @export
analizar_percentiles_columnaBinaria <- function(datos, columna_evaluar) {
  if (!is.data.frame(datos)) stop("`datos` debe ser data.frame.")
  if (!columna_evaluar %in% names(datos)) stop("Columna no existe.")
  x <- datos[[columna_evaluar]]
  q50 <- stats::quantile(x, probs = 0.5, na.rm = TRUE, type = 7)
  datos$categoriaBinaria <- cut(x, breaks = c(-Inf, q50, Inf),
                                labels = c("negativo", "positivo"),
                                right = FALSE, include_lowest = TRUE)
  datos
}


#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Categorización binaria positivo-negativo
#' @description
#' Calcula \code{positivo - negativo} y clasifica en dos grupos vía mediana.
#'
#' @examples
#' df <- data.frame(pos = c(0.2,0.6,0.9), neg = c(0.7,0.2,0.1))
#' analizar_comentarios_XP(df, "pos", "neg")
#' @family utilidades
#' @export
#' @param datos (auto) TODO: describir parámetro.
#' @param indice_positivo (auto) TODO: describir parámetro.
#' @param indice_negativo (auto) TODO: describir parámetro.
#' @return (auto) Verificar y completar la descripción del valor retornado.
analizar_comentarios_XP <- function(datos, indice_positivo, indice_negativo) {
  datos <- analizar_comentarios(datos, indice_positivo, indice_negativo,
                                probs = 0.5, labels = c("negativo", "positivo"))
  names(datos)[names(datos) == "categoria"] <- "categoria"
  datos
}


#' Resumen tipo \code{summary()} por columna en un data frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Aplica \code{summary()}/estadísticos básicos a cada columna y retorna un
#' \code{data.frame} con una fila por columna original y columnas de resumen
#' tipo cinco números + media + conteo de \code{NA}'s.
#'
#' @details
#' - Para columnas **numéricas** (incluye \code{integer} y \code{numeric}),
#'   se calculan: \code{Min.}, \code{1st Qu.}, \code{Median}, \code{Mean},
#'   \code{3rd Qu.}, \code{Max.}, \code{NA's} (con \code{na.rm = TRUE} cuando aplica).
#' - Para columnas **no numéricas** (p. ej. \code{character}, \code{factor},
#'   \code{logical}, \code{Date}/\code{POSIXt}), se reporta \code{NA} en los
#'   estadísticos numéricos y sólo se completa \code{NA's} con el conteo de
#'   valores perdidos. Esto mantiene forma estable de salida sin coerciones
#'   implícitas.
#' - El orden de filas respeta el orden de las columnas en \code{df}.
#' - Los nombres de fila corresponden a los nombres de las columnas de \code{df}.
#'
#' @param df \code{data.frame}. Estructura tabular a resumir.
#'
#' @return \code{data.frame} con filas = columnas de \code{df} y columnas:
#' \code{Min.}, \code{1st Qu.}, \code{Median}, \code{Mean}, \code{3rd Qu.}, \code{Max.}, \code{NA's}.
#' Los estadísticos numéricos se devuelven como numéricos cuando existen; en caso contrario \code{NA}.
#'
#' @examples
#' calcular_resumen(iris)
#' df <- data.frame(x = c(1, 2, NA, 4), y = c("a", "b", NA, "b"))
#' calcular_resumen(df)
#'
#' @family resumen_y_frecuencias
#' @export
calcular_resumen <- function(df) {
  if (!is.data.frame(df)) stop("El input debe ser un data frame", call. = FALSE)

  # columnas esperadas en el orden deseado
  esperadas <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.", "NA's")

  # resumidor seguro por columna (solo numéricas calculan; resto = NA excepto NA's)
  resumir_col <- function(col) {
    nas <- sum(is.na(col))
    if (is.numeric(col)) {
      qs <- stats::quantile(col, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE, names = FALSE, type = 7)
      c(
        "Min."     = qs[1],
        "1st Qu."  = qs[2],
        "Median"   = qs[3],
        "Mean"     = mean(col, na.rm = TRUE),
        "3rd Qu."  = qs[4],
        "Max."     = qs[5],
        "NA's"     = nas
      )
    } else {
      # no numéricas: solo NA's y el resto NA para mantener forma
      c(
        "Min."     = NA_real_,
        "1st Qu."  = NA_real_,
        "Median"   = NA_real_,
        "Mean"     = NA_real_,
        "3rd Qu."  = NA_real_,
        "Max."     = NA_real_,
        "NA's"     = nas
      )
    }
  }

  # manejar data.frame vacío
  if (ncol(df) == 0L) {
    out <- as.data.frame(matrix(nrow = 0, ncol = length(esperadas)))
    names(out) <- esperadas
    return(out)
  }

  # aplicar con vapply para asegurar longitudes/nombres
  res_list <- lapply(df, resumir_col)
  out <- do.call(rbind, res_list)

  # garantizar data.frame y tipos
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  # columnas faltantes (por seguridad) → crear
  faltan <- setdiff(esperadas, colnames(out))
  if (length(faltan)) for (nm in faltan) out[[nm]] <- NA_real_
  # reordenar columnas
  out <- out[, esperadas, drop = FALSE]

  # nombres de fila = nombres de columnas originales (si existían)
  rn <- names(df)
  if (!is.null(rn) && length(rn) == nrow(out)) rownames(out) <- rn

  # coerciones finales: NA's debe ser numérica
  if (!is.numeric(out[["NA's"]])) out[["NA's"]] <- as.numeric(out[["NA's"]])

  out
}


#' Consistencia entre lexicones: intersección, visualización e indicadores
#'
#' @description
#' A partir de una matriz con columnas de puntajes por lexicón (p. ej., \code{syuzhet},
#' \code{bing}, \code{afinn}, \code{nrc}) y filas como tokens, clasifica cada token
#' según el consenso interlexical (consenso positivo, consenso negativo, divergente,
#' mixto/neutral), construye visualizaciones y calcula indicadores resumen.
#'
#' @param matriz \code{data.frame} o \code{matrix} con columnas numéricas por lexicón y
#'   \code{rownames} como tokens.
#' @param metodos \code{character} opcional. Subconjunto de columnas a usar. Si \code{NULL},
#'   se usan todas las columnas numéricas.
#' @param umbral \code{numeric}. Punto de corte para clasificar: valores mayores a \code{umbral}
#'   son positivos; menores a \code{umbral} son negativos; iguales a \code{umbral} son neutros.
#'   Por defecto \code{0}.
#' @param mostrar_pie \code{logical}. Si \code{TRUE}, agrega un gráfico de pastel (además del
#'   gráfico de barras). Por defecto \code{FALSE}.
#' @param palette \code{character} opcional. Paleta para las categorías. Si \code{NULL}, se
#'   asignan colores por defecto.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{clasificacion_por_token}: \code{data.frame} con columnas \code{Token},
#'         \code{categoria}, \code{positivos}, \code{negativos}, \code{neutros}.
#'   \item \code{resumen_categorias}: tabla con frecuencias y porcentajes por categoría.
#'   \item \code{indicadores}: lista con ICP, ICN, IDL, CN, BPN.
#'   \item \code{grafico_barras}: \code{ggplot} de proporciones por categoría.
#'   \item \code{grafico_pie}: \code{ggplot} de pastel (si \code{mostrar_pie=TRUE}).
#'   \item \code{metodos_usados}: vector de columnas efectivamente utilizadas.
#' }
#'
#' @examples
#' \dontrun{
#' # Ejemplo con tu objeto procesado:
#' mc <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup
#' out <- analizar_consistencia_lexicos(mc, metodos = c("syuzhet","bing","afinn","nrc"))
#' out$indicadores
#' print(out$grafico_barras)
#' }
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs theme_minimal theme element_text
#' @family utilidades
#' @export
analizar_consistencia_lexicos <- function(matriz,
                                          metodos = NULL,
                                          umbral = 0,
                                          mostrar_pie = FALSE,
                                          palette = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere el paquete 'ggplot2'.")
  }
  # Coerción y chequeos
  if (is.matrix(matriz)) matriz <- as.data.frame(matriz, check.names = FALSE)
  if (!is.data.frame(matriz)) stop("`matriz` debe ser data.frame o matrix.")
  if (is.null(rownames(matriz))) stop("`matriz` debe tener rownames (tokens).")

  # Filtrar columnas numéricas y/o metodos
  num_cols <- vapply(matriz, is.numeric, TRUE)
  if (!any(num_cols)) stop("No hay columnas numéricas en `matriz`.")
  df <- matriz[, num_cols, drop = FALSE]
  if (!is.null(metodos)) {
    metodos <- intersect(metodos, colnames(df))
    if (!length(metodos)) stop("Ninguno de los `metodos` está en las columnas numéricas.")
    df <- df[, metodos, drop = FALSE]
  }
  metodos <- colnames(df)

  # Clasificación por lexicón (pos/neg/neu)
  clasif <- apply(df, 2, function(col)
    ifelse(col > umbral, "pos", ifelse(col < umbral, "neg", "neu"))
  )

  # Conteos por token
  positivos <- rowSums(clasif == "pos")
  negativos <- rowSums(clasif == "neg")
  neutros   <- rowSums(clasif == "neu")

  # Categoría final por token
  n_met <- ncol(df)
  categoria <- ifelse(
    positivos == n_met, "Consenso positivo",
    ifelse(negativos == n_met, "Consenso negativo",
           ifelse(positivos > 0 & negativos > 0, "Divergente", "Mixto/neutral"))
  )

  tabla_tokens <- data.frame(
    Token = rownames(df),
    categoria = factor(categoria,
                       levels = c("Consenso positivo","Consenso negativo","Divergente","Mixto/neutral")),
    positivos = positivos,
    negativos = negativos,
    neutros   = neutros,
    stringsAsFactors = FALSE
  )

  # Resumen por categoría
  resumen <- as.data.frame(table(tabla_tokens$categoria))
  names(resumen) <- c("Categoria","Frecuencia")
  total <- sum(resumen$Frecuencia)
  resumen$Porcentaje <- round(100 * resumen$Frecuencia / total, 2)

  # Indicadores
  get_val <- function(cat) {
    v <- resumen$Frecuencia[match(cat, resumen$Categoria)]
    ifelse(is.na(v), 0, v)
  }
  n_cp  <- get_val("Consenso positivo")
  n_cn  <- get_val("Consenso negativo")
  n_div <- get_val("Divergente")
  n_mix <- get_val("Mixto/neutral")

  indicadores <- list(
    ICP = n_cp  / total,                        # Índice de Consistencia Positiva
    ICN = n_cn  / total,                        # Índice de Consistencia Negativa
    IDL = n_div / total,                        # Índice de Divergencia Léxica
    CN  = n_mix / total,                        # Coeficiente de Neutralidad
    BPN = (n_cp - n_cn) / total                 # Balance Positivo/Negativo
  )

  # Paleta
  if (is.null(palette)) {
    palette <- c("Consenso positivo" = "#2E8B57",
                 "Consenso negativo" = "#C0392B",
                 "Divergente"        = "#8E44AD",
                 "Mixto/neutral"     = "#7F8C8D")
  } else {
    # completar si faltan nombres
    if (is.null(names(palette))) {
      names(palette) <- c("Consenso positivo","Consenso negativo","Divergente","Mixto/neutral")[seq_along(palette)]
    }
  }

  # Gráfico de barras
  df_plot <- resumen
  df_plot$Categoria <- factor(df_plot$Categoria,
                              levels = c("Consenso positivo","Consenso negativo","Divergente","Mixto/neutral"))
  g_barras <- ggplot2::ggplot(df_plot,
                              ggplot2::aes(x = Categoria, y = Porcentaje, fill = Categoria)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Porcentaje, "%")),
                       vjust = -0.35, size = 3.5) +
    ggplot2::labs(title = "Consistencia de sentimiento entre lexicones",
                  subtitle = sprintf("Total tokens: %d | Métodos: %s", total, paste(metodos, collapse = ", ")),
                  x = "Categoría", y = "Porcentaje de tokens") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 20, hjust = 1)) +
    ggplot2::scale_fill_manual(values = palette)

  # Opcional: gráfico de pastel
  g_pie <- NULL
  if (isTRUE(mostrar_pie)) {
    # Opcional: gráfico de pastel mejorado
    g_pie <- NULL
    if (isTRUE(mostrar_pie)) {
      df_pie <- df_plot[order(-df_plot$Porcentaje), ]
      df_pie$label <- paste0(df_pie$Categoria, "\n", df_pie$Porcentaje, "%")

      g_pie <- ggplot2::ggplot(df_pie, ggplot2::aes(x = "", y = Porcentaje, fill = Categoria)) +
        ggplot2::geom_col(width = 1, color = "white") +
        ggplot2::coord_polar("y", start = 0) +
        ggplot2::geom_text(ggplot2::aes(label = label),
                           position = ggplot2::position_stack(vjust = 0.5),
                           size = 3.7, color = "white", fontface = "bold") +
        ggplot2::scale_fill_manual(values = palette) +
        ggplot2::labs(title = "Distribución porcentual por categoría",
                      subtitle = "Consistencia de sentimiento entre lexicones",
                      x = NULL, y = NULL) +
        ggplot2::theme_void(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray30"),
          legend.position = "none"
        )
    }

  }

  list(
    clasificacion_por_token = tabla_tokens,
    resumen_categorias      = resumen,
    indicadores             = indicadores,
    grafico_barras          = g_barras,
    grafico_pie             = g_pie,
    metodos_usados          = metodos
  )
}


#' Segmentar y comparar consistencia de sentimiento entre lexicones
#'
#' @description
#' Aplica \code{analizar_consistencia_lexicos()} sobre subconjuntos definidos por
#' una variable de segmentación (por ejemplo, documento, tema o fuente).
#'
#' @param data data.frame con columnas de lexicones + columna de segmento.
#' @param columna_segmento nombre (character) de la columna usada para segmentar.
#' @param metodos vector opcional de nombres de lexicones.
#' @param ... argumentos adicionales pasados a \code{analizar_consistencia_lexicos()}.
#'
#' @return lista con resultados por segmento y un resumen global de indicadores.
#' @examples
#' \dontrun{
#' data_seg <- cbind(dfx[, c("syuzhet","bing","afinn","nrc")],
#'                   Tema = sample(c("Noticias","Tweets","Comentarios"), nrow(dfx), TRUE))
#' seg <- segmentar_consistencia_lexicos(data_seg, "Tema")
#' seg$resumen_global
#' }
#' @family utilidades
#' @export
segmentar_consistencia_lexicos <- function(data, columna_segmento, metodos = NULL, ...) {
  if (!columna_segmento %in% colnames(data))
    stop("La columna de segmentación no existe en los datos.")

  grupos <- unique(data[[columna_segmento]])
  resultados <- list()
  resumen_global <- data.frame()

  for (g in grupos) {
    sub <- data[data[[columna_segmento]] == g, , drop = FALSE]
    rownames(sub) <- rownames(data)[data[[columna_segmento]] == g]

    res <- analizar_consistencia_lexicos(sub[, setdiff(colnames(sub), columna_segmento), drop = FALSE],
                                         metodos = metodos, ...)

    resultados[[g]] <- res
    resumen_global <- rbind(resumen_global,
                            data.frame(
                              Segmento = g,
                              ICP = res$indicadores$ICP,
                              ICN = res$indicadores$ICN,
                              IDL = res$indicadores$IDL,
                              CN  = res$indicadores$CN,
                              BPN = res$indicadores$BPN
                            )
    )
  }

  return(list(
    resultados_segmentados = resultados,
    resumen_global = resumen_global
  ))
}


#' Analisis de consistencia entre lexicos (v2, robusto)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Alinea mc y acdfx por tokens, valida metodos numericos, clasifica tokens por
#' consistencia de polaridad entre metodos, y (opcional) resume perfiles de emociones NRC.
#'
#' @param mc data.frame/matrix con filas = tokens y columnas = metodos lexicos (numericas).
#' @param acdfx data.frame con filas = los mismos tokens y columnas auxiliares
#'   (puede incluir emociones NRC por token y/o columna de segmento).
#' @param metodos character. Subconjunto de columnas de \code{mc} a usar (p.ej. c("syuzhet","bing","afinn","nrc")).
#' @param emociones_cols character|NULL. Nombres de columnas en \code{acdfx} con emociones NRC
#'   (p.ej. c("ira","anticipacion","disgusto","miedo","alegria","tristeza","sorpresa","confianza","positivo","negativo")).
#'   Si es NULL, el modulo de emociones es opcional y no es requerido.
#' @param segmentar_por character|NULL. Nombre de columna en \code{acdfx} para segmentar (opcional).
#' @param mostrar_pie logical. Si TRUE y hay ggplot2, retorna grafico de torta adicional.
#' @return lista con:
#'   \itemize{
#'     \item \code{indicadores}: data.frame con indicadores globales.
#'     \item \code{resumen_categorias}: conteo y proporcion por categoria_lexico.
#'     \item \code{perfil_emociones_por_consistencia}: medias por categoria (si hay emociones).
#'     \item \code{clasificacion_por_token}: data.frame por token con signos y categoria.
#'     \item \code{grafico_barras_consistencia}: ggplot (si ggplot2 disponible).
#'     \item \code{grafico_pie_consistencia}: ggplot (si aplica).
#'     \item \code{grafico_emociones_vs_consistencia}: heatmap ggplot (si hay emociones).
#'   }
#' @family utilidades
#' @export
#' @param "bing" (auto) TODO: describir parámetro.
#' @param "afinn" (auto) TODO: describir parámetro.
#' @param "nrc" (auto) TODO: describir parámetro.
analizar_consistencia_lexicos_v2 <- function(
    mc,
    acdfx,
    metodos = c("syuzhet","bing","afinn","nrc"),
    emociones_cols = NULL,
    segmentar_por = NULL,
    mostrar_pie = TRUE
) {
  # ---- helpers locales ----
  as_df <- function(x) {
    if (is.matrix(x)) x <- as.data.frame(x, check.names = FALSE)
    if (!is.data.frame(x)) stop("`mc` y `acdfx` deben ser data.frame o matrix.")
    x
  }
  var_pos <- function(df) {
    if (is.null(df) || !ncol(df)) return(df)
    keep <- vapply(df, function(v){
      vv <- suppressWarnings(as.numeric(v))
      s <- suppressWarnings(stats::sd(vv, na.rm = TRUE))
      is.finite(s) && s > 0
    }, TRUE)
    if (!any(keep)) return(df[, 0, drop = FALSE])
    df[, keep, drop = FALSE]
  }
  to_num_cols <- function(df) {
    for (nm in colnames(df)) df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    df
  }
  has_pkg <- function(p) isTRUE(requireNamespace(p, quietly = TRUE))

  # ---- coercion y alineacion por tokens ----
  mc <- as_df(mc)
  acdfx <- as_df(acdfx)

  if (is.null(rownames(mc)) || is.null(rownames(acdfx)))
    stop("mc y acdfx deben tener rownames (tokens).")

  tokens_comunes <- intersect(rownames(mc), rownames(acdfx))
  if (!length(tokens_comunes))
    stop("mc y acdfx no comparten tokens.")

  mc <- mc[tokens_comunes, , drop = FALSE]
  acdfx <- acdfx[tokens_comunes, , drop = FALSE]

  # ---- filtrar metodos presentes y numericos ----
  metodos <- intersect(metodos, colnames(mc))
  if (!length(metodos)) stop("Ninguno de los `metodos` existe en mc.")

  # forzar numerico
  mc_num <- to_num_cols(mc[, metodos, drop = FALSE])
  # quitar varianza cero
  mc_num <- var_pos(mc_num)
  metodos_usados <- colnames(mc_num)

  if (length(metodos_usados) < 1)
    stop("Tras filtrar, no quedan metodos numericos con varianza > 0.")

  # si solo queda 1 metodo, se puede continuar (clasificacion basica)
  # si quieres obligatorio >=2, cambia a:
  # if (length(metodos_usados) < 2) stop("Se requieren >= 2 metodos con varianza > 0.")

  # ---- signos por token y clasificacion de consistencia ----
  signos <- as.data.frame(apply(mc_num, 2, function(col) {
    x <- suppressWarnings(as.numeric(col))
    ifelse(is.na(x), NA_real_, ifelse(x > 0, 1, ifelse(x < 0, -1, 0)))
  }), check.names = FALSE)
  rownames(signos) <- rownames(mc_num)

  clasificar_token <- function(v_signos) {
    v <- as.numeric(v_signos)
    if (all(is.na(v))) return("sin_datos")
    v <- v[!is.na(v)]
    if (!length(v)) return("sin_datos")
    if (all(v == 0)) return("neutro")
    # todos los no-cero con el mismo signo
    nz <- v[v != 0]
    if (!length(nz)) return("neutro")
    if (all(nz > 0)) return("cons_pos")
    if (all(nz < 0)) return("cons_neg")
    # mezcla de signos
    "mixto"
  }

  categoria_lexico <- apply(signos, 1, clasificar_token)
  clasificacion_por_token <- data.frame(
    token = rownames(signos),
    categoria = categoria_lexico,
    signos,
    row.names = NULL,
    check.names = FALSE
  )

  # ---- indicadores y resumen por categoria ----
  tab_cat <- as.data.frame(table(clasificacion_por_token$categoria), stringsAsFactors = FALSE)
  colnames(tab_cat) <- c("categoria", "n")
  tab_cat$prop <- round(tab_cat$n / sum(tab_cat$n), 4)

  indicadores <- data.frame(
    n_tokens = nrow(signos),
    n_metodos = length(metodos_usados),
    prop_cons_pos = if ("cons_pos" %in% tab_cat$categoria) tab_cat$prop[tab_cat$categoria == "cons_pos"] else 0,
    prop_cons_neg = if ("cons_neg" %in% tab_cat$categoria) tab_cat$prop[tab_cat$categoria == "cons_neg"] else 0,
    prop_mixto    = if ("mixto"    %in% tab_cat$categoria) tab_cat$prop[tab_cat$categoria == "mixto"]    else 0,
    prop_neutro   = if ("neutro"   %in% tab_cat$categoria) tab_cat$prop[tab_cat$categoria == "neutro"]   else 0,
    prop_sin_datos= if ("sin_datos"%in% tab_cat$categoria) tab_cat$prop[tab_cat$categoria == "sin_datos"]else 0,
    row.names = NULL,
    check.names = FALSE
  )

  # ---- integrar categoria_lexico a acdfx (para posibles segmentaciones) ----
  acdfx$categoria_lexico <- categoria_lexico

  # ---- modulo de emociones (opcional) ----
  usar_emociones <- FALSE
  perfil_emociones <- NULL
  if (is.null(emociones_cols)) {
    # detectar automaticamente si existen columnas NRC
    posibles <- c("ira","anticipacion","disgusto","miedo","alegria",
                  "tristeza","sorpresa","confianza","positivo","negativo")
    emociones_cols <- intersect(posibles, colnames(acdfx))
  }
  if (length(emociones_cols) > 0) {
    # forzar numerico y NA->0
    emo_df <- as.data.frame(lapply(acdfx[, emociones_cols, drop = FALSE], function(v) {
      x <- suppressWarnings(as.numeric(v)); x[is.na(x)] <- 0; x
    }), check.names = FALSE)
    # promedio por categoria_lexico
    cats <- unique(acdfx$categoria_lexico)
    perfil_emociones <- do.call(rbind, lapply(cats, function(cc) {
      idx <- which(acdfx$categoria_lexico == cc)
      if (!length(idx)) return(NULL)
      colMeans(emo_df[idx, , drop = FALSE], na.rm = TRUE)
    }))
    if (!is.null(perfil_emociones)) {
      perfil_emociones <- as.data.frame(perfil_emociones, check.names = FALSE)
      perfil_emociones$categoria <- rownames(perfil_emociones)
      rownames(perfil_emociones) <- NULL
      usar_emociones <- TRUE
    }
  }

  # ---- graficos (si ggplot2 disponible) ----
  grafico_barras <- NULL
  grafico_pie <- NULL
  grafico_heat <- NULL

  if (has_pkg("ggplot2")) {
    gg <- ggplot2::ggplot
    aes <- ggplot2::aes
    geom_col <- ggplot2::geom_col
    theme_minimal <- ggplot2::theme_minimal
    labs <- ggplot2::labs
    coord_polar <- ggplot2::coord_polar
    geom_tile <- ggplot2::geom_tile
    scale_fill_gradient <- ggplot2::scale_fill_gradient
    theme <- ggplot2::theme
    element_text <- ggplot2::element_text

    # barras categorias
    df_barras <- tab_cat
    df_barras$categoria <- factor(df_barras$categoria,
                                  levels = c("cons_pos","cons_neg","mixto","neutro","sin_datos"))
    grafico_barras <- gg(df_barras, aes(x = categoria, y = n)) +
      geom_col() +
      theme_minimal() +
      labs(title = "Consistencia por categoria lexica", x = "Categoria", y = "Frecuencia")

    # pie (opcional)
    if (isTRUE(mostrar_pie)) {
      df_pie <- df_barras
      df_pie$prop_pct <- df_pie$prop * 100
      grafico_pie <- gg(df_pie, aes(x = "", y = prop, fill = categoria)) +
        geom_col(width = 1) +
        coord_polar(theta = "y") +
        theme_minimal() +
        labs(title = "Distribucion de categorias (proporcion)") +
        theme(axis.title.x = element_text(color = NA),
              axis.text.x = element_text(color = NA),
              axis.ticks.x = element_text(color = NA))
    }

    # heatmap emociones vs categoria (si hay emociones)
    if (isTRUE(usar_emociones) && !is.null(perfil_emociones)) {
      df_heat <- reshape2::melt(perfil_emociones,
                                id.vars = "categoria",
                                variable.name = "emocion",
                                value.name = "valor")
      grafico_heat <- gg(df_heat, aes(x = emocion, y = categoria, fill = valor)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        theme_minimal() +
        labs(title = "Perfil de emociones por categoria lexica",
             x = "Emocion", y = "Categoria")
    }
  }

  # ---- segmentacion opcional ----
  resumen_segmentos <- NULL
  if (!is.null(segmentar_por) && segmentar_por %in% colnames(acdfx)) {
    seg <- acdfx[[segmentar_por]]
    resumen_segmentos <- as.data.frame.matrix(
      stats::xtabs(~ seg + acdfx$categoria_lexico)
    )
    # podrias agregar proporciones por fila/columna si lo deseas
  }

  # ---- salida ----
  out <- list(
    indicadores = indicadores,
    resumen_categorias = tab_cat,
    perfil_emociones_por_consistencia = perfil_emociones,
    clasificacion_por_token = clasificacion_por_token,
    grafico_barras_consistencia = grafico_barras,
    grafico_pie_consistencia = grafico_pie,
    grafico_emociones_vs_consistencia = grafico_heat,
    metodos_usados = metodos_usados,
    resumen_segmentacion = resumen_segmentos
  )
  return(out)
}
