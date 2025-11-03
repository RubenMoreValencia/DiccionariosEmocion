#' Visualizar curvas de sentimiento acumulado con resumen
#'
#' @description
#' Grafica curvas TDC (trazo de contenido) de sentimiento para varios métodos
#' (p. ej., Syuzhet/Bing/Afinn/NRC) y devuelve estadísticas básicas (promedio,
#' máximo, mínimo) por método. La función intenta encontrar el vector de cada
#' método aun si el nombre en \code{lista_resultados} varía en mayúsculas o separadores.
#'
#' @param lista_resultados Lista (típicamente salida de \code{UnirExcelITFF_Optimizada()}).
#'   Debe contener, para cada método, un vector con la curva acumulada, por ejemplo:
#'   \code{TDCSyuzhet}, \code{TDCBing}, \code{TDCAfinn}, \code{TDCNrc}.
#' @param metodos Character. Métodos a incluir. Por defecto
#'   \code{c("syuzhet","bing","afinn","nrc")}.
#' @param rescale_values Logical. Si \code{TRUE}, reescala cada curva a \code{[0,1]} para
#'   comparabilidad visual. Por defecto \code{FALSE}.
#' @param smooth_loess Logical. Si \code{TRUE}, agrega una curva LOESS por método.
#'   Por defecto \code{FALSE}.
#' @param span Numeric \code{(0,1]}. Span del LOESS si \code{smooth_loess=TRUE}. Por defecto \code{0.25}.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{grafico}: objeto \code{ggplot} con las curvas por método.
#'   \item \code{resumen_estadistico}: \code{data.frame} con Promedio, Máximo y Mínimo por método.
#'   \item \code{metodos_encontrados}: nombres efectivamente incluidos.
#' }
#'
#' @examples
#' \dontrun{
#' out <- visualizar_curvas_sentimiento(resultados)
#' print(out$grafico)
#' out$resumen_estadistico
#' }
#' @importFrom ggplot2 ggplot aes geom_line theme_minimal labs theme
#' @importFrom ggplot2 guide_legend guides
#' @importFrom scales rescale
#' @family visualizacion
#' @export
#' @param "bing" (auto) TODO: describir parámetro.
#' @param "afinn" (auto) TODO: describir parámetro.
#' @param "nrc" (auto) TODO: describir parámetro.
visualizar_curvas_sentimiento <- function(
    lista_resultados,
    metodos = c("syuzhet","bing","afinn","nrc"),
    rescale_values = FALSE,
    smooth_loess   = FALSE,
    span           = 0.25
) {
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Se requiere el paquete 'ggplot2'.")

  # helper para encontrar una curva dado el nombre del método
  # busca variantes típicas de nombres: TDCSyuzhet, TDC_Syuzhet, tdc_syuzhet, etc.
  localizar_curva <- function(lst, metodo) {
    base <- paste0(toupper(substr(metodo,1,1)), substr(metodo,2,nchar(metodo))) # "Syuzhet"
    cand <- c(
      paste0("TDC", base),            # TDCSyuzhet
      paste0("TDC_", base),           # TDC_Syuzhet
      paste0("tdc", base),            # tdcSyuzhet
      paste0("tdc_", base),           # tdc_Syuzhet
      paste0("TDC", tolower(metodo)), # TDCsyuzhet
      paste0("tdc", tolower(metodo))  # tdcsyuzhet
    )
    # también aceptar nombres ya exactos del vector
    cand <- unique(c(cand, metodo, base, paste0("TDC", metodo)))
    for (nm in cand) {
      if (!is.null(lst[[nm]]) && is.numeric(lst[[nm]])) return(lst[[nm]])
    }
    return(NULL)
  }

  data_plot <- list()
  resumen   <- list()
  met_ok    <- character(0)

  for (met in metodos) {
    curva <- localizar_curva(lista_resultados, met)
    if (is.null(curva)) {
      message(sprintf("Aviso: no se encontró curva para método '%s'. Omitiendo.", met))
      next
    }
    v <- as.numeric(curva)
    if (isTRUE(rescale_values)) v <- scales::rescale(v, to = c(0,1), from = range(v, na.rm = TRUE))

    df <- data.frame(
      Posicion = seq_along(v),
      Valor    = v,
      Metodo   = met,
      stringsAsFactors = FALSE
    )
    data_plot[[length(data_plot)+1]] <- df
    resumen[[length(resumen)+1]] <- data.frame(
      Metodo   = met,
      Promedio = round(mean(v, na.rm = TRUE), 3),
      Maximo   = round(max(v,  na.rm = TRUE), 3),
      Minimo   = round(min(v,  na.rm = TRUE), 3),
      stringsAsFactors = FALSE
    )
    met_ok <- c(met_ok, met)
  }

  if (length(data_plot) == 0L) stop("No se encontraron curvas para los métodos solicitados.")
  data_plot <- do.call(rbind, data_plot)
  resumen   <- do.call(rbind, resumen)

  g <- ggplot2::ggplot(data_plot, ggplot2::aes(x = Posicion, y = Valor, color = Metodo)) +
    ggplot2::geom_line(linewidth = 1.05, na.rm = TRUE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(
      title = "Curvas de sentimiento acumulado por método",
      subtitle = paste("Promedios:", paste0(resumen$Metodo, ": ", resumen$Promedio, collapse = ", ")),
      x = "Posición en texto",
      y = if (rescale_values) "Sentimiento (reescalado 0–1)" else "Sentimiento acumulado",
      color = "Método léxico"
    ) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Método"))

  if (isTRUE(smooth_loess)) {
    # loess por método
    g <- g + ggplot2::geom_line(
      data = data_plot,
      ggplot2::aes(y = stats::predict(stats::loess(Valor ~ Posicion), newdata = data_plot["Posicion"], group = Metodo)),
      linewidth = 0.8, alpha = 0.6
    )
    # Nota: para spans distintos por método puedes facetear, pero mantenemos simple aquí.
  }

  list(
    grafico             = g,
    resumen_estadistico = resumen,
    metodos_encontrados = met_ok
  )
}


#' Título (auto) — reemplazar por uno descriptivo.
#'
#' @title Visualizar emociones NRC por sentimiento
#' @description Muestra un gráfico de barras de la intensidad relativa (porcentual) de cada emoción NRC (excluye positivo y negativo) en un idioma específico. Calcula porcentajes por separado para emociones y polaridades.
#' @param matriz_emociones data.frame con conteo de emociones por palabra (como VectorTextoSentimientosNRC)
#' @param idioma destino del idioma de las emociones (por ejemplo, "es" para español, "pt" para portugués)
#' @param funcion_traduccion función que traduzca los nombres de las emociones (debe aceptar vector y devolver vector)
#' @return una lista con un gráfico de barras ggplot de emociones básicas, y dos data.frames con porcentajes por grupo
#' @examples
#' visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC, idioma = "es", funcion_traduccion = traducir_emociones)
#' @family matrices_valoradas
#' @export
visualizar_emociones_nrc <- function(matriz_emociones, idioma = "es", funcion_traduccion = NULL) {
  resultado <- list()
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Se requiere el paquete ggplot2")
  if (!is.data.frame(matriz_emociones)) stop("La matriz de emociones debe ser un data.frame")

  suma_emociones <- colSums(matriz_emociones, na.rm = TRUE)
  df_all <- data.frame(
    Emocion = names(suma_emociones),
    Frecuencia = suma_emociones,
    stringsAsFactors = FALSE
  )

  df_polares <- df_all[df_all$Emocion %in% c("positive", "negative"), ]
  df_emociones <- df_all[!df_all$Emocion %in% c("positive", "negative"), ]

  # Porcentajes relativos por grupo
  total_emociones <- sum(df_emociones$Frecuencia)
  total_polares <- sum(df_polares$Frecuencia)

  df_emociones$Porcentaje <- round(100 * df_emociones$Frecuencia / total_emociones, 2)
  df_polares$Porcentaje <- round(100 * df_polares$Frecuencia / total_polares, 2)

  if (!is.null(funcion_traduccion) && is.function(funcion_traduccion)) {
    df_emociones$Emocion <- funcion_traduccion(df_emociones$Emocion, idioma = idioma)
    df_polares$Emocion <- funcion_traduccion(df_polares$Emocion, idioma = idioma)
  }

  g <- ggplot2::ggplot(df_emociones, ggplot2::aes(x = reorder(Emocion, -Frecuencia), y = Porcentaje, fill = Emocion)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = paste0(Porcentaje, "%")), vjust = -0.3, size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Distribución porcentual de emociones NRC (idioma:", idioma, ")"),
      x = "Emoción",
      y = "Porcentaje (%)"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  resultado$grafico <- g
  resultado$resumen <- df_emociones
  resultado$polaridades <- df_polares
  return(resultado)
}

#' Visualizar emociones NRC por sentimiento (con opciones de normalización y traducción)
#'
#' @description
#' Muestra un gráfico de barras de la intensidad relativa (porcentual) o absoluta (frecuencia)
#' de cada emoción NRC (excluye \code{"positive"} y \code{"negative"}), y calcula
#' porcentajes por separado para emociones y polaridades. Permite traducir las etiquetas de
#' emociones pasando una función de traducción.
#'
#' @param matriz_emociones \code{data.frame} o \code{matrix} con conteos por emoción
#'   (por columnas) – típico de \code{resultados$VectorTextoSentimientosNRC}.
#' @param idioma \code{character}. Código/descriptor de idioma de destino para las
#'   etiquetas (p.ej., `"es"`, `"pt"`). Solo se usa si \code{funcion_traduccion} no es \code{NULL}.
#' @param funcion_traduccion \code{function} opcional para traducir el vector de nombres
#'   de emociones. Debe aceptar al menos un vector de \code{character}. Si además acepta
#'   argumento \code{idioma}, se intentará llamar como \code{func(vec, idioma=idioma)}.
#' @param normalizar \code{c("porcentaje","frecuencia")}. Modo de la barra: porcentaje dentro
#'   del grupo de emociones (no incluye polaridades) o frecuencia absoluta. Por defecto `"porcentaje"`.
#' @param top_n \code{integer} opcional. Si se indica, muestra solo las \code{top_n} emociones
#'   con mayor valor (según \code{normalizar}).
#' @param palette \code{character} opcional. Vector de colores para \code{fill}; si \code{NULL}
#'   usa \code{viridisLite::viridis(n)}.
#' @param digits \code{integer}. Decimales para el porcentaje. Por defecto \code{2}.
#' @param mostrar_polaridades \code{logical}. Si \code{TRUE}, devuelve también un gráfico de
#'   polaridades (positivo/negativo). Por defecto \code{FALSE}.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{grafico_emociones}: \code{ggplot} de emociones (sin positivo/negativo).
#'   \item \code{tabla_emociones}: \code{data.frame} con \code{Emocion}, \code{Frecuencia}, \code{Porcentaje}.
#'   \item \code{tabla_polaridades}: \code{data.frame} de polaridades (si existen columnas).
#'   \item \code{grafico_polaridades}: \code{ggplot} de polaridades (solo si \code{mostrar_polaridades=TRUE}).
#' }
#'
#' @examples
#' \dontrun{
#' # Uso básico con salida de UnirExcelITFF_Optimizada():
#' out <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC, idioma = "es")
#' out$grafico_emociones
#' out$tabla_emociones
#'
#' # Con top-5 y porcentajes:
#' out <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC, top_n = 5)
#'
#' # Mostrar también polaridades y sin normalizar (frecuencia):
#' out <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC,
#'                                 normalizar = "frecuencia", mostrar_polaridades = TRUE)
#'
#' # Con función de traducción:
#' traducir_emociones <- function(x, idioma="es") {
#'   dic <- c(anger="ira", anticipation="anticipación", disgust="disgusto", fear="miedo",
#'            joy="alegría", sadness="tristeza", surprise="sorpresa", trust="confianza",
#'            positive="positivo", negative="negativo")
#' unname(ifelse(x \%in\% names(dic), dic[x], x))
#' }
#' out <- visualizar_emociones_nrc(resultados$VectorTextoSentimientosNRC,
#'                                 idioma="es", funcion_traduccion=traducir_emociones)
#' }
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs theme_minimal theme element_text
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom viridisLite viridis
#' @export
#' @param "frecuencia" (auto) TODO: describir parámetro.
visualizar_emociones_nrc <- function(matriz_emociones,
                                     idioma = "es",
                                     funcion_traduccion = NULL,
                                     normalizar = c("porcentaje","frecuencia"),
                                     top_n = NULL,
                                     palette = NULL,
                                     digits = 2,
                                     mostrar_polaridades = FALSE) {
  normalizar <- match.arg(normalizar)
  if (!is.data.frame(matriz_emociones) && !is.matrix(matriz_emociones)) {
    stop("`matriz_emociones` debe ser data.frame o matrix.")
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere el paquete 'ggplot2'.")
  }

  # Asegurar nombres de columnas
  mat <- as.data.frame(matriz_emociones, check.names = FALSE)
  if (is.null(colnames(mat))) stop("`matriz_emociones` debe tener nombres de columnas (emociones).")

  # Sumas por emoción
  suma_emociones <- colSums(mat, na.rm = TRUE)
  df_all <- data.frame(
    Emocion = names(suma_emociones),
    Frecuencia = as.numeric(suma_emociones),
    stringsAsFactors = FALSE
  )

  # Separar polaridades y emociones básicas
  idx_polares <- df_all$Emocion %in% c("positive", "negative")
  df_polares  <- df_all[idx_polares, , drop = FALSE]
  df_emociones <- df_all[!idx_polares, , drop = FALSE]

  # Protección si no hay emociones (p.ej., solo polaridades)
  if (nrow(df_emociones) == 0) stop("No hay columnas de emociones NRC (solo polaridades).")

  # Porcentajes relativos al grupo de emociones / polaridades
  total_emociones <- sum(df_emociones$Frecuencia, na.rm = TRUE)
  total_polares   <- sum(df_polares$Frecuencia,   na.rm = TRUE)

  df_emociones$Porcentaje <- if (total_emociones > 0) {
    round(100 * df_emociones$Frecuencia / total_emociones, digits)
  } else { 0 }

  if (nrow(df_polares) > 0) {
    df_polares$Porcentaje <- if (total_polares > 0) {
      round(100 * df_polares$Frecuencia / total_polares, digits)
    } else { 0 }
  }

  # Traducción (si procede)
  if (!is.null(funcion_traduccion) && is.function(funcion_traduccion)) {
    # intentar con idioma si lo soporta
    try_with_lang <- try(funcion_traduccion(df_emociones$Emocion, idioma = idioma), silent = TRUE)
    if (!inherits(try_with_lang, "try-error")) {
      df_emociones$Emocion <- try_with_lang
      if (nrow(df_polares) > 0) df_polares$Emocion <- funcion_traduccion(df_polares$Emocion, idioma = idioma)
    } else {
      df_emociones$Emocion <- funcion_traduccion(df_emociones$Emocion)
      if (nrow(df_polares) > 0) df_polares$Emocion <- funcion_traduccion(df_polares$Emocion)
    }
  }

  # Orden y top-n
  if (identical(normalizar, "porcentaje")) {
    df_emociones <- df_emociones[order(-df_emociones$Porcentaje, df_emociones$Emocion), , drop = FALSE]
  } else {
    df_emociones <- df_emociones[order(-df_emociones$Frecuencia, df_emociones$Emocion), , drop = FALSE]
  }
  if (!is.null(top_n)) {
    top_n <- as.integer(top_n)
    if (top_n >= 1L) df_emociones <- head(df_emociones, top_n)
  }

  # Paleta
  if (is.null(palette)) {
    palette <- viridisLite::viridis(n = nrow(df_emociones))
  } else if (length(palette) < nrow(df_emociones)) {
    palette <- rep_len(palette, length.out = nrow(df_emociones))
  }

  # Variable y etiqueta según normalización
  y_var <- if (identical(normalizar, "porcentaje")) "Porcentaje" else "Frecuencia"
  y_lab <- if (identical(normalizar, "porcentaje")) "Porcentaje (%)" else "Frecuencia"

  # Reordenar factor por valor mostrado
  df_emociones$Emocion <- factor(df_emociones$Emocion, levels = df_emociones$Emocion)

  g <- ggplot2::ggplot(df_emociones, ggplot2::aes(x = Emocion, y = .data[[y_var]], fill = Emocion)) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = if (y_var == "Porcentaje") paste0(.data[[y_var]], "%") else .data[[y_var]]),
                       vjust = -0.3, size = 3) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = sprintf("Distribución %s de emociones NRC (idioma: %s)",
                      if (y_var == "Porcentaje") "porcentual" else "absoluta", idioma),
      x = "Emoción",
      y = y_lab
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, .1))) +
    ggplot2::scale_fill_manual(values = palette)

  res <- list(
    grafico_emociones   = g,
    tabla_emociones     = df_emociones,
    tabla_polaridades   = df_polares
  )

  if (isTRUE(mostrar_polaridades) && nrow(df_polares) > 0) {
    df_polares$Emocion <- factor(df_polares$Emocion, levels = df_polares$Emocion)
    gp <- ggplot2::ggplot(df_polares, ggplot2::aes(x = Emocion,
                                                   y = if (y_var == "Porcentaje") Porcentaje else Frecuencia,
                                                   fill = Emocion)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::geom_text(ggplot2::aes(label =
                                        if (y_var == "Porcentaje") paste0(Porcentaje, "%") else Frecuencia),
                         vjust = -0.3, size = 3) +
      ggplot2::theme_minimal() +
      ggplot2::labs(
        title = "Polaridades NRC",
        x = NULL, y = y_lab
      )
    res$grafico_polaridades <- gp
  }

  res
}

#' Analizar y visualizar tokens extremos por lexicón (lollipop + tablas)
#'
#' @description
#' Identifica los \emph{tokens} más positivos y más negativos en una matriz comparativa
#' de lexicones y los muestra en un gráfico tipo \emph{lollipop} y tablas. Soporta:
#' selección de métodos, traducción opcional de tokens, escalamiento por método y
#' control de salida (sin imprimir por consola).
#'
#' @param matriz_comparativa \code{data.frame} o \code{matrix} con columnas por lexicón
#'   (por ejemplo: \code{syuzhet}, \code{bing}, \code{afinn}, \code{nrc}) y filas como tokens.
#'   Debe tener \code{rownames} con los tokens (si no, se generan índices).
#' @param n \code{integer}. Número de tokens extremos a mostrar por cada extremo y método.
#'   Por defecto \code{10}.
#' @param idioma \code{character} opcional. Código/descriptor de idioma para traducir tokens
#'   (por ejemplo, \code{"es"}). Se usa solo si \code{funcion_traduccion} no es \code{NULL}.
#' @param funcion_traduccion \code{function} opcional que traduzca un vector de \code{character}.
#'   Si además acepta argumento \code{idioma}, se intentará llamar como
#'   \code{funcion_traduccion(x, idioma = idioma)}.
#' @param metodos \code{character} opcional. Subconjunto de columnas a procesar. Si \code{NULL},
#'   se usan todas las columnas numéricas.
#' @param scale_method \code{c("none","zscore","range01")}. Escalamiento de cada método antes
#'   de seleccionar extremos: \code{"none"} (sin escalamiento), \code{"zscore"} (media 0, sd 1),
#'   \code{"range01"} (reescala a `[0, 1]` por método). Por defecto \code{"none"}.
#' @param facet \code{logical}. Si \code{TRUE}, facetear por método en el gráfico. Por defecto \code{TRUE}.
#' @param print_tables \code{logical}. Si \code{TRUE}, imprime tablas en consola (útil en notebooks).
#'   Por defecto \code{FALSE}. (No requiere \pkg{knitr}; puedes formatear tú con \code{knitr::kable}).
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{tabla_positivos}: lista de data.frames por método (Top positivos).
#'   \item \code{tabla_negativos}: lista de data.frames por método (Top negativos).
#'   \item \code{tabla_combinada}: data.frame largo con columnas \code{Metodo}, \code{Token}, \code{Valor}, \code{Tipo}, \code{Rank}.
#'   \item \code{grafico}: objeto \code{ggplot} con el lollipop.
#'   \item \code{metodos_usados}: vector de métodos efectivamente procesados.
#' }
#'
#' @examples
#' \dontrun{
#' # Usando tu objeto:
#' mc <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuestaSinDup
#' out <- analizar_tokens_extremos(mc, n = 12, metodos = c("syuzhet","bing","afinn","nrc"),
#'                                 scale_method = "zscore")
#' print(out$grafico)
#' head(out$tabla_combinada)
#'
#' # Con traducción de tokens:
#' traducir <- function(x, idioma="es") x  # reemplaza por tu traductor
#' out2 <- analizar_tokens_extremos(mc, n = 8, funcion_traduccion = traducir, idioma = "es")
#' }
#' @importFrom ggplot2 ggplot aes geom_segment geom_point coord_flip labs theme_minimal theme element_blank element_text
#' @family utilidades
#' @export
#' @param "zscore" (auto) TODO: describir parámetro.
#' @param "range01" (auto) TODO: describir parámetro.
analizar_tokens_extremos <- function(matriz_comparativa,
                                     n = 10,
                                     idioma = NULL,
                                     funcion_traduccion = NULL,
                                     metodos = NULL,
                                     scale_method = c("none","zscore","range01"),
                                     facet = TRUE,
                                     print_tables = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Se requiere el paquete 'ggplot2'.")
  }
  scale_method <- match.arg(scale_method)

  # Coerción a data.frame y chequeos
  if (is.matrix(matriz_comparativa)) {
    matriz_comparativa <- as.data.frame(matriz_comparativa, check.names = FALSE)
  }
  if (!is.data.frame(matriz_comparativa)) {
    stop("`matriz_comparativa` debe ser data.frame o matrix.")
  }
  if (is.null(rownames(matriz_comparativa))) {
    rownames(matriz_comparativa) <- paste0("token_", seq_len(nrow(matriz_comparativa)))
  }

  # Filtrar columnas numéricas y/o metodos solicitados
  num_cols <- vapply(matriz_comparativa, is.numeric, TRUE)
  if (!any(num_cols)) stop("No hay columnas numéricas en `matriz_comparativa`.")
  df <- matriz_comparativa[, num_cols, drop = FALSE]

  if (!is.null(metodos)) {
    metodos <- intersect(metodos, colnames(df))
    if (length(metodos) == 0) stop("Ninguno de los `metodos` está en las columnas numéricas.")
    df <- df[, metodos, drop = FALSE]
  }
  metodos <- colnames(df)

  # Función local de traducción de tokens (si procede)
  traducir_tokens <- function(x) {
    if (!is.null(funcion_traduccion)) {
      try_lang <- try(funcion_traduccion(x, idioma = idioma), silent = TRUE)
      if (!inherits(try_lang, "try-error")) return(try_lang)
      return(funcion_traduccion(x))
    }
    x
  }

  # Escalamiento por método
  if (scale_method != "none") {
    for (m in metodos) {
      v <- df[[m]]
      if (scale_method == "zscore") {
        mu <- mean(v, na.rm = TRUE); s <- stats::sd(v, na.rm = TRUE)
        df[[m]] <- if (isTRUE(all.equal(s, 0)) || is.na(s)) v*0 else (v - mu)/s
      } else if (scale_method == "range01") {
        r <- range(v, na.rm = TRUE)
        if (r[1] == r[2]) df[[m]] <- v*0 else df[[m]] <- (v - r[1]) / (r[2] - r[1])
      }
    }
  }

  # Construcción de tablas top
  tokens <- rownames(df)
  pos_list <- list(); neg_list <- list(); long_rows <- list()

  for (m in metodos) {
    v <- df[[m]]
    ok <- !is.na(v)
    v_ok <- v[ok]; tok_ok <- tokens[ok]
    if (!length(v_ok)) next

    ord_pos <- order(v_ok, decreasing = TRUE)
    ord_neg <- order(v_ok, decreasing = FALSE)

    nn <- min(n, length(v_ok))
    top_pos_idx <- ord_pos[seq_len(nn)]
    top_neg_idx <- ord_neg[seq_len(nn)]

    pos_df <- data.frame(Token = tok_ok[top_pos_idx],
                         Valor = as.numeric(v_ok[top_pos_idx]),
                         Metodo = m,
                         Tipo   = "Positivo",
                         Rank   = seq_len(nn),
                         stringsAsFactors = FALSE)

    neg_df <- data.frame(Token = tok_ok[top_neg_idx],
                         Valor = as.numeric(v_ok[top_neg_idx]),
                         Metodo = m,
                         Tipo   = "Negativo",
                         Rank   = seq_len(nn),
                         stringsAsFactors = FALSE)

    # Traducción de tokens (si se pide)
    pos_df$Token <- traducir_tokens(pos_df$Token)
    neg_df$Token <- traducir_tokens(neg_df$Token)

    pos_list[[m]] <- pos_df[, c("Token","Valor")]
    neg_list[[m]] <- neg_df[, c("Token","Valor")]

    long_rows[[length(long_rows) + 1]] <- pos_df
    long_rows[[length(long_rows) + 1]] <- neg_df

    if (isTRUE(print_tables)) {
      cat("\n---\n**", m, ": Tokens más POSITIVOS**\n", sep = "")
      print(pos_df[, c("Token","Valor")], row.names = FALSE)
      cat("\n**", m, ": Tokens más NEGATIVOS**\n", sep = "")
      print(neg_df[, c("Token","Valor")], row.names = FALSE)
    }
  }

  if (!length(long_rows)) stop("No se pudieron construir tablas de extremos (¿todo NA?).")

  long_df <- do.call(rbind, long_rows)

  # Orden para lollipop dentro de cada método
  long_df$Token <- as.factor(long_df$Token)
  # Ordenar tokens por |Valor| decreciente dentro de (Metodo, Tipo)
  long_df <- long_df[order(long_df$Metodo, long_df$Tipo, -abs(long_df$Valor), long_df$Rank), , drop = FALSE]

  # Gráfico lollipop
  p <- ggplot2::ggplot(long_df, ggplot2::aes(x = Token, y = Valor, color = Tipo)) +
    ggplot2::geom_segment(ggplot2::aes(x = Token, xend = Token, y = 0, yend = Valor), linewidth = 0.8) +
    ggplot2::geom_point(size = 2.8) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Tokens extremos por método (lollipop)",
                  y = "Valor de sentimiento", x = "Token") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(vjust = 1),
      axis.title.x = ggplot2::element_text(vjust = 0)
    )

  if (facet) {
    p <- p + ggplot2::theme(strip.text = ggplot2::element_text(face = "bold")) +
      ggplot2::facet_wrap(~ Metodo, scales = "free_y")
  }

  list(
    tabla_positivos = pos_list,
    tabla_negativos = neg_list,
    tabla_combinada = long_df[, c("Metodo","Token","Valor","Tipo","Rank")],
    grafico = p,
    metodos_usados = metodos
  )
}

#' Visualizar matriz de co-ocurrencia NRC (heatmap o red)
#'
#' @description
#' Muestra la co-ocurrencia de emociones NRC como \emph{heatmap} o \emph{red}.
#' Permite normalización, ordenamiento y filtrado por peso mínimo/top-M aristas.
#'
#' @param matriz \code{matrix}/\code{data.frame} cuadrada con nombres de fila/columna (emociones).
#' @param tipo \code{c("heatmap","red")}. Tipo de visualización.
#' @param normalizar \code{c("none","prop","max")}. \code{"prop"} divide por la suma total;
#'   \code{"max"} divide por el valor máximo; \code{"none"} no normaliza. Por defecto \code{"none"}.
#' @param diag_cero \code{logical}. Si \code{TRUE}, pone la diagonal en 0 (evita auto-coocurrencia).
#'   Por defecto \code{TRUE}.
#' @param ordenar \code{c("none","sum","hclust")}. Reordena emociones por suma decreciente o clustering
#'   jerárquico (\code{hclust}) sobre la matriz simétrica. Por defecto \code{"hclust"}.
#' @param palette \code{character} o vector de colores. Si \code{NULL}, usa \code{viridis}.
#' @param min_edge \code{numeric}. Umbral mínimo de peso de arista para la red (excluye menores).
#'   Por defecto \code{0}.
#' @param top_edges \code{integer} opcional. Si se indica, mantiene solo las \code{top_edges} aristas
#'   más pesadas (tras aplicar \code{min_edge}).
#' @param layout_red \code{character}. Layout de \pkg{ggraph} para la red. Por defecto \code{"fr"}.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{matriz_procesada}: matriz tras normalización/orden/diagonal.
#'   \item \code{datos_long}: \code{data.frame} largo (para heatmap).
#'   \item \code{edges}: \code{data.frame} de aristas (para red).
#'   \item \code{grafico}: objeto \code{ggplot}.
#' }
#'
#' @examples
#' \dontrun{
#' # Suponiendo que ya construiste 'cooc' (matriz de co-ocurrencia NRC):
#' v1 <- visualizar_coocurrencia_nrc(cooc, tipo = "heatmap", normalizar = "prop")
#' print(v1$grafico)
#'
#' v2 <- visualizar_coocurrencia_nrc(cooc, tipo = "red", normalizar = "max",
#'                                   min_edge = 0.05, top_edges = 50, layout_red = "fr")
#' print(v2$grafico)
#' }
#' @importFrom ggplot2 ggplot aes geom_tile geom_text scale_fill_gradient scale_fill_viridis_c
#' @importFrom ggplot2 labs theme_minimal theme element_text coord_polar geom_col element_blank
#' @importFrom tidyr pivot_longer
#' @family matrices_valoradas
#' @export
#' @param "red" (auto) TODO: describir parámetro.
visualizar_coocurrencia_nrc <- function(matriz,
                                        tipo = c("heatmap","red"),
                                        normalizar = c("none","prop","max"),
                                        diag_cero = TRUE,
                                        ordenar = c("hclust","sum","none"),
                                        palette = NULL,
                                        min_edge = 0,
                                        top_edges = NULL,
                                        layout_red = "fr") {
  tipo        <- match.arg(tipo)
  normalizar  <- match.arg(normalizar)
  ordenar     <- match.arg(ordenar)

  if (is.data.frame(matriz)) matriz <- as.matrix(matriz)
  if (!is.matrix(matriz)) stop("`matriz` debe ser matrix/data.frame cuadrada.")
  if (nrow(matriz) != ncol(matriz)) stop("`matriz` debe ser cuadrada.")
  if (is.null(rownames(matriz)) || is.null(colnames(matriz)))
    stop("`matriz` debe tener rownames y colnames (emociones).")

  # Copia segura
  M <- matriz
  mode(M) <- "numeric"

  # Normalización
  if (normalizar == "prop") {
    s <- sum(M, na.rm = TRUE); if (s > 0) M <- M / s
  } else if (normalizar == "max") {
    mx <- max(M, na.rm = TRUE); if (mx > 0) M <- M / mx
  }

  # Diagonal a cero (opcional)
  if (isTRUE(diag_cero)) diag(M) <- 0

  # Ordenar
  if (ordenar == "sum") {
    ord <- order(rowSums(M, na.rm = TRUE), decreasing = TRUE)
    M <- M[ord, ord, drop = FALSE]
  } else if (ordenar == "hclust") {
    # Asegurar simetría (si viene no simétrica)
    Msym <- (M + t(M)) / 2
    d <- try(stats::as.dist(1 - stats::cor(t(Msym) + 1e-12)), silent = TRUE)
    if (inherits(d, "try-error")) {
      d <- stats::dist(Msym) # fallback
    }
    hc <- stats::hclust(d, method = "average")
    ord <- hc$order
    M <- M[ord, ord, drop = FALSE]
  }

  # Paleta
  use_viridis <- is.null(palette)
  # Datos largos para heatmap
  df_long <- data.frame(Emocion1 = rep(rownames(M), times = ncol(M)),
                        Emocion2 = rep(colnames(M), each = nrow(M)),
                        Frecuencia = as.numeric(M),
                        stringsAsFactors = FALSE)

  if (tipo == "heatmap") {
    g <- ggplot2::ggplot(df_long, ggplot2::aes(x = Emocion1, y = Emocion2, fill = Frecuencia)) +
      ggplot2::geom_tile(color = "white") +
      {
        if (use_viridis) ggplot2::scale_fill_viridis_c()
        else ggplot2::scale_fill_gradient(low = palette[1], high = tail(palette, 1))
      } +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Matriz de co-ocurrencia de emociones NRC",
        subtitle = paste("Normalización:", normalizar, "| Orden:", ordenar),
        x = NULL, y = NULL, fill = if (normalizar == "none") "Frecuencia" else "Intensidad"
      ) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid = ggplot2::element_blank()
      )

    return(list(
      matriz_procesada = M,
      datos_long = df_long,
      edges = NULL,
      grafico = g
    ))
  }

  # ---- modo RED ----
  if (!requireNamespace("igraph", quietly = TRUE) ||
      !requireNamespace("ggraph", quietly = TRUE)) {
    stop("Se requieren 'igraph' y 'ggraph' para tipo = 'red'.")
  }

  # Edges: no loops, filtra por min_edge y top_edges
  edges <- df_long[df_long$Emocion1 != df_long$Emocion2 & df_long$Frecuencia > min_edge, ]
  if (nrow(edges) == 0) stop("No hay aristas > min_edge para graficar.")

  # Colapsar pares duplicados (matriz simétrica) quedándonos con el máximo
  edges$key <- ifelse(as.character(edges$Emocion1) < as.character(edges$Emocion2),
                      paste(edges$Emocion1, edges$Emocion2, sep = "||"),
                      paste(edges$Emocion2, edges$Emocion1, sep = "||"))
  edges <- aggregate(Frecuencia ~ key, data = edges, FUN = max)
  tmp <- do.call(rbind, strsplit(edges$key, "\\|\\|"))
  edges$from <- tmp[,1]; edges$to <- tmp[,2]
  edges$key <- NULL

  # Top edges (opcional)
  if (!is.null(top_edges) && is.finite(top_edges) && top_edges > 0) {
    ord_e <- order(edges$Frecuencia, decreasing = TRUE)
    edges <- edges[head(ord_e, min(top_edges, length(ord_e))), , drop = FALSE]
  }

  # Construir grafo: 'Frecuencia' queda como atributo de arista
  g_ig <- igraph::graph_from_data_frame(edges[, c("from","to","Frecuencia")], directed = FALSE)

  # Tamaño de nodo por grado (nice defaults)
  deg <- igraph::degree(g_ig)
  nsize <- 3 + 4 * (deg - min(deg)) / (max(deg) - min(deg) + 1e-12)  # 3–7

  # Graficar: usar atributos del grafo en aes (NO edges$Frecuencia)
  g <- ggraph::ggraph(g_ig, layout = layout_red) +
    # grosor por index estadístico (reemplaza ..index.. con after_stat(index))
    ggraph::geom_edge_link(ggplot2::aes(edge_width = after_stat(index),
                                        edge_alpha = Frecuencia),
                           lineend = "round",
                           show.legend = TRUE) +
    ggraph::scale_edge_width(range = c(0.3, 2.8), guide = "none") +
    ggraph::scale_edge_alpha(range = c(0.3, 0.9)) +
    ggraph::geom_node_point(size = nsize, colour = "#2C3E50") +
    ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3.5) +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(
      title = "Red de co-ocurrencia de emociones NRC",
      subtitle = paste0("Normalización: ", normalizar,
                        " | Umbral min_edge: ", min_edge,
                        if (!is.null(top_edges)) paste0(" | Top edges: ", top_edges) else ""),
      x = NULL, y = NULL
    )

  return(list(
    matriz_procesada = M,
    datos_long = df_long,
    edges = edges,
    grafico = g
  ))

}

#' Visualizar co-ocurrencia NRC (heatmap o red) con color por comunidad/grado
#'
#' @description
#' Extiende la visualización a red con: color de nodos por **comunidad** (Louvain/Walktrap/Infomap)
#' o **grado**, layouts alternativos y opciones de filtrado de aristas. Mantiene el modo heatmap.
#'
#' @inheritParams visualizar_coocurrencia_nrc
#' @param color_nodes \code{c("none","community","degree")}. Cómo colorear nodos en la red.
#' @param community_method \code{c("louvain","walktrap","infomap")}. Algoritmo de comunidad.
#' @param node_palette Vector de colores (si \code{color_nodes="community"}) o nombre de paleta viridis.
#' @param remove_isolated \code{logical}. Si \code{TRUE}, elimina nodos aislados tras filtro de aristas.
#' @param seed \code{integer} para reproducibilidad del layout.
#'
#' @return Lista con matriz procesada, edges, y \code{grafico}. Si \code{color_nodes="community"},
#'   devuelve además \code{comunidades} (vector por nodo).
#' @examples
#' \dontrun{
#' # Red con color por comunidad (Louvain) y layout de Kamada-Kawai
#' v2c <- visualizar_coocurrencia_nrc_v2(
#'   cooc, tipo = "red", normalizar = "max",
#'   min_edge = 0.05, top_edges = 60,
#'   color_nodes = "community", community_method = "louvain",
#'   layout_red = "kk", seed = 123
#' )
#' print(v2c$grafico)
#'
#' # Red coloreando por grado y layout de Fruchterman-Reingold
#' v2d <- visualizar_coocurrencia_nrc_v2(
#'   cooc, tipo = "red", normalizar = "prop",
#'   color_nodes = "degree", layout_red = "fr", seed = 42
#' )
#' print(v2d$grafico)
#' }
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c scale_fill_gradient labs theme_minimal theme element_text element_blank
#' @importFrom ggraph ggraph geom_edge_link geom_node_point geom_node_text scale_edge_width scale_edge_alpha
#' @family matrices_valoradas
#' @export
#' @param matriz (auto) TODO: describir parámetro.
#' @param tipo (auto) TODO: describir parámetro.
#' @param "red" (auto) TODO: describir parámetro.
visualizar_coocurrencia_nrc_v2 <- function(matriz,
                                           tipo = c("heatmap","red"),
                                           normalizar = c("none","prop","max"),
                                           diag_cero = TRUE,
                                           ordenar = c("hclust","sum","none"),
                                           palette = NULL,
                                           min_edge = 0,
                                           top_edges = NULL,
                                           layout_red = c("fr","kk","dh","lgl","graphopt","mds"),
                                           color_nodes = c("none","community","degree"),
                                           community_method = c("louvain","walktrap","infomap"),
                                           node_palette = NULL,
                                           remove_isolated = TRUE,
                                           seed = NULL) {
  tipo        <- match.arg(tipo)
  normalizar  <- match.arg(normalizar)
  ordenar     <- match.arg(ordenar)
  layout_red  <- match.arg(layout_red)
  color_nodes <- match.arg(color_nodes)
  community_method <- match.arg(community_method)

  if (is.data.frame(matriz)) matriz <- as.matrix(matriz)
  if (!is.matrix(matriz) || nrow(matriz) != ncol(matriz)) stop("`matriz` debe ser cuadrada.")
  if (is.null(rownames(matriz)) || is.null(colnames(matriz))) stop("Faltan nombres de filas/columnas.")

  M <- matriz
  mode(M) <- "numeric"
  if (normalizar == "prop") { s <- sum(M, na.rm=TRUE); if (s > 0) M <- M/s }
  if (normalizar == "max")  { mx<- max(M, na.rm=TRUE); if (mx>0) M <- M/mx }
  if (isTRUE(diag_cero)) diag(M) <- 0

  # Orden
  if (ordenar == "sum") {
    ord <- order(rowSums(M, na.rm = TRUE), decreasing = TRUE)
    M <- M[ord, ord, drop = FALSE]
  } else if (ordenar == "hclust") {
    Msym <- (M + t(M))/2
    d <- try(stats::as.dist(1 - stats::cor(t(Msym) + 1e-12)), silent = TRUE)
    if (inherits(d, "try-error")) d <- stats::dist(Msym)
    hc <- stats::hclust(d, method = "average")
    M <- M[hc$order, hc$order, drop = FALSE]
  }

  # Datos largos
  df_long <- data.frame(
    Emocion1 = rep(rownames(M), times=ncol(M)),
    Emocion2 = rep(colnames(M), each = nrow(M)),
    Frecuencia = as.numeric(M),
    stringsAsFactors = FALSE
  )

  if (tipo == "heatmap") {
    use_viridis <- is.null(palette)
    g <- ggplot2::ggplot(df_long, ggplot2::aes(Emocion1, Emocion2, fill = Frecuencia)) +
      ggplot2::geom_tile(color = "white") +
      { if (use_viridis) ggplot2::scale_fill_viridis_c()
        else ggplot2::scale_fill_gradient(low = palette[1], high = tail(palette,1)) } +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Matriz de co-ocurrencia de emociones NRC",
        subtitle = paste("Normalización:", normalizar, "| Orden:", ordenar),
        x = NULL, y = NULL, fill = if (normalizar=="none") "Frecuencia" else "Intensidad"
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                     panel.grid = ggplot2::element_blank())
    return(list(matriz_procesada = M, datos_long = df_long, edges = NULL, grafico = g))
  }

  # --- RED ---
  if (!requireNamespace("igraph", quietly = TRUE) ||
      !requireNamespace("ggraph", quietly = TRUE)) {
    stop("Se requieren 'igraph' y 'ggraph' para tipo = 'red'.")
  }
  if (!is.null(seed)) set.seed(seed)

  # edges: sin loops y filtrados
  edges <- df_long[df_long$Emocion1 != df_long$Emocion2 & df_long$Frecuencia > min_edge, ]
  if (nrow(edges) == 0) stop("No hay aristas > min_edge para graficar.")
  edges$key <- ifelse(as.character(edges$Emocion1) < as.character(edges$Emocion2),
                      paste(edges$Emocion1, edges$Emocion2, sep="||"),
                      paste(edges$Emocion2, edges$Emocion1, sep="||"))
  edges <- aggregate(Frecuencia ~ key, data = edges, FUN = max)
  tmp <- do.call(rbind, strsplit(edges$key, "\\|\\|"))
  edges$from <- tmp[,1]; edges$to <- tmp[,2]
  edges$key <- NULL

  if (!is.null(top_edges) && is.finite(top_edges) && top_edges > 0) {
    ord_e <- order(edges$Frecuencia, decreasing = TRUE)
    edges <- edges[head(ord_e, min(top_edges, length(ord_e))), , drop = FALSE]
  }

  # construir grafo
  g_ig <- igraph::graph_from_data_frame(edges[, c("from","to","Frecuencia")], directed = FALSE)

  # ✅ eliminar nodos aislados si se solicita
  if (isTRUE(remove_isolated)) {
    deg <- igraph::degree(g_ig)
    keep <- igraph::V(g_ig)[deg > 0]
    g_ig <- igraph::induced_subgraph(g_ig, vids = keep)
  }

  # coloreo de nodos
  node_df <- data.frame(name = igraph::V(g_ig)$name, stringsAsFactors = FALSE)
  if (color_nodes == "community") {
    comm <- switch(community_method,
                   louvain  = igraph::cluster_louvain(g_ig),
                   walktrap = igraph::cluster_walktrap(g_ig),
                   infomap  = igraph::cluster_infomap(g_ig)
    )
    node_df$grupo <- as.factor(igraph::membership(comm))
    if (is.null(node_palette)) {
      node_palette <- scales::hue_pal()(length(levels(node_df$grupo)))
    }
    names(node_palette) <- levels(node_df$grupo)
  } else if (color_nodes == "degree") {
    node_df$grado <- igraph::degree(g_ig)
  }

  # tamaño de nodo por grado
  deg <- igraph::degree(g_ig)
  nsize <- 3 + 4 * (deg - min(deg)) / (max(deg) - min(deg) + 1e-12)

  # layout
  lay <- switch(layout_red,
                fr  = "fr", kk = "kk", dh = "dh",
                lgl = "lgl", graphopt = "graphopt", mds = "stress"
  )

  p <- ggraph::ggraph(g_ig, layout = lay) +
    ggraph::geom_edge_link(ggplot2::aes(edge_width = after_stat(index),
                                        edge_alpha = after_stat(index)),
                           lineend = "round", show.legend = FALSE) +
    ggraph::scale_edge_width(range = c(0.3, 2.8)) +
    ggraph::scale_edge_alpha(range = c(0.25, 0.9))

  if (color_nodes == "community") {
    p <- p + ggraph::geom_node_point(ggplot2::aes(color = node_df$grupo), size = nsize) +
      ggraph::geom_node_text(ggplot2::aes(label = name, color = node_df$grupo),
                             repel = TRUE, size = 3.5, show.legend = FALSE) +
      ggplot2::scale_color_manual(values = node_palette, name = "Comunidad")
  } else if (color_nodes == "degree") {
    p <- p + ggraph::geom_node_point(ggplot2::aes(color = node_df$grado), size = nsize) +
      ggraph::geom_node_text(ggplot2::aes(label = name, color = node_df$grado),
                             repel = TRUE, size = 3.5, show.legend = FALSE) +
      ggplot2::scale_color_viridis_c(name = "Grado")
  } else {
    p <- p + ggraph::geom_node_point(size = nsize, colour = "#2C3E50") +
      ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE, size = 3.5)
  }

  p <- p +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::labs(
      title = "Red de co-ocurrencia de emociones NRC",
      subtitle = paste0("Normalización: ", normalizar,
                        " | Umbral min_edge: ", min_edge,
                        if (!is.null(top_edges)) paste0(" | Top edges: ", top_edges) else "",
                        if (color_nodes=="community") paste0(" | Comunidad: ", community_method) else ""),
      x = NULL, y = NULL
    )

  out <- list(
    matriz_procesada = M,
    datos_long = df_long,
    edges = edges,
    grafico = p
  )
  if (color_nodes == "community") {
    out$comunidades <- setNames(as.integer(node_df$grupo), node_df$name)
  }
  if (color_nodes == "degree") {
    out$grado_nodos <- setNames(as.numeric(node_df$grado), node_df$name)
  }
  out

}


#' Graficar proyecciones PCA con estructura emocional
#'
#' @description
#' Genera visualizaciones a partir de un objeto definido por \code{prcomp()}:
#' (1) dispersión de individuos coloreada por clase (opcionalmente con etiquetas y elipses),
#' (2) el mismo scatter con elipses por grupo,
#' (3) vectores de carga (loadings) en 2D.
#' Los ejes incluyen el \% de varianza explicada por cada componente.
#'
#' @param pca Objeto de clase \code{prcomp}.
#' @param data \code{data.frame} con las variables originales (sin transformar). Si contiene
#'   exactamente las mismas columnas usadas en \code{prcomp}, \code{predict()} aplicará
#'   el centrado/escalado almacenado en \code{pca} automáticamente.
#' @param labels Vector (factor/character) con la clase para colorear puntos. Debe tener
#'   la misma longitud que \code{nrow(data)}. Si es \code{NULL}, se usa \code{rownames(data)}
#'   o índices.
#' @param componentes Integer vector con los índices de componentes a graficar. Por defecto \code{c(1, 2)}.
#' @param titulo Character. Título base para los gráficos. Por defecto \code{"PCA de emociones"}.
#' @param label_points Logical. Si \code{TRUE}, muestra etiquetas de los puntos con \pkg{ggrepel}. Por defecto \code{TRUE}.
#' @param ellipse Logical. Si \code{TRUE}, dibuja elipses por grupo en el gráfico 2. Por defecto \code{TRUE}.
#' @param ellipse_type Character. Tipo de elipse; opciones \code{"norm"}, \code{"t"}, \code{"euclid"}.
#'   Por defecto \code{"norm"}.
#' @param ellipse_level Numeric. Nivel de confianza de la elipse (ej., 0.95). Por defecto \code{0.95}.
#' @param palette Vector de colores o nombre de paleta (p.ej., \code{"Dark2"}) para clases.
#'   Si \code{NULL}, usa escala discreta por defecto de ggplot2.
#' @param point_size Numeric. Tamaño de los puntos. Por defecto \code{1.8}.
#' @param point_alpha Numeric. Transparencia de los puntos en \eqn{[0, 1]}. Por defecto \code{0.75}.
#' @param loading_scale Numeric. Factor para escalar las flechas de cargas cuando \code{length(componentes) == 2}.
#'   Útil si las cargas quedan muy cortas/largas respecto al marco. Por defecto \code{1}.
#' @param top_n_loadings Integer o \code{NULL}. Si se indica, etiqueta solo las \code{top_n_loadings}
#'   variables con mayor norma de carga en el plano seleccionado. Por defecto \code{NULL} (todas).
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{scatter}: \code{ggplot} de dispersión coloreado por clase.
#'   \item \code{elipses}: \code{ggplot} con elipses por grupo (si \code{ellipse=TRUE}).
#'   \item \code{cargas}: \code{ggplot} con vectores de carga (si \code{length(componentes)==2}).
#'   \item \code{data_proyectada}: \code{data.frame} con PCX, PCY, Clase y Etiqueta.
#'   \item \code{loadings}: \code{data.frame} con las cargas usadas en \code{cargas} (o \code{NULL}).
#'   \item \code{var_exp}: \code{numeric} con varianza explicada (proporciones) para todos los PCs.
#' }
#'
#' @examples
#' \dontrun{
#' # Supón que acdfx_emoc contiene solo columnas de emociones numéricas:
#' pca <- prcomp(acdfx_emoc, center = TRUE, scale. = TRUE)
#' clases <- acdfx$category  # p.ej., "positivo"/"negativo"/"neutral"
#'
#' g <- graficar_pca_emociones(
#'   pca = pca,
#'   data = acdfx_emoc,
#'   labels = clases,
#'   componentes = c(1, 2),
#'   titulo = "PCA emociones",
#'   palette = "Dark2",
#'   top_n_loadings = 12,
#'   loading_scale = 1.2
#' )
#' print(g$scatter); print(g$elipses); print(g$cargas)
#' head(g$data_proyectada)
#' }
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal theme element_text scale_color_brewer scale_color_manual
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats predict
#' @importFrom grid unit
#' @family reduccion_dimensional
#' @export
#' @param 2 (auto) TODO: describir parámetro.
graficar_pca_emociones <- function(
    pca,
    data,
    labels = NULL,
    componentes = c(1, 2),
    titulo = "PCA de emociones",
    label_points = TRUE,
    ellipse = TRUE,
    ellipse_type = "norm",
    ellipse_level = 0.95,
    palette = NULL,
    point_size = 1.8,
    point_alpha = 0.75,
    loading_scale = 1,
    top_n_loadings = NULL
) {
  # --- Validaciones
  if (!inherits(pca, "prcomp")) stop("El objeto 'pca' debe provenir de prcomp().")
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (!all(is.finite(componentes)) || any(componentes < 1) || any(componentes > ncol(pca$rotation))) {
    stop("Índices en 'componentes' fuera de rango.")
  }

  # Varianza explicada (todas las PCs)
  var_all <- (pca$sdev ^ 2)
  var_exp <- var_all / sum(var_all)

  # Proyección (predict aplica el centrado/escalado almacenado en 'pca')
  proj_raw <- stats::predict(pca, newdata = data)
  proj <- as.data.frame(proj_raw[, componentes, drop = FALSE])
  colnames(proj) <- paste0("PC", componentes)

  # Labels
  if (is.null(labels)) {
    labels <- rownames(data)
    if (is.null(labels)) labels <- seq_len(nrow(proj))
  }
  if (length(labels) != nrow(proj)) stop("La longitud de 'labels' no coincide con nrow(data).")
  proj$Clase <- as.factor(labels)
  proj$Etiqueta <- if (!is.null(rownames(data))) rownames(data) else as.character(seq_len(nrow(proj)))

  # Etiquetas de ejes con varianza explicada
  axis_x <- paste0("PC", componentes[1], " (", sprintf("%.1f", 100 * var_exp[componentes[1]]), "%)")
  axis_y <- paste0("PC", componentes[2], " (", sprintf("%.1f", 100 * var_exp[componentes[2]]), "%)")

  # --- Scatter básico
  g1 <- ggplot2::ggplot(proj, ggplot2::aes(x = proj[[1]], y = proj[[2]], color = Clase)) +
    ggplot2::geom_point(alpha = point_alpha, size = point_size) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = titulo, x = axis_x, y = axis_y, color = "Clase") +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(palette)) {
    if (length(palette) == 1L) {
      g1 <- g1 + ggplot2::scale_color_brewer(palette = palette)
    } else {
      g1 <- g1 + ggplot2::scale_color_manual(values = palette)
    }
  }

  if (isTRUE(label_points)) {
    g1 <- g1 +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = Etiqueta),
        size = 2.8, max.overlaps = 100, segment.size = 0.2
      )
  }

  # --- Scatter con elipses por clase
  g2 <- ggplot2::ggplot(proj, ggplot2::aes(x = proj[[1]], y = proj[[2]], color = Clase)) +
    ggplot2::geom_point(alpha = point_alpha * 0.8, size = point_size) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = paste(titulo, "— elipses por clase"), x = axis_x, y = axis_y, color = "Clase") +
    ggplot2::theme(legend.position = "bottom")

  if (!is.null(palette)) {
    if (length(palette) == 1L) {
      g2 <- g2 + ggplot2::scale_color_brewer(palette = palette)
    } else {
      g2 <- g2 + ggplot2::scale_color_manual(values = palette)
    }
  }

  if (isTRUE(ellipse)) {
    g2 <- g2 +
      ggplot2::stat_ellipse(type = ellipse_type, level = ellipse_level, linewidth = 0.6, alpha = 0.25)
  }
  if (isTRUE(label_points)) {
    g2 <- g2 +
      ggrepel::geom_text_repel(
        ggplot2::aes(label = Etiqueta),
        size = 2.8, max.overlaps = 100, segment.size = 0.2
      )
  }

  # --- Cargas (solo si 2D)
  g3 <- NULL
  load_df <- NULL
  if (length(componentes) == 2) {
    loadings <- as.data.frame(pca$rotation[, componentes, drop = FALSE])
    colnames(loadings) <- c("PCX", "PCY")
    loadings$Variable <- rownames(loadings)
    loadings$Norma <- sqrt(loadings$PCX^2 + loadings$PCY^2)

    # top-N variables más "importantes" según norma
    if (!is.null(top_n_loadings) && is.finite(top_n_loadings) && top_n_loadings > 0) {
      ord <- order(loadings$Norma, decreasing = TRUE)
      loadings <- loadings[head(ord, min(top_n_loadings, nrow(loadings))), , drop = FALSE]
    }

    # Escala de flechas
    loadings$PCX <- loadings$PCX * loading_scale
    loadings$PCY <- loadings$PCY * loading_scale

    g3 <- ggplot2::ggplot(loadings, ggplot2::aes(x = 0, y = 0)) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = PCX, yend = PCY),
        arrow = ggplot2::arrow(length = grid::unit(0.20, "cm")),
        color = "gray40", linewidth = 0.6
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(x = PCX, y = PCY, label = Variable),
        size = 3, segment.size = 0.2
      ) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Vectores de carga PCA",
        x = paste0("PC", componentes[1], " (", sprintf('%.1f', 100 * var_exp[componentes[1]]), "%)"),
        y = paste0("PC", componentes[2], " (", sprintf('%.1f', 100 * var_exp[componentes[2]]), "%)")
      )

    load_df <- loadings
  }

  # Renombrar columnas proyectadas a PCX/PCY para facilitar usos posteriores
  proy <- proj
  names(proy)[1:2] <- c("PCX", "PCY")

  return(list(
    scatter = g1,
    elipses = g2,
    cargas = g3,
    data_proyectada = proy,
    loadings = load_df,
    var_exp = var_exp
  ))
}


#' Graficar emociones agrupadas (positivo, negativo, activación)
#'
#' @description
#' Genera un gráfico de barras a partir de un resumen de categorías emocionales
#' (p. ej., \code{positivo}, \code{negativo}, \code{activacion}). Soporta entradas
#' como vector con nombres o \code{data.frame} de una fila, opciones de
#' normalización/ordenamiento y etiquetas porcentuales.
#'
#' @details
#' Esta función es agnóstica del origen de los valores (frecuencias, sumas,
#' intensidades). Si se especifica \code{normalizar = "percent"} los ejes y
#' etiquetas se muestran en porcentaje sobre el total. Para \code{normalizar = "prop"}
#' se devuelven proporciones en  \code(reescala a `[0, 1]`. \code{"none"}.
#'
#' @param emocion_agru_df \code{data.frame} de una fila con columnas de categorías
#'   (p. ej., \code{positivo}, \code{negativo}, \code{activacion}) o un \code{numeric}
#'   con nombres (vector con nombres).
#' @param titulo \code{character}. Título del gráfico. Por defecto \code{"Emociones agrupadas"}.
#' @param normalizar \code{c("none","percent","prop")}. \code{"none"}: usa los valores tal cual;
#'   \code{"percent"}: escala a porcentaje (suma=100); \code{"prop"}: escala a proporción (suma=1).
#' @param ordenar \code{c("none","desc","asc")}. Orden de barras por valor (desc/asc) o sin ordenar.
#' @param mostrar_etiquetas \code{logical}. Si \code{TRUE}, añade etiquetas sobre las barras.
#' @param digits \code{integer}. Decimales para etiquetas en modo \code{"percent"} o \code{"prop"}.
#' @param palette \code{NULL}, nombre de paleta de \pkg{ggplot2} (p. ej. \code{"Dark2"}),
#'   o vector de colores (mismo largo que categorías).
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{grafico}: objeto \code{ggplot2}.
#'   \item \code{datos}: \code{data.frame} con \code{categoria}, \code{valor}, \code{valor_plot}.
#'   \item \code{total}: suma original (antes de normalizar).
#' }
#'
#' @examples
#' \dontrun{
#' # 1) Con salida típica de agrupar_emociones(): data.frame de 1 fila
#' df1 <- data.frame(positivo = 120, negativo = 80, activacion = 60)
#' p1 <- graficar_emociones_agrupadas(df1, normalizar = "percent", ordenar = "desc",
#'                                    titulo = "Perfil emocional (porcentaje)")
#' print(p1$grafico)
#'
#' # 2) Con vector con nombres
#' v <- c(positivo = 40, negativo = 25, activacion = 35)
#' p2 <- graficar_emociones_agrupadas(v, normalizar = "prop", mostrar_etiquetas = TRUE)
#' print(p2$grafico)
#'
#' # 3) Integración rápida con tus datos (ejemplo):
#' # emo_cols_pos <- c("alegría","confianza","anticipación","sorpresa")
#' # emo_cols_neg <- c("ira","disgusto","miedo","tristeza")
#' # df_agru <- data.frame(
#' #   positivo   = sum(colSums(acdfx[, emo_cols_pos, drop=FALSE])),
#' #   negativo   = sum(colSums(acdfx[, emo_cols_neg, drop=FALSE])),
#' #   activacion = sum(colSums(acdfx[, c("sorpresa","ira","miedo"), drop=FALSE]))
#' # )
#' # g <- graficar_emociones_agrupadas(df_agru, normalizar="percent", ordenar="desc")
#' # print(g$grafico)
#' }
#' @importFrom ggplot2 ggplot aes geom_col geom_text labs theme_minimal theme element_text
#' @importFrom ggplot2 scale_color_brewer scale_fill_brewer scale_fill_manual scale_y_continuous
#' @family visualizacion
#' @export
#' @param "percent" (auto) TODO: describir parámetro.
#' @param "prop" (auto) TODO: describir parámetro.
graficar_emociones_agrupadas <- function(emocion_agru_df,
                                         titulo = "Emociones agrupadas",
                                         normalizar = c("none","percent","prop"),
                                         ordenar = c("none","desc","asc"),
                                         mostrar_etiquetas = TRUE,
                                         digits = 1,
                                         palette = NULL) {
  stopifnot(requireNamespace("ggplot2", quietly = TRUE))
  normalizar <- match.arg(normalizar)
  ordenar    <- match.arg(ordenar)

  # ---- Coerción a data.frame largo ----
  if (is.numeric(emocion_agru_df) && !is.null(names(emocion_agru_df))) {
    df_plot <- data.frame(
      categoria = names(emocion_agru_df),
      valor     = as.numeric(emocion_agru_df),
      stringsAsFactors = FALSE
    )
  } else if (is.data.frame(emocion_agru_df)) {
    if (nrow(emocion_agru_df) == 0) {
      warning("No hay datos para graficar emociones agrupadas.")
      return(NULL)
    }
    if (nrow(emocion_agru_df) > 1) {
      # tomar la primera fila explícitamente (comportamiento consistente)
      emocion_agru_df <- emocion_agru_df[1, , drop = FALSE]
    }
    df_plot <- data.frame(
      categoria = colnames(emocion_agru_df),
      valor     = as.numeric(emocion_agru_df[1, ]),
      stringsAsFactors = FALSE
    )
  } else {
    stop("`emocion_agru_df` debe ser data.frame (1 fila) o vector numérico con nombres.")
  }

  # eliminar NA/Inf
  df_plot <- df_plot[is.finite(df_plot$valor), , drop = FALSE]
  if (nrow(df_plot) == 0) {
    warning("Todos los valores son NA/Inf; no se puede graficar.")
    return(NULL)
  }

  # ---- Normalización y etiquetas ----
  total <- sum(df_plot$valor, na.rm = TRUE)
  if (normalizar == "percent") {
    df_plot$valor_plot <- if (total > 0) 100 * df_plot$valor / total else 0
    y_lab <- "Porcentaje (%)"
    etiq  <- paste0(round(df_plot$valor_plot, digits), "%")
  } else if (normalizar == "prop") {
    df_plot$valor_plot <- if (total > 0) df_plot$valor / total else 0
    y_lab <- "Proporción"
    etiq  <- round(df_plot$valor_plot, digits)
  } else {
    df_plot$valor_plot <- df_plot$valor
    y_lab <- "Valor"
    etiq  <- round(df_plot$valor_plot, digits)
  }

  # ---- Orden ----
  if (ordenar == "desc") {
    df_plot <- df_plot[order(-df_plot$valor_plot), , drop = FALSE]
  } else if (ordenar == "asc") {
    df_plot <- df_plot[order(df_plot$valor_plot), , drop = FALSE]
  }
  df_plot$categoria <- factor(df_plot$categoria, levels = df_plot$categoria)

  # ---- Paleta ----
  use_manual <- !is.null(palette) && length(palette) > 1
  use_brewer <- !is.null(palette) && length(palette) == 1

  # ---- Gráfico ----
  g <- ggplot2::ggplot(df_plot, ggplot2::aes(x = categoria, y = valor_plot, fill = categoria)) +
    ggplot2::geom_col(width = 0.72, show.legend = FALSE) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = titulo, x = "Categoría emocional", y = y_lab)

  if (isTRUE(mostrar_etiquetas)) {
    g <- g + ggplot2::geom_text(ggplot2::aes(label = etiq),
                                vjust = -0.35, size = 3.6)
  }

  # paletas
  if (use_manual) {
    g <- g + ggplot2::scale_fill_manual(values = palette)
  } else if (use_brewer) {
    g <- g + ggplot2::scale_fill_brewer(palette = palette)
  }

  # margen superior según etiquetas
  ymax <- max(df_plot$valor_plot, na.rm = TRUE)
  g <- g + ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.00, if (isTRUE(mostrar_etiquetas)) 0.08 else 0.02)))

  return(list(
    grafico = g,
    datos   = df_plot[, c("categoria","valor","valor_plot")],
    total   = total
  ))
}


#' Graficar PCA de emociones con etiquetas y biplot combinado
#'
#' @description
#' Genera visualizaciones a partir de un objeto \code{prcomp()}:
#' (1) dispersión coloreada por clúster con etiquetas opcionales,
#' (2) dispersión con elipses por grupo,
#' (3) vectores de carga (solo si 2D),
#' (4) biplot combinado (puntos + cargas en el mismo panel, con auto-escalado).
#' Los ejes incluyen el \% de varianza explicada.
#'
#' @param pca Objeto de clase \code{prcomp}.
#' @param data \code{data.frame} con las variables usadas para el PCA.
#' @param labels_cluster Vector con la etiqueta de clúster/grupo (longitud = nrow(data)).
#' @param labels_texto (opcional) Vector de etiquetas para cada punto. Si \code{NULL} o longitud distinta,
#'   se asignan \code{ID_1}, \code{ID_2}, etc.
#' @param componentes Integer vector con PCs a proyectar. Por defecto \code{c(1, 2)}.
#' @param titulo Título base. Por defecto \code{"PCA de emociones"}.
#' @param show_labels Logical, mostrar etiquetas de puntos. Por defecto \code{TRUE}.
#' @param repel Logical, usar \pkg{ggrepel} para evitar solapamientos. Por defecto \code{TRUE}.
#' @param ellipse Logical, dibujar elipses por grupo. Por defecto \code{TRUE}.
#' @param ellipse_type \code{"norm"}, \code{"t"} o \code{"euclid"}. Por defecto \code{"norm"}.
#' @param ellipse_level Nivel de confianza (p. ej. 0.95). Por defecto \code{0.95}.
#' @param palette \code{NULL}, nombre de paleta de \pkg{ggplot2} (p. ej. \code{"Dark2"}) o vector de colores.
#' @param point_size Tamaño de punto. Por defecto \code{1.8}.
#' @param point_alpha Alpha de puntos. Por defecto \code{0.75}.
#' @param loading_scale Factor extra para escalar flechas de cargas (2D). Por defecto \code{1}.
#' @param top_n_loadings \code{NULL} o entero: etiqueta solo las \code{top_n_loadings} variables por norma.
#' @param biplot Logical. Si \code{TRUE} y \code{length(componentes)==2}, devuelve \code{biplot} combinado. Por defecto \code{FALSE}.
#' @param biplot_alpha Escala relativa de auto–ajuste de cargas al rango de scores en el panel \code{[0,1]}. Por defecto \code{0.9}.
#'
#' @return Lista con:
#' \itemize{
#'   \item \code{scatter}, \code{elipses}, \code{cargas} (si 2D),
#'   \item \code{biplot} (si \code{biplot=TRUE} y 2D),
#'   \item \code{data_proyectada}, \code{loadings}, \code{var_exp}.
#' }
#'
#' @examples
#' \dontrun{
#' pca <- prcomp(acdfx_emoc, center = TRUE, scale. = TRUE)
#' res <- graficar_pca_emociones_etiquetas(
#'   pca, acdfx_emoc, labels_cluster = acdfx$categoria,
#'   labels_texto = rownames(acdfx_emoc),
#'   palette = "Dark2", top_n_loadings = 10,
#'   biplot = TRUE, biplot_alpha = 0.9
#' )
#' print(res$scatter); print(res$elipses); print(res$cargas); print(res$biplot)
#' }
#' @importFrom ggplot2 ggplot aes geom_point geom_segment geom_text labs theme_minimal theme element_text scale_color_brewer scale_color_manual
#' @importFrom ggplot2 stat_ellipse
#' @importFrom ggrepel geom_text_repel
#' @importFrom stats predict
#' @importFrom grid unit
#' @family reduccion_dimensional
#' @export
#' @param 2 (auto) TODO: describir parámetro.
graficar_pca_emociones_etiquetas <- function(
    pca,
    data,
    labels_cluster,
    labels_texto = NULL,
    componentes = c(1, 2),
    titulo = "PCA de emociones",
    show_labels = TRUE,
    repel = TRUE,
    ellipse = TRUE,
    ellipse_type = "norm",
    ellipse_level = 0.95,
    palette = NULL,
    point_size = 1.8,
    point_alpha = 0.75,
    loading_scale = 1,
    top_n_loadings = NULL,
    biplot = FALSE,
    biplot_alpha = 0.9
) {
  if (!inherits(pca, "prcomp")) stop("'pca' debe ser prcomp().")
  if (!is.data.frame(data)) data <- as.data.frame(data)
  if (length(labels_cluster) != nrow(data)) stop("'labels_cluster' debe tener longitud nrow(data).")
  if (!all(is.finite(componentes)) || any(componentes < 1) || any(componentes > ncol(pca$rotation))) {
    stop("'componentes' fuera de rango.")
  }

  # Varianza explicada
  var_all <- pca$sdev^2
  var_exp <- var_all / sum(var_all)

  # Proyección
  proj_raw <- stats::predict(pca, newdata = data)
  proj <- as.data.frame(proj_raw[, componentes, drop = FALSE])
  colnames(proj) <- paste0("PC", componentes)

  # Clusters y etiquetas
  proj$Cluster  <- as.factor(labels_cluster)
  proj$Etiqueta <- if (!is.null(labels_texto) && length(labels_texto) == nrow(data)) labels_texto else paste0("ID_", seq_len(nrow(data)))

  # Ejes con varianza
  axis_x <- paste0("PC", componentes[1], " (", sprintf('%.1f', 100 * var_exp[componentes[1]]), "%)")
  axis_y <- paste0("PC", componentes[2], " (", sprintf('%.1f', 100 * var_exp[componentes[2]]), "%)")

  # --- Scatter
  g1 <- ggplot2::ggplot(
    proj,
    ggplot2::aes(
      x = .data[[paste0("PC", componentes[1])]],
      y = .data[[paste0("PC", componentes[2])]],
      color = Cluster
    )
  ) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = titulo, x = axis_x, y = axis_y, color = "Cluster") +
    ggplot2::theme(legend.position = "bottom")

  # --- Elipses
  g2 <- ggplot2::ggplot(
    proj,
    ggplot2::aes(
      x = .data[[paste0("PC", componentes[1])]],
      y = .data[[paste0("PC", componentes[2])]],
      color = Cluster
    )
  ) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = paste(titulo, "— elipses por cluster"), x = axis_x, y = axis_y, color = "Cluster") +
    ggplot2::theme(legend.position = "bottom")
  if (isTRUE(ellipse)) {
    g2 <- g2 + ggplot2::stat_ellipse(type = ellipse_type, level = ellipse_level, linewidth = 0.6, alpha = 0.25)
  }


  # ---------- Cargas (solo 2D) ----------
  g3 <- NULL
  load_df <- NULL
  if (length(componentes) == 2) {
    loadings <- as.data.frame(pca$rotation[, componentes, drop = FALSE])
    colnames(loadings) <- c("PCX", "PCY")
    loadings$Variable <- rownames(loadings)
    loadings$Norma <- sqrt(loadings$PCX^2 + loadings$PCY^2)
    if (!is.null(top_n_loadings) && is.finite(top_n_loadings) && top_n_loadings > 0) {
      loadings <- loadings[order(loadings$Norma, decreasing = TRUE), ]
      loadings <- loadings[seq_len(min(top_n_loadings, nrow(loadings))), , drop = FALSE]
    }
    loadings$PCX <- loadings$PCX * loading_scale
    loadings$PCY <- loadings$PCY * loading_scale

    g3 <- ggplot2::ggplot(loadings, ggplot2::aes(x = 0, y = 0)) +
      ggplot2::geom_segment(
        ggplot2::aes(xend = PCX, yend = PCY),
        arrow = ggplot2::arrow(length = grid::unit(0.20, "cm")),
        linewidth = 0.6, colour = "gray40"
      ) +
      ggrepel::geom_text_repel(
        ggplot2::aes(x = PCX, y = PCY, label = Variable),
        size = 3, segment.size = 0.2
      ) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Vectores de carga PCA",
        x = paste0("PC", componentes[1], " (", sprintf('%.1f', 100 * var_exp[componentes[1]]), "%)"),
        y = paste0("PC", componentes[2], " (", sprintf('%.1f', 100 * var_exp[componentes[2]]), "%)")
      )
    load_df <- loadings
  }

  # ---------- Biplot combinado (si 2D) ----------
  g4 <- NULL
  if (isTRUE(biplot) && length(componentes) == 2) {
    # 1) scores en el plano con nombres estables
    scores <- proj[, paste0("PC", componentes), drop = FALSE]
    names(scores) <- c("PCX", "PCY")
    scores_df <- data.frame(scores, Cluster = proj$Cluster, Etiqueta = proj$Etiqueta, stringsAsFactors = FALSE)

    # 2) cargas originales
    L <- as.data.frame(pca$rotation[, componentes, drop = FALSE])
    names(L) <- c("PCX", "PCY")
    L$Variable <- rownames(L)

    # 3) auto-escalado de cargas al rango de scores
    rng_scores <- max(abs(unlist(scores_df[, c("PCX","PCY")])), na.rm = TRUE)
    rng_load   <- max(abs(c(L$PCX, L$PCY)), na.rm = TRUE)
    scale_auto <- if (rng_load > 0) (biplot_alpha * rng_scores) / rng_load else 1
    L$PCX <- L$PCX * scale_auto * loading_scale
    L$PCY <- L$PCY * scale_auto * loading_scale
    L$Norma <- sqrt(L$PCX^2 + L$PCY^2)

    if (!is.null(top_n_loadings) && is.finite(top_n_loadings) && top_n_loadings > 0) {
      L <- L[order(L$Norma, decreasing = TRUE), ]
      L <- L[seq_len(min(top_n_loadings, nrow(L))), , drop = FALSE]
    }

    # 4) (opcional) limitar cantidad de etiquetas de puntos para evitar saturación
    label_max <- 80  # <— ajusta a gusto
    if (isTRUE(show_labels) && nrow(scores_df) > label_max) {
      scores_df$Etiqueta_plot <- ""
      scores_df$Etiqueta_plot[seq_len(label_max)] <- scores_df$Etiqueta[seq_len(label_max)]
    } else {
      scores_df$Etiqueta_plot <- if (isTRUE(show_labels)) scores_df$Etiqueta else ""
    }

    # 5) biplot
    g4 <- ggplot2::ggplot() +
      ggplot2::geom_point(
        data = scores_df,
        ggplot2::aes(PCX, PCY, color = Cluster),
        size = point_size, alpha = point_alpha
      ) +
      { if (isTRUE(show_labels)) {
        if (isTRUE(repel)) ggrepel::geom_text_repel(
          data = scores_df,
          ggplot2::aes(PCX, PCY, label = Etiqueta_plot),
          size = 2.6, max.overlaps = Inf, segment.size = 0.2
        )
        else ggplot2::geom_text(
          data = scores_df,
          ggplot2::aes(PCX, PCY, label = Etiqueta_plot),
          size = 2.4, vjust = 1.2, hjust = 0.5, check_overlap = TRUE
        )
      } } +
      ggplot2::geom_segment(
        data = L,
        ggplot2::aes(x = 0, y = 0, xend = PCX, yend = PCY),
        arrow = ggplot2::arrow(length = grid::unit(0.20, "cm")),
        linewidth = 0.6, colour = "gray40"
      ) +
      ggrepel::geom_text_repel(
        data = L, ggplot2::aes(PCX, PCY, label = Variable),
        size = 3, segment.size = 0.2
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = paste(titulo, "— biplot"),
        x = paste0("PC", componentes[1], " (", sprintf('%.1f', 100 * var_exp[componentes[1]]), "%)"),
        y = paste0("PC", componentes[2], " (", sprintf('%.1f', 100 * var_exp[componentes[2]]), "%)"),
        color = "Cluster"
      ) +
      ggplot2::theme(legend.position = "bottom")

    if (!is.null(palette)) {
      g4 <- g4 + if (length(palette) == 1L) ggplot2::scale_color_brewer(palette = palette) else ggplot2::scale_color_manual(values = palette)
    }
  }


  # Renombrar para salida
  proy <- proj
  names(proy)[1:2] <- c("PCX", "PCY")

  list(
    scatter = g1,
    elipses = g2,
    cargas  = g3,
    biplot  = g4,
    data_proyectada = proy,
    loadings = load_df,
    var_exp  = var_exp
  )
}


#' Generar PCA sobre matriz emocional
#'
#' @description
#' Realiza un Análisis de Componentes Principales (PCA) sobre una matriz de emociones
#' por comentario/documento. Incluye manejo de valores faltantes, eliminación de
#' columnas constantes, y devuelve la proyección (scores), cargas (loadings) y
#' varianza explicada.
#'
#' @details
#' - Si hay \code{NA}, el argumento \code{na_action} controla el tratamiento:
#'   \itemize{
#'     \item \code{"omit"}: elimina filas con cualquier NA (comportamiento clásico).
#'     \item \code{"zero"}: reemplaza NA por 0 (útil si los conteos/frecuencias ausentes se interpretan como 0).
#'     \item \code{"fail"}: lanza error si existen \code{NA}.
#'   }
#' - Columnas con varianza 0 (constantes) se eliminan para evitar fallos en \code{prcomp()}.
#' - El centrado/escalado se controla con \code{center} y \code{scale.} (alias \code{escalar}).
#'
#' @param data_matriz \code{data.frame} numérico con columnas de emociones
#'   (p. ej., \code{ira}, \code{alegria}, \code{miedo}, ...).
#' @param escalar \code{logical}. Si \code{TRUE}, aplica escalado (equivale a \code{scale.=TRUE} en \code{prcomp}). Por defecto \code{TRUE}.
#' @param center \code{logical}. Si \code{TRUE}, centra los datos antes del escalado. Por defecto \code{TRUE}.
#' @param na_action \code{c("omit","zero","fail")}. Estrategia para tratar \code{NA}. Por defecto \code{"omit"}.
#' @param keep_row_names \code{logical}. Si \code{TRUE}, preserva \code{rownames} en la salida de \code{proyeccion}. Por defecto \code{TRUE}.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{pca}: objeto \code{prcomp}.
#'   \item \code{proyeccion}: \code{data.frame} con los scores (coordenadas PC) de cada fila usada.
#'   \item \code{cargas}: \code{data.frame} con las cargas (loadings) por variable.
#'   \item \code{var_exp}: \code{data.frame} con varianza explicada por componente (proporción y acumulada).
#'   \item \code{escalado}: lista con \code{center} y \code{scale} usados por \code{prcomp}.
#'   \item \code{cols_usadas}: vector con los nombres de columnas finalmente utilizadas.
#'   \item \code{filas_usadas}: vector con los nombres/índices de filas incluidas tras el tratamiento de NA.
#' }
#'
#' @examples
#' \dontrun{
#' # 1) Con una matriz emocional ya curada (solo emociones NRC)
#' emo_cols <- c("ira","anticipación","disgusto","miedo","alegría","tristeza","sorpresa","confianza")
#' acdfx_emoc <- acdfx[, emo_cols]
#' res_pca <- generar_pca_emocional(acdfx_emoc, escalar = TRUE, center = TRUE, na_action = "omit")
#' head(res_pca$proyeccion)
#' res_pca$var_exp
#'
#' # 2) Integración con tus funciones de gráfico
#' g <- graficar_pca_emociones_etiquetas(
#'   pca = res_pca$pca,
#'   data = acdfx_emoc,
#'   labels_cluster = acdfx$categoria,
#'   labels_texto = rownames(acdfx_emoc),
#'   palette = "Dark2",
#'   top_n_loadings = 10,
#'   biplot = TRUE
#' )
#' print(g$scatter); print(g$elipses); print(g$cargas); print(g$biplot)
#'
#' # 3) NA como 0 (si ausencias significan ausencia de emoción)
#' res_pca0 <- generar_pca_emocional(acdfx_emoc, na_action = "zero")
#' }
#'
#' @importFrom stats prcomp
#' @family reduccion_dimensional
#' @export
#' @param "zero" (auto) TODO: describir parámetro.
#' @param "fail" (auto) TODO: describir parámetro.
generar_pca_emocional <- function(data_matriz,
                                  escalar = TRUE,
                                  center = TRUE,
                                  na_action = c("omit", "zero", "fail"),
                                  keep_row_names = TRUE) {
  na_action <- match.arg(na_action)

  # --- Validaciones básicas
  if (!is.data.frame(data_matriz)) {
    stop("`data_matriz` debe ser un data.frame con columnas emocionales numéricas.")
  }
  datos <- as.data.frame(data_matriz)
  if (!all(vapply(datos, is.numeric, logical(1)))) {
    stop("Todas las columnas de `data_matriz` deben ser numéricas.")
  }

  # --- Tratamiento de NA
  if (na_action == "fail" && anyNA(datos)) {
    stop("Se encontraron NA y `na_action='fail'`.")
  } else if (na_action == "omit") {
    filas_usadas <- which(stats::complete.cases(datos))
    if (length(filas_usadas) == 0) stop("No quedan filas tras omitir NA.")
    datos <- datos[filas_usadas, , drop = FALSE]
  } else if (na_action == "zero") {
    datos[is.na(datos)] <- 0
    filas_usadas <- seq_len(nrow(datos))
  } else {
    filas_usadas <- seq_len(nrow(datos))
  }

  # --- Eliminar columnas constantes (varianza 0)
  vars <- vapply(datos, stats::var, numeric(1), na.rm = TRUE)
  cols_validas <- names(vars)[vars > 0]
  if (length(cols_validas) == 0) stop("Todas las columnas tienen varianza 0.")
  if (length(cols_validas) < ncol(datos)) {
    warning("Se eliminaron columnas constantes: ", paste(setdiff(names(vars), cols_validas), collapse = ", "))
  }
  datos <- datos[, cols_validas, drop = FALSE]

  # --- Ejecutar PCA (usa centrado/escalado interno de prcomp)
  pca <- stats::prcomp(datos, center = center, scale. = escalar)

  # --- Salidas organizadas
  proy   <- as.data.frame(pca$x)          # scores
  cargas <- as.data.frame(pca$rotation)   # loadings

  # Varianza explicada
  s2 <- pca$sdev^2
  var_prop <- s2 / sum(s2)
  var_exp <- data.frame(
    PC = paste0("PC", seq_along(var_prop)),
    Proporcion = var_prop,
    Acumulada = cumsum(var_prop),
    row.names = NULL
  )

  # Escalado (si prcomp centró/escaló, los guarda en estos slots)
  esc <- list(
    center = if (!is.null(pca$center)) pca$center else (if (isTRUE(center)) attr(scale(datos, center = TRUE, scale = FALSE), "scaled:center") else NULL),
    scale  = if (!is.null(pca$scale))  pca$scale  else (if (isTRUE(escalar)) attr(scale(datos, center = FALSE, scale = TRUE), "scaled:scale") else NULL)
  )

  # Preservar rownames si se pide
  if (isTRUE(keep_row_names) && !is.null(rownames(data_matriz))) {
    rownames(proy) <- rownames(datos)  # ya viene, pero reforzamos por si hubo filtrado
  }

  list(
    pca = pca,
    proyeccion = proy,
    cargas = cargas,
    var_exp = var_exp,
    escalado = esc,
    cols_usadas = cols_validas,
    filas_usadas = if (!is.null(rownames(data_matriz))) rownames(data_matriz)[filas_usadas] else filas_usadas
  )
}

#' Pipeline completo de emociones: tokenización → resumen → PCA → clustering → gráficos
#'
#' @description
#' Ejecuta un flujo completo de análisis emocional a partir de **textos crudos** o de una
#' **lista de tokens**, usando un **lexicón** (filas = tokens, columnas = emociones/polaridades).
#' Incluye resumen por comentario, tratamiento de NA/columnas constantes, PCA (centrado/escalado),
#' k-means reproducible y gráficos (scatter, elipses y biplot), además de métricas de calidad (OOV).
#'
#' @param comentarios \code{character} opcional. Vector de textos (uno por comentario).
#' @param lista_tokens \code{list} opcional. Lista de vectores de tokens por comentario.
#'   Si se proveen ambos, se usa \code{lista_tokens}.
#' @param lexicon \code{data.frame} o \code{matrix}. Filas = tokens del diccionario (mismo idioma
#'   que los tokens), columnas = emociones/polaridades (numéricas). Debe tener \code{rownames} con los tokens.
#' @param tokenizer \code{function} opcional. Función que convierte un texto en vector de tokens.
#'   Por defecto se usa una tokenización básica robusta a acentos y puntuación.
#' @param resumen_fun \code{c("sum","mean")}. Cómo agregar emociones por comentario. Por defecto \code{"sum"}.
#' @param k \code{integer}. Número de clusters para k-means. Por defecto \code{4}.
#' @param nstart \code{integer}. Reintentos aleatorios del k-means. Por defecto \code{20}.
#' @param seed \code{NULL} o \code{integer}. Semilla para reproducibilidad. Por defecto \code{123}.
#' @param na_action \code{c("omit","zero","fail")}. Tratamiento de NA tras el resumen. Por defecto \code{"omit"}.
#' @param columnas_map \code{NULL} o \code{named list}. Mapeo flexible de nombres NRC/polaridades (regex).
#'   Si \code{NULL}, se intenta detectar automáticamente en inglés y español.
#' @param hacer_graficos \code{logical}. Si \code{TRUE}, devuelve gráficos PCA (scatter, elipses y biplot). Por defecto \code{TRUE}.
#' @param palette \code{NULL}, nombre de paleta de \pkg{ggplot2} (p.ej. \code{"Dark2"}) o vector de colores para clusters.
#' @param top_n_loadings \code{NULL} o \code{integer}. En biplot, número de variables con mayores cargas a etiquetar. Por defecto \code{10}.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{matriz_emocional}: \code{data.frame} con emociones/polaridades por comentario, cluster y emoción predominante.
#'   \item \code{pca}, \code{proyeccion}, \code{cargas}, \code{var_exp}: resultados del PCA.
#'   \item \code{clusters}: objeto \code{kmeans}.
#'   \item \code{graficos}: lista con \code{scatter}, \code{elipses}, \code{biplot} (si \code{hacer_graficos=TRUE}).
#'   \item \code{metricas}: lista con \code{oov_rate} (tokens fuera del diccionario) y conteos útiles.
#'   \item \code{cols_emociones}: vector con los nombres de las columnas emocionales usadas (8 NRC).
#' }
#'
#' @examples
#' \dontrun{
#' # Usando tus objetos:
#' comentarios <- resultados$TextoSentencias
#' lexicon     <- as.data.frame(resultados$VectorTextoSentimientosNRC)  # filas=tokens
#'
#' out <- pipeline_emociones(
#'   comentarios = comentarios,
#'   lexicon = lexicon,
#'   k = 3, resumen_fun = "sum", seed = 42,
#'   palette = "Dark2"
#' )
#'
#' # Vistas:
#' head(out$matriz_emocional)
#' out$var_exp
#' print(out$graficos$scatter)
#' print(out$graficos$elipses)
#' print(out$graficos$biplot)
#' }
#' @importFrom stats prcomp kmeans var complete.cases predict
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal theme element_text scale_color_brewer scale_color_manual
#' @importFrom ggplot2 stat_ellipse geom_segment coord_fixed geom_text
#' @importFrom ggrepel geom_text_repel
#' @family utilidades
#' @export
#' @param "mean" (auto) TODO: describir parámetro.
pipeline_emociones <- function(
    comentarios = NULL,
    lista_tokens = NULL,
    lexicon,
    tokenizer = NULL,
    resumen_fun = c("sum","mean"),
    k = 4,
    nstart = 20,
    seed = 123,
    na_action = c("omit","zero","fail"),
    columnas_map = NULL,
    hacer_graficos = TRUE,
    palette = NULL,
    top_n_loadings = 10
) {
  # ---- Validaciones
  resumen_fun <- match.arg(resumen_fun)
  na_action   <- match.arg(na_action)
  if (is.null(lista_tokens) && is.null(comentarios)) {
    stop("Proporciona `lista_tokens` o `comentarios`.")
  }
  lex_df <- as.data.frame(lexicon, stringsAsFactors = FALSE)
  if (is.null(rownames(lex_df))) stop("`lexicon` debe tener rownames = tokens del diccionario.")
  cols_num <- names(lex_df)[vapply(lex_df, is.numeric, logical(1))]
  if (!length(cols_num)) stop("`lexicon` no tiene columnas numéricas (emociones/polaridades).")

  # ---- Tokenización o uso directo de lista
  if (is.null(lista_tokens)) {
    if (is.null(tokenizer)) {
      tokenizer <- function(x) {
        x <- tolower(x)
        x <- gsub("[^[:alnum:]áéíóúüñ\\s]", " ", x)
        x <- gsub("\\s+", " ", x)
        unlist(strsplit(trimws(x), " "), use.names = FALSE)
      }
    }
    lista_tokens <- lapply(comentarios, tokenizer)
    names(lista_tokens) <- if (!is.null(names(comentarios))) names(comentarios) else paste0("doc_", seq_along(lista_tokens))
  }

  # ---- Métrica OOV
  tok_all <- unique(tolower(unlist(lista_tokens)))
  hit <- tolower(rownames(lex_df))
  oov_rate <- mean(!(tok_all %in% hit))

  # ---- Resumen emociones por comentario (sum/mean)
  lex_df$.tok <- tolower(rownames(lex_df))
  resumir_emociones_por_comentario <- function(lista_tokens, matriz_lexica, fun = c("sum","mean")) {
    fun <- match.arg(fun)
    cols_num <- names(matriz_lexica)[vapply(matriz_lexica, is.numeric, logical(1))]
    out <- lapply(seq_along(lista_tokens), function(i) {
      v <- tolower(lista_tokens[[i]])
      sub <- matriz_lexica[match(v, matriz_lexica$.tok), cols_num, drop = FALSE]
      sub <- sub[stats::complete.cases(sub), , drop = FALSE]
      if (!nrow(sub)) return(setNames(numeric(length(cols_num)), cols_num))
      if (fun == "sum") colSums(sub, na.rm = TRUE) else colMeans(sub, na.rm = TRUE)
    })
    out <- do.call(rbind, out)
    rownames(out) <- names(lista_tokens)
    as.data.frame(out)
  }
  mat_res <- resumir_emociones_por_comentario(lista_tokens, lex_df, resumen_fun)

  # ---- Mapeo flexible de columnas NRC/polaridades
  if (is.null(columnas_map)) {
    columnas_map <- list(
      ira           = "ira|anger",
      anticipación  = "anticipación|anticipacion|anticipation",
      disgusto      = "disgusto|asco|disgust",
      miedo         = "miedo|fear",
      alegría       = "alegría|alegria|joy",
      tristeza      = "tristeza|sadness",
      sorpresa      = "sorpresa|surprise",
      confianza     = "confianza|trust",
      negativo      = "negativo|negative",
      positivo      = "positivo|positive"
    )
  }
  pick_col <- function(pat) {
    hits <- grep(pat, names(mat_res), ignore.case = TRUE, value = TRUE)
    if (length(hits) > 0) hits[1] else NA_character_
  }
  cols <- vapply(columnas_map, pick_col, FUN.VALUE = character(1))
  cols_emoc <- unname(cols[c("ira","anticipación","disgusto","miedo","alegría","tristeza","sorpresa","confianza")])
  if (anyNA(cols_emoc)) stop("No se identificaron las 8 emociones NRC en el resumen. Revisa `columnas_map`.")
  cols_pola <- unname(stats::na.omit(cols[c("negativo","positivo")]))

  mat_work <- mat_res[, c(cols_emoc, cols_pola), drop = FALSE]

  # ---- Tratamiento de NA
  if (na_action == "fail" && anyNA(mat_work)) stop("Se encontraron NA y `na_action='fail'`.")
  if (na_action == "omit") {
    keep <- stats::complete.cases(mat_work)
    if (!any(keep)) stop("No quedan filas tras omitir NA.")
    mat_work <- mat_work[keep, , drop = FALSE]
  } else if (na_action == "zero") {
    mat_work[is.na(mat_work)] <- 0
  }

  # ---- PCA (sobre 8 emociones)
  pca <- stats::prcomp(mat_work[, cols_emoc, drop = FALSE], center = TRUE, scale. = TRUE)
  proy <- as.data.frame(pca$x)
  cargas <- as.data.frame(pca$rotation)
  s2 <- pca$sdev^2
  var_exp <- data.frame(PC = paste0("PC", seq_along(s2)),
                        Proporcion = s2 / sum(s2),
                        Acumulada  = cumsum(s2 / sum(s2)))

  # ---- Clustering (sobre PCs)
  if (!is.null(seed)) set.seed(seed)
  km <- stats::kmeans(mat_work[, cols_emoc, drop = FALSE], centers = k, nstart = nstart)
  cluster_fac <- factor(km$cluster)

  # ---- Emoción predominante
  emocion_mas_fuerte <- function(df8) {
    nm <- names(df8)
    nm[max.col(df8, ties.method = "first")]
  }
  emo_pred <- emocion_mas_fuerte(mat_work[, cols_emoc, drop = FALSE])

  # ---- Armar salida tabular
  matriz_emocional <- cbind(mat_work, cluster = cluster_fac, emocion_predominante = emo_pred)
  rownames(matriz_emocional) <- rownames(mat_work)

  # ---- Gráficos (opcional)
  graficos <- list()
  if (isTRUE(hacer_graficos)) {
    # ejes con % varianza
    axis_x <- paste0("PC1 (", sprintf('%.1f', 100 * var_exp$Proporcion[1]), "%)")
    axis_y <- paste0("PC2 (", sprintf('%.1f', 100 * var_exp$Proporcion[2]), "%)")

    # datos para plot
    proj_plot <- proy[, 1:2, drop = FALSE]
    names(proj_plot) <- c("PCX","PCY")
    proj_plot$Cluster  <- cluster_fac
    proj_plot$Etiqueta <- rownames(proj_plot)

    # paleta
    add_pal <- function(p) {
      if (is.null(palette)) return(p)
      if (length(palette) == 1L) p + ggplot2::scale_color_brewer(palette = palette) else p + ggplot2::scale_color_manual(values = palette)
    }

    # scatter
    g_scatter <- ggplot2::ggplot(proj_plot, ggplot2::aes(PCX, PCY, color = Cluster)) +
      ggplot2::geom_point(size = 1.8, alpha = 0.75) +
      ggrepel::geom_text_repel(ggplot2::aes(label = Etiqueta), size = 2.6, max.overlaps = 100, segment.size = 0.2) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "PCA de emociones", x = axis_x, y = axis_y, color = "Cluster") +
      ggplot2::theme(legend.position = "bottom")
    g_scatter <- add_pal(g_scatter)

    # elipses
    g_elipses <- ggplot2::ggplot(proj_plot, ggplot2::aes(PCX, PCY, color = Cluster)) +
      ggplot2::geom_point(size = 1.8, alpha = 0.75) +
      ggplot2::stat_ellipse(type = "norm", level = 0.95, linewidth = 0.6, alpha = 0.25) +
      ggrepel::geom_text_repel(ggplot2::aes(label = Etiqueta), size = 2.6, max.overlaps = 100, segment.size = 0.2) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "PCA de emociones — elipses", x = axis_x, y = axis_y, color = "Cluster") +
      ggplot2::theme(legend.position = "bottom")
    g_elipses <- add_pal(g_elipses)

    # biplot (puntos + cargas)
    L <- cargas[, 1:2, drop = FALSE]
    names(L) <- c("PCX","PCY")
    L$Variable <- rownames(L)
    # auto-escalado de cargas al rango de scores
    rng_scores <- max(abs(unlist(proj_plot[, c("PCX","PCY")])), na.rm = TRUE)
    rng_load   <- max(abs(c(L$PCX, L$PCY)), na.rm = TRUE)
    scale_auto <- if (rng_load > 0) (0.9 * rng_scores) / rng_load else 1
    L$Norma <- sqrt(L$PCX^2 + L$PCY^2)
    if (!is.null(top_n_loadings) && is.finite(top_n_loadings) && top_n_loadings > 0) {
      L <- L[order(L$Norma, decreasing = TRUE), ]
      L <- L[seq_len(min(top_n_loadings, nrow(L))), , drop = FALSE]
    }
    L$PCX <- L$PCX * scale_auto
    L$PCY <- L$PCY * scale_auto

    g_biplot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = proj_plot, ggplot2::aes(PCX, PCY, color = Cluster), size = 1.8, alpha = 0.75) +
      ggrepel::geom_text_repel(data = proj_plot, ggplot2::aes(PCX, PCY, label = Etiqueta), size = 2.4, max.overlaps = 100, segment.size = 0.2) +
      ggplot2::geom_segment(data = L, ggplot2::aes(x = 0, y = 0, xend = PCX, yend = PCY),
                            arrow = ggplot2::arrow(length = grid::unit(0.20, "cm")), linewidth = 0.6, colour = "gray40") +
      ggrepel::geom_text_repel(data = L, ggplot2::aes(PCX, PCY, label = Variable), size = 3, segment.size = 0.2) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = "PCA de emociones — biplot",
                    x = axis_x, y = axis_y, color = "Cluster") +
      ggplot2::theme(legend.position = "bottom")
    g_biplot <- add_pal(g_biplot)

    graficos <- list(scatter = g_scatter, elipses = g_elipses, biplot = g_biplot)
  }

  list(
    matriz_emocional = matriz_emocional,
    pca = pca,
    proyeccion = proy,
    cargas = cargas,
    var_exp = var_exp,
    clusters = km,
    graficos = graficos,
    metricas = list(oov_rate = oov_rate,
                    n_docs = nrow(matriz_emocional),
                    n_tokens_unicos = length(tok_all)),
    cols_emociones = cols_emoc
  )
}




#' Procesar emociones por comentario con traducción y clustering
#'
#' @description
#' Dada una lista de tokens por comentario y una matriz léxica de emociones por token,
#' resume emociones a nivel de comentario (suma/mean), ejecuta PCA (centrado/escalado),
#' aplica \emph{k}-means para segmentar y genera etiquetas textuales auxiliares.
#' Soporta mapeo flexible de nombres NRC y manejo de NA.
#'
#' @details
#' - La función asume que tienes una función helper \code{resumir_emociones_por_comentario()}
#'   que consolida los tokens de cada comentario contra la matriz léxica
#'   (filas = tokens, columnas = emociones/polaridades).
#' - Si no cuentas con \code{generar_pca_emocional()}, se usa \code{prcomp()} directamente.
#' - \code{kmeans} se ejecuta con \code{nstart} configurable y semilla reproducible.
#' - El mapeo de columnas emocionales es flexible: puedes pasar alias como
#'   \code{anger/ira}, \code{joy/alegria}, \code{anticipation/anticipación}, etc.
#'
#' @param lista_tokens \code{list} de vectores (tokens por comentario) en el mismo orden
#'   que quieras en la salida.
#' @param matriz_emociones_traducida \code{data.frame} o \code{matrix} con filas = tokens,
#'   columnas = emociones/polaridades (p. ej. NRC + \code{positive}/\code{negative}).
#' @param metodo_lexico \code{character}. Nombre del método léxico a usar para
#'   \code{obtener_palabra_mas_emocional()} (solo si esa helper está disponible).
#'   Por defecto \code{"syuzhet"}.
#' @param k \code{integer}. Número de clusters para \code{kmeans}. Por defecto \code{4}.
#' @param nstart \code{integer}. Reintentos aleatorios de \code{kmeans}. Por defecto \code{20}.
#' @param seed \code{NULL} o \code{integer}. Semilla para reproducibilidad. Por defecto \code{123}.
#' @param resumen_fun \code{c("sum","mean")}. Cómo agregar emociones por comentario. Por defecto \code{"sum"}.
#' @param columnas_map \code{NULL} o \code{named list} con mapeo de nombres a usar.
#'   Si \code{NULL} se intenta detectar automáticamente. Ejemplo:
#'   \preformatted{list(
#'     ira="ira|anger", anticipación="anticipación|anticipacion|anticipation",
#'     disgusto="disgusto|asco|disgust", miedo="miedo|fear", alegría="alegría|alegria|joy",
#'     tristeza="tristeza|sadness", sorpresa="sorpresa|surprise", confianza="confianza|trust",
#'     negativo="negativo|negative", positivo="positivo|positive"
#'   )}
#' @param na_action \code{c("omit","zero","fail")}. Tratamiento de NA en la matriz resumida.
#' @param devolver_etiquetas \code{logical}. Si \code{TRUE}, intenta devolver etiquetas
#'   textuales por comentario con \code{obtener_palabra_mas_emocional()} si existe. Por defecto \code{TRUE}.
#'
#' @return Una \code{list} con:
#' \itemize{
#'   \item \code{matriz_emocional}: \code{data.frame} de emociones/polaridades por comentario + \code{cluster} y \code{emocion_predominante}.
#'   \item \code{pca}: objeto \code{prcomp}.
#'   \item \code{proyeccion}: \code{data.frame} con scores del PCA (PC1, PC2, ...).
#'   \item \code{cargas}: \code{data.frame} con loadings del PCA.
#'   \item \code{var_exp}: \code{data.frame} con proporción/acumulada de varianza.
#'   \item \code{clusters}: objeto \code{kmeans}.
#'   \item \code{etiquetas_texto}: \code{character} (si \code{devolver_etiquetas=TRUE} y helper disponible), o \code{NULL}.
#'   \item \code{cols_emociones}: nombres finales de columnas emocionales usadas.
#' }
#'
#' @examples
#' \dontrun{
#' # lista_tokens: lista donde cada elemento son tokens del comentario i
#' # matriz_emociones_traducida: filas = tokens, columnas = emociones NRC + polaridades
#'
#' out <- procesar_emociones_comentarios(
#'   lista_tokens = lista_de_tokens,
#'   matriz_emociones_traducida = matriz_lexica,  # p.ej. resultados$VectorTextoSentimientosNRC
#'   k = 3, metodo_lexico = "syuzhet", resumen_fun = "sum"
#' )
#'
#' # Visualizar PCA con tu función:
#' g <- graficar_pca_emociones_etiquetas(
#'   pca = out$pca, data = out$matriz_emocional[, out$cols_emociones, drop=FALSE],
#'   labels_cluster = out$matriz_emocional$cluster,
#'   labels_texto   = rownames(out$matriz_emocional),
#'   palette = "Dark2", biplot = TRUE, top_n_loadings = 10
#' )
#' print(g$scatter); print(g$elipses); print(g$biplot)
#' }
#' @importFrom stats prcomp kmeans var complete.cases
#' @family utilidades
#' @export
#' @param "mean" (auto) TODO: describir parámetro.
procesar_emociones_comentarios <- function(
    lista_tokens,
    matriz_emociones_traducida,
    metodo_lexico = "syuzhet",
    k = 4,
    nstart = 20,
    seed = 123,
    resumen_fun = c("sum","mean"),
    columnas_map = NULL,
    na_action = c("omit","zero","fail"),
    devolver_etiquetas = TRUE
) {
  # ---- Validaciones básicas
  if (!is.list(lista_tokens)) stop("`lista_tokens` debe ser una lista de vectores de tokens.")
  if (!is.data.frame(matriz_emociones_traducida) && !is.matrix(matriz_emociones_traducida)) {
    stop("`matriz_emociones_traducida` debe ser data.frame o matrix (filas=tokens, columnas=emociones).")
  }
  resumen_fun <- match.arg(resumen_fun)
  na_action   <- match.arg(na_action)

  lex_df <- as.data.frame(matriz_emociones_traducida, stringsAsFactors = FALSE)

  # ---- Helper requerida: resumir_emociones_por_comentario()
  if (!exists("resumir_emociones_por_comentario", mode = "function")) {
    stop("Falta la función helper `resumir_emociones_por_comentario()` en el entorno.")
  }
  # Resumen por comentario: filas = comentarios, columnas = emociones/polaridades
  mat_res <- resumir_emociones_por_comentario(
    lista_tokens = lista_tokens,
    matriz_lexica = lex_df,
    fun = resumen_fun # tu helper debería usar 'sum' o 'mean'
  )
  mat_res <- as.data.frame(mat_res, stringsAsFactors = FALSE)

  # ---- Mapeo de columnas emocionales (flex)
  if (is.null(columnas_map)) {
    columnas_map <- list(
      ira           = "ira|anger",
      anticipación  = "anticipación|anticipacion|anticipation",
      disgusto      = "disgusto|asco|disgust",
      miedo         = "miedo|fear",
      alegría       = "alegría|alegria|joy",
      tristeza      = "tristeza|sadness",
      sorpresa      = "sorpresa|surprise",
      confianza     = "confianza|trust",
      negativo      = "negativo|negative",
      positivo      = "positivo|positive"
    )
  }

  pick_col <- function(pat) {
    hits <- grep(pat, names(mat_res), ignore.case = TRUE, value = TRUE)
    if (length(hits) > 0) hits[1] else NA_character_
  }
  cols <- vapply(columnas_map, pick_col, FUN.VALUE = character(1))
  if (anyNA(cols[1:8])) {
    stop("No se pudieron identificar todas las 8 emociones básicas NRC en `mat_res` (revisa nombres/aliases).")
  }

  # Reordenar y construir matriz final de trabajo (8 emociones + polaridades si existen)
  cols_emoc   <- unname(cols[c("ira","anticipación","disgusto","miedo","alegría","tristeza","sorpresa","confianza")])
  cols_pola   <- unname(na.omit(cols[c("negativo","positivo")]))
  mat_work    <- mat_res[, c(cols_emoc, cols_pola), drop = FALSE]

  # ---- Tratamiento de NA a nivel de comentario
  if (na_action == "fail" && anyNA(mat_work)) stop("Se encontraron NA y `na_action='fail'`.")
  if (na_action == "omit") {
    keep <- stats::complete.cases(mat_work)
    if (!any(keep)) stop("No quedan filas tras omitir NA.")
    mat_work <- mat_work[keep, , drop = FALSE]
  } else if (na_action == "zero") {
    mat_work[is.na(mat_work)] <- 0
  }

  # ---- PCA (centrar/escalar)
  pca <- stats::prcomp(mat_work[, cols_emoc, drop = FALSE], center = TRUE, scale. = TRUE)
  proy <- as.data.frame(pca$x)
  cargas <- as.data.frame(pca$rotation)
  s2 <- pca$sdev^2
  var_exp <- data.frame(
    PC = paste0("PC", seq_along(s2)),
    Proporcion = s2 / sum(s2),
    Acumulada  = cumsum(s2 / sum(s2)),
    row.names = NULL
  )

  # ---- Clustering k-means (sobre PCs o sobre emociones: aquí sobre PCs)
  if (!is.null(seed)) set.seed(seed)
  km <- stats::kmeans(proy[, 1:min(5, ncol(proy)), drop = FALSE], centers = k, nstart = nstart)
  cluster_fac <- factor(km$cluster)

  # ---- Emoción predominante por comentario (argmax sobre 8 emociones)
  emocion_mas_fuerte <- function(df8) {
    nm <- names(df8)
    nm[max.col(df8, ties.method = "first")]
  }
  emo_pred <- emocion_mas_fuerte(mat_work[, cols_emoc, drop = FALSE])

  # ---- Ensamblar matriz emocional final para reporting
  matriz_emocional <- cbind(
    mat_work,
    cluster = cluster_fac,
    emocion_predominante = emo_pred
  )
  rownames(matriz_emocional) <- rownames(mat_work)

  # ---- Etiquetas textuales (opcional, si helper existe)
  etiquetas <- NULL
  if (isTRUE(devolver_etiquetas) && exists("obtener_palabra_mas_emocional", mode = "function")) {
    # La helper suele esperar tokens en filas; si 'lex_df' está en filas=tokens, columnas=métodos, ajusta según tu implementación
    # Aquí un intento genérico, ajusta si tu helper usa otra firma:
    etiquetas <- tryCatch(
      obtener_palabra_mas_emocional(
        t(lex_df),                       # <- tokens en filas (si tu helper lo requiere)
        metodo = metodo_lexico,
        devolver_valor = TRUE
      ),
      error = function(e) NULL
    )
  }

  list(
    matriz_emocional = matriz_emocional,
    pca = pca,
    proyeccion = proy,
    cargas = cargas,
    var_exp = var_exp,
    clusters = km,
    etiquetas_texto = etiquetas,
    cols_emociones = cols_emoc
  )
}


#' Visualizar resultados de emociones por comentario (PCA + barras)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Genera visualizaciones clave a partir de una matriz emocional por comentario:
#' (1) gráficos PCA (dispersión, elipses y biplot opcional) coloreados por \code{cluster},
#' con etiquetas de texto; (2) barras agrupadas/resumidas de las emociones.
#'
#' @param pca Objeto de clase \code{prcomp} (PCA ya calculado sobre emociones).
#' @param matriz_emocional \code{data.frame} con columnas numéricas de emociones y una
#'        columna factor \code{cluster}. Puede contener además \code{positivo} y \code{negativo}.
#' @param etiquetas \code{character}. Vector de etiquetas textuales por comentario (mismo largo que filas).
#' @param cols_emociones \code{character} opcional. Nombres de columnas de emociones a graficar en PCA
#'        (por defecto intenta detectar las 8 NRC: ira, anticipación, disgusto, miedo,
#'        alegría, tristeza, sorpresa, confianza — en ES o EN).
#' @param componentes \code{integer} vector de longitud 2. Componentes a proyectar (por defecto \code{c(1,2)}).
#' @param palette \code{NULL} | nombre de paleta \pkg{ggplot2} (p.ej. "Dark2") | vector de colores.
#' @param biplot \code{logical}. Si \code{TRUE}, añade biplot con vectores de carga (solo si hay 2 componentes).
#' @param top_n_loadings \code{integer} o \code{NULL}. Número de variables con mayor norma de carga a etiquetar en biplot.
#'        Por defecto \code{10}. Usa \code{NULL} para mostrar todas.
#' @param barras_modo \code{c("total","porcentaje")}. Modo del gráfico de barras agregadas (suma o \%).
#' @param titulo_base \code{character}. Título base para los gráficos.
#'
#' @return Lista con:
#' \describe{
#'   \item{pca}{Lista con \code{scatter}, \code{elipses} y (si aplica) \code{biplot}.}
#'   \item{barras}{Objeto \code{ggplot2} con barras agregadas de emociones.}
#'   \item{data_barras}{\code{data.frame} usado en las barras (útil para tablas/reportes).}
#' }
#'
#' @details
#' El biplot escala automáticamente los vectores de carga al rango de los scores para
#' que ambos sean visibles en el mismo plano. El gráfico de barras resume las emociones
#' especificadas en \code{cols_emociones}; si existen \code{positivo} y \code{negativo}, se incluyen también.
#'
#' @examples
#' \dontrun{
#' # Asumiendo salida del pipeline:
#' out <- pipeline_emociones(comentarios, lexicon, k = 3, seed = 42)
#' viz <- visualizar_emociones_comentarios(
#'   pca = out$pca,
#'   matriz_emocional = out$matriz_emocional,
#'   etiquetas = rownames(out$matriz_emocional),
#'   cols_emociones = out$cols_emociones,   # las 8 NRC detectadas
#'   palette = "Dark2",
#'   biplot = TRUE,
#'   barras_modo = "porcentaje"
#' )
#' print(viz$pca$scatter)
#' print(viz$pca$elipses)
#' print(viz$pca$biplot)
#' print(viz$barras)
#' head(viz$data_barras)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal theme element_text scale_color_brewer scale_color_manual
#' @importFrom ggplot2 stat_ellipse geom_segment coord_fixed geom_col geom_text
#' @importFrom ggrepel geom_text_repel
#' @export
# Reemplaza tu función completa por esta versión o, si prefieres, copia solo la sección "Barras" de aquí.

visualizar_emociones_comentarios <- function(
    pca,
    matriz_emocional,
    etiquetas,
    cols_emociones = NULL,
    componentes = c(1, 2),
    palette = NULL,
    biplot = TRUE,
    top_n_loadings = 10,
    barras_modo = c("total","porcentaje"),
    titulo_base = "Emociones por comentario",
    label_max_overlaps = 200   # <-- nuevo: controla solapamiento de ggrepel
) {
  stopifnot(inherits(pca, "prcomp"), is.data.frame(matriz_emocional))
  if (!"cluster" %in% names(matriz_emocional)) stop("Se requiere columna factor `cluster`.")
  if (length(etiquetas) != nrow(matriz_emocional)) stop("`etiquetas` debe coincidir con filas.")
  barras_modo <- match.arg(barras_modo)

  # Detecta emociones si no se pasan
  if (is.null(cols_emociones)) {
    patrones <- c("ira|anger","anticipación|anticipacion|anticipation","disgusto|asco|disgust",
                  "miedo|fear","alegría|alegria|joy","tristeza|sadness",
                  "sorpresa|surprise","confianza|trust")
    pick <- function(pat) grep(pat, names(matriz_emocional), ignore.case = TRUE, value = TRUE)[1]
    cols_emociones <- vapply(patrones, pick, FUN.VALUE = character(1))
    if (anyNA(cols_emociones)) stop("No se detectaron las 8 emociones NRC; pasa `cols_emociones` explícitamente.")
  }

  # Proyección PCA
  if (max(componentes) > ncol(pca$x)) stop("`pca` no tiene tantas componentes.")
  proj <- as.data.frame(pca$x[, componentes, drop = FALSE])
  names(proj) <- c("PCX","PCY")
  proj$Cluster  <- factor(matriz_emocional$cluster)
  proj$Etiqueta <- etiquetas

  s2 <- pca$sdev^2
  var_exp <- s2 / sum(s2)
  axis_x <- paste0("PC", componentes[1], " (", sprintf('%.1f', 100 * var_exp[componentes[1]]), "%)")
  axis_y <- paste0("PC", componentes[2], " (", sprintf('%.1f', 100 * var_exp[componentes[2]]), "%)")

  add_pal <- function(p) {
    if (is.null(palette)) return(p)
    if (length(palette) == 1L) p + ggplot2::scale_color_brewer(palette = palette)
    else p + ggplot2::scale_color_manual(values = palette)
  }

  g_scatter <- ggplot2::ggplot(proj, ggplot2::aes(PCX, PCY, color = Cluster)) +
    ggplot2::geom_point(size = 1.9, alpha = 0.8) +
    ggrepel::geom_text_repel(ggplot2::aes(label = Etiqueta), size = 2.6,
                             max.overlaps = label_max_overlaps, segment.size = 0.2) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = paste(titulo_base, "— PCA"),
                  x = axis_x, y = axis_y, color = "Cluster") +
    ggplot2::theme(legend.position = "bottom")
  g_scatter <- add_pal(g_scatter)

  g_elipses <- ggplot2::ggplot(proj, ggplot2::aes(PCX, PCY, color = Cluster)) +
    ggplot2::geom_point(size = 1.6, alpha = 0.6) +
    ggplot2::stat_ellipse(type = "norm", level = 0.95, linewidth = 0.6, alpha = 0.25) +
    ggrepel::geom_text_repel(ggplot2::aes(label = Etiqueta), size = 2.4,
                             max.overlaps = label_max_overlaps, segment.size = 0.2) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = paste(titulo_base, "— PCA con elipses"),
                  x = axis_x, y = axis_y, color = "Cluster") +
    ggplot2::theme(legend.position = "bottom")
  g_elipses <- add_pal(g_elipses)

  g_biplot <- NULL
  if (isTRUE(biplot) && length(componentes) == 2) {
    L <- as.data.frame(pca$rotation[, componentes, drop = FALSE])
    names(L) <- c("PCX","PCY"); L$Variable <- rownames(L)
    rng_scores <- max(abs(unlist(proj[, c("PCX","PCY")])), na.rm = TRUE)
    rng_load   <- max(abs(c(L$PCX, L$PCY)), na.rm = TRUE)
    scale_auto <- if (rng_load > 0) (0.9 * rng_scores) / rng_load else 1
    L$Norma <- sqrt(L$PCX^2 + L$PCY^2)
    if (!is.null(top_n_loadings) && is.finite(top_n_loadings) && top_n_loadings > 0) {
      L <- L[order(L$Norma, decreasing = TRUE), ][seq_len(min(top_n_loadings, nrow(L))), , drop = FALSE]
    }
    L$PCX <- L$PCX * scale_auto; L$PCY <- L$PCY * scale_auto

    g_biplot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = proj, ggplot2::aes(PCX, PCY, color = Cluster), size = 1.8, alpha = 0.8) +
      ggrepel::geom_text_repel(data = proj, ggplot2::aes(PCX, PCY, label = Etiqueta), size = 2.4,
                               max.overlaps = label_max_overlaps, segment.size = 0.2) +
      ggplot2::geom_segment(data = L, ggplot2::aes(x = 0, y = 0, xend = PCX, yend = PCY),
                            arrow = ggplot2::arrow(length = grid::unit(0.20, "cm")), linewidth = 0.6, colour = "gray40") +
      ggrepel::geom_text_repel(data = L, ggplot2::aes(PCX, PCY, label = Variable), size = 3,
                               max.overlaps = label_max_overlaps, segment.size = 0.2) +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(title = paste(titulo_base, "— biplot"),
                    x = axis_x, y = axis_y, color = "Cluster") +
      ggplot2::theme(legend.position = "bottom")
    g_biplot <- add_pal(g_biplot)
  }

  # -------- Barras agregadas (ARREGLADO) --------
  df_emox <- matriz_emocional[, cols_emociones, drop = FALSE]
  extra_cols <- intersect(c("positivo","positive","negativo","negative"), names(matriz_emocional))
  if (length(extra_cols)) df_emox <- cbind(df_emox, matriz_emocional[, extra_cols, drop = FALSE])

  suma <- colSums(df_emox, na.rm = TRUE)
  data_barras <- data.frame(Emocion = names(suma), Valor = as.numeric(suma), stringsAsFactors = FALSE)

  if (barras_modo == "porcentaje") {
    total <- sum(data_barras$Valor)
    data_barras$Porcentaje <- if (total > 0) 100 * data_barras$Valor / total else 0
    yvar <- "Porcentaje"
    ylab <- "Porcentaje (%)"
    data_barras$label <- paste0(sprintf("%.1f", data_barras$Porcentaje), "%")
  } else {
    yvar <- "Valor"
    ylab <- "Suma (conteo/peso)"
    data_barras$label <- data_barras$Valor
  }

  # Orden descendente por la métrica usada
  ord_col <- if (barras_modo == "porcentaje") "Porcentaje" else "Valor"
  data_barras <- data_barras[order(-data_barras[[ord_col]]), ]
  data_barras$Emocion <- factor(data_barras$Emocion, levels = data_barras$Emocion)

  g_barras <- ggplot2::ggplot(
    data_barras,
    ggplot2::aes(x = Emocion, y = .data[[yvar]], fill = Emocion)   # <-- y global
  ) +
    ggplot2::geom_col(show.legend = FALSE) +
    ggplot2::geom_text(ggplot2::aes(y = .data[[yvar]], label = label),   # <-- y para geom_text
                       vjust = -0.3, size = 3) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::labs(title = paste(titulo_base, "— resumen emociones"),
                  x = "Emoción / Polaridad", y = ylab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

  list(
    pca = list(scatter = g_scatter, elipses = g_elipses, biplot = g_biplot),
    barras = g_barras,
    data_barras = data_barras
  )
}

#' Resumir emociones/lexicones por comentario a partir de tokens
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Suma por comentario los puntajes de una matriz valorada por token (filas = tokens,
#' columnas = emociones o lexicones). Soporta normalización por fila, z-score y salida
#' en formato ancho o largo. Es tolerante a mayúsculas/acentos y a tokens no presentes.
#'
#' @param lista_tokens Lista de vectores de tokens por comentario. Si es \code{character},
#'   se asumirá texto con tokens separados por espacios y se tokenizará con \code{strsplit}.
#' @param matriz_valores \code{data.frame} / \code{matrix} con \strong{rownames} = tokens y
#'   \strong{columnas} = variables a resumir (p.ej., \code{syuzhet}, \code{bing}, \code{afinn}, \code{nrc}
#'   o emociones NRC). Recomendado: \code{MVM$MatrizTranspuestaSinDup}.
#' @param columnas Vector de nombres de columnas a resumir. Si \code{NULL}, se usarán todas
#'   las columnas numéricas de \code{matriz_valores}.
#' @param normalizar \code{c("none","row_sum","zscore")}. \code{"row_sum"} divide cada fila
#'   por la suma de sus valores; \code{"zscore"} estandariza cada columna (media 0, sd 1).
#'   Por defecto \code{"none"}.
#' @param tolower Logical. Si \code{TRUE}, convierte tokens y vocabulario a minúsculas. Default \code{TRUE}.
#' @param quitar_acentos Logical. Si \code{TRUE}, normaliza acentos (requiere \pkg{stringi}). Default \code{TRUE}.
#' @param return_long Logical. Si \code{TRUE}, devuelve además una versión \emph{long} con
#'   columnas \code{Comentario}, \code{Variable}, \code{Valor}. Default \code{FALSE}.
#'
#' @return Una lista con:
#' \describe{
#'   \item{\code{resumen}}{data.frame ancho: una fila por comentario, columnas = variables resumidas.}
#'   \item{\code{resumen_long}}{(opcional) data.frame largo con Comentario, Variable, Valor.}
#'   \item{\code{tokens_fuera_vocab}}{lista de tokens por comentario que no están en el vocabulario.}
#' }
#'
#' @examples
#' # Suponiendo:
#' # lista_de_tokens <- lapply(resultados$TextoSentencias, tokenizar_basico)
#' # matriz <- MVM$MatrizTranspuestaSinDup  # filas=tokens, cols=syuzhet/bing/afinn/nrc
#' out <- resumir_emociones_por_comentario(
#'   lista_tokens   = lista_de_tokens,
#'   matriz_valores = MVM$MatrizTranspuestaSinDup,
#'   columnas       = c("syuzhet","bing","afinn","nrc"),
#'   normalizar     = "none",
#'   return_long    = TRUE
#' )
#' head(out$resumen)
#' head(out$resumen_long)
#'
#' @export
#' @param "row_sum" (auto) TODO: describir parámetro.
#' @param "zscore" (auto) TODO: describir parámetro.
resumir_emociones_por_comentario <- function(
    lista_tokens,
    matriz_valores,
    columnas = NULL,
    normalizar = c("none","row_sum","zscore"),
    tolower = TRUE,
    quitar_acentos = TRUE,
    return_long = FALSE
){
  normalizar <- match.arg(normalizar)

  # ---- validar y preparar lista de tokens ----
  if (!is.list(lista_tokens)) {
    if (is.character(lista_tokens)) {
      lista_tokens <- strsplit(lista_tokens, "\\s+")
    } else {
      stop("`lista_tokens` debe ser lista de vectores o un vector character con tokens separados por espacio.")
    }
  }

  # ---- preparar matriz y columnas ----
  if (is.matrix(matriz_valores)) matriz_valores <- as.data.frame(matriz_valores)
  if (!is.data.frame(matriz_valores)) stop("`matriz_valores` debe ser data.frame/matrix.")
  if (is.null(rownames(matriz_valores))) stop("`matriz_valores` requiere rownames = tokens.")

  # Selección de columnas
  if (is.null(columnas)) {
    columnas <- names(matriz_valores)[vapply(matriz_valores, is.numeric, logical(1))]
  } else {
    faltan <- setdiff(columnas, names(matriz_valores))
    if (length(faltan)) stop("Columnas no encontradas en `matriz_valores`: ", paste(faltan, collapse=", "))
  }
  X <- as.matrix(matriz_valores[, columnas, drop = FALSE])

  # ---- normalización (columna o fila) ----
  if (normalizar == "zscore") {
    X <- scale(X)  # estandariza por columna
    X[is.na(X)] <- 0
  }

  # ---- armonizar vocabulario/tokens: minúsculas y acentos ----
  vocab <- rownames(X)

  norm_txt <- function(v) {
    if (tolower) v <- base::tolower(v)
    if (quitar_acentos) {
      if (!requireNamespace("stringi", quietly = TRUE))
        stop("Para quitar acentos, instala `stringi` o usa quitar_acentos = FALSE.")
      v <- stringi::stri_trans_general(v, "Latin-ASCII")
    }
    v
  }
  vocab_n <- norm_txt(vocab)

  # si cambió, reindexar X con nombres normalizados
  if (!identical(vocab, vocab_n)) {
    rownames(X) <- vocab_n
    vocab <- vocab_n
  }

  # ---- sumar por comentario (rápido y tolerante a repetidos) ----
  n <- length(lista_tokens)
  S <- matrix(0, nrow = n, ncol = ncol(X), dimnames = list(NULL, colnames(X)))
  fuera <- vector("list", n)

  for (i in seq_len(n)) {
    tk <- lista_tokens[[i]]
    if (!length(tk)) next
    tk <- norm_txt(tk)
    idx <- match(tk, vocab)
    ok  <- !is.na(idx)
    if (!any(ok)) {
      fuera[[i]] <- unique(tk)
      next
    }
    # suma por filas seleccionadas (si hay repetidos, se suman naturalmente)
    Xi <- X[idx[ok], , drop = FALSE]
    if (is.null(dim(Xi))) Xi <- matrix(Xi, nrow = 1, dimnames = list(NULL, colnames(X)))
    S[i, ] <- colSums(Xi, na.rm = TRUE)

    # guardar los que quedaron fuera
    if (any(!ok)) fuera[[i]] <- unique(tk[!ok])
  }

  # normalización por fila (proporciones dentro del comentario)
  if (normalizar == "row_sum") {
    rs <- rowSums(S, na.rm = TRUE)
    rs[rs == 0] <- 1
    S <- S / rs
  }

  resumen <- as.data.frame(S, stringsAsFactors = FALSE)
  rownames(resumen) <- paste0("C", seq_len(n))

  out <- list(resumen = resumen, tokens_fuera_vocab = fuera)

  if (isTRUE(return_long)) {
    if (!requireNamespace("tidyr", quietly = TRUE))
      stop("Para `return_long=TRUE` instala `tidyr`.")
    resumen$Comentario <- rownames(resumen)
    out$resumen_long <- tidyr::pivot_longer(
      resumen,
      cols = all_of(colnames(S)),
      names_to = "Variable",
      values_to = "Valor"
    )
  }

  out
}

#' Graficar curvas ROC para clasificación multiclase
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función toma una lista de objetos ROC generados por `pROC::roc()` para cada clase
#' y produce una visualización superpuesta de las curvas ROC con sus respectivos AUC.
#'
#' @param roc_list Lista de objetos ROC por clase. Usualmente se obtiene de una función de evaluación multiclase.
#'
#' @return Un objeto `ggplot2` con las curvas ROC por clase. Devuelve `NULL` si no hay curvas válidas.
#' @family visualizacion
#' @export
#'
#' @examples
#' graficar_roc_multiclase(resultados_modelo$roc_obj)
graficar_roc_multiclase <- function(roc_list) {
  library(ggplot2)

  datos_roc <- do.call(rbind, lapply(names(roc_list), function(cl) {
    roc_data <- roc_list[[cl]]

    if (is.null(roc_data) || !inherits(roc_data, "roc")) return(NULL)

    auc_val <- tryCatch(as.numeric(pROC::auc(roc_data)), error = function(e) NA)

    data.frame(
      FPR = 1 - roc_data$specificities,
      TPR = roc_data$sensitivities,
      Clase = cl,
      AUC = auc_val
    )
  }))

  if (is.null(datos_roc) || nrow(datos_roc) == 0) {
    message("No se pudo graficar ROC: no hay curvas válidas.")
    return(NULL)
  }

  leyenda_auc <- aggregate(AUC ~ Clase, data = datos_roc, FUN = mean)

  p1 <- ggplot(datos_roc, aes(x = FPR, y = TPR, color = Clase, linetype = Clase)) +
    geom_line(size = 0.7) +
    geom_abline(linetype = "dotted", color = "gray") +
    labs(title = "Curvas ROC por Clase",
         x = "Tasa de Falsos Positivos (FPR)",
         y = "Tasa de Verdaderos Positivos (TPR)",
         color = "Clase",
         linetype = "Clase") +
    theme_minimal()

  p2 <- gridExtra::tableGrob(leyenda_auc, rows = NULL)

  gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(3, 1))
}

#' Graficar curvas Precision-Recall para clasificación multiclase
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función genera un gráfico con las curvas Precision-Recall (PR) por clase
#' a partir de una lista de objetos retornados por `PRROC::pr.curve()`.
#'
#' @param pr_list Lista de objetos PR por clase, que contengan una curva y AUC.
#'
#' @return Un objeto `ggplot2` con las curvas PR por clase. Devuelve `NULL` si no hay curvas válidas.
#' @family visualizacion
#' @export
#'
#' @examples
#' graficar_pr_multiclase(resultados_modelo$pr_obj)
graficar_pr_multiclase <- function(pr_list) {
  library(ggplot2)

  datos_pr <- do.call(rbind, lapply(names(pr_list), function(cl) {
    pr_data <- pr_list[[cl]]
    if (is.null(pr_data) || is.null(pr_data$curve)) return(NULL)

    auc_val <- tryCatch(pr_data$auc.integral, error = function(e) NA)

    data.frame(
      Recall = pr_data$curve[, 1],
      Precision = pr_data$curve[, 2],
      Clase = cl,
      AUC = auc_val
    )
  }))

  if (is.null(datos_pr) || nrow(datos_pr) == 0) {
    message("No se pudo graficar PR: no hay curvas válidas.")
    return(NULL)
  }

  leyenda_auc <- aggregate(AUC ~ Clase, data = datos_pr, FUN = mean)

  p1 <- ggplot(datos_pr, aes(x = Recall, y = Precision, color = Clase, linetype = Clase)) +
    geom_line(size = 0.7) +
    labs(title = "Curvas Precision-Recall por Clase",
         x = "Recall",
         y = "Precisión",
         color = "Clase",
         linetype = "Clase") +
    theme_minimal()

  p2 <- gridExtra::tableGrob(leyenda_auc, rows = NULL)

  gridExtra::grid.arrange(p1, p2, ncol = 1, heights = c(3, 1))
}


#' Graficar la importancia de variables de un modelo de Gradient Boosting
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función toma un data frame de importancia de variables, como el devuelto por `summary(modelo, plotit = FALSE)`
#' de un modelo `gbm`, y genera un gráfico de barras ordenado por importancia relativa.
#'
#' @param importancia_df Data frame de importancia de variables (debe contener columnas `var` e `rel.inf`).
#' @param titulo Título opcional del gráfico.
#'
#' @return Un objeto `ggplot` con la visualización de la importancia de las variables.
#' @family visualizacion
#' @export
#'
#' @examples
#' res <- ajustar_gradient_boosting(acdfx, vPred_expandido, "diferencia")
#' print(graficar_importancia_variables(res$importancia))
graficar_importancia_variables <- function(importancia_df, titulo = "Importancia de Variables - GBM") {
  library(ggplot2)

  if (!all(c("var", "rel.inf") %in% colnames(importancia_df))) {
    stop("El data frame de importancia debe contener las columnas 'var' y 'rel.inf'")
  }

  importancia_df <- importancia_df[order(importancia_df$rel.inf, decreasing = TRUE), ]
  importancia_df$var <- factor(importancia_df$var, levels = rev(importancia_df$var))  # para orden en gráfico

  ggplot(importancia_df, aes(x = var, y = rel.inf)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    labs(title = titulo, x = "Variables", y = "Importancia Relativa") +
    theme_minimal()
}

#' Visualizar modelo de regresión polinomial
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función genera un gráfico de dispersión con curva ajustada (si hay 1 predictor)
#' o un gráfico 3D interactivo con superficie ajustada (si hay 2 predictores).
#'
#' @param df Data frame original con los datos.
#' @param modelo Objeto de modelo ajustado generado por \code{ajustar_modelo_polinomial}.
#' @param columnas_predictoras Vector con 1 o 2 nombres de columnas utilizadas como predictores.
#' @param columna_dependiente Nombre de la variable dependiente.
#'
#' @return Un gráfico \code{ggplot} o \code{plotly} según el número de predictores.
#' @family visualizacion
#' @export
#'
#' @examples
#' res <- ajustar_modelo_polinomial(acdfx, c("ira"), "diferencia", grado = 3)
#' g <- visualizar_modelo_polinomial(acdfx, res$modelo, c("ira"), "diferencia")
#' print(g)
visualizar_modelo_polinomial <- function(df, modelo, columnas_predictoras, columna_dependiente) {
  if (length(columnas_predictoras) == 1) {
    pred <- columnas_predictoras[1]
    df$Predicho <- predict(modelo, newdata = df)

    ggplot(df, aes_string(x = pred, y = columna_dependiente)) +
      geom_point(color = "steelblue", alpha = 0.6) +
      geom_line(aes_string(y = "Predicho"), color = "darkred", size = 1) +
      labs(title = "Regresión Polinomial",
           x = pred,
           y = columna_dependiente) +
      theme_minimal()

  } else if (length(columnas_predictoras) == 2) {
    pred1 <- columnas_predictoras[1]
    pred2 <- columnas_predictoras[2]

    grid <- expand.grid(
      pred1 = seq(min(df[[pred1]]), max(df[[pred1]]), length.out = 40),
      pred2 = seq(min(df[[pred2]]), max(df[[pred2]]), length.out = 40)
    )
    colnames(grid) <- columnas_predictoras
    grid$Predicho <- predict(modelo, newdata = grid)

    plot_ly(grid, x = ~get(pred1), y = ~get(pred2), z = ~Predicho,
            type = "surface", showscale = TRUE) %>%
      layout(title = "Superficie ajustada (Regresión Polinomial)",
             scene = list(xaxis = list(title = pred1),
                          yaxis = list(title = pred2),
                          zaxis = list(title = "Predicción")))

  } else {
    stop("Esta función solo soporta visualización para 1 o 2 predictores.")
  }
}

#' Curva de sentimiento con suavizados (LOESS y media móvil)
#'
#' @title CrearGraficoSentimiento
#' @description
#' Genera un gráfico de línea del vector de sentimiento a lo largo de las sentencias,
#' con dos suavizados opcionales: **LOESS** y **media móvil**. Puede **escalar** los
#' valores a `[0, 1]` para facilitar la comparación entre documentos.
#'
#' @param vectorSentimiento Numeric. Vector de puntajes de sentimiento (puede contener `NA`).
#' @param titulo Character. Título del gráfico. Por defecto `"Análisis de Sentimiento"`.
#' @param rescale_values Logical. Si `TRUE`, escala los valores a `[0, 1]`
#'   (vía `scales::rescale`).
#' @param k Integer. Tamaño de ventana para la media móvil centrada (`zoo::rollmean`). Por defecto `5`.
#' @param span Numeric `(0, 1]`. Span para el LOESS (más alto = mayor suavizado). Por defecto `0.2`.
#' @param center_line NULL o Numeric. Valor de la línea horizontal de referencia.
#'   Si `NULL`, usa `0` si el vector parece centrado en `[-1, 1]`; en otro caso `0.5`
#'   si se escalan los datos, o `median(vector, na.rm = TRUE)` si no se escalan.
#' @param colores Named character vector con colores para las series.
#'   Debe contener nombres: `"Sentimiento"`, `"Loess (suavizado)"`, `"Media móvil"`.
#'
#' @return
#' Una `list` con:
#' \itemize{
#'   \item \code{plot}: objeto ggplot.
#'   \item \code{data}: data.frame con columnas \code{sentencias}, \code{sentimientos}, \code{loess}, \code{rolling_mean}.
#' }
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- cumsum(rnorm(120, sd = 0.15))
#' res <- CrearGraficoSentimiento(x, titulo = "Sentimiento simulado",
#'                                rescale_values = TRUE, k = 7, span = 0.3)
#' res$plot
#' head(res$data)
#' }
#' @seealso \code{\link[stats]{loess}}, \code{\link[zoo]{rollmean}}, \code{\link[scales]{rescale}}
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_manual geom_hline theme_minimal theme guide_legend guides
#' @importFrom zoo rollmean
#' @importFrom scales rescale
#' @family visualizacion
#' @export
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
#' @param "Loess (suavizado (auto) TODO: describir parámetro.
CrearGraficoSentimiento <- function(vectorSentimiento,
                                    titulo = "Análisis de Sentimiento",
                                    rescale_values = TRUE,
                                    k = 5,
                                    span = 0.2,
                                    center_line = NULL,
                                    colores = c(
                                      "Sentimiento"        = "#2D2D2D",
                                      "Loess (suavizado)"  = "#1F77B4",
                                      "Media móvil"        = "#D62728"
                                    )) {
  # ---- Validaciones básicas ----
  if (!is.numeric(vectorSentimiento)) {
    stop("`vectorSentimiento` debe ser numérico.")
  }
  n <- length(vectorSentimiento)
  if (n < 3L) {
    stop("Se requieren al menos 3 observaciones para graficar y suavizar.")
  }
  if (!is.null(k) && (!is.numeric(k) || k < 2L)) {
    stop("`k` (ventana de media móvil) debe ser >= 2.")
  }
  if (!is.numeric(span) || span <= 0 || span > 1) {
    stop("`span` (LOESS) debe estar en (0,1].")
  }

  # ---- Preparación de la serie ----
  y <- as.numeric(vectorSentimiento)

  # Escalamiento opcional a [0,1]
  if (isTRUE(rescale_values)) {
    y_scaled <- scales::rescale(y, to = c(0, 1), from = range(y, na.rm = TRUE))
  } else {
    y_scaled <- y
  }

  df <- data.frame(
    sentencias  = seq_len(n),
    sentimientos = y_scaled
  )

  # ---- LOESS (solo con suficientes datos no NA) ----
  loess_fit <- rep(NA_real_, n)
  idx_ok <- stats::complete.cases(df$sentencias, df$sentimientos)
  if (sum(idx_ok) >= 6L) { # regla simple para loess razonable
    fit <- try(stats::loess(sentimientos ~ sentencias,
                            data = df[idx_ok, , drop = FALSE],
                            span = span, control = stats::loess.control(surface = "direct")),
               silent = TRUE)
    if (!inherits(fit, "try-error")) {
      loess_pred <- stats::predict(fit, newdata = df["sentencias"])
      loess_fit[seq_along(loess_pred)] <- loess_pred
    }
  }
  df$loess <- loess_fit

  # ---- Media móvil centrada (zoo::rollmean) ----
  if (!is.null(k) && k >= 2L && sum(idx_ok) >= k) {
    df$rolling_mean <- zoo::rollmean(df$sentimientos, k = k, fill = NA, align = "center")
  } else {
    df$rolling_mean <- NA_real_
  }

  # ---- Línea de referencia ----
  if (is.null(center_line)) {
    if (isTRUE(rescale_values)) {
      center_line <- 0.5
    } else {
      # Heurística: si parece en [-1,1], usar 0; si no, mediana
      r <- range(y, na.rm = TRUE)
      if (r[1] >= -1.1 && r[2] <= 1.1) {
        center_line <- 0
      } else {
        center_line <- stats::median(y, na.rm = TRUE)
      }
    }
  }

  # ---- Gráfico ----
  p <- ggplot2::ggplot(df, ggplot2::aes(sentencias, sentimientos)) +
    ggplot2::geom_line(ggplot2::aes(color = "Sentimiento"), linewidth = 0.7, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = loess, color = "Loess (suavizado)"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = rolling_mean, color = "Media móvil"), linewidth = 0.9, na.rm = TRUE) +
    ggplot2::labs(title = titulo, x = "Sentencias", y = "Sentimiento") +
    ggplot2::scale_color_manual(values = colores) +
    ggplot2::geom_hline(yintercept = center_line, linetype = "dashed") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "top") +
    ggplot2::guides(color = ggplot2::guide_legend(title = "Series"))

  list(plot = p, data = df)
}


