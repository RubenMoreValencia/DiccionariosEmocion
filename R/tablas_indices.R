#' Maximos y minimos por fila en una tabla de contingencia
#'
#' @title TabContingencia_MaxMinXFila
#' @description Calcula, por cada fila, el indice y el nombre de la columna con
#' el valor maximo y minimo. Es vectorizada y tolerante a NA.
#' @param dataMM Data.frame o matriz numerica.
#' @param NombreCMax Nombre de la columna de salida para la clase maxima.
#' @param NombreCMin Nombre de la columna de salida para la clase minima.
#' @return Lista con: \code{tabla} (data.frame con indices y nombres) y \code{longitud} (n filas).
#' @examples
#' m <- matrix(c(1,2,NA, 5,4,3), nrow=2, byrow=TRUE)
#' colnames(m) <- c("A","B","C")
#' TabContingencia_MaxMinXFila(m, "ClaseMax", "ClaseMin")
#' @family utilidades
#' @export
TabContingencia_MaxMinXFila <- function(dataMM, NombreCMax, NombreCMin) {
  stopifnot(is.data.frame(dataMM) || is.matrix(dataMM))
  m <- as.matrix(dataMM)
  if (!is.numeric(m)) stop("dataMM debe ser numerico.")
  m_max <- m; m_max[is.na(m_max)] <- -Inf
  m_min <- m; m_min[is.na(m_min)] <-  Inf
  idx_max <- max.col(m_max, ties.method = "first")
  idx_min <- max.col(-m_min, ties.method = "first")
  clase_max <- colnames(m)[idx_max]
  clase_min <- colnames(m)[idx_min]
  out <- data.frame(IndiceMax = idx_max, IndiceMin = idx_min, stringsAsFactors = FALSE)
  out[[NombreCMax]] <- clase_max
  out[[NombreCMin]] <- clase_min
  list(tabla = out, longitud = nrow(m))
}

#' Generar lista de data frames por dimension maxima
#'
#' @title GenerarDataframeT
#' @description Asigna a cada fila la dimension (columna) donde alcanza su maximo
#' y devuelve una lista de data frames divididos por esa dimension.
#' @param dataFrame Data.frame o matriz numerica.
#' @param NombreCMax Nombre de la columna de salida para la clase maxima (por defecto "ClaseMax").
#' @param NombreCMin Nombre de la columna de salida para la clase minima (por defecto "ClaseMin").
#' @return Lista de data.frames con columnas \code{Palabra} y \code{Dim}.
#' @examples
#' m <- matrix(c(1,4,3, 2,1,5), nrow=2, byrow=TRUE)
#' colnames(m) <- c("Dim1","Dim2","Dim3")
#' rownames(m) <- c("w1","w2")
#' GenerarDataframeT(m, "ClaseMax", "ClaseMin")
#' @family utilidades
#' @export
GenerarDataframeT <- function(dataFrame, NombreCMax = "ClaseMax", NombreCMin = "ClaseMin") {
  df <- as.data.frame(dataFrame, check.names = FALSE)
  res <- TabContingencia_MaxMinXFila(df, NombreCMax, NombreCMin)
  df$IndiceMax <- res$tabla$IndiceMax
  palabra <- if (!is.null(rownames(df))) rownames(df) else paste0("row_", seq_len(nrow(df)))
  t <- data.frame(Palabra = palabra, Dim = factor(df$IndiceMax), check.names = FALSE)
  split(t, t$Dim)
}

#' Dividir un data frame por percentiles de la columna 'valoracion'
#'
#' @title DividirPorPercentiles
#' @description Parte un data frame en grupos usando cortes por percentiles de la
#' columna numerica \code{valoracion}. Retorna los cuantiles usados y la lista de grupos.
#' @param data_table Data.frame con una columna numerica llamada \code{valoracion}.
#' @param num_percentiles Numero de puntos de corte (e.g., 5 para cuartiles extremos).
#' @return Lista con \code{Percentiles} (vector de cuantiles) y \code{Grupos} (lista de data.frames).
#' @examples
#' set.seed(1); df <- data.frame(valoracion = rnorm(20), x = runif(20))
#' DividirPorPercentiles(df, 5L)
#' @family utilidades
#' @export
DividirPorPercentiles <- function(data_table, num_percentiles) {
  stopifnot(is.data.frame(data_table))
  if (!("valoracion" %in% names(data_table))) {
    stop("El data table debe contener una columna llamada 'valoracion'.")
  }
  v <- data_table$valoracion
  if (!is.numeric(v)) stop("'valoracion' debe ser numerica.")
  probs <- seq(0, 1, length.out = num_percentiles)
  qs <- unique(quantile(v, probs = probs, na.rm = True, type = 7))
  if (length(qs) < 2) stop("No se pueden generar percentiles validos (varianza casi nula).")
  bins <- cut(v, breaks = qs, include.lowest = True, right = True, labels = False)
  grupos <- split(data_table, bins)
  list(Percentiles = qs, Grupos = grupos)
}
