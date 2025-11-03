#' Crear un diccionario emocional base
#'
#' Esta función genera un diccionario de emociones con palabras asociadas a ocho emociones básicas
#' más polaridades positivas y negativas.
#'
#' @return Un data.frame con palabras y sus valores emocionales.
#' @family utilidades
#' @export
crear_diccionario_emociones <- function() {
  emociones <- c("ira", "anticipacion", "asco", "miedo", "alegria",
                 "tristeza", "sorpresa", "confianza", "negativo", "positivo")

  # Plantilla de ejemplo (puede ser reemplazada por carga de archivo)
  palabras <- c("gobierno", "comunidad", "vacio", "policia", "desigual", "confrontacion")
  valores <- matrix(sample(0:1, length(palabras) * length(emociones), replace = TRUE),
                    nrow = length(palabras), dimnames = list(palabras, emociones))

  diccionario <- as.data.frame(valores)
  diccionario$palabra <- rownames(diccionario)
  rownames(diccionario) <- NULL

  return(diccionario)
}

#' Mostrar las primeras palabras del diccionario emocional
#'
#' Muestra una vista previa del diccionario generado o cargado.
#'
#' @param diccionario Un data.frame con palabras y emociones, generado por `crear_diccionario_emociones()`.
#' @param n Número de filas a mostrar. Por defecto 6.
#'
#' @return Un data.frame con las primeras n filas.
#' @family utilidades
#' @export
mostrar_diccionario_emociones <- function(diccionario, n = 6) {
  stopifnot(is.data.frame(diccionario))
  head(diccionario, n)
}

#' Buscar palabras específicas en el diccionario emocional
#'
#' Permite filtrar las palabras del diccionario que coincidan con los términos proporcionados.
#'
#' @param diccionario Data frame con palabras y columnas de emociones.
#' @param palabras Vector de palabras a buscar.
#'
#' @return Subconjunto del diccionario con las palabras encontradas.
#' @family resumen_y_frecuencias
#' @export
buscar_palabras_diccionario <- function(diccionario, palabras) {
  stopifnot("palabra" %in% names(diccionario))
  subset(diccionario, palabra %in% palabras)
}
