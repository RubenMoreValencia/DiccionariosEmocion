#' Concatenar elementos de un vector en una cadena
#'
#' @title vector_a_cadena
#' @description Une los elementos de un vector en una sola cadena separada por comas.
#' @param x Vector de caracteres o numerico a concatenar.
#' @return Cadena de texto con los elementos unidos por ", ".
#' @examples
#' vector_a_cadena(c("a","b","c"))
#' vector_a_cadena(1:5)
#' @family utilidades
#' @export
vector_a_cadena <- function(x) paste(x, collapse = ", ")
