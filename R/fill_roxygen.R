# Auto-esqueleto de cabeceras roxygen para funciones sin documentación completa
# Uso sugerido:
# source("tools/fill_roxygen.R")
# fill_roxygen(dir = "R", dry_run = TRUE)  # vista previa
# fill_roxygen(dir = "R", dry_run = FALSE) # escribe cambios

fill_roxygen <- function(dir = "R", dry_run = TRUE) {
  stopifnot(dir.exists(dir))
  files <- list.files(dir, pattern = "\\\\.[rR]$", full.names = TRUE, recursive = TRUE)
  pat_fun <- "^[[:space:]]*([A-Za-z][A-Za-z0-9_.]*)[[:space:]]*(<-|=)[[:space:]]*function[[:space:]]*\\\\"
  for (f in files) {
    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    n <- length(txt)
    i <- 1
    while (i <= n) {
      line <- txt[i]
      if (grepl(pat_fun, line, perl = TRUE)) {
        fn_name <- sub(pat_fun, "\\\\1", line, perl = TRUE)
        # Busca si hay roxygen inmediatamente arriba
        j <- i - 1
        has_roxy <- FALSE
        if (j >= 1) {
          has_roxy <- grepl("^#'", txt[j])
        }
        if (!has_roxy) {
          block <- c(
            "#' Titulo pendiente",
            "#'",
            "#' Descripcion breve.",
            "#' @details Completar detalles, supuestos y referencias.",
            "#' @param ... completar parametros",
            "#' @return Objeto de salida (clase/estructura).",
            "#' @examples",
            "#' \\dontrun{",
            paste0("#' ", fn_name, "()"),
            "#' }",
            "#' @export",
            "#' @md"
          )
          if (dry_run) {
            message(sprintf("[DRY-RUN] Insertar roxygen para %s en %s (linea %d)", fn_name, f, i))
          } else {
            txt <- append(txt, values = block, after = i - 1)
            i <- i + length(block) # avanzar sobre el bloque insertado
            n <- length(txt)
            message(sprintf("Insertado roxygen para %s en %s (linea %d)", fn_name, f, i))
          }
        } else {
          # Si ya hay roxygen, sugerir completar @param y @return si faltan
          # (búsqueda simple en lineas previas contiguas)
          j <- i - 1
          roxy_idx <- integer()
          while (j >= 1 && grepl("^#'", txt[j])) {
            roxy_idx <- c(j, roxy_idx)
            j <- j - 1
          }
          roxy <- txt[roxy_idx]
          has_param <- any(grepl("@param", roxy, fixed = TRUE))
          has_return <- any(grepl("@return", roxy, fixed = TRUE))
          if (!(has_param && has_return)) {
            if (dry_run) {
              message(sprintf("[DRY-RUN] Completar roxygen de %s en %s: falta %s%s",
                              fn_name, f,
                              if (!has_param) "@param " else "",
                              if (!has_return) "@return" else ""))
            } else {
              # insertar sugerencias al final del bloque roxygen
              insert <- character()
              if (!has_param) insert <- c(insert, "#' @param ... completar parametros")
              if (!has_return) insert <- c(insert, "#' @return Objeto de salida (clase/estructura).")
              if (length(insert)) {
                ins_pos <- max(roxy_idx)
                txt <- append(txt, values = insert, after = ins_pos)
                i <- i + length(insert)
                n <- length(txt)
                message(sprintf("Insertadas sugerencias roxygen para %s en %s", fn_name, f))
              }
            }
          }
        }
      }
      i <- i + 1
    }
    if (!dry_run) {
      writeLines(txt, f, useBytes = TRUE)
    }
  }
  invisible(TRUE)
}
