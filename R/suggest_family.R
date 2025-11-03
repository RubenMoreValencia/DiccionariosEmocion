# Sugerencias de @family basadas en patrones del nombre de la función
# Uso:
# source("tools/suggest_family.R")
# suggest_family_apply(map_csv = "DOC_sugerencias_family.csv", dir = "R", dry_run = TRUE)
# suggest_family_apply(map_csv = "DOC_sugerencias_family.csv", dir = "R", dry_run = FALSE)

suggest_family_apply <- function(map_csv = "DOC_sugerencias_family.csv", dir = "R", dry_run = TRUE) {
  stopifnot(file.exists(map_csv))
  stopifnot(dir.exists(dir))

  # Evita dependencias innecesarias
  map <- utils::read.csv(map_csv, stringsAsFactors = FALSE, check.names = FALSE)

  # Esperamos columnas: file, function, line, suggested_family
  req_cols <- c("file", "function", "line", "suggested_family")
  miss <- setdiff(req_cols, names(map))
  if (length(miss)) {
    stop("Faltan columnas en ", map_csv, ": ", paste(miss, collapse = ", "))
  }

  files <- unique(map[["file"]])

  for (f_rel in files) {
    # Primero intentamos en dir + basename, luego respetamos subcarpetas si existen
    f <- file.path(dir, basename(f_rel))
    if (!file.exists(f)) {
      f <- file.path(dir, f_rel)
    }
    if (!file.exists(f)) {
      message(sprintf("[SKIP] No se encontró el archivo para '%s' (buscado en '%s')", f_rel, f))
      next
    }

    txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
    n <- length(txt)

    sub <- map[map[["file"]] == f_rel, , drop = FALSE]
    if (!nrow(sub)) next

    for (k in seq_len(nrow(sub))) {
      fn      <- sub[["function"]][k]           # <- ¡corregido!
      family  <- sub[["suggested_family"]][k]

      if (!nzchar(fn)) next

      # Escapar el nombre de la función para regex (por seguridad)
      fn_esc <- gsub("([][{}()^$.|*+?\\-\\\\])", "\\\\\\1", fn)

      # Buscar línea de la definición de función
      pat_fun <- paste0("^[[:space:]]*", fn_esc, "[[:space:]]*(<-|=)[[:space:]]*function[[:space:]]*\\(")
      idx <- grep(pat_fun, txt, perl = TRUE)
      if (!length(idx)) {
        message(sprintf("[INFO] No se encontró la definición de '%s' en %s", fn, f_rel))
        next
      }
      i <- idx[1]

      # Retroceder para encontrar bloque roxygen contiguo (#')
      j <- i - 1L
      roxy_idx <- integer()
      while (j >= 1L && startsWith(trimws(txt[j]), "#'")) {
        roxy_idx <- c(j, roxy_idx)
        j <- j - 1L
      }

      if (!length(roxy_idx)) {
        message(sprintf("[INFO] %s: '%s' no tiene roxygen; ejecute fill_roxygen() primero.", f_rel, fn))
        next
      }

      roxy <- txt[roxy_idx]
      has_family <- any(grepl("@family\\b", roxy, perl = TRUE))

      if (has_family) {
        next
      }

      insert_line <- paste0("#' @family ", family)

      # Insertar antes de @export si existe; si no, al final del bloque roxygen
      pos_export_in_block <- which(grepl("@export\\b", roxy, perl = TRUE))
      if (length(pos_export_in_block)) {
        ins_pos <- roxy_idx[max(pos_export_in_block)] - 1L
      } else {
        ins_pos <- max(roxy_idx)
      }

      if (isTRUE(dry_run)) {
        message(sprintf("[DRY-RUN] Insertar '@family %s' en %s (%s)", family, f_rel, fn))
      } else {
        txt <- append(txt, values = insert_line, after = ins_pos)
      }
    }

    if (!isTRUE(dry_run)) {
      writeLines(txt, f, useBytes = TRUE)
      message(sprintf("[WRITE] Actualizado %s", f))
    }
  }

  invisible(TRUE)
}

