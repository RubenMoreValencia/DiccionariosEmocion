
# auto_fix_roxygen_v2.R
# Reglas adicionales sobre el V1:
# - Inserta título si el bloque comienza con @tag (title_missing)
# - Asegura línea en blanco tras el título
# - Crea @description si no existe
# - Refuerza escape de %, comillas, backslash (como V1)
# - Borra .Rd con problemas estructurales para regenerar

auto_fix_roxygen_v2 <- function(root = ".", report_csv = "outputs_roxygen/reporte_roxygen.csv", dry_run = TRUE) {
  cat("== Auto-fix Roxygen V2 ==\n")
  if (!file.exists(report_csv)) stop("No se encuentra el CSV de reporte: ", report_csv)
  rep <- tryCatch(read.csv(report_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
                  error = function(e) read.csv(report_csv, stringsAsFactors = FALSE))
  rep$file <- as.character(rep$file)
  rep$issue <- as.character(rep$issue)

  repR  <- subset(rep, grepl("/R/[^/]+[.][Rr]$", file))
  repRd <- subset(rep, issue == "Rd_parse_error")

  read_utf8 <- function(p) tryCatch(readLines(p, warn=FALSE, encoding="UTF-8"),
                                    error=function(e) readLines(p, warn=FALSE))
  write_utf8 <- function(contenido, p) {
    con <- file(p, open = "w", encoding = "UTF-8"); on.exit(close(con))
    writeLines(contenido, con, sep = "\n", useBytes = TRUE)
  }

  is_roxy    <- function(x) grepl("^\\s*#'", x)
  strip_roxy <- function(x) sub("^\\s*#'\\s*", "", x)

  fx_percent <- function(lines) {
    for (i in seq_along(lines)) {
      if (!is_roxy(lines[i])) next
      body <- strip_roxy(lines[i])
      if (grepl("^@", body)) next
      body <- gsub("(?<!\\\\)%", "\\\\%", body, perl = TRUE)
      lines[i] <- paste0("#' ", body)
    }
    lines
  }
  fx_quotes <- function(lines) {
    for (i in seq_along(lines)) {
      if (!is_roxy(lines[i])) next
      body <- strip_roxy(lines[i])
      body <- gsub('"([^"]{1,120})"', "\\\\dQuote{\\1}", body)
      lines[i] <- paste0("#' ", body)
    }
    lines
  }
  fx_backslash <- function(lines) {
    for (i in seq_along(lines)) {
      if (!is_roxy(lines[i])) next
      body <- strip_roxy(lines[i])
      body <- gsub("\\\\(\\s|$)", "\\\\\\\\ \\1", body, perl = TRUE)
      lines[i] <- paste0("#' ", body)
    }
    lines
  }

  ensure_return_export <- function(lines) {
    n <- length(lines); i <- 1L; out <- character(0)
    while (i <= n) {
      if (is_roxy(lines[i])) {
        start <- i
        while (i <= n && is_roxy(lines[i])) i <- i + 1
        end <- i - 1
        block <- lines[start:end]
        has_return <- any(grepl("^\\s*#'\\s*@return\\b", block))
        has_export <- any(grepl("^\\s*#'\\s*@export\\b", block))
        if (!has_return) block <- c(block, "#' @return (auto) Verificar y completar la descripción del valor retornado.")
        if (!has_export) block <- c(block, "#' @export")
        out <- c(out, block)
      } else { out <- c(out, lines[i]); i <- i + 1 }
    }
    out
  }

  # Inserta título si el bloque comienza con @tag o está vacío
  ensure_title_description <- function(lines) {
    n <- length(lines); i <- 1L; out <- character(0)
    while (i <= n) {
      if (is_roxy(lines[i])) {
        start <- i
        while (i <= n && is_roxy(lines[i])) i <- i + 1
        end <- i - 1
        block <- lines[start:end]
        # detecta si la primera línea útil es @tag
        btxt <- strip_roxy(block)
        first_nonempty <- which(trimws(btxt) != "")[1]
        needs_title <- FALSE
        if (length(first_nonempty)) {
          needs_title <- grepl("^@", btxt[first_nonempty])
        } else {
          needs_title <- TRUE
        }
        if (needs_title) {
          block <- c("#' Título (auto) — reemplazar por uno descriptivo.", block)
        }
        # asegurar @description si no existe
        has_desc <- any(grepl("^\\s*#'\\s*@description\\b", block))
        if (!has_desc) {
          # insertar una línea en blanco tras el título y luego @description
          block <- c(block[1], "#'", "#' @description Descripción (auto). Completar con propósito, entradas y salidas.", block[-1])
        } else {
          # asegurar una línea en blanco después del título si no hay
          if (!grepl("^\\s*#'\\s*$", block[2])) {
            block <- append(block, values = "#'", after = 1)
          }
        }
        out <- c(out, block)
      } else { out <- c(out, lines[i]); i <- i + 1 }
    }
    out
  }

  fx_params_from_sig <- function(lines) {
    get_fun_sig <- function(lines, idx_after_block) {
      n <- length(lines); j <- idx_after_block
      while (j <= n && grepl("^\\s*$", lines[j])) j <- j + 1
      if (j > n) return(character(0))
      sig <- lines[j]
      m <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\((.*)$", sig)
      rs <- regmatches(sig, m)[[1]]
      if (length(rs) < 3) return(character(0))
      par_open <- gregexpr("\\(", sig)[[1]][1]
      fun_sig <- substring(sig, par_open + 1)
      k <- j + 1; open <- 1; close <- 0; tmp <- fun_sig
      while (k <= n && open > close) {
        tmp2 <- lines[k]
        open  <- open  + length(gregexpr("\\(", tmp2)[[1]])
        close <- close + length(gregexpr("\\)", tmp2)[[1]])
        tmp <- paste0(tmp, "\n", tmp2); k <- k + 1
      }
      args_str <- sub("\\).*", "", tmp)
      args     <- trimws(sub("=.*", "", unlist(strsplit(args_str, ","))))
      args[args != ""]
    }
    n <- length(lines); i <- 1L; out <- character(0)
    while (i <= n) {
      if (is_roxy(lines[i])) {
        start <- i
        while (i <= n && is_roxy(lines[i])) i <- i + 1
        end <- i - 1
        block <- lines[start:end]
        params_present <- sub("\\s.*$", "", sub("^\\s*#'\\s*@param\\s+", "", grep("^\\s*#'\\s*@param", block, value = TRUE)))
        args <- get_fun_sig(lines, end + 1L)
        faltan <- setdiff(args, c(params_present, "..."))
        if (length(faltan)) {
          add <- paste0("#' @param ", faltan, " (auto) TODO: describir parámetro.")
          block <- c(block, add)
        }
        out <- c(out, block)
      } else { out <- c(out, lines[i]); i <- i + 1 }
    }
    out
  }

  fixed_summary <- list()
  if (nrow(repR) > 0) {
    for (f in unique(repR$file)) {
      file_path <- f
      if (!file.exists(file_path)) next
      lines <- read_utf8(file_path)
      orig  <- lines
      kinds <- unique(subset(repR, file == f)$issue)

      # Orden: primero saneo estructural, luego escapes, luego metadatos
      lines <- ensure_title_description(lines)
      if ("percent_not_escaped" %in% kinds) lines <- fx_percent(lines)
      if ("unbalanced_quotes"  %in% kinds) lines <- fx_quotes(lines)
      if ("backslash_suspect"  %in% kinds) lines <- fx_backslash(lines)
      if (any(c("return_missing","export_missing") %in% kinds)) lines <- ensure_return_export(lines)
      if ("param_missing"      %in% kinds) lines <- fx_params_from_sig(lines)

      if (!dry_run && !identical(orig, lines)) {
        bak <- paste0(file_path, ".bak")
        if (!file.exists(bak)) file.copy(file_path, bak, overwrite = FALSE)
        write_utf8(lines, file_path)
      }
      fixed_summary[[length(fixed_summary)+1]] <- data.frame(file = file_path, modified = !identical(orig, lines), stringsAsFactors = FALSE)
    }
  }

  removed_rd <- character(0)
  if (nrow(repRd) > 0) {
    for (p in unique(repRd$file)) {
      if (file.exists(p) && !dry_run) { file.remove(p); removed_rd <- c(removed_rd, p) }
    }
  }

  cat("== Resumen V2 ==\n")
  if (length(fixed_summary)) print(do.call(rbind, fixed_summary)) else cat("No hubo archivos R/ para modificar.\n")
  if (length(removed_rd)) { cat("Eliminados .Rd para regenerar:\n"); print(removed_rd) }
  invisible(list(mods = fixed_summary, removed_rd = removed_rd))
}
