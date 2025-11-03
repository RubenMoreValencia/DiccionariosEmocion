# tools/auditar_roxygen.R
# Requiere: utils, tools, stats
# (Opcional) roxygen2, devtools si quieres regenerar docs al final.

validUTF8_safe <- function(x) {
  ok <- tryCatch(all(validUTF8(paste(x, collapse=""))), error = function(e) NA)
  isTRUE(ok)
}

leer_archivo <- function(path) {
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
           error = function(e) readLines(path, warn = FALSE))
}

extraer_bloques_roxygen <- function(path) {
  ln <- leer_archivo(path)
  n  <- length(ln)
  blocks <- list()
  i <- 1
  while (i <= n) {
    if (grepl("^\\s*#'", ln[i])) {
      start <- i
      while (i <= n && grepl("^\\s*#'", ln[i])) i <- i + 1
      end <- i - 1
      # siguiente línea no vacía para firma de función
      sig_line <- NA_integer_; fun_name <- NA_character_; fun_args <- character(0)
      j <- i
      while (j <= n && grepl("^\\s*$", ln[j])) j <- j + 1
      if (j <= n) {
        sig_line <- j
        sig <- ln[j]
        m <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\((.*)$", sig)
        rs <- regmatches(sig, m)[[1]]
        if (length(rs) >= 3) {
          fun_name <- rs[2]
          # Capturar argumentos entre paréntesis (posible multilinea)
          par_open <- gregexpr("\\(", sig)[[1]][1]
          # Tomar líneas hasta cerrar ')'
          fun_sig <- substring(sig, par_open + 1)
          k <- j + 1
          open <- 1; close <- 0
          tmp <- fun_sig
          while (k <= n && open > close) {
            tmp2 <- ln[k]
            open  <- open  + length(gregexpr("\\(", tmp2)[[1]])
            close <- close + length(gregexpr("\\)", tmp2)[[1]])
            tmp <- paste0(tmp, "\n", tmp2)
            k <- k + 1
          }
          # arg names (rústico pero útil)
          args_str <- sub("\\).*", "", tmp)
          args     <- unlist(strsplit(args_str, ","), use.names = FALSE)
          args     <- trimws(sub("=.*", "", args))
          args     <- args[args != ""]
          fun_args <- args
        }
      }
      blocks[[length(blocks)+1]] <- list(
        file = path, start = start, end = end,
        lines = ln[start:end], sig_line = sig_line,
        fun = fun_name, args = fun_args
      )
    } else {
      i <- i + 1
    }
  }
  blocks
}

analizar_bloque <- function(block) {
  issues <- list()
  file <- block$file
  lines <- block$lines
  start <- block$start
  fun   <- block$fun
  args  <- block$args

  # 1) Balance de comillas dobles (excluye \")
  txt <- gsub('\\"', "", paste(lines, collapse = "\n"))
  dq  <- gregexpr('"', txt)[[1]]
  if (length(dq) > 0 && dq[1] != -1 && (length(dq) %% 2) == 1) {
    issues[[length(issues)+1]] <- c("unbalanced_quotes",
                                    start, "Comillas dobles desbalanceadas (posible \" sin cerrar).")
  }

  # 2) % sin escapar (ignora líneas @tags y code fences simples)
  for (k in seq_along(lines)) {
    L <- sub("^\\s*#'\\s*", "", lines[k])
    if (!grepl("^@", L) && grepl("%", L) && !grepl("\\\\%", L)) {
      issues[[length(issues)+1]] <- c("percent_not_escaped",
                                      start + k - 1, sprintf("Posible %% sin escapar en: %s", substr(L,1,120)))
    }
  }

  # 3) Backslashes sospechosos (no seguidos de letras) => "\\ " o fin de línea
  for (k in seq_along(lines)) {
    L <- sub("^\\s*#'\\s*", "", lines[k])
    if (grepl("\\\\(\\s|$)", L)) {
      issues[[length(issues)+1]] <- c("backslash_suspect",
                                      start + k - 1, sprintf("Barra invertida aislada: %s", substr(L,1,120)))
    }
  }

  # 4) UTF-8 inválido
  if (!validUTF8_safe(lines)) {
    issues[[length(issues)+1]] <- c("encoding_utf8",
                                    start, "Bloque con caracteres no-UTF8 (revisa Encoding: UTF-8 en DESCRIPTION).")
  }

  # 5) @param vs args
  params <- trimws(sub("^@param\\s+", "", sub("^\\s*#'\\s*", "", grep("^\\s*#'\\s*@param", lines, value = TRUE))))
  params <- sub("\\s.*$", "", params)
  if (length(args) > 0) {
    faltan <- setdiff(args, c(params, "..."))
    sobr   <- setdiff(params, c(args, "..."))
    if (length(faltan)) {
      issues[[length(issues)+1]] <- c("param_missing",
                                      start, sprintf("@param faltantes: %s", paste(faltan, collapse=", ")))
    }
    if (length(sobr)) {
      issues[[length(issues)+1]] <- c("param_extra",
                                      start, sprintf("@param sin correspondencia en firma: %s", paste(sobr, collapse=", ")))
    }
  }

  # 6) Tags mínimos sugeridos
  has_export <- any(grepl("^\\s*#'\\s*@export\\b", lines))
  has_title  <- length(lines) > 0 && grepl("^\\s*#'\\s*[^@\\s]", lines[1]) # primera línea no @
  has_return <- any(grepl("^\\s*#'\\s*@return\\b", lines))
  if (!has_title)  issues[[length(issues)+1]] <- c("title_missing",  start, "Falta título (primera línea del bloque).")
  if (!has_return) issues[[length(issues)+1]] <- c("return_missing", start, "Falta @return.")
  if (!has_export) issues[[length(issues)+1]] <- c("export_missing", start, "Falta @export (si debe ser pública).")

  if (length(issues) == 0) return(NULL)
  do.call(rbind, issues)
}

auditar_Roxygen_R <- function(root = ".") {
  # --- helpers locales (auto-contenidos) ---
  validUTF8_safe <- function(x) {
    ok <- tryCatch(all(validUTF8(paste(x, collapse=""))), error = function(e) NA)
    isTRUE(ok)
  }
  leer_archivo <- function(path) {
    tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
             error = function(e) readLines(path, warn = FALSE))
  }
  extraer_bloques_roxygen <- function(path) {
    ln <- leer_archivo(path)
    n  <- length(ln); blocks <- list(); i <- 1
    while (i <= n) {
      if (grepl("^\\s*#'", ln[i])) {
        start <- i
        while (i <= n && grepl("^\\s*#'", ln[i])) i <- i + 1
        end <- i - 1
        # intentar detectar firma de función inmediatamente después del bloque
        sig_line <- NA_integer_; fun_name <- NA_character_; fun_args <- character(0)
        j <- i
        while (j <= n && grepl("^\\s*$", ln[j])) j <- j + 1
        if (j <= n) {
          sig_line <- j
          sig <- ln[j]
          m <- regexec("^\\s*([A-Za-z0-9_.]+)\\s*<-\\s*function\\s*\\((.*)$", sig)
          rs <- regmatches(sig, m)[[1]]
          if (length(rs) >= 3) {
            fun_name <- rs[2]
            # capturar argumentos (posible multilínea)
            par_open <- gregexpr("\\(", sig)[[1]][1]
            fun_sig <- substring(sig, par_open + 1)
            k <- j + 1; open <- 1; close <- 0; tmp <- fun_sig
            while (k <= n && open > close) {
              tmp2 <- ln[k]
              open  <- open  + length(gregexpr("\\(", tmp2)[[1]])
              close <- close + length(gregexpr("\\)", tmp2)[[1]])
              tmp <- paste0(tmp, "\n", tmp2)
              k <- k + 1
            }
            args_str <- sub("\\).*", "", tmp)
            args     <- unlist(strsplit(args_str, ","), use.names = FALSE)
            args     <- trimws(sub("=.*", "", args))
            args     <- args[args != ""]
            fun_args <- args
          }
        }
        blocks[[length(blocks)+1]] <- list(
          file = path, start = start, end = end,
          lines = ln[start:end], sig_line = sig_line,
          fun = fun_name, args = fun_args
        )
      } else i <- i + 1
    }
    blocks
  }
  analizar_bloque <- function(block) {
    issues <- list()
    file <- block$file; lines <- block$lines; start <- block$start
    fun  <- block$fun;  args  <- block$args

    # 1) Comillas dobles desbalanceadas (ignora \")
    txt <- gsub('\\"', "", paste(lines, collapse = "\n"))
    dq  <- gregexpr('"', txt)[[1]]
    if (length(dq) > 0 && dq[1] != -1 && (length(dq) %% 2) == 1) {
      issues[[length(issues)+1]] <- c("unbalanced_quotes",
                                      start, "Comillas dobles desbalanceadas (posible \" sin cerrar).")
    }

    # 2) % sin escapar (evita líneas @tag)
    for (k in seq_along(lines)) {
      L <- sub("^\\s*#'\\s*", "", lines[k])
      if (!grepl("^@", L) && grepl("%", L) && !grepl("\\\\%", L)) {
        issues[[length(issues)+1]] <- c("percent_not_escaped",
                                        start + k - 1, sprintf("Posible %% sin escapar en: %s", substr(L,1,120)))
      }
    }

    # 3) Backslash sospechoso "\ " o fin de línea
    for (k in seq_along(lines)) {
      L <- sub("^\\s*#'\\s*", "", lines[k])
      if (grepl("\\\\(\\s|$)", L)) {
        issues[[length(issues)+1]] <- c("backslash_suspect",
                                        start + k - 1, sprintf("Barra invertida aislada: %s", substr(L,1,120)))
      }
    }

    # 4) UTF-8 inválido
    if (!validUTF8_safe(lines)) {
      issues[[length(issues)+1]] <- c("encoding_utf8",
                                      start, "Bloque con caracteres no-UTF8 (revisa Encoding: UTF-8 en DESCRIPTION).")
    }

    # 5) @param vs argumentos de la firma
    params <- trimws(sub("^@param\\s+", "", sub("^\\s*#'\\s*", "", grep("^\\s*#'\\s*@param", lines, value = TRUE))))
    params <- sub("\\s.*$", "", params)
    if (length(args) > 0) {
      faltan <- setdiff(args, c(params, "..."))
      sobr   <- setdiff(params, c(args, "..."))
      if (length(faltan)) {
        issues[[length(issues)+1]] <- c("param_missing",
                                        start, sprintf("@param faltantes: %s", paste(faltan, collapse=", ")))
      }
      if (length(sobr)) {
        issues[[length(issues)+1]] <- c("param_extra",
                                        start, sprintf("@param sin correspondencia en firma: %s", paste(sobr, collapse=", ")))
      }
    }

    # 6) Tags mínimos sugeridos
    has_export <- any(grepl("^\\s*#'\\s*@export\\b", lines))
    has_title  <- length(lines) > 0 && grepl("^\\s*#'\\s*[^@\\s]", lines[1])
    has_return <- any(grepl("^\\s*#'\\s*@return\\b", lines))
    if (!has_title)  issues[[length(issues)+1]] <- c("title_missing",  start, "Falta título (primera línea del bloque).")
    if (!has_return) issues[[length(issues)+1]] <- c("return_missing", start, "Falta @return.")
    if (!has_export) issues[[length(issues)+1]] <- c("export_missing", start, "Falta @export (si debe ser pública).")

    if (length(issues) == 0) return(NULL)
    do.call(rbind, issues)
  }

  # --- recorrido de archivos R/ ---
  Rdir <- file.path(root, "R")
  files <- list.files(Rdir, pattern = "[.][Rr]$", full.names = TRUE, recursive = FALSE)
  res <- list()
  for (f in files) {
    blks <- tryCatch(extraer_bloques_roxygen(f), error = function(e) {
      list(list(file = f, start = 1L, lines = character(), fun = NA, args = character(0),
                .error = conditionMessage(e)))
    })
    if (length(blks) == 0) next
    for (b in blks) {
      if (!is.null(b$.error)) {
        df <- data.frame(
          file    = b$file,
          fun_name= NA_character_,
          issue   = "block_scan_error",
          line    = 1L,
          message = b$.error,
          stringsAsFactors = FALSE
        )
        res[[length(res)+1]] <- df
        next
      }
      iss <- tryCatch(analizar_bloque(b), error = function(e) {
        matrix(c("block_parse_error", b$start, paste("Error al analizar bloque:", conditionMessage(e))), nrow=1)
      })
      if (!is.null(iss)) {
        df <- data.frame(
          file    = b$file,
          fun_name= ifelse(is.na(b$fun), NA, b$fun),
          issue   = iss[,1],
          line    = as.integer(iss[,2]),
          message = iss[,3],
          stringsAsFactors = FALSE
        )
        res[[length(res)+1]] <- df
      }
    }
  }

  if (length(res) == 0) {
    out <- data.frame(file=character(), fun_name=character(), issue=character(), line=integer(), message=character())
  } else {
    out <- do.call(rbind, res)
    ord <- order(out$file, out$line, out$issue, na.last = TRUE)
    out <- out[ord, , drop = FALSE]
    rownames(out) <- NULL
  }
  out
}

auditar_Rd_parse <- function(root = ".") {
  mandir <- file.path(root, "man")
  if (!dir.exists(mandir)) return(data.frame())
  rds <- list.files(mandir, pattern="\\.Rd$", full.names = TRUE)
  out <- list()
  for (rd in rds) {
    er <- tryCatch({
      tools::parse_Rd(rd, encoding = "UTF-8")
      NULL
    }, error = function(e) e)
    if (!is.null(er)) {
      out[[length(out)+1]] <- data.frame(
        file = rd, issue = "Rd_parse_error", line = NA_integer_,
        message = conditionMessage(er), stringsAsFactors = FALSE
      )
    }
  }
  if (length(out) == 0) data.frame() else do.call(rbind, out)
}

auditar_roxygen_proyecto <- function(root = ".", write_csv = TRUE) {
  align_cols <- function(df, cols) {
    if (nrow(df) == 0L) {
      # crea df vacío con esas columnas
      out <- as.data.frame(setNames(replicate(length(cols), logical(0), simplify = FALSE), cols))
      return(out)
    }
    miss <- setdiff(cols, names(df))
    if (length(miss)) for (m in miss) df[[m]] <- NA
    df <- df[, cols, drop = FALSE]
    rownames(df) <- NULL
    df
  }

  message(">> Auditando roxygen en R/ …")
  a1 <- tryCatch(auditar_Roxygen_R(root), error = function(e) {
    data.frame(file = NA_character_, fun_name = NA_character_,
               issue = "auditar_Roxygen_R_error", line = NA_integer_,
               message = conditionMessage(e), stringsAsFactors = FALSE)
  })

  message(">> Auditando parseo de Rd en man/ …")
  a2 <- tryCatch(auditar_Rd_parse(root), error = function(e) {
    data.frame(file = NA_character_, issue = "auditar_Rd_parse_error",
               line = NA_integer_, message = conditionMessage(e),
               stringsAsFactors = FALSE)
  })

  # unificar columnas
  cols_union <- union(names(a1), names(a2))
  if (!"file"     %in% cols_union) cols_union <- c("file", cols_union)
  if (!"issue"    %in% cols_union) cols_union <- c(cols_union, "issue")
  if (!"line"     %in% cols_union) cols_union <- c(cols_union, "line")
  if (!"message"  %in% cols_union) cols_union <- c(cols_union, "message")
  if (!"fun_name" %in% cols_union) cols_union <- c(cols_union, "fun_name")

  a1a <- align_cols(a1, cols_union)
  a2a <- align_cols(a2, cols_union)

  reporte <- rbind(a1a, a2a)
  # orden sugerido
  ord_cols <- c("file","fun_name","issue","line","message")
  keep <- intersect(ord_cols, names(reporte))
  reporte <- reporte[, c(keep, setdiff(names(reporte), keep)), drop = FALSE]

  if (nrow(reporte) == 0) {
    message("✅ Sin problemas detectados en roxygen/Rd.")
  } else {
    # orden por archivo/línea/tipo
    o <- order(reporte$file, reporte$line, reporte$issue, na.last = TRUE)
    reporte <- reporte[o, , drop = FALSE]
    if (write_csv) {
      outdir <- file.path(root, "outputs_roxygen")
      dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
      fn <- file.path(outdir, "reporte_roxygen.csv")
      utils::write.csv(reporte, fn, row.names = FALSE, fileEncoding = "UTF-8")
      message("⚠️  Se encontraron ", nrow(reporte), " posibles problemas.")
      message("📄 Reporte: ", fn)
    }
  }

  return(reporte)
}
