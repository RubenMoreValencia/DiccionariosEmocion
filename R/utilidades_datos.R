#' Configurar la clave API de OpenAI para traducciones
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función facilita la configuración de la variable de entorno `OPENAI_API_KEY`,
#' necesaria para utilizar la función `TraduceA()` con el modelo GPT.
#'
#' @param clave Cadena con la clave API de OpenAI (comienza con `"sk-"`).
#' @param permanente Lógico. Si `TRUE`, agrega la clave al archivo `.Renviron` para uso permanente.
#'
#' @details Si `permanente = TRUE`, se abre el archivo `.Renviron` para que el usuario agregue
#' manualmente la línea `OPENAI_API_KEY=su_clave`. Esto se hace por seguridad y control explícito del usuario.
#'
#' @return Mensaje informativo y configuración temporal si `permanente = FALSE`.
#'
#' @examples
#' \dontrun{
#' configurar_openai_clave("sk-xxxxxxx")  # Para esta sesión
#' configurar_openai_clave("sk-xxxxxxx", permanente = TRUE)  # Para todas las sesiones
#' }
#'
#' @family utilidades
#' @export
configurar_openai_clave <- function(clave, permanente = FALSE) {
  if (!startsWith(clave, "sk-")) {
    stop("La clave debe comenzar con 'sk-'.")
  }

  if (permanente) {
    message("Abriendo el archivo .Renviron... Agrega esta línea al final y guarda:\n\n",
            sprintf("OPENAI_API_KEY=%s", clave))
    usethis::edit_r_environ()
  } else {
    Sys.setenv(OPENAI_API_KEY = clave)
    message("✅ Clave API configurada temporalmente para esta sesión.")
  }
}

nombresDataFrameEstudio<-c("ira","anticipación","disgusto","miedo","alegría",
                           "tristeza","sorpresa","confianza","negativo",
                           "positivo","syuzhet","bing","afinn","nrc")

#' Calcular un puntaje emocional total
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Suma los valores emocionales de todas las columnas en un data.frame de emociones.
#'
#' @param emocion_df Data frame con columnas de emociones numéricas.
#' @param tipo Tipo de puntaje a calcular: `"total"` (suma) o `"media"`.
#'
#' @return Un valor numérico correspondiente al puntaje emocional.
#' @family utilidades
#' @export
calcular_puntaje_emocional <- function(emocion_df, tipo = "total") {
  if (nrow(emocion_df) == 0) return(NA)

  valores <- as.numeric(emocion_df[1, ])

  if (tipo == "total") {
    return(sum(valores, na.rm = TRUE))
  } else if (tipo == "media") {
    return(mean(valores, na.rm = TRUE))
  } else {
    stop("El tipo debe ser 'total' o 'media'.")
  }
}

#' Filtrar el diccionario por emociones específicas
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Devuelve solo las columnas del diccionario correspondientes a ciertas emociones seleccionadas.
#'
#' @param diccionario Data frame que incluye una columna `palabra` y columnas de emociones.
#' @param emociones Vector de nombres de emociones a conservar.
#'
#' @return Data frame reducido con solo las emociones seleccionadas.
#' @family utilidades
#' @export
filtrar_diccionario <- function(diccionario, emociones) {
  if (!"palabra" %in% names(diccionario)) {
    stop("El diccionario debe tener una columna llamada 'palabra'.")
  }

  columnas_validas <- intersect(emociones, names(diccionario))
  if (length(columnas_validas) == 0) {
    warning("Ninguna emoción seleccionada está presente en el diccionario.")
    return(diccionario["palabra"])
  }

  return(diccionario[, c("palabra", columnas_validas), drop = FALSE])
}

#' Leer un archivo CSV y asignar nombres de columnas
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param archivo Ruta al archivo CSV.
#' @param nombres Vector opcional de nombres para las columnas.
#' @return Un data.frame leído del archivo.
#' @family utilidades
#' @export
leerCSVConNombres <- function(archivo, nombres = NULL) {
  datos <- read.csv(archivo, stringsAsFactors = FALSE)
  if (!is.null(nombres)) {
    names(datos) <- nombres
  }
  return(datos)
}

#' Guardar un data.frame como archivo CSV
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param datos Data frame a guardar.
#' @param archivo Ruta del archivo de salida.
#' @family utilidades
#' @export
#' @return (auto) Verificar y completar la descripción del valor retornado.
guardarDataFrameCSV <- function(datos, archivo) {
  write.csv(datos, archivo, row.names = FALSE)
}

#' Eliminar filas duplicadas de un data frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param df Data frame de entrada.
#' @return Data frame sin filas duplicadas.
#' @family utilidades
#' @export
eliminarduplicados <- function(df) {
  return(unique(df))
}

#' Obtener la intersección entre dos data frames por nombres de fila
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función identifica las filas comunes entre dos data frames (según sus nombres de fila),
#' los filtra por esas filas y los combina por columnas. Además, devuelve una cadena limpia con los nombres de fila comunes.
#'
#' @param df1 Primer data frame.
#' @param df2 Segundo data frame.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{TablaCompletaNRC}{Data frame combinado con filas comunes y columnas de ambos data frames.}
#'   \item{NombreFilas}{Cadena con los nombres de fila comunes, limpiada y sin tildes.}
#' }
#' @family utilidades
#' @export
interseccionDataFrames <- function(df1, df2) {
  # Validación básica
  if (is.null(rownames(df1)) || is.null(rownames(df2))) {
    stop("Ambos data frames deben tener nombres de fila.")
  }

  # Nombres comunes
  nombres_comunes <- intersect(rownames(df1), rownames(df2))
  if (length(nombres_comunes) == 0) {
    stop("No hay nombres de fila en común entre los data frames.")
  }

  # Subconjuntos por nombres
  df1_interseccion <- df1[nombres_comunes, , drop = FALSE]
  df2_interseccion <- df2[nombres_comunes, , drop = FALSE]

  # Unión por columnas
  interseccion <- cbind(df1_interseccion, df2_interseccion)

  # Procesar nombres
  nombres <- rownames(interseccion)
  palabras <- gsub(" ", "", nombres)
  palabras <- tolower(palabras)

  # Eliminar tildes
  palabras <- lapply(palabras, function(palabra) {
    palabra <- gsub("[áäâà]", "a", palabra)
    palabra <- gsub("[éëêè]", "e", palabra)
    palabra <- gsub("[íïîì]", "i", palabra)
    palabra <- gsub("[óöôò]", "o", palabra)
    palabra <- gsub("[úüûù]", "u", palabra)
    return(palabra)
  })
  cadenaLimpia <- paste(unlist(palabras), collapse = ", ")

  return(list(
    TablaCompletaNRC = interseccion,
    NombreFilas = cadenaLimpia
  ))
}


#' Cambiar los nombres de columnas de un data frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param df Data frame de entrada.
#' @param nuevos_nombres Vector de nombres nuevos.
#' @return Data frame con nombres de columnas modificados.
#' @family utilidades
#' @export
cambiar_nombres_columnas <- function(df, nuevos_nombres) {
  names(df) <- nuevos_nombres
  return(df)
}

#' Convertir columnas de un data frame en factores
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param df Data frame de entrada.
#' @param columnas Vector de nombres o índices de columnas a convertir.
#' @return Data frame con columnas convertidas a factor.
#' @family utilidades
#' @export
convertir_a_factores_arbol <- function(df, columnas) {
  df[, columnas] <- lapply(df[, columnas], as.factor)
  return(df)
}

#' Agregar columna con recuento de filas agrupadas
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param df Data frame de entrada.
#' @param columna Nombre de la columna por la que agrupar.
#' @return Data frame con columna adicional 'frecuencia'.
#' @family utilidades
#' @export
agregar_recuento_filas <- function(df, columna) {
  df$frecuencia <- ave(rep(1, nrow(df)), df[[columna]], FUN = length)
  return(df)
}


#' Unir archivos Excel con tuits y analizar sentimientos (versión optimizada)
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función automatiza la lectura de múltiples archivos Excel (solo la primera hoja de cada uno),
#' para extraer comentarios tipo tuit, limpiarlos, traducirlos si es necesario, y analizar los sentimientos
#' usando métodos lexicográficos como Syuzhet, NRC, Bing y Afinn.
#'
#' @importFrom readxl read_excel
#' @importFrom syuzhet get_sentences get_tokens get_sentiment get_percentage_values get_dct_transform
#' @importFrom syuzhet get_nrc_sentiment
#' @param nombreBase Ruta base (sin índice ni extensión) de los archivos Excel a cargar.
#' @param IndIni Índice inicial de archivos a leer (por ejemplo, 1 si el archivo es BambasEn_1.xlsx).
#' @param IndFin Índice final de archivos a leer.
#' @param IdiomaIni Idioma original de los textos (por ejemplo, "en").
#' @param IdiomaFin Idioma al que se desea traducir (si \code{SiTrad = TRUE}).
#' @param IdiomaLimpieza Idioma de referencia para limpieza de texto (usado en stopwords, etc.).
#' @param valorPor Número de porciones o segmentos en los que se dividirán los textos para análisis.
#' @param SiTrad Lógico. ¿Se requiere traducir los textos del idioma original al final?
#'
#' @return Una lista que contiene:
#' \itemize{
#'   \item \code{UnionXLSX}: Tibble con los comentarios unidos.
#'   \item \code{SoloTuit}: Vector con solo los tuits.
#'   \item \code{TextoIdiomaFinal}: Vector con los tuits (traducidos si aplica).
#'   \item \code{TextoSentencias}: Sentencias limpiadas.
#'   \item \code{TextoTokens}, \code{TextoTokens_Rep}: Tokens procesados.
#'   \item \code{VectorTextoSentimientos}, \code{VectorSTextoSentimientosNRC}: Vectores de puntajes emocionales.
#'   \item \code{PorNP...Vector}: Proporciones de análisis por Syuzhet, Bing, Afinn y NRC.
#'   \item \code{TDC...}: Distribuciones acumuladas para cada método.
#'   \item \code{MatrizSimplificadaTokensValorados}: Matrices y traducciones de tokens valorados.
#'   \item \code{MatrizComparaMetodosVectores}: Comparación entre métodos.
#' }
#'
#' @examples
#' \dontrun{
#' resultados <- UnirExcelITFF_Optimizada(
#'   nombreBase = "Data/DocMoreR/BambasEn_",
#'   IndIni = 1, IndFin = 2,
#'   IdiomaIni = "en", IdiomaFin = "en", IdiomaLimpieza = "english",
#'   valorPor = 10, SiTrad = FALSE
#' )
#' head(resultados$UnionXLSX)
#' }
#'
#' @family utilidades
#' @export
UnirExcelITFF_Optimizada <- function(nombreBase, IndIni, IndFin, IdiomaIni, IdiomaFin, IdiomaLimpieza, valorPor, SiTrad) {
  archivo <- paste0(nombreBase, IndIni, ".xlsx")
  if (!file.exists(archivo)) {
    stop(paste("El archivo", archivo, "no existe."))
  }

  # Validar parámetros de entrada
  if (IndIni > IndFin) stop("El índice inicial no puede ser mayor que el índice final.")

  # Leer y unir los archivos Excel en un solo paso
  dataF <- purrr::map_dfr(IndIni:IndFin, ~{
    archivo <- paste0(nombreBase, .x, ".xlsx")
    if (!file.exists(archivo)) stop(paste("El archivo", archivo, "no existe."))
    read_excel(archivo, col_names = c("usuario", "tuit", "linkEstado", "fecha"))
  })

  # Realizar traducción si es necesario
  textoFinal <- if (SiTrad) {
    TraduceIdioma(dataF$tuit, IdiomaIni, IdiomaFin)
  } else {
    dataF$tuit
  }

  # Limpieza y tokenización del texto
  textoLimpio <- LimpiaTexto(textoFinal, IdiomaLimpieza)
  textoSentencias <- get_sentences(textoLimpio)
  poa_Palabras_v_rep <- get_tokens(textoSentencias, pattern = "\\W")
  poa_Palabras_v <- eliminarduplicados(poa_Palabras_v_rep)

  # Análisis de sentimientos usando diferentes métodos
  metodos <- c("syuzhet", "bing", "afinn", "nrc")
  matricesSentimiento <- lapply(metodos, function(metodo) {
    get_sentiment(poa_Palabras_v, method = metodo, lang = IdiomaLimpieza)
  })

  # Crear matriz de comparación de métodos
  MCMV <- do.call(rbind, matricesSentimiento)
  colnames(MCMV) <- poa_Palabras_v
  rownames(MCMV) <- metodos

  # Matriz simplificada valorada por método
  MatSimpMCMC <- MatrizValoradaXMetodo(MCMV, SiTrad)

  # Obtener porcentajes y aplicar DCT
  porcentajes <- lapply(matricesSentimiento, function(vectorSentimiento) {
    get_percentage_values(vectorSentimiento, bins = valorPor)
  })

  dct_valores <- lapply(matricesSentimiento, function(vectorSentimiento) {
    get_dct_transform(vectorSentimiento, low_pass_size = 5, x_reverse_len = 100,
                      scale_vals = FALSE, scale_range = TRUE)
  })

  # Obtener sentimientos por tokens y sentencias
  tSentimientos_data <- get_sentiment(poa_Palabras_v)
  tSentimientos_nrc_data <- get_nrc_sentiment(poa_Palabras_v)
  rownames(tSentimientos_nrc_data) <- poa_Palabras_v

  sSentimientos_data <- get_sentiment(textoSentencias)
  sSentimientos_nrc_data <- get_nrc_sentiment(textoSentencias)

  # Retornar resultados en una lista
  return(list(
    UnionXLSX = dataF,
    SoloTuit = dataF$tuit,
    TextoIdiomaFinal = textoFinal,
    TextoSentencias = textoSentencias,
    TextoTokens = poa_Palabras_v,
    TextoTokens_Rep = poa_Palabras_v_rep,
    PorNPSyuzhetVector = porcentajes[[1]],
    PorNPBingVector = porcentajes[[2]],
    PorNPAfinnVector = porcentajes[[3]],
    PorNPNRCVector = porcentajes[[4]],
    TDCSyuzhet = dct_valores[[1]],
    TDCBing = dct_valores[[2]],
    TDCAfinn = dct_valores[[3]],
    TDCNRC = dct_valores[[4]],
    MatrizComparaMetodosVectores = MCMV,
    MatrizSimplificadaTokensValorados = MatSimpMCMC,
    VectorTextoSentimientos = tSentimientos_data,
    VectorSTextoSentimientos = sSentimientos_data,
    VectorTextoSentimientosNRC = tSentimientos_nrc_data,
    VectorSTextoSentimientosNRC = sSentimientos_nrc_data
  ))
}

#' Limpiar texto para análisis de sentimientos
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función aplica una serie de transformaciones para limpiar texto crudo:
#' convierte a minúsculas, elimina puntuación, números, URLs, espacios extra, y
#' remueve palabras vacías (`stopwords`) según el idioma especificado.
#'
#' @param nov_text Vector de texto a limpiar.
#' @param idioma Idioma para la eliminación de stopwords (por ejemplo, "english").
#' @return Vector de texto limpio.
#' @examples
#' LimpiaTexto("Hello! This is a test... Visit http://example.com", "english")
#' @family utilidades
#' @export
LimpiaTexto <- function(nov_text, idioma) {
  nov_text <- nov_text %>% as.matrix()

  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

  nov_text <- gsub("[[:cntrl:]]", " ", nov_text)
  nov_text <- tolower(nov_text)
  nov_text <- tm::removeWords(nov_text, words = tm::stopwords(idioma))
  nov_text <- tm::removePunctuation(nov_text)
  nov_text <- tm::removeNumbers(nov_text)
  nov_text <- removeNumPunct(nov_text)
  nov_text <- removeURL(nov_text)
  nov_text <- tm::stripWhitespace(nov_text)
  nov_text <- trimws(nov_text)

  return(nov_text)
}

#' Generar matrices léxicas por método y diccionario de traducciones (opcional)
#'
#' @description
#' Construye matrices léxicas (tokens × métodos: syuzhet, bing, afinn, nrc) listas para
#' el pipeline y, opcionalmente, un diccionario de traducciones para etiquetado.
#' **No reescribe rownames** con el idioma destino (evita errores por duplicados).
#'
#' @param mdata \code{matrix}/\code{data.frame} con filas = métodos (syuzhet, bing, afinn, nrc)
#'              y columnas = tokens. Suele ser \code{resultados$MatrizSimplificadaTokensValorados$MatrizCol0}.
#' @param si_trad \code{logical}. Si \code{TRUE}, genera \code{$TraduccTokens} y vectores
#'              \code{$TokenIdioma}, \code{$TokenIdiomaUnico}.
#' @param idioma_destino \code{character}. Idioma destino ("espanol","ingles","portugues","frances", etc.).
#' @param trad_fun \code{c("auto","gpt","traducea","none")}.
#'   \itemize{
#'     \item "auto": intenta \code{TraduceA()}, y si falla usa GPT.
#'     \item "gpt": usa \code{vTraduceEn_gpt()}.
#'     \item "traducea": usa \code{TraduceA()}.
#'     \item "none": no traduce (devuelve estructuras sin diccionario).
#'   }
#' @param model \code{character}. Modelo GPT (si \code{trad_fun="gpt"} o se cae a GPT en "auto").
#' @param batch_size \code{integer}. Tamaño de lote para GPT.
#' @param max_retries \code{integer}. Reintentos para GPT.
#'
#' @return \code{list} con:
#' \itemize{
#'   \item \code{MatrizCol0}: métodos × tokens (filtrada por columnas con algún ≠ 0).
#'   \item \code{MatrizTranspuesta}: tokens × métodos.
#'   \item \code{MatrizTranspuestaSinDup}: tokens × métodos (eliminando filas duplicadas por token).
#'   \item \code{TraduccTokens}: \code{data.frame} (\code{Orig}, \code{IdioOrig}, \code{Traduccion}) si \code{si_trad=TRUE}, sino vacío.
#'   \item \code{TokenIdioma}: vector alineado a \code{rownames(MatrizTranspuesta)} (si \code{si_trad=TRUE}).
#'   \item \code{TokenIdiomaUnico}: idem, pero desambiguado en orden (si \code{si_trad=TRUE}).
#'   \item \code{NumeroFilasConDup}, \code{NumeroFilasSinDup}: conteos útiles.
#'   \item \code{IdiomaDestinoUsado}, \code{FuenteTraduccion}: metadatos.
#' }
#' @family matrices_valoradas
#' @export
#' @param "gpt" (auto) TODO: describir parámetro.
#' @param "traducea" (auto) TODO: describir parámetro.
#' @param "none" (auto) TODO: describir parámetro.
MatrizValoradaXMetodo <- function(
    mdata,
    si_trad        = FALSE,
    idioma_destino = "espanol",
    trad_fun       = c("auto","gpt","traducea","none"),
    model          = "gpt-4o-mini",
    batch_size     = 20,
    max_retries    = 3
){
  trad_fun <- match.arg(trad_fun)

  stopifnot(is.matrix(mdata) || is.data.frame(mdata))
  mdata <- as.data.frame(mdata, check.names = FALSE)

  # --- Métodos soportados y verificación básica
  methods_whitelist <- c("syuzhet","bing","afinn","nrc")
  if (is.null(rownames(mdata))) stop("`mdata` debe tener rownames = métodos.")
  if (!all(methods_whitelist %in% rownames(mdata))) {
    # Acepta subconjunto, pero advierte
    warning("No todos los métodos están presentes en `mdata`. Usando intersección.")
  }
  met_present <- intersect(rownames(mdata), methods_whitelist)
  if (!length(met_present)) stop("`mdata` no contiene ninguno de los métodos esperados.")

  # --- Filtro de columnas con algún valor ≠ 0 en los métodos presentes
  msub <- mdata[met_present, , drop = FALSE]
  if (ncol(msub) == 0) stop("`mdata` no tiene columnas de tokens.")
  col_keep <- apply(msub, 2, function(v) any(as.numeric(v) != 0, na.rm = TRUE))
  if (!any(col_keep)) stop("Todas las columnas son cero para los métodos presentes.")
  mcol0 <- msub[, col_keep, drop = FALSE]

  # --- Transponer a tokens × métodos
  mtran <- t(as.matrix(mcol0))
  mode(mtran) <- "numeric"

  # --- Sin duplicados (conserva la primera aparición)
  mtranSinDup <- mtran[!duplicated(rownames(mtran)), , drop = FALSE]

  # --- Helpers de traducción
  normalize_for_traducea <- function(idioma) {
    x <- tolower(trimws(idioma))
    x <- iconv(x, to = "ASCII//TRANSLIT")
    x <- gsub("[^a-z0-9]+", "", x)
    if (x %in% c("es","esp","espanol","castellano")) return("espanol")
    if (x %in% c("en","eng","ingles","english"))     return("ingles")
    if (x %in% c("pt","por","portugues","portuguese")) return("portugues")
    if (x %in% c("fr","fra","frances","french"))     return("frances")
    return("espanol")
  }

  make_unique_ordered <- function(x){
    seen <- list(); out <- character(length(x))
    for (i in seq_along(x)){
      v <- x[i]
      if (is.null(seen[[v]])) { seen[[v]] <- 1L; out[i] <- v }
      else { seen[[v]] <- seen[[v]] + 1L; out[i] <- paste0(v, " (", seen[[v]], ")") }
    }
    out
  }

  # --- Traducciones (opcional)
  traducciones_df <- data.frame(Orig=character(0), IdioOrig=character(0),
                                Traduccion=character(0), stringsAsFactors = FALSE)
  token_idioma <- token_idioma_unico <- NULL
  fuente_trad  <- "none"
  idioma_usado <- idioma_destino

  if (isTRUE(si_trad) && trad_fun != "none") {
    tokens_unique <- unique(rownames(mtran))
    if (length(tokens_unique)) {
      if (trad_fun == "traducea" || trad_fun == "auto") {
        idioma_norm <- normalize_for_traducea(idioma_destino)
        res <- try(TraduceA(tokens_unique, idioma_destino = idioma_norm), silent = TRUE)
        if (!inherits(res, "try-error") && !is.null(res)) {
          if (is.data.frame(res) && all(c("Orig","Traduccion") %in% names(res))) {
            # Si IdioOrig no viene, complétalo como NA
            if (!"IdioOrig" %in% names(res)) res$IdioOrig <- NA_character_
            traducciones_df <- data.frame(
              Orig       = as.character(res$Orig),
              IdioOrig   = as.character(res$IdioOrig),
              Traduccion = as.character(res$Traduccion),
              stringsAsFactors = FALSE
            )
            fuente_trad <- "TraduceA"
          } else if (trad_fun == "auto") {
            # fallback a GPT
            traducciones_df <- vTraduceEn_gpt(tokens_unique, model = model,
                                              batch_size = batch_size, max_retries = max_retries)
            fuente_trad <- "GPT"
          } else {
            stop("La salida de TraduceA() no tiene columnas esperadas (Orig/Traduccion).")
          }
        } else if (trad_fun == "auto") {
          traducciones_df <- vTraduceEn_gpt(tokens_unique, model = model,
                                            batch_size = batch_size, max_retries = max_retries)
          fuente_trad <- "GPT"
        } else {
          stop("Fallo en TraduceA() y trad_fun != 'auto'.")
        }
      } else if (trad_fun == "gpt") {
        traducciones_df <- vTraduceEn_gpt(tokens_unique, model = model,
                                          batch_size = batch_size, max_retries = max_retries)
        fuente_trad <- "GPT"
      }
      # Vector nombrado Orig -> Traduccion
      dict <- setNames(traducciones_df$Traduccion, traducciones_df$Orig)
      token_idioma        <- unname(dict[rownames(mtran)])
      token_idioma[is.na(token_idioma) | token_idioma==""] <- rownames(mtran)[is.na(token_idioma) | token_idioma==""]
      token_idioma_unico  <- make_unique_ordered(token_idioma)
    }
  }

  # --- Resultado
  return(list(
    MatrizCol0                 = as.data.frame(mcol0, check.names = FALSE),
    MatrizTranspuesta          = as.data.frame(mtran, check.names = FALSE),
    MatrizTranspuestaSinDup    = as.data.frame(mtranSinDup, check.names = FALSE),
    TraduccTokens              = traducciones_df,           # (Orig, IdioOrig, Traduccion)
    TokenIdioma                = token_idioma,              # alineado a rownames(MatrizTranspuesta)
    TokenIdiomaUnico           = token_idioma_unico,        # idem, desambiguado
    NumeroFilasConDup          = nrow(mtran),
    NumeroFilasSinDup          = nrow(mtranSinDup),
    IdiomaDestinoUsado         = idioma_usado,
    FuenteTraduccion           = fuente_trad
  ))
}


# ============================================================
# vTraduceEn_gpt: Traduce un vector de textos usando la API de GPT (OpenAI)
# ------------------------------------------------------------
# Parámetros:
#   texto: vector de textos a traducir
#   model: modelo de OpenAI (p.ej., "gpt-4o-mini")
#   batch_size: tamaño de lote para llamadas por lotes (evita prompts gigantes)
#   max_retries: reintentos ante errores transitorios / rate limit
# Retorna:
#   data.frame con columnas: Orig, IdioOrig (código BCP-47), Traduccion
# ============================================================

vTraduceEn_gpt <- function(texto = character(),
                           model = "gpt-4o-mini",
                           batch_size = 20,
                           max_retries = 3) {

  stopifnot(is.character(texto))
  if (length(texto) == 0) {
    return(data.frame(Orig = character(0),
                      IdioOrig = character(0),
                      Traduccion = character(0),
                      stringsAsFactors = FALSE))
  }

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) stop("Defina OPENAI_API_KEY en el entorno (Sys.setenv).")

  library(httr2)
  library(jsonlite)

  # ---- Helper: llamada segura a Chat Completions ----
  call_openai <- function(payload, attempt = 1) {
    req <- request("https://api.openai.com/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type"  = "application/json"
      ) |>
      req_body_json(payload)

    resp <- tryCatch(req_perform(req), error = function(e) e)
    if (inherits(resp, "error")) {
      if (attempt < max_retries) {
        # backoff exponencial suave
        Sys.sleep(1.5 ^ attempt)
        return(call_openai(payload, attempt + 1))
      } else {
        stop("Error al llamar a OpenAI: ", conditionMessage(resp))
      }
    }

    json <- tryCatch(resp_body_json(resp), error = function(e) e)
    if (inherits(json, "error")) {
      if (attempt < max_retries) {
        Sys.sleep(1.5 ^ attempt)
        return(call_openai(payload, attempt + 1))
      } else stop("No se pudo parsear JSON de OpenAI.")
    }
    json
  }

  # ---- Helper: traduce un lote y devuelve data.frame ----
  translate_batch <- function(text_batch, offset_id = 0L) {
    # Construimos una lista numerada para mantener orden
    batch <- lapply(seq_along(text_batch), function(i) {
      list(id = offset_id + i, text = text_batch[[i]])
    })

    user_payload <- toJSON(list(inputs = batch), auto_unbox = TRUE)

    sys_msg <- list(
      role = "system",
      content = paste(
        "Eres un motor de traducción profesional.",
        "Para cada objeto de 'inputs', detecta el idioma de 'text' y tradúcelo al español.",
        "Responde SOLO un JSON con un array de objetos, uno por input, MISMO ORDEN,",
        "con campos: id (entero), orig (texto original), idio_orig (código BCP-47 como 'en', 'es', 'pt'),",
        "y traduccion (texto traducido). No agregues comentarios ni texto extra."
      )
    )

    user_msg <- list(role = "user", content = user_payload)

    payload <- list(
      model = model,
      messages = list(sys_msg, user_msg),
      temperature = 0
    )

    out <- call_openai(payload)

    content_txt <- out$choices[[1]]$message$content
    # El modelo devuelve un JSON; lo parseamos
    parsed <- tryCatch(fromJSON(content_txt), error = function(e) NULL)

    if (is.null(parsed)) {
      # fallback mínimo: si falla el parse, devolvemos identidad
      return(data.frame(
        Orig        = text_batch,
        IdioOrig    = rep(NA_character_, length(text_batch)),
        Traduccion  = text_batch,
        stringsAsFactors = FALSE
      ))
    }

    # Esperamos data.frame/ lista con columnas id, orig, idio_orig, traduccion
    df <- as.data.frame(parsed, stringsAsFactors = FALSE)

    # Orden por id para respetar orden original
    if ("id" %in% names(df)) {
      df <- df[order(df$id), , drop = FALSE]
    }

    # Normaliza nombres esperados
    rename_safe <- function(nm) {
      sub("^traducción$", "traduccion", nm, ignore.case = TRUE, perl = TRUE)
    }
    names(df) <- rename_safe(names(df))

    # Completa columnas faltantes si fuera necesario
    if (!"orig"       %in% names(df)) df$orig <- text_batch
    if (!"idio_orig"  %in% names(df)) df$idio_orig <- NA_character_
    if (!"traduccion" %in% names(df)) df$traduccion <- text_batch

    # Salida consistente con tu contrato
    data.frame(
      Orig        = df$orig,
      IdioOrig    = df$idio_orig,
      Traduccion  = df$traduccion,
      stringsAsFactors = FALSE
    )
  }

  # ---- Procesamiento por lotes para robustez ----
  n <- length(texto)
  idx <- split(seq_len(n), ceiling(seq_len(n) / batch_size))

  res_list <- vector("list", length(idx))
  for (k in seq_along(idx)) {
    ids <- idx[[k]]
    res_list[[k]] <- translate_batch(texto[ids], offset_id = min(ids) - 1L)
  }

  # Bind y devolver
  out <- do.call(rbind, res_list)
  rownames(out) <- NULL
  out
}






#' Eliminar filas repetidas de un data frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función identifica y elimina filas duplicadas, conservando una sola instancia y
#' agregando una columna que indica cuántas veces se repitió cada fila eliminada.
#'
#' @param dataframe Data frame con posibles filas duplicadas.
#'
#' @return Data frame sin duplicados, con columna `Repeticiones`.
#'
#' @examples
#' df <- data.frame(a = c(1, 1, 2), b = c("x", "x", "y"))
#' eliminar_filas_repetidas(df)
#'
#' @family utilidades
#' @export
eliminar_filas_repetidas <- function(dataframe) {
  dataframe$FilaTexto <- do.call(paste, c(dataframe, sep = "_"))
  duplicados <- data.frame(table(dataframe$FilaTexto))
  dataframe$Repeticiones <- 0
  for (i in 1:nrow(duplicados)) {
    fila_duplicada <- duplicados[i, ]
    fila_indices <- which(dataframe$FilaTexto == fila_duplicada$Var1)
    dataframe[fila_indices, "Repeticiones"] <- fila_duplicada$Freq - 1
  }
  dataframe <- dataframe[!duplicated(dataframe$FilaTexto), ]
  dataframe$FilaTexto <- NULL
  return(dataframe)
}

#' Guardar un data frame como archivo CSV
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función guarda un data frame como archivo `.csv` en la ruta especificada.
#'
#' @param dataFrame Data frame a guardar.
#' @param nombreArchivo Cadena de texto con la ruta y nombre del archivo destino.
#'
#' @return No retorna valor; guarda un archivo en disco.
#'
#' @export
guardarDataFrameCSV <- function(dataFrame, nombreArchivo) {
  write.csv(dataFrame, file = nombreArchivo, row.names = TRUE)
}

#' Leer un archivo CSV con nombres de fila
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función carga un archivo `.csv` considerando la primera columna como nombres de fila.
#'
#' @param archivoCSV Ruta al archivo CSV a leer.
#'
#' @return Data frame con nombres de fila.
#'
#' @export
leerCSVConNombres <- function(archivoCSV) {
  datos <- read.csv(archivoCSV, header = TRUE, row.names = 1)
  return(datos)
}

#' Cambiar los nombres de columna de un data frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' @param data Data frame al que se desea cambiar los nombres.
#' @param nombres Vector de nombres de la misma longitud que el número de columnas del data frame.
#'
#' @return Data frame con los nombres de columnas modificados.
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 4:6)
#' cambiar_nombres_columnas(df, c("A", "B"))
#'
#' @export
cambiar_nombres_columnas <- function(data, nombres) {
  if (length(nombres) != ncol(data)) {
    stop("La longitud del vector de nombres debe ser igual al número de columnas del data.frame.")
  }
  colnames(data) <- nombres
  return(data)
}

#' Modificar los nombres de fila de un data frame
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función reemplaza los nombres de fila con los nombres provistos como cadena
#' separados por comas. Asegura que los nombres resultantes sean únicos.
#'
#' @param dataFrame Data frame a modificar.
#' @param nombresCadena Cadena de texto con los nuevos nombres de fila separados por comas.
#'
#' @return Data frame con nombres de fila actualizados.
#'
#' @examples
#' df <- data.frame(x = 1:3, y = 4:6)
#' modificarNombresFilas(df, "uno, dos, tres")
#'
#' @export
# Función para modificar los nombres de fila de un data frame
modificarNombresFilas <- function(dataFrame, nombresCadena) {
  # Dividir la cadena de nombres por comas para obtener un vector de nombres
  nuevosNombres <- unlist(strsplit(nombresCadena, ","))

  # Eliminar espacios en blanco alrededor de los nombres
  nuevosNombres <- trimws(nuevosNombres)

  # Verificar y agregar sufijos a los nombres duplicados
  nombresUnicos <- make.unique(nuevosNombres, sep = "_")

  # Asignar los nuevos nombres a las filas del data frame
  rownames(dataFrame) <- nombresUnicos
  colnames(dataFrame)<- nombresDataFrameEstudio
  return(dataFrame)
}
#' Generar resumen y gráfico de las palabras más frecuentes
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función genera un resumen con las 20 palabras más frecuentes de un vector de texto.
#' Calcula frecuencia absoluta y relativa, y construye un gráfico de barras con etiquetas.
#'
#' @param vector_palabras Vector de texto con las palabras a analizar.
#' @param palabras_excluidas Vector de palabras a excluir del análisis (por ejemplo: stopwords). Opcional.
#'
#' @return Una lista con dos elementos:
#' \describe{
#'   \item{resumen_palabras}{Un `data.table` con las 20 palabras más frecuentes.}
#'   \item{plot_barras}{Un objeto `ggplot2` con el gráfico de barras.}
#' }
#'
#' @examples
#' texto <- c("gobierno", "mineria", "pueblo", "gobierno", "protesta", "gobierno")
#' generar_resumen_palabras(texto)
#'
#' @import data.table
#' @import ggplot2
#' @family resumen_y_frecuencias
#' @export
generar_resumen_palabras <- function(vector_palabras, palabras_excluidas = NULL) {
  dt_palabras <- data.table(palabra = vector_palabras)

  if (!is.null(palabras_excluidas)) {
    dt_palabras <- dt_palabras[!palabra %in% palabras_excluidas]
  }

  dt_resumen <- dt_palabras[, .(frec_abs = .N), by = palabra][order(-frec_abs)]
  dt_resumen[, frec_rel := frec_abs / sum(frec_abs) * 100]

  dt_resumen_top20 <- dt_resumen[1:min(20, .N)]

  plot_barras <- ggplot(dt_resumen_top20, aes(x = reorder(palabra, -frec_rel), y = frec_rel, label = sprintf("%.1f%%", frec_rel))) +
    geom_bar(stat = "identity", fill = "lightgrey") +
    geom_text(size = 3, vjust = -0.5) +
    labs(x = "Palabra", y = "Frecuencia Relativa (%)", title = "Top 20 Palabras Más Frecuentes") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  lista_resultados <- list(resumen_palabras = dt_resumen_top20, plot_barras = plot_barras)
  return(lista_resultados)
}

#' Analizar la frecuencia de palabras en un vector de texto
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función calcula la frecuencia absoluta y relativa de las palabras, con opción
#' de excluir ciertas palabras, traducirlas, y visualizar las principales como gráfico.
#'
#' @param vector_palabras Vector de palabras a analizar.
#' @param palabras_excluidas Vector de palabras que se deben excluir (opcional).
#' @param idioma_traduccion Idioma de destino para traducción (ej. "spanish").
#' @param usar_chatgpt Lógico. Si TRUE, usa la función TraduceA() para traducir las palabras.
#' @param n_top Número de palabras más frecuentes a mostrar.
#' @param debug Lógico. Si TRUE, muestra mensajes informativos durante el análisis.
#'
#' @return Una lista con:
#'   - resumen_palabras: `data.table` con palabras más frecuentes y frecuencias.
#'   - plot_barras: Objeto ggplot2 con la visualización.
#'
#' @family resumen_y_frecuencias
#' @export
analizar_frecuencia_palabras <- function(vector_palabras,
                                         palabras_excluidas = NULL,
                                         idioma_traduccion = NULL,
                                         usar_chatgpt = FALSE,
                                         n_top = 20,
                                         debug = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Requiere los paquetes 'data.table' y 'ggplot2'.")
  }

  if (length(vector_palabras) == 0 || all(is.na(vector_palabras))) {
    stop("El vector de palabras está vacío o solo contiene NA.")
  }

  dt_palabras <- data.table::data.table(palabra = vector_palabras)
  dt_palabras <- dt_palabras[!is.na(palabra) & palabra != ""]

  if (!is.null(palabras_excluidas)) {
    dt_palabras <- dt_palabras[!palabra %in% palabras_excluidas]
    if (debug) message("Palabras excluidas aplicadas: ", length(palabras_excluidas))
  }

  if (nrow(dt_palabras) == 0) {
    stop("No quedan palabras para analizar después de aplicar exclusiones.")
  }

  dt_resumen <- dt_palabras[, .(frec_abs = .N), by = palabra][order(-frec_abs)]
  dt_resumen[, frec_rel := frec_abs / sum(frec_abs) * 100]
  dt_resumen_top <- head(dt_resumen, n_top)

  if (!is.null(idioma_traduccion) && usar_chatgpt) {
    if (debug) message("Intentando traducir palabras con TraduceA()...")

    palabras_para_traducir <- paste(dt_resumen_top$palabra, collapse = ", ")
    tryCatch({
      traducciones <- TraduceA(palabras_para_traducir, idioma_destino = idioma_traduccion)
      traducciones_vector <- unlist(strsplit(traducciones, ",[[:space:]]*"))
      traducciones_vector <- gsub(" ", "", traducciones_vector)

      if (length(traducciones_vector) == nrow(dt_resumen_top)) {
        dt_resumen_top[, palabra := traducciones_vector]
        if (debug) message("Traducción completada con éxito.")
      } else {
        warning("⚠️ Número de traducciones no coincide con el número de palabras. Se conservan las originales.")
        if (debug) message("Traducciones: ", paste(traducciones_vector, collapse = ", "))
      }
    }, error = function(e) {
      warning("❌ Error durante la traducción: ", e$message)
    })
  }

  plot_barras <- ggplot2::ggplot(dt_resumen_top,
                                 ggplot2::aes(x = reorder(palabra, -frec_rel), y = frec_rel,
                                              label = sprintf("%.1f%%", frec_rel))) +
    ggplot2::geom_bar(stat = "identity", fill = "lightgrey", color = "blue") +
    ggplot2::geom_text(size = 3, vjust = -0.5) +
    ggplot2::labs(x = "Palabra", y = "Frecuencia Relativa (%)",
                  title = paste("Top", n_top, "Palabras Más Frecuentes")) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.background = ggplot2::element_rect(fill = "white"))

  return(list(
    resumen_palabras = dt_resumen_top,
    plot_barras = plot_barras
  ))
}

#' Traducir un vector de palabras (estable) con OpenAI
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Acepta vector o UNA cadena "a, b, c". Devuelve mismo largo/orden.
#' @param palabras character o una cadena separada por comas.
#' @param idioma_destino "español" por defecto.
#' @param limite_palabras tamaño de bloque (50 por defecto).
#' @param debug lógico; si TRUE, imprime trazas.
#' @return vector character traducido.
#' @family utilidades
#' @export
TraduceA <- function(palabras, idioma_destino = "español", limite_palabras = 50, debug = FALSE) {
  if (!verificar_clave_openai()) stop("❌ No se ha configurado la clave de API de OpenAI.")
  if (!is.character(palabras)) stop("❌ 'palabras' debe ser character.")

  # Aceptar "cadena con comas" o vector
  if (length(palabras) == 1L) {
    palabras <- unlist(strsplit(palabras, ",[[:space:]]*"), use.names = FALSE)
  }

  palabras <- trimws(palabras)
  palabras <- palabras[palabras != ""]
  if (!length(palabras)) return(character(0))

  bloques <- split(palabras, ceiling(seq_along(palabras) / limite_palabras))
  resultados <- vector("list", length(bloques))

  for (i in seq_along(bloques)) {
    bloque <- bloques[[i]]
    mensaje <- paste(bloque, collapse = ", ")

    resultado <- openai::create_chat_completion(
      model = "gpt-4",
      messages = list(
        list(
          role = "system",
          content = paste(
            "You are a translator. Translate each word from the input into",
            idioma_destino,
            "separated by commas. Return exactly the same number of words, in the same order."
          )
        ),
        list(role = "user", content = mensaje)
      ),
      temperature = 0,
      max_tokens = 512,
      top_p = 1,
      frequency_penalty = 0,
      presence_penalty = 0
    )

    # parseo simple y estable
    traducciones <- unlist(strsplit(resultado$choices$message.content, ",[[:space:]]*"))
    traducciones <- trimws(traducciones)

    # normalización ligera (si quieres conservar tildes, borra este bloque)
    traducciones <- gsub("[áäâà]", "a", traducciones)
    traducciones <- gsub("[éëêè]", "e", traducciones)
    traducciones <- gsub("[íïîì]", "i", traducciones)
    traducciones <- gsub("[óöôò]", "o", traducciones)
    traducciones <- gsub("[úüûù]", "u", traducciones)

    # quitar puntuación final accidental (p.ej., "extrañar.")
    traducciones <- sub("[[:punct:]]+$", "", traducciones)

    # asegurar mismo largo que el bloque
    if (length(traducciones) != length(bloque)) {
      traducciones <- traducciones[seq_len(min(length(traducciones), length(bloque)))]
      traducciones <- c(traducciones, rep("", length(bloque) - length(traducciones)))
    }

    resultados[[i]] <- traducciones

    if (debug) {
      cat("\n🧩 Bloque", i, "de", length(bloques), "\n",
          "🔤 Originales:\n", paste(bloque, collapse = ", "), "\n",
          "🈂️ Traducciones:\n", paste(traducciones, collapse = ", "), "\n")
    }
  }
  unlist(resultados, use.names = FALSE)
}


#' Verificar si la clave OPENAI_API_KEY está configurada
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función comprueba si la variable de entorno `OPENAI_API_KEY` ha sido definida
#' en la sesión de R. Si no lo está, lanza un error con una sugerencia para configurarla.
#'
#' @return Silenciosamente `TRUE` si la clave está definida. Lanza `stop()` si no lo está.
#'
#' @examples
#' \dontrun{
#' verificar_clave_openai()
#' }
#'
#' @family utilidades
#' @export
verificar_clave_openai <- function() {
  if (Sys.getenv("OPENAI_API_KEY") == "") {
    stop("❌ La variable de entorno 'OPENAI_API_KEY' no está definida.\n",
         "➡️  Usa `configurar_openai_clave(\"sk-...\")` o `Sys.setenv()` antes de continuar.")
  }
  return(TRUE)
}

#' Reescalar valores y clasificarlos cualitativamente
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función toma una columna numérica (por nombre o índice) de un data frame y genera:
#' una versión reescalada desde el mínimo a 0, una clasificación cualitativa por rangos de percentiles
#' y el valor del percentil correspondiente a cada fila.
#'
#' @param datos Un data frame que contiene la columna a analizar.
#' @param columna_valor Nombre o índice de la columna numérica que se va a reescalar.
#' @param nombre_columna_resultado Nombre de la nueva columna para los valores reescalados (por defecto: "diferencia").
#'
#' @return El mismo data frame con tres columnas adicionales:
#' \code{nombre_columna_resultado}, \code{categoria} (clasificación cualitativa), y \code{percentil}.
#' @family utilidades
#' @export
analizar_comentarios_ReEscalado_v3 <- function(datos, columna_valor, nombre_columna_resultado = "diferencia") {
  # Si se pasa un índice, convertirlo al nombre de la columna
  if (is.numeric(columna_valor)) {
    if (columna_valor > ncol(datos) || columna_valor < 1) {
      stop("Índice de columna fuera de rango.")
    }
    columna_valor <- names(datos)[columna_valor]
  }

  # Verificar que la columna existe
  if (!columna_valor %in% names(datos)) {
    stop("La columna especificada no existe en los datos.")
  }

  # Crear columna de valor ajustado desde el mínimo
  valores_originales <- datos[[columna_valor]]
  valor_minimo <- min(valores_originales, na.rm = TRUE)
  valores_ajustados <- valores_originales - valor_minimo
  datos[[nombre_columna_resultado]] <- valores_ajustados

  # Lista de configuraciones
  configuraciones <- list(
    list(cortes = c(0.2, 0.4, 0.6, 0.8), etiquetas = c("muy negativo", "negativo", "Ni negativo Ni positivo", "positivo", "muy positivo")),
    list(cortes = c(0.25, 0.5, 0.75), etiquetas = c("muy negativo", "negativo", "positivo", "muy positivo")),
    list(cortes = c(0.33, 0.66), etiquetas = c("negativo", "Ni negativo Ni positivo", "positivo")),
    list(cortes = 0.5, etiquetas = c("negativo", "positivo"))
  )

  percentiles <- NULL
  etiquetas <- NULL
  for (config in configuraciones) {
    percentiles <- quantile(valores_ajustados, probs = config$cortes, na.rm = TRUE)
    if (length(unique(percentiles)) == length(config$cortes)) {
      etiquetas <- config$etiquetas
      break
    }
  }

  # Fallback si los percentiles no son únicos
  if (is.null(etiquetas) || length(percentiles) == 0) {
    percentiles <- quantile(valores_ajustados, probs = 0.5, na.rm = TRUE)
    etiquetas <- c("negativo", "positivo")
  }

  # Clasificación cualitativa y asignación de percentiles
  datos$categoria <- cut(valores_ajustados,
                         breaks = c(-Inf, percentiles, Inf),
                         labels = etiquetas,
                         right = FALSE)

  datos$percentil <- sapply(valores_ajustados, function(x) {
    p <- percentiles[percentiles >= x][1]
    if (is.na(p)) p <- percentiles[length(percentiles)]
    return(p)
  })

  return(datos)
}

#' Reescalar y clasificar comentarios en diferentes modos
#'
#' @description Descripción (auto). Completar con propósito, entradas y salidas.
#'
#' Esta función permite aplicar estrategias de reescalado y clasificación cualitativa
#' a una columna numérica o a la diferencia entre dos columnas (positiva y negativa).
#' Es útil para clasificar sentimientos o puntajes en rangos interpretables como
#' "positivo", "negativo", entre otros.
#'
#' @param datos Un data frame con las columnas necesarias para el reescalado.
#' @param modo Uno de: `"una_columna"` (usar una sola columna), `"dos_columnas"` (usar diferencia entre dos columnas), `"diferencia"` (usar columna ya calculada).
#' @param columna_valor Nombre o índice de la columna (solo para `una_columna` o `diferencia`).
#' @param col_positiva Nombre o índice de la columna positiva (solo para `dos_columnas`).
#' @param col_negativa Nombre o índice de la columna negativa (solo para `dos_columnas`).
#' @param nombre_columna_resultado Nombre de la nueva columna de valores reescalados. Por defecto `"diferencia"`.
#'
#' @return El data frame original con tres columnas adicionales: la columna reescalada, una variable `categoria` y otra `percentil`.
#' @family utilidades
#' @export
#'
#' @examples
#' # Ejemplo con datos simulados
#' df <- data.frame(
#'   positivo = c(0.1, 0.4, 0.6, 0.8, 1.0),
#'   negativo = c(0.2, 0.3, 0.2, 0.1, 0.0),
#'   syuzhet = c(-0.3, 0.2, 0.4, 0.6, 0.9)
#' )
#'
#' # Modo: diferencia entre dos columnas
#' analizar_comentarios_ReEscalado_general(df, modo = "dos_columnas",
#'                                         col_positiva = "positivo",
#'                                         col_negativa = "negativo")
#'
#' # Modo: una sola columna
#' analizar_comentarios_ReEscalado_general(df, modo = "una_columna",
#'                                         columna_valor = "syuzhet")
#'
#' # Modo: diferencia ya calculada
#' df$diferencia <- df$positivo - df$negativo
#' analizar_comentarios_ReEscalado_general(df, modo = "diferencia",
#'                                         columna_valor = "diferencia")
#' @param "dos_columnas" (auto) TODO: describir parámetro.
#' @param "diferencia" (auto) TODO: describir parámetro.
analizar_comentarios_ReEscalado_general <- function(
    datos,
    modo = c("una_columna", "dos_columnas", "diferencia"),
    columna_valor = NULL,
    col_positiva = NULL,
    col_negativa = NULL,
    nombre_columna_resultado = "diferencia"
) {
  modo <- match.arg(modo)

  if (modo == "dos_columnas") {
    if (is.numeric(col_positiva)) col_positiva <- names(datos)[col_positiva]
    if (is.numeric(col_negativa)) col_negativa <- names(datos)[col_negativa]
    if (!(col_positiva %in% names(datos) && col_negativa %in% names(datos))) {
      stop("Las columnas positiva o negativa no existen en los datos.")
    }
    valores <- datos[[col_positiva]] - datos[[col_negativa]]

  } else if (modo %in% c("una_columna", "diferencia")) {
    if (is.null(columna_valor)) stop("Debe especificar 'columna_valor'.")
    if (is.numeric(columna_valor)) columna_valor <- names(datos)[columna_valor]
    if (!(columna_valor %in% names(datos))) stop("La columna especificada no existe.")
    valores <- datos[[columna_valor]]
  }

  valor_minimo <- min(valores, na.rm = TRUE)
  valores_ajustados <- valores - valor_minimo
  datos[[nombre_columna_resultado]] <- valores_ajustados

  configuraciones <- list(
    list(cortes = c(0.2, 0.4, 0.6, 0.8),
         etiquetas = c("muy negativo", "negativo", "Ni negativo Ni positivo", "positivo", "muy positivo")),
    list(cortes = c(0.25, 0.5, 0.75),
         etiquetas = c("muy negativo", "negativo", "positivo", "muy positivo")),
    list(cortes = c(0.33, 0.66),
         etiquetas = c("negativo", "Ni negativo Ni positivo", "positivo")),
    list(cortes = 0.5,
         etiquetas = c("negativo", "positivo"))
  )

  percentiles <- NULL
  etiquetas <- NULL
  for (config in configuraciones) {
    percentiles <- quantile(valores_ajustados, probs = config$cortes, na.rm = TRUE)
    if (length(unique(percentiles)) == length(config$cortes)) {
      etiquetas <- config$etiquetas
      break
    }
  }

  if (is.null(etiquetas) || length(percentiles) == 0) {
    percentiles <- quantile(valores_ajustados, probs = 0.5, na.rm = TRUE)
    etiquetas <- c("negativo", "positivo")
  }

  datos$categoria <- cut(valores_ajustados,
                         breaks = c(-Inf, percentiles, Inf),
                         labels = etiquetas,
                         right = FALSE)

  datos$percentil <- sapply(valores_ajustados, function(x) {
    p <- percentiles[percentiles >= x][1]
    if (is.na(p)) p <- percentiles[length(percentiles)]
    return(p)
  })

  return(datos)
}

#' Anotar tokens en un idioma destino (simple y robusto)
#'
#' @description
#' Agrega a \code{inter$TablaCompletaNRC} las columnas \code{token_orig},
#' \code{token_idioma} y \code{token_idioma_unico}, sin modificar \code{rownames}.
#' Primero intenta mapear con \code{mvxm$TraduccTokens} (si existe). Para los que
#' falten, usa \code{TraduceA()} en bloque (longitud 1:1, sin reciclaje).
#'
#' Anotar tokens en un idioma destino (simple y robusto, con control del diccionario)
#'
#' @param inter Lista con $TablaCompletaNRC (data.frame, con rownames= tokens)
#' @param mvxm  Lista (opcional) con $TraduccTokens (diccionario Orig->Traduccion en ES)
#' @param idioma_destino Idioma solicitado para TraduceA() (ej. "español","portugués","francés")
#' @param use_mvxm c("auto","always","never"):
#'   - "auto": usa mvxm solo si el idioma_destino es español;
#'   - "always": siempre intenta usar mvxm si existe;
#'   - "never": ignora mvxm y usa TraduceA.
#' @return data.frame con columnas token_orig, token_idioma, token_idioma_unico
#' @family utilidades
#' @export
#' @param "always" (auto) TODO: describir parámetro.
#' @param "never" (auto) TODO: describir parámetro.
anotar_tokens_idioma_simple <- function(inter, mvxm = NULL, idioma_destino = "español",
                                        use_mvxm = c("auto","always","never")) {
  use_mvxm <- match.arg(use_mvxm)
  stopifnot(is.list(inter), "TablaCompletaNRC" %in% names(inter))
  dfx <- inter$TablaCompletaNRC
  stopifnot(is.data.frame(dfx), !is.null(rownames(dfx)))

  tokens <- rownames(dfx)

  # Normalizar idioma solo para decidir si usamos el diccionario ES
  is_es <- tolower(gsub("[^a-záéíóúüñ]", "", idioma_destino)) %in%
    c("es","espanol","español","castellano")

  # 1) Diccionario desde mvxm$TraduccTokens (solo si corresponde usarlo)
  dict <- NULL
  can_use_dict <- (!is.null(mvxm) && !is.null(mvxm$TraduccTokens) &&
                     is.data.frame(mvxm$TraduccTokens) &&
                     all(c("Orig","Traduccion") %in% colnames(mvxm$TraduccTokens)))

  if (use_mvxm == "always" && can_use_dict) {
    tt <- mvxm$TraduccTokens
    dict <- setNames(as.character(tt$Traduccion), as.character(tt$Orig))
  } else if (use_mvxm == "auto" && is_es && can_use_dict) {
    tt <- mvxm$TraduccTokens
    dict <- setNames(as.character(tt$Traduccion), as.character(tt$Orig))
  } else {
    dict <- NULL  # fuerza TraduceA
  }

  # 2) Mapear por diccionario si existe
  token_idioma <- if (!is.null(dict)) unname(dict[tokens]) else rep(NA_character_, length(tokens))

  # 3) Completar faltantes con TraduceA() 1:1 (sin reciclar)
  faltan <- which(is.na(token_idioma) | token_idioma == "")
  if (length(faltan)) {
    safe_translate <- function(v, idioma) {
      res <- try(TraduceA(v, idioma_destino = idioma), silent = TRUE)
      out <- rep(NA_character_, length(v))
      if (!inherits(res, "try-error") && !is.null(res)) {
        if (is.data.frame(res) && "Traduccion" %in% names(res)) {
          tr <- as.character(res$Traduccion)
        } else if (is.atomic(res)) {
          tr <- as.character(res)
        } else tr <- character(0)

        if (length(tr) == length(v)) out <- tr
        else if (length(tr) == 1L)   out <- rep(tr, length(v))
        else if (length(tr) > 1L)    out[seq_len(min(length(tr), length(v)))] <- tr[seq_len(min(length(tr), length(v)))]
      }
      out[is.na(out) | out == ""] <- v[is.na(out) | out == ""]
      out
    }
    token_idioma[faltan] <- safe_translate(tokens[faltan], idioma_destino)
  }

  # 4) Etiqueta única
  make_unique_ordered <- function(x){
    seen <- list(); out <- character(length(x))
    for (i in seq_along(x)){
      v <- x[i]
      if (is.null(seen[[v]])) { seen[[v]] <- 1L; out[i] <- v }
      else { seen[[v]] <- seen[[v]] + 1L; out[i] <- paste0(v, " (", seen[[v]], ")") }
    }
    out
  }

  dfx$token_orig         <- tokens
  dfx$token_idioma       <- token_idioma
  dfx$token_idioma_unico <- make_unique_ordered(token_idioma)

  dfx
}
