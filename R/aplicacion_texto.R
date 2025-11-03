#' @title Comparar métodos léxicos, matriz de co-ocurrencia NRC y clustering emocional
#' @description Funciones auxiliares para explorar relaciones entre métodos léxicos, emociones y patrones en comentarios
#' @return (auto) Verificar y completar la descripción del valor retornado.
#' @export

#' @title Comparar métodos léxicos
#' @description Calcula la matriz de correlación entre diferentes métodos léxicos (ej: syuzhet, bing, afinn).
#' @param matriz_valores data.frame o matriz con columnas como métodos y filas como tokens o documentos.
#' @return Matriz de correlación entre métodos léxicos y gráfico corrplot.
#' @examples
#' comparar_metodos_lexicos(matriz_valores)
#' matriz <- resultados$MatrizSimplificadaTokensValorados$MatrizTranspuesta
#' comparar_metodos_lexicos(matriz)
#' @export
# Comparar métodos léxicos por correlación
comparar_metodos_lexicos <- function(matriz_valores) {
  if (!requireNamespace("corrplot", quietly = TRUE)) stop("Requiere el paquete corrplot")
  cor_matrix <- cor(matriz_valores, use = "complete.obs")
  print(corrplot::corrplot(cor_matrix, method = "ellipse", type = "upper", tl.cex = 0.8))
  return(cor_matrix)
}

#' @title Matriz de coocurrencia NRC
#' @description Calcula la cantidad de veces que las emociones NRC ocurren juntas en comentarios individuales.
#' @param nrc_matrix data.frame con columnas como emociones NRC y filas como comentarios.
#' @return Matriz de coocurrencia (simétrica).
#' @examples
#' matriz_coocurrencia_nrc(resultados$VectorTextoSentimientosNRC)
#' cooc <- matriz_coocurrencia_nrc(resultados$VectorTextoSentimientosNRC)
#' print(cooc)
#' @export
# Matriz de coocurrencia de emociones NRC (por filas de comentarios)
matriz_coocurrencia_nrc <- function(nrc_matrix) {
  binaria <- as.matrix(nrc_matrix) > 0
  coocurrencia <- t(binaria) %*% binaria
  return(coocurrencia)
}

#' @title Clustering emocional con k-means
#' @description Aplica agrupamiento de comentarios en base a vectores emocionales (NRC, syuzhet, etc.) usando kmeans.
#' @param matriz_emocional matriz o data.frame de emociones por comentario.
#' @param k número de clústeres deseados.
#' @param metodo nombre del método (solo para título del gráfico).
#' @return Objeto de clase kmeans y gráfico de clúster si aplica.
#' @examples
#' clusterizar_comentarios_emocionalmente(resultados$VectorSTextoSentimientosNRC, k = 3)
#' km <- clusterizar_comentarios_emocionalmente(resultados$VectorSTextoSentimientosNRC, k = 4, metodo = "nrc")
#' table(km$cluster)
#' @export
# Clustering emocional de comentarios con kmeans
clusterizar_comentarios_emocionalmente <- function(matriz_emocional, k = 3, metodo = "syuzhet") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Requiere ggplot2 para visualizar clustering")
  if (!requireNamespace("stats", quietly = TRUE)) stop("Requiere funciones de clustering del paquete stats")

  datos <- scale(matriz_emocional)
  km <- kmeans(datos, centers = k, nstart = 20)

  if (ncol(datos) > 2 && requireNamespace("FactoMineR", quietly = TRUE)) {
    pca <- FactoMineR::PCA(as.data.frame(datos), graph = FALSE)
    coord <- pca$ind$coord[, 1:2]
    df_plot <- data.frame(PC1 = coord[,1], PC2 = coord[,2], Cluster = factor(km$cluster))
    p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = PC1, y = PC2, color = Cluster)) +
      ggplot2::geom_point() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste("Clustering emocional (", metodo, ")"))
    print(p)
  }
  return(km)
}

#' @title Visualizar matriz de coocurrencia NRC
#' @description Muestra visualmente la matriz de co-ocurrencia de emociones NRC como heatmap o red.
#' @param matriz matriz de coocurrencia generada por matriz_coocurrencia_nrc().
#' @param tipo "heatmap" o "red".
#' @return Gráfico mostrado en pantalla.
#' @examples
#' visualizar_coocurrencia_nrc(cooc, tipo = "heatmap")
#' @export
# Visualización de matriz de coocurrencia NRC
visualizar_coocurrencia_nrc <- function(matriz, tipo = "heatmap") {
  if (tipo == "heatmap") {
    if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("reshape2", quietly = TRUE)) {
      stop("Se requieren ggplot2 y reshape2 para visualizar el heatmap")
    }
    df <- reshape2::melt(matriz)
    colnames(df) <- c("Emocion1", "Emocion2", "Frecuencia")
    g <- ggplot2::ggplot(df, ggplot2::aes(x = Emocion1, y = Emocion2, fill = Frecuencia)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Matriz de co-ocurrencia de emociones NRC")
    print(g)
  } else if (tipo == "red") {
    if (!requireNamespace("igraph", quietly = TRUE) || !requireNamespace("ggraph", quietly = TRUE)) {
      stop("Se requieren igraph y ggraph para visualización tipo red")
    }
    edges <- reshape2::melt(matriz)
    edges <- edges[edges$value > 0 & as.character(edges$Var1) != as.character(edges$Var2), ]
    g <- igraph::graph_from_data_frame(edges, directed = FALSE)
    ggraph::ggraph(g, layout = "fr") +
      ggraph::geom_edge_link(aes(width = value), alpha = 0.6) +
      ggraph::geom_node_point(color = "steelblue", size = 5) +
      ggraph::geom_node_text(ggplot2::aes(label = name), repel = TRUE) +
      ggplot2::theme_void() +
      ggplot2::labs(title = "Red de co-ocurrencia de emociones NRC")
  } else {
    stop("Tipo de visualización no válido: use 'heatmap' o 'red'")
  }
}


#' Analizar emociones presentes en un texto
#'
#' Aplica un diccionario emocional a un vector de palabras para calcular la presencia de emociones.
#'
#' @param texto Vector de palabras (preprocesadas).
#' @param diccionario Diccionario de emociones, generado por `crear_diccionario_emociones()`.
#'
#' @return Un data.frame con las emociones y sus frecuencias en el texto.
#' @family utilidades
#' @export
analizar_emociones_texto <- function(texto, diccionario) {
  stopifnot(is.character(texto), "palabra" %in% names(diccionario))
  palabras_validas <- intersect(texto, diccionario$palabra)

  if (length(palabras_validas) == 0) {
    warning("No se encontraron palabras del diccionario en el texto.")
    return(data.frame())
  }

  emocion_data <- diccionario[diccionario$palabra %in% palabras_validas, ]
  emocion_data <- emocion_data[, setdiff(names(emocion_data), "palabra")]
  emocion_suma <- colSums(emocion_data)
  return(as.data.frame(t(emocion_suma)))
}

#' Determinar la emoción predominante en un texto
#'
#' Devuelve la emoción con mayor valor en un conjunto de emociones.
#'
#' @param emocion_df Data frame con columnas de emociones (salida de `analizar_emociones_texto`).
#'
#' @return Nombre de la emoción con mayor puntuación.
#' @family utilidades
#' @export
emocion_mas_fuerte <- function(emocion_df) {
  if (nrow(emocion_df) == 0 || ncol(emocion_df) == 0) return(NA)
  emocion <- names(emocion_df)[which.max(emocion_df[1, ])]
  return(emocion)
}

#' Agrupar emociones en polaridades y dimensiones
#'
#' Reagrupa las emociones en categorías como 'positivas', 'negativas', etc.
#'
#' @param emocion_df Data frame con columnas emocionales individuales.
#'
#' @return Un data.frame con columnas agrupadas por dimensión afectiva.
#' @family utilidades
#' @export
agrupar_emociones <- function(emocion_df) {
  if (nrow(emocion_df) == 0) return(data.frame())

  agrupado <- data.frame(
    positivo = rowSums(emocion_df[c("alegria", "confianza", "positivo")], na.rm = TRUE),
    negativo = rowSums(emocion_df[c("ira", "asco", "tristeza", "miedo", "negativo")], na.rm = TRUE),
    activacion = rowSums(emocion_df[c("sorpresa", "anticipacion")], na.rm = TRUE)
  )
  return(agrupado)
}
