library(FactoMineR)
library(factoextra)
library(corrplot)
library(RColorBrewer)
library(PerformanceAnalytics)
library(gplots)
library(stats)
library(readxl)
library(Hmisc)
library(corrplot)
library(plotly)
library(ggcorrplot)
library(ggthemes)
library(data.table)
library(viridis)
library(patchwork)
#' Analisis de Correspondencias (CA) con metricas y biplots
#'
#' @title resModeloCA
#' @description Ejecuta CA sobre una tabla de contingencia y retorna porcentajes,
#' chi-cuadrado, varianza explicada y biplots listos para graficar con \code{ggplot2}.
#' @param dataCA Matriz o data.frame numerico (tabla de contingencia).
#' @param titulo (Opcional) titulo descriptivo.
#' @param col_paleta (Opcional) paleta para los graficos (vector de colores o nombre).
#' @return Lista con elementos: \code{Tabla}, \code{PorcentajeGlobal}, \code{PorcentajeFila},
#' \code{PorcentajeCol}, \code{ChiCuadrado}, \code{P_Valor}, \code{modelo},
#' \code{VarianzaExplicada}, \code{BiplotSimetrico}, \code{BiplotAsimetrico},
#' \code{PlotCos2Fila}, \code{PlotCos2Col}.
#' @examples
#' \dontrun{
#' library(FactoMineR); library(factoextra); library(corrplot)
#' tb <- as.table(matrix(c(12,7,3, 5,6,4), nrow=2, byrow=TRUE))
#' res <- resModeloCA(tb, "Ejemplo CA")
#' res$BiplotSimetrico
#' }
#' @family modelado_supervisado
#' @export
resModeloCA <- function(dataCA, titulo = NULL, col_paleta = NULL) {
  dataCA <- as.matrix(dataCA)
  if (!all(is.finite(dataCA) | is.na(dataCA))) stop("dataCA contiene valores no numericos.")
  res.ca <- FactoMineR::CA(dataCA, graph = FALSE)
  chq <- suppressWarnings(stats::chisq.test(dataCA))
  prop_global <- round(100 * prop.table(dataCA), 2)
  prop_fila   <- round(100 * prop.table(dataCA, 1), 2)
  prop_col    <- round(100 * prop.table(dataCA, 2), 2)
  eig.val <- factoextra::get_eigenvalue(res.ca)
  biplot_sim <- factoextra::fviz_ca_biplot(res.ca, repel = TRUE, geom = c("point", "text"),
                                           alpha.row = 0.6, palette = col_paleta)
  biplot_rowp <- factoextra::fviz_ca_biplot(res.ca, map = "rowprincipal", arrow = c(True, True),
                                            repel = True, palette = col_paleta)
  cos2_row <- res.ca$row$cos2; cos2_col <- res.ca$col$cos2
  grDevices::recordGraphics({ corrplot::corrplot(cos2_row, is.corr = False, tl.cex = 0.7) },
                            list(), getNamespace("graphics"))
  plot_row <- grDevices::recordPlot()
  grDevices::recordGraphics({ corrplot::corrplot(cos2_col, is.corr = False, tl.cex = 0.7) },
                            list(), getNamespace("graphics"))
  plot_col <- grDevices::recordPlot()
  list(Tabla = dataCA, PorcentajeGlobal = prop_global, PorcentajeFila = prop_fila,
       PorcentajeCol = prop_col, ChiCuadrado = unname(chq$statistic), P_Valor = chq$p.value,
       modelo = res.ca, VarianzaExplicada = eig.val, BiplotSimetrico = biplot_sim,
       BiplotAsimetrico = biplot_rowp, PlotCos2Fila = plot_row, PlotCos2Col = plot_col)
}

#' PCA con graficos clave y clustering de variables
#'
#' @title resModeloPCA
#' @description Ejecuta PCA (FactoMineR) y devuelve objetos graficables (factoextra)
#' y clustering k-means de variables activas.
#' @param dataPCA Data.frame o matriz donde una columna (indCluster) es factor/grupo.
#' @param indCluster Indice (entero, 1..p) de la columna que representa el grupo.
#' @return Lista con modelo PCA, varianza, objetos de variables/individuos y graficos.
#' @examples
#' \dontrun{
#' library(FactoMineR); library(factoextra); set.seed(1)
#' X <- iris[,1:4]; g <- iris$Species; df <- cbind(X, grupo = g)
#' res <- resModeloPCA(df, indCluster = 5)
#' res$graficaBiplot
#' }
#' @family reduccion_dimensional
#' @export
resModeloPCA <- function(dataPCA, indCluster) {
  df <- as.data.frame(dataPCA)
  if (indCluster < 1 || indCluster > ncol(df)) stop("indCluster fuera de rango.")
  X <- df[, -indCluster, drop = False]
  if (!all(vapply(X, is.numeric, True))) stop("Todas las columnas excepto indCluster deben ser numericas.")
  res.pca <- FactoMineR::PCA(X, ncp = 4, graph = False)
  eig.val <- factoextra::get_eigenvalue(res.pca)
  var <- factoextra::get_pca_var(res.pca)
  ind <- factoextra::get_pca_ind(res.pca)
  set.seed(123)
  res.km <- stats::kmeans(var$coord, centers = 2, nstart = 15)
  grp <- as.factor(res.km$cluster)
  g_varianza <- factoextra::fviz_eig(res.pca, addlabels = True, ylim = c(0, 50))
  g_mapa_var <- factoextra::fviz_pca_var(res.pca, col.var = "cos2",
                                         gradient.cols = viridis::viridis(3), repel = True)
  g_mapa_ind <- factoextra::fviz_pca_ind(res.pca, col.ind = "cos2",
                                         gradient.cols = c("#FF0000", "#AAAAAA", "#0000FF"), repel = True)
  g_cluster_var <- factoextra::fviz_pca_var(res.pca, col.var = grp,
                                            palette = viridis::viridis(length(unique(grp))), legend.title = "Cluster de Variables")
  g_cluster_ind <- factoextra::fviz_pca_ind(res.pca, geom.ind = "point",
                                            col.ind = df[, indCluster],
                                            palette = viridis::viridis(length(unique(df[, indCluster]))),
                                            addEllipses = True, legend.title = "Grupos")
  g_biplot <- factoextra::fviz_pca_biplot(res.pca, col.var = "contrib",
                                          gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                          col.ind = "black", repel = True)
  g_comb <- (g_varianza + g_mapa_var) / (g_mapa_ind + g_cluster_var)
  list(modelo = res.pca, propVarianza = eig.val, variablesActivas = var,
       graficaVarianza = g_varianza, graficaMapaVar = g_mapa_var, graficaMapaInd = g_mapa_ind,
       graficaClusterVar = g_cluster_var, graficaClusterInd = g_cluster_ind,
       graficaBiplot = g_biplot, graficaCombinada = g_comb)
}

#' PCA + HCPC con (opcional) escalamiento y graficas listas
#'
#' @title resModeloCluster
#' @description Ejecuta un flujo de **PCA** (FactoMineR) sobre variables numéricas
#' (excluyendo una columna de clase), seguido de **HCPC** (clustering jerárquico
#' sobre componentes). Incluye escalamiento opcional (varianza unitaria) y retorna
#' objetos de graficación listos via \code{factoextra}.
#'
#' @param dataClus \code{data.frame} con variables numéricas y una columna de clase.
#' @param indiceClase \code{integer}. Índice (1..p) de la columna de clase a excluir del PCA.
#' @param vectorClase \code{factor} o \code{character}. Etiquetas para colorear individuos en los plots.
#'        Debe tener longitud igual al número de filas de \code{dataClus}.
#' @param scale_unit \code{logical}. Si \code{TRUE}, estandariza variables a varianza 1
#'        en la PCA (equivalente a \code{scale.unit=TRUE} en \code{FactoMineR::PCA}).
#' @param ncp \code{integer} o \code{NULL}. Número de componentes a retener en la PCA.
#'        Si \code{NULL}, usa \code{min(5, ncol(X))}.
#' @param hcpc_args \code{list}. Parámetros extra para \code{FactoMineR::HCPC}
#'        (por ejemplo, \code{list(nb.clust = -1)} para detección automática de k).
#'
#' @details
#' La función:
#' 1) Verifica tipos y longitudes; 2) excluye la columna de clase del PCA;
#' 3) elimina filas con NA de manera segura (\code{na.omit}); 4) remueve columnas
#'    con varianza cero; 5) corre \code{PCA} con centrado y escalamiento opcional;
#' 6) aplica \code{HCPC} al resultado de la PCA; 7) construye visualizaciones
#'    (cluster plot, dendrograma, mapas PCA por grupos).
#'
#' @return \code{list} con:
#' \itemize{
#'   \item \code{modelo} objeto \code{PCA}.
#'   \item \code{propVarianza} data.frame de eigenvalores (\code{factoextra::get_eigenvalue}).
#'   \item \code{variablesActivas} lista de cargas/coord/cos2 (\code{factoextra::get_pca_var}).
#'   \item \code{hcpc} objeto \code{HCPC}.
#'   \item \code{filas_usadas} \code{integer} índices de filas conservadas (tras \code{na.omit}).
#'   \item \code{graficaCluster} ggplot de clusters.
#'   \item \code{graficaDend} ggplot del dendrograma.
#'   \item \code{graficaInd} ggplot de individuos (PCA) coloreados por \code{vectorClase}.
#'   \item \code{graficaDim} ggplot (individuos + elipses por grupo).
#'   \item \code{graficaCof} ggplot (elipses de confianza por grupo).
#'   \item \code{graficaBiplot}, \code{graficaBiplot2} biplots listos.
#' }
#'
#' @examples
#' \dontrun{
#' library(FactoMineR); library(factoextra)
#' df <- iris
#' df$Species <- factor(df$Species)
#' # Excluir clase (columna 5) del PCA; colorear por Species
#' res <- resModeloCluster(
#'   dataClus    = df,
#'   indiceClase = 5,
#'   vectorClase = df$Species,
#'   scale_unit  = TRUE,
#'   hcpc_args   = list(nb.clust = -1)  # elige k automáticamente
#' )
#' res$graficaCluster
#' res$graficaDend
#' res$graficaBiplot
#' }
#' @seealso \code{\link[FactoMineR]{PCA}}, \code{\link[FactoMineR]{HCPC}}
#' @family modelado_supervisado
#' @export
resModeloCluster <- function(dataClus,
                             indiceClase,
                             vectorClase,
                             scale_unit = TRUE,
                             ncp = NULL,
                             hcpc_args = list(nb.clust = -1)) {

  # --- Validaciones básicas ---
  df <- as.data.frame(dataClus)
  if (!is.numeric(indiceClase) || length(indiceClase) != 1L ||
      indiceClase < 1L || indiceClase > ncol(df)) {
    stop("indiceClase debe ser un entero valido (1..ncol(dataClus)).")
  }
  if (length(vectorClase) != nrow(df)) {
    stop("vectorClase debe tener la misma longitud que nrow(dataClus).")
  }
  vectorClase <- as.factor(vectorClase)

  # Separar X (solo numericas) y limpiar
  X <- df[, -indiceClase, drop = FALSE]
  # Mantener solo columnas numericas
  es_num <- vapply(X, is.numeric, TRUE)
  if (!all(es_num)) X <- X[, es_num, drop = FALSE]
  if (ncol(X) < 2) stop("Se requieren >= 2 columnas numericas para PCA.")

  # Remover columnas con varianza ~ 0
  var_sd <- vapply(X, stats::sd, numeric(1), na.rm = TRUE)
  X <- X[, var_sd > 0, drop = FALSE]
  if (ncol(X) < 2) stop("Tras remover varianza cero, quedan < 2 columnas.")

  # Quitar filas con NA (y alinear vectorClase)
  ok <- stats::complete.cases(X)
  X <- X[ok, , drop = FALSE]
  vlab <- vectorClase[ok]
  if (nrow(X) < 3) stop("Muy pocas filas sin NA para ejecutar PCA/HCPC.")

  # Elegir ncp si no se define
  if (is.null(ncp)) ncp <- min(5L, ncol(X))

  # --- PCA (centrada por defecto; escalamiento opcional) ---
  res.pca <- FactoMineR::PCA(X,
                             ncp        = ncp,
                             graph      = FALSE,
                             scale.unit = isTRUE(scale_unit))

  eig.val <- factoextra::get_eigenvalue(res.pca)
  var     <- factoextra::get_pca_var(res.pca)

  # --- HCPC con argumentos flexibles ---
  # (nb.clust=-1 => seleccion automatica del numero de clusters)
  res.hcpc <- do.call(FactoMineR::HCPC, c(list(res.pca, graph = FALSE), hcpc_args))

  # --- Graficos ---
  graficaInd <- factoextra::fviz_pca_ind(
    res.pca, repel = TRUE, habillage = vlab, addEllipses = FALSE, palette = "jco"
  )
  graficaDim <- factoextra::fviz_pca_ind(
    res.pca, geom.ind = "point", col.ind = vlab,
    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
    addEllipses = TRUE, legend.title = "Grupos"
  )
  graficaCof <- factoextra::fviz_pca_ind(
    res.pca, geom.ind = "point", col.ind = vlab,
    palette = c("#00AFBB", "#E7B800", "#FC4E07"),
    addEllipses = TRUE, ellipse.type = "confidence", legend.title = "Grupos"
  )
  graficaBiplot <- factoextra::fviz_pca_biplot(
    res.pca, col.ind = vlab, palette = "lancet",
    addEllipses = TRUE, label = "var", col.var = "black", repel = TRUE,
    legend.title = "Estudio Clases"
  )
  graficaBiplot2 <- factoextra::fviz_pca_biplot(
    res.pca, geom.ind = "point", fill.ind = vlab, col.ind = "black",
    pointshape = 21, pointsize = 2, palette = "lancet",
    addEllipses = TRUE, alpha.var = "contrib", col.var = "contrib",
    gradient.cols = "Set1",
    legend.title = list(fill = "Estudio", color = "Contrib", alpha = "Contrib")
  )

  # Paleta dinamica para clusters
  n_clusters <- length(unique(res.hcpc$data.clust))
  pal <- viridis::viridis(n_clusters)

  graficaCluster <- factoextra::fviz_cluster(
    res.hcpc,
    repel = TRUE,
    show.clust.cent = TRUE,
    palette = pal,
    ggtheme = ggplot2::theme_minimal(),  # tema base de ggplot2
    main = "Factor map"
  )



  graficaDend <- factoextra::fviz_dend(
    res.hcpc, cex = 0.7, palette = pal,
    rect = TRUE, rect_fill = TRUE, rect_border = "jco",
    labels_track_height = 0.8
  )

  list(
    modelo           = res.pca,
    propVarianza     = eig.val,
    variablesActivas = var,
    hcpc             = res.hcpc,
    filas_usadas     = which(ok),
    graficaDend      = graficaDend,
    graficaCluster   = graficaCluster,
    graficaDim       = graficaDim,
    graficaCof       = graficaCof,
    graficaInd       = graficaInd,
    graficaBiplot    = graficaBiplot,
    graficaBiplot2   = graficaBiplot2
  )
}


#' MFA (Multiple Factor Analysis) con visualizaciones clave
#'
#' @title resModeloMFA
#' @description Ejecuta MFA con FactoMineR devolviendo varianza, variables (cuantitativas/cualitativas)
#' y graficos listos de \code{factoextra}.
#' @param dataMFA Data.frame con bloques de variables.
#' @param grupos Vector de enteros con tamanos de cada bloque.
#' @param tipos Vector de tipos por bloque ("s", "n", "f", etc.).
#' @param nombres Nombres de grupos.
#' @param rangoDim Numero de grupos suplementarios (opcional).
#' @param indice Indice de columna para \code{habillage} en individuos.
#' @return Lista con modelo, varianza y graficos.
#' @examples
#' \dontrun{
#' # Ver ?FactoMineR::MFA para un ejemplo detallado
#' }
#' @family modelado_supervisado
#' @export
resModeloMFA <- function(dataMFA, grupos, tipos, nombres, rangoDim = NULL, indice) {
  res.mfa <- FactoMineR::MFA(dataMFA, group = grupos, type = tipos, name.group = nombres,
                             num.group.sup = rangoDim, graph = False)
  eig.val    <- factoextra::get_eigenvalue(res.mfa)
  group      <- factoextra::get_mfa_var(res.mfa, "group")
  quanti.var <- factoextra::get_mfa_var(res.mfa, "quanti.var")
  quali.var  <- factoextra::get_mfa_var(res.mfa, "quali.var")
  list(modelo = res.mfa, propVarianza = eig.val,
       graficaDim = factoextra::fviz_screeplot(res.mfa),
       grupos = group, graficaGrupos = factoextra::fviz_mfa_var(res.mfa, "group"),
       varCuanti = quanti.var,
       graficaCorrCuanti = factoextra::fviz_mfa_var(res.mfa, "quanti.var", palette = "lancet",
                                                    col.var.sup = "violet", repel = True, geom = c("point","text"), legend = "bottom"),
       graficaCorrCuali = factoextra::fviz_mfa_var(res.mfa, "quali.var", palette = "lancet",
                                                   col.var.sup = "violet", repel = True, geom = c("point","text"), legend = "bottom"),
       graficaEjesParciales = factoextra::fviz_mfa_axes(res.mfa),
       graficaInd = factoextra::fviz_mfa_ind(res.mfa, col.ind = "cos2",
                                             gradient.cols = c("#00AFBB","#E7B800","#FC4E07"), repel = True),
       graficaIndCat = factoextra::fviz_mfa_ind(res.mfa, habillage = names(dataMFA[indice]),
                                                palette = c("#00AFBB","#E7B800","#FC4E07"), addEllipses = True,
                                                ellipse.type = "confidence", repel = True))
}
