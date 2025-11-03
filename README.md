DiccionariosEmocion — README
================
Rubén Alexander More Valencia
2025-11-03

# Propósito del proyecto

**DiccionariosEmocion** es un marco analítico para **minería de texto y
modelado de emociones en R**. Permite construir matrices valoradas con
diccionarios (NRC, Bing, Afinn, Syuzhet), integrar modelos
supervisados/no supervisados, analizar componentes (p. ej., PCA), y
**generar reportes** reproducibles.

# Justificación científica

El análisis computacional de emociones relaciona **contenidos
textuales** con **constructos afectivos** (ira, alegría, miedo, etc.).
El paquete integra enfoques **léxicos**, **estadísticos**, **temáticos**
y **distribucionales** (e.g., LDA, BERT), con **visualización** y
**reportes**.

# Arquitectura (extracto)

> Inserte aquí (opcional) un árbol de carpetas del proyecto si desea
> mantenerlo en el README. Puede generarlo con utilidades como
> `fs::dir_tree()` o mantener un bloque manual.

# Instalación y entorno

``` r
# Instalar herramientas de desarrollo (solo si no las tiene)
# install.packages(c("devtools", "roxygen2", "renv", "rmarkdown"))

# (Opcional) Reproducibilidad de entorno
# renv::init()      # una sola vez
# renv::restore()   # cuando quiera recrear el entorno

# Cargar el paquete en desarrollo (desde la raíz del proyecto)
# devtools::load_all()

# Generar documentación roxygen
# devtools::document()
```

# Flujo analítico base

Ajuste rutas y nombres según su proyecto. Las funciones mencionadas
pertenecen al propio paquete/proyecto.

``` r
# 1) Carga de datos (ejemplo reproducible si existe params$datos_csv)
if (file.exists(params$datos_csv)) {
  df <- readr::read_csv(params$datos_csv, show_col_types = FALSE)
} else {
  # Datos de ejemplo mínimos
  df <- tibble::tibble(
    id = 1:5,
    texto = c(
      "texto positivo y alegre",
      "mensaje con miedo y preocupación",
      "disgusto y enojo por problemas",
      "confianza y sorpresa en el avance",
      "tristeza y dudas"
    )
  )
}
print(utils::head(df))
```

    #> # A tibble: 5 × 2
    #>      id texto                            
    #>   <int> <chr>                            
    #> 1     1 texto positivo y alegre          
    #> 2     2 mensaje con miedo y preocupación 
    #> 3     3 disgusto y enojo por problemas   
    #> 4     4 confianza y sorpresa en el avance
    #> 5     5 tristeza y dudas

``` r
# 2) Construcción de matrices valoradas por método (NRC, Bing, Afinn, Syuzhet)
# Nota: Estas funciones deben existir en su proyecto/paquete.
# mat_nrc     <- MatrizValoradaXMetodo(df, metodo = "NRC")
# mat_bing    <- MatrizValoradaXMetodo(df, metodo = "Bing")
# mat_afinn   <- MatrizValoradaXMetodo(df, metodo = "Afinn")
# mat_syuzhet <- MatrizValoradaXMetodo(df, metodo = "Syuzhet")

# Para fines demostrativos (si no existen aún), creamos placeholders:
mat_nrc     <- NULL
mat_bing    <- NULL
mat_afinn   <- NULL
mat_syuzhet <- NULL
```

``` r
# 3) Resumen/Agregación y exploración
# resumen_pal <- generar_resumen_palabras(list(NRC=mat_nrc, Bing=mat_bing, Afinn=mat_afinn, Syuzhet=mat_syuzhet))
# freq_pal    <- analizar_frecuencia_palabras(df, top_n = 30)

# Ejemplo básico de conteo de palabras si aún no dispone de funciones:
freq_pal <- df |>
  dplyr::mutate(tokens = stringr::str_split(texto, "\\s+")) |>
  tidyr::unnest(tokens) |>
  dplyr::count(tokens, sort = TRUE)
utils::head(freq_pal, 10)
```

    #> # A tibble: 10 × 2
    #>    tokens        n
    #>    <chr>     <int>
    #>  1 y             5
    #>  2 alegre        1
    #>  3 avance        1
    #>  4 con           1
    #>  5 confianza     1
    #>  6 disgusto      1
    #>  7 dudas         1
    #>  8 el            1
    #>  9 en            1
    #> 10 enojo         1

``` r
# 4) Análisis de componentes (ejemplo con placeholder)
# comp <- analisis_componentes_general(
#   entradas = list(NRC=mat_nrc, Bing=mat_bing, Afinn=mat_afinn, Syuzhet=mat_syuzhet),
#   config   = list(escalar = TRUE, metodo_reduccion = "PCA")
# )

# Placeholder si no existe la función en el namespace de su sesión:
comp <- list(info = "Resultado de PCA/UMAP/etc. (ejemplo)")
str(comp)
```

    #> List of 1
    #>  $ info: chr "Resultado de PCA/UMAP/etc. (ejemplo)"

``` r
# 5) Exportación de artefactos (RDS)
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)
artefacto <- list(
  comp = comp,
  # resumen = resumen_pal,
  freq = freq_pal
)
saveRDS(artefacto, file = params$artefacto)
params$artefacto
```

    #> [1] "outputs/analisis_emociones.rds"

# Reportes

Genere reportes específicos de análisis a partir del artefacto RDS:

``` r
# rmarkdown::render("reportes/Informe_Emociones.Rmd",
#   params = list(artefacto = "outputs/analisis_emociones.rds")
# )
```

# Buenas prácticas

- Documentación con **roxygen2** y ejemplos.
- Nombres de variables **sin tildes**.
- Lógica en `R/`, datos en `data/`/`inst/extdata/`, reportes en
  `inst/reportes/` o `reportes/`.
- Versionar funciones; incorporar pruebas cuando aplique.

# Cita

> More Valencia, R. A. (Año). *DiccionariosEmocion: Marco analítico para
> minería de texto y modelado de emociones en R*. Proyecto Doctorado
> Diccionarios Emoción.

# Sesión

``` r
sessionInfo()
```

    #> R version 4.4.1 (2024-06-14)
    #> Platform: x86_64-apple-darwin20
    #> Running under: macOS 15.5
    #> 
    #> Matrix products: default
    #> BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
    #> LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> time zone: America/Lima
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>   [1] tidyselect_1.2.1     timeDate_4041.110    dplyr_1.1.4          farver_2.1.2        
    #>   [5] S7_0.2.0             fastmap_1.2.0        TH.data_1.1-4        pROC_1.19.0.1       
    #>   [9] caret_7.0-1          digest_0.6.37        rpart_4.1.24         timechange_0.3.0    
    #>  [13] estimability_1.5.1   lifecycle_1.0.4      factoextra_1.0.7     cluster_2.1.8.1     
    #>  [17] multcompView_0.1-10  survival_3.8-3       magrittr_2.0.4       kernlab_0.9-33      
    #>  [21] compiler_4.4.1       sass_0.4.10          rlang_1.1.6          tools_4.4.1         
    #>  [25] utf8_1.2.6           yaml_2.3.10          data.table_1.17.8    knitr_1.50          
    #>  [29] ggsignif_0.6.4       htmlwidgets_1.6.4    scatterplot3d_0.3-44 plyr_1.8.9          
    #>  [33] RColorBrewer_1.1-3   abind_1.4-8          multcomp_1.4-28      withr_3.0.2         
    #>  [37] purrr_1.1.0          nnet_7.3-20          grid_4.4.1           stats4_4.4.1        
    #>  [41] ggpubr_0.6.1         xtable_1.8-4         future_1.67.0        ggplot2_4.0.0       
    #>  [45] globals_0.18.0       emmeans_1.11.2-8     scales_1.4.0         iterators_1.0.14    
    #>  [49] MASS_7.3-65          flashClust_1.01-2    cli_3.6.5            mvtnorm_1.3-3       
    #>  [53] rmarkdown_2.30       generics_0.1.4       rstudioapi_0.17.1    future.apply_1.20.0 
    #>  [57] reshape2_1.4.4       cachem_1.1.0         stringr_1.5.2        splines_4.4.1       
    #>  [61] parallel_4.4.1       vctrs_0.6.5          hardhat_1.4.2        glmnet_4.1-10       
    #>  [65] Matrix_1.7-4         sandwich_3.1-1       jsonlite_2.0.0       carData_3.0-5       
    #>  [69] car_3.1-3            rstatix_0.7.2        ggrepel_0.9.6        Formula_1.2-5       
    #>  [73] listenv_0.9.1        FactoMineR_2.12      foreach_1.5.2        jquerylib_0.1.4     
    #>  [77] gower_1.0.2          tidyr_1.3.1          recipes_1.3.1        glue_1.8.0          
    #>  [81] parallelly_1.45.1    codetools_0.2-20     DT_0.34.0            lubridate_1.9.4     
    #>  [85] stringi_1.8.7        shape_1.4.6.1        gtable_0.3.6         tibble_3.3.0        
    #>  [89] pillar_1.11.1        htmltools_0.5.8.1    ipred_0.9-15         lava_1.8.1          
    #>  [93] R6_2.6.1             evaluate_1.0.5       lattice_0.22-7       backports_1.5.0     
    #>  [97] leaps_3.2            broom_1.0.10         bslib_0.9.0          class_7.3-23        
    #> [101] Rcpp_1.1.0           coda_0.19-4.1        nlme_3.1-168         prodlim_2025.04.28  
    #> [105] xfun_0.53            zoo_1.8-14           ModelMetrics_1.2.2.2 pkgconfig_2.0.3
