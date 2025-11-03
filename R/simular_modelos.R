#' @title Simulacion usando modelo de regresion lineal
#' @description Genera predicciones a partir de un modelo de regresion lineal previamente entrenado.
#' @param modelo modelo entrenado
#' @param nuevos_datos dataframe con nuevas observaciones
#' @return vector de predicciones
#' @family utilidades
#' @export
simular_regresion <- function(modelo, nuevos_datos) {
  predict(modelo, newdata = nuevos_datos)
}

#' @title Simulacion usando SVM Multiclase
#' @description Genera predicciones a partir de un modelo SVM multiclase entrenado con e1071.
#' @param modelo objeto de clase \code{svm} entrenado (salida de \code{ajustar_svm_multiclase()$modelo})
#' @param nuevos_datos data.frame con las columnas predictoras, en el mismo orden que en el entrenamiento
#' @param columnas_referencia vector opcional con las columnas que deben verificarse y reordenarse
#' @param valores_reales vector factor opcional con las clases verdaderas para calcular matriz de confusión
#' @param mostrar_grafico lógico, si es TRUE se imprime una matriz de confusión y gráfico de calor con ggplot2
#' @return una lista con \code{predicciones} y \code{probabilidades}
#' @examples
#' modelo_svm <- ajustar_svm_multiclase(acdfx, columnas_predictoras = vPred, columna_clase = "categoria")
#' nuevos <- head(acdfx, 3)
#' resultado <- simular_svm_multiclase(modelo_svm$modelo, nuevos, columnas_referencia = vPred,
#'                                     valores_reales = nuevos$categoria, mostrar_grafico = TRUE)
#' @family modelado_supervisado
#' @export
simular_svm_multiclase <- function(modelo, nuevos_datos, columnas_referencia = NULL,
                                   valores_reales = NULL, mostrar_grafico = FALSE) {
  if (!inherits(modelo, "svm")) {
    stop("El modelo proporcionado no es un objeto de clase 'svm'.")
  }

  if (!is.null(columnas_referencia)) {
    if (!all(columnas_referencia %in% colnames(nuevos_datos))) {
      stop("Faltan columnas requeridas en los nuevos datos.")
    }
    nuevos_datos <- nuevos_datos[, columnas_referencia, drop = FALSE]
  }

  pred_clase <- predict(modelo, nuevos_datos)
  pred_prob  <- attr(predict(modelo, nuevos_datos, probability = TRUE), "probabilities")

  if (!is.null(valores_reales) && mostrar_grafico) {
    if (!is.factor(valores_reales)) valores_reales <- as.factor(valores_reales)
    if (!all(levels(valores_reales) %in% levels(pred_clase))) {
      warning("Los niveles de los valores reales no coinciden con los del modelo.")
    }

    cm <- caret::confusionMatrix(pred_clase, valores_reales)
    print(cm)

    # Métricas
    accuracy <- cm$overall["Accuracy"]
    macro_f1 <- mean(cm$byClass[, "F1"], na.rm = TRUE)

    cm_tab <- cm$table
    TP <- sum(diag(cm_tab))
    FP <- sum(colSums(cm_tab)) - TP
    FN <- sum(rowSums(cm_tab)) - TP
    precision_micro <- TP / (TP + FP)
    recall_micro <- TP / (TP + FN)
    micro_f1 <- if ((precision_micro + recall_micro) > 0) {
      2 * precision_micro * recall_micro / (precision_micro + recall_micro)
    } else {
      NA
    }

    subtitle_text <- paste0("Accuracy = ", round(accuracy, 3),
                            " | Macro-F1 = ", round(macro_f1, 3),
                            " | Micro-F1 = ", round(micro_f1, 3))

    cm_df <- as.data.frame(cm_tab)
    colnames(cm_df) <- c("Referencia", "Prediccion", "Frecuencia")

    ggplot2::ggplot(cm_df, ggplot2::aes(x = Referencia, y = Prediccion, fill = Frecuencia)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = Frecuencia), color = "black", size = 4) +
      ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
      ggplot2::labs(
        title = "Matriz de Confusión (SVM Multiclase)",
        subtitle = subtitle_text,
        x = "Real",
        y = "Predicción"
      ) +
      ggplot2::theme_minimal()
  }

  return(list(
    predicciones = pred_clase,
    probabilidades = pred_prob
  ))
}



#' @title Simulacion usando modelo PCA + SVM
#' @description Aplica el modelo SVM entrenado sobre proyecciones PCA de nuevos datos ya escalados.
#' @param modelo modelo SVM entrenado
#' @param nuevos_datos data.frame con observaciones nuevas sin transformar
#' @param pca objeto PCA (salida de prcomp)
#' @param n_comp número de componentes principales a usar
#' @param escalado lista con center y scale para aplicar a nuevos datos
#' @return lista con predicciones de clase y matriz de probabilidades
#' @examples
#' resultado <- ajustar_pca_svm(acdfx, vPred, "categoria")
#' nuevos <- head(acdfx, 5)[, vPred]
#' sim <- simular_pca_svm(resultado$modelo, nuevos,
#'                        pca = resultado$pca, n_comp = resultado$n_comp,
#'                        escalado = resultado$escalado)
#' print(sim$pred)
#' print(sim$prob)
#' @family reduccion_dimensional
#' @export
simular_pca_svm <- function(modelo, nuevos_datos, pca = NULL, n_comp = NULL, escalado = NULL) {
  if (!inherits(nuevos_datos, "data.frame")) stop("nuevos_datos debe ser un data.frame")

  # Aplicar escalado si se proporciona
  if (!is.null(escalado)) {
    nuevos_datos <- scale(nuevos_datos, center = escalado$center, scale = escalado$scale)
  }

  # Aplicar PCA si se proporciona
  if (!is.null(pca) && !is.null(n_comp)) {
    nuevos_datos <- predict(pca, nuevos_datos)[, 1:n_comp, drop = FALSE]
  }

  # Predecir clases y probabilidades
  pred <- predict(modelo, as.data.frame(nuevos_datos), probability = TRUE)
  probs <- attr(predict(modelo, as.data.frame(nuevos_datos), probability = TRUE), "probabilities")

  proj <- as.data.frame(nuevos_datos)
  proj$Clase_Predicha <- pred
  if (!is.null(colnames(nuevos_datos))) {
    colnames(proj)[1:2] <- paste0("PC", 1:2)
  } else {
    names(proj)[1:2] <- c("PC1", "PC2")
  }

  # Gráfico proyectado
  plot_pca_pred <- ggplot2::ggplot(proj, ggplot2::aes(x = PC1, y = PC2, color = Clase_Predicha)) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Proyección PCA con predicciones SVM",
                  x = "PC1", y = "PC2") +
    ggplot2::theme(legend.position = "bottom")

  return(list(
    pred = pred,
    prob = probs,
    plot = plot_pca_pred
  ))
}



#' @title Simulacion usando arbol de decision
#' @description Realiza predicciones a partir de un arbol de decision entrenado.
#' @param modelo modelo entrenado (rpart)
#' @param nuevos_datos dataframe con nuevas observaciones. Debe incluir la columna 'clase' si se desea matriz de confusion.
#' @return vector de predicciones
#' @examples
#' modelo_ajustado <- ajustar_modelo_arboles(datos = acdfx, predictoras = vPred, clase = "categoria", metodo = "arbol")
#' nuevos <- head(acdfx, 3)
#' pred <- simular_arbol(modelo_ajustado$modelo, nuevos)
#' print(pred)
#' @family utilidades
#' @export
#' @param mostrar_graficos (auto) TODO: describir parámetro.
simular_arbol <- function(modelo, nuevos_datos, mostrar_graficos = TRUE) {
  if (!requireNamespace("rpart.plot", quietly = TRUE)) stop("Se requiere el paquete rpart.plot")
  if (!requireNamespace("caret", quietly = TRUE)) stop("Se requiere el paquete caret")

  predicciones <- predict(modelo, newdata = nuevos_datos, type = "class")

  # Visualización del árbol
  if (mostrar_graficos) {
    rpart.plot::rpart.plot(modelo, type = 3, extra = 101, under = TRUE, fallen.leaves = TRUE, cex = 0.6, roundint = FALSE)
  }

  # Matriz de confusión si existe la columna 'clase'
  if (!is.null(nuevos_datos$clase)) {
    if (!all(levels(nuevos_datos$clase) %in% levels(modelo$y))) {
      warning("Los niveles de la columna 'clase' no coinciden con los del modelo. La matriz de confusión puede ser inexacta o no mostrarse.")
    } else {
      verdad <- nuevos_datos$clase
      cm <- caret::confusionMatrix(predicciones, verdad)
      if (mostrar_graficos) print(cm)
    }
  }
  return(predicciones)
}
#' @title Simulacion usando Random Forest
#' @description Realiza predicciones a partir de un modelo Random Forest entrenado.
#' @param modelo modelo entrenado (randomForest)
#' @param nuevos_datos dataframe con nuevas observaciones
#' @return lista con predicciones y probabilidades
#' @examples
#' modelo_rf <- ajustar_modelo_arboles(datos = acdfx, predictoras = vPred, clase = "categoria", metodo = "rf")
#' nuevos <- head(acdfx, 3)
#' resultado_rf <- simular_random_forest(modelo_rf$modelo, nuevos)
#' print(resultado_rf$predicciones)
#' print(resultado_rf$probabilidades)
#' @family utilidades
#' @export
#' @param mostrar_graficos (auto) TODO: describir parámetro.
simular_random_forest <- function(modelo, nuevos_datos, mostrar_graficos = TRUE) {
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Se requiere el paquete randomForest")
  if (!requireNamespace("caret", quietly = TRUE)) stop("Se requiere el paquete caret")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Se requiere el paquete ggplot2")

  predicciones <- predict(modelo, newdata = nuevos_datos, type = "response")
  probabilidades <- predict(modelo, newdata = nuevos_datos, type = "prob")

  # Visualización de matriz de confusión
  if (!is.null(nuevos_datos$clase)) {
    if (!all(levels(nuevos_datos$clase) %in% levels(modelo$y))) {
      warning("Los niveles de la columna 'clase' no coinciden con los del modelo. La matriz de confusión puede ser inexacta o no mostrarse.")
    } else {
      verdad <- nuevos_datos$clase
      cm <- caret::confusionMatrix(predicciones, verdad)
      print(cm)
      if (mostrar_graficos) graphics::plot(cm$table, main = "Matriz de Confusión", col = heat.colors(5), margins = c(5,5), margins = c(5,5))
    }
  }

  # Gráfico de importancia de variables
  if (!is.null(modelo$importance)) {
    importancia <- as.data.frame(modelo$importance)
    importancia$Variable <- rownames(importancia)
    graf <- ggplot2::ggplot(importancia, ggplot2::aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Importancia de Variables - Random Forest", x = "Variable", y = "Mean Decrease Gini")
    if (mostrar_graficos) print(graf)
  }

  return(list(
    predicciones = predicciones,
    probabilidades = probabilidades
  ))
}

#' @title Simulacion usando red neuronal
#' @description Utiliza una red neuronal entrenada con nnet para realizar predicciones.
#' @param modelo modelo entrenado (nnet)
#' @param nuevos_datos dataframe con nuevas observaciones
#' @return vector de predicciones
#' @family utilidades
#' @export
simular_red_neuronal <- function(modelo, nuevos_datos) {
  predict(modelo, newdata = nuevos_datos, type = "raw")
}

#' @title Simulacion usando red neuronal unificada con keras
#' @description Genera predicciones de clases y probabilidades a partir de un modelo keras entrenado con ajuste unificado.
#' @param modelo modelo keras entrenado (resultado de \code{ajustar_red_nn_unificada()$modelo})
#' @param nuevos_datos data.frame con variables predictoras en el mismo orden y nombres usados en el entrenamiento
#' @param clase_levels vector de niveles de clase usado en el entrenamiento (resultado de \code{levels()} de la variable de clase)
#' @param columnas_referencia vector opcional de nombres de columnas (predictoras) esperadas, para validacion
#' @return lista con las predicciones de clase y matriz de probabilidades
#' @examples
#' # Supongamos que ya se ajustó un modelo con ajustar_red_nn_unificada
#' rnn <- ajustar_red_nn_unificada(acdfx, predictoras = vPred, clase = "categoria")
#' nuevos <- head(acdfx, 3)
#' resultado <- simular_red_nn_unificada(rnn$modelo, nuevos[, vPred], levels(acdfx$categoria), columnas_referencia = vPred)
#' print(resultado$predicciones)
#' print(resultado$probabilidades)
#' @family utilidades
#' @export
simular_red_nn_unificada <- function(modelo, nuevos_datos, clase_levels, columnas_referencia = NULL) {
  # Validación del tipo de modelo
  if (!inherits(modelo, "keras.engine.training.Model")) {
    stop("El objeto 'modelo' no es un modelo válido de keras.")
  }

  # Validar coincidencia de columnas si se proporciona la referencia
  if (!is.null(columnas_referencia)) {
    if (!all(columnas_referencia %in% colnames(nuevos_datos))) {
      faltan <- setdiff(columnas_referencia, colnames(nuevos_datos))
      stop("Faltan columnas requeridas en los nuevos datos: ", paste(faltan, collapse = ", "))
    }
    # Reordenar columnas para asegurar consistencia
    nuevos_datos <- nuevos_datos[, columnas_referencia]
  }

  # Preparar matriz de entrada
  x_nuevos <- as.matrix(nuevos_datos)

  # Predicción
  pred_probs <- predict(modelo, x_nuevos)
  pred_int <- apply(pred_probs, 1, which.max) - 1
  pred_factor <- factor(clase_levels[pred_int + 1], levels = clase_levels)

  return(list(
    predicciones = pred_factor,
    probabilidades = pred_probs
  ))
}


#' @title Simulacion usando modelo RNN para regresion
#' @description Genera predicciones a partir de un modelo RNN entrenado con datos estructurados como series.
#' @param modelo modelo keras entrenado (resultado de \code{ajustar_rnn_regresion()$modelo})
#' @param nuevos_datos data.frame con las columnas predictoras, en el mismo orden y nombre que durante el entrenamiento
#' @param columnas_referencia vector opcional con los nombres de las columnas usadas como input, para verificación y reordenamiento
#' @return vector numérico con predicciones generadas por el modelo RNN
#' @examples
#' rnn_reg <- ajustar_rnn_regresion(acdfx, columnas_predictoras = vPred, columna_clase = "diferencia")
#' nuevos <- head(acdfx, 3)
#' predicciones <- simular_rnn_regresion(rnn_reg$modelo, nuevos[, vPred], columnas_referencia = vPred)
#' print(predicciones)
#' @family utilidades
#' @export
simular_rnn_regresion <- function(modelo, nuevos_datos, columnas_referencia = NULL) {
  # Validar clase del modelo
  if (!inherits(modelo, "keras.engine.training.Model")) {
    stop("El objeto proporcionado no es un modelo keras válido.")
  }

  # Validar y reordenar columnas si se proporciona referencia
  if (!is.null(columnas_referencia)) {
    if (!all(columnas_referencia %in% colnames(nuevos_datos))) {
      stop("Las siguientes columnas faltan en los nuevos datos: ",
           paste(setdiff(columnas_referencia, colnames(nuevos_datos)), collapse = ", "))
    }
    nuevos_datos <- nuevos_datos[, columnas_referencia]
  }

  # Convertir a array 3D
  x_nuevo <- array(as.matrix(nuevos_datos),
                   dim = c(nrow(nuevos_datos), ncol(nuevos_datos), 1))

  # Realizar predicción
  predicciones <- as.vector(predict(modelo, x_nuevo))

  return(predicciones)
}

#' @title Simulacion usando modelo RNN con Torch
#' @description Genera predicciones a partir de un modelo RNN de regresion entrenado con la libreria torch.
#' @param modelo objeto `nn_module` entrenado (salida de \code{ajustar_rnn_regresion_torch()$modelo})
#' @param nuevos_datos data.frame con las columnas predictoras en el mismo orden y nombre usados en el entrenamiento
#' @param columnas_referencia vector opcional con los nombres de columnas esperadas para validar y reordenar
#' @param valores_reales vector numérico opcional con los valores verdaderos, para comparación gráfica y métricas
#' @param mostrar_grafico lógico. Si es TRUE, se mostrará una gráfica de comparación entre predicciones y reales
#' @return vector numérico con predicciones generadas por el modelo
#' @examples
#' rnn_torch <- ajustar_rnn_regresion_torch(acdfx, columnas_predictoras = vPred, columna_clase = "nrc")
#' nuevos <- head(acdfx, 3)
#' pred <- simular_rnn_regresion_torch(rnn_torch$modelo, nuevos[, vPred], columnas_referencia = vPred,
#'                                     valores_reales = nuevos$nrc, mostrar_grafico = TRUE)
#' @family utilidades
#' @export
simular_rnn_regresion_torch <- function(modelo, nuevos_datos, columnas_referencia = NULL,
                                        valores_reales = NULL, mostrar_grafico = FALSE) {
  if (!inherits(modelo, "nn_module")) {
    stop("El objeto proporcionado no es un modelo válido torch nn_module.")
  }

  if (!is.null(columnas_referencia)) {
    if (!all(columnas_referencia %in% colnames(nuevos_datos))) {
      faltan <- setdiff(columnas_referencia, colnames(nuevos_datos))
      stop("Faltan columnas en los nuevos datos: ", paste(faltan, collapse = ", "))
    }
    nuevos_datos <- nuevos_datos[, columnas_referencia]
  }

  x_nuevo <- torch_tensor(as.matrix(nuevos_datos), dtype = torch_float())$unsqueeze(2)

  modelo$eval()
  with_no_grad({
    pred_tensor <- modelo(x_nuevo)$squeeze()$cpu()
  })
  predicciones <- as.numeric(pred_tensor)

  # Visualización con métricas
  if (mostrar_grafico && !is.null(valores_reales)) {
    if (length(valores_reales) != length(predicciones)) {
      warning("Los valores reales y las predicciones no tienen la misma longitud. No se mostrará gráfico.")
    } else {
      mae <- mean(abs(valores_reales - predicciones))
      rmse <- sqrt(mean((valores_reales - predicciones)^2))
      r2 <- 1 - sum((valores_reales - predicciones)^2) / sum((valores_reales - mean(valores_reales))^2)

      df_comp <- data.frame(Real = valores_reales, Predicho = predicciones)
      label_metrics <- paste0("MAE = ", round(mae, 3),
                              "\nRMSE = ", round(rmse, 3),
                              "\nR² = ", round(r2, 3))

      p <- ggplot(df_comp, aes(x = Real, y = Predicho)) +
        geom_point(color = "darkorange") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
        theme_minimal() +
        labs(title = "Predicción vs Real (Torch)", x = "Valor Real", y = "Predicción") +
        annotate("text", x = min(df_comp$Real), y = max(df_comp$Predicho),
                 label = label_metrics, hjust = 0, vjust = 1, size = 4.2, fontface = "italic")
      print(p)
    }
  }

  return(predicciones)
}


#' @title Simulacion usando regresion multinomial
#' @description Aplica un modelo de regresion multinomial para predecir clases y probabilidades.
#' @param modelo modelo multinomial entrenado (multinom)
#' @param nuevos_datos data.frame con variables predictoras
#' @return lista con predicciones y probabilidades
#' @examples
#' # Supongamos que ya se ha ajustado un modelo con ajustar_regresion_multinomial()
#' modelo_entrenado <- ajustar_regresion_multinomial(datos = acdfx, predictoras = vPred, clase = "categoria")
#' nuevos <- head(acdfx, 3)
#' resultados <- simular_regresion_multinomial(modelo_entrenado$modelo, nuevos)
#' print(resultados$predicciones)
#' print(resultados$probabilidades)
#' @family utilidades
#' @export
simular_regresion_multinomial <- function(modelo, nuevos_datos) {
  # Validar si hay columnas faltantes
  predictores_modelo <- names(coef(modelo))
  faltantes <- setdiff(predictores_modelo, colnames(nuevos_datos))
  if (length(faltantes) > 0) {
    stop("Faltan columnas en los nuevos datos: ", paste(faltantes, collapse = ", "))
  }

  # Predicciones
  predicciones <- predict(modelo, newdata = nuevos_datos, type = "class")
  probabilidades <- predict(modelo, newdata = nuevos_datos, type = "probs")

  return(list(
    predicciones = predicciones,
    probabilidades = probabilidades
  ))
}

#' @title Comparacion de simulaciones de multiples modelos
#' @description Ejecuta multiples funciones de simulacion segun el tipo de modelo y devuelve un data.frame con predicciones.
#' @param lista_modelos lista con nombres y modelos
#' @param nuevos_datos nuevos datos a simular
#' @return data.frame con predicciones por modelo
#' @family utilidades
#' @export
simular_comparacion_todos <- function(lista_modelos, nuevos_datos) {
  resultados <- data.frame()
  for (nombre in names(lista_modelos)) {
    modelo <- lista_modelos[[nombre]]
    if (grepl("regresion", nombre)) {
      pred <- simular_regresion(modelo, nuevos_datos)
    } else if (grepl("pca_svm", nombre)) {
      pred <- simular_pca_svm(modelo, nuevos_datos)
    } else if (grepl("arbol", nombre)) {
      pred <- simular_arbol(modelo, nuevos_datos)
    } else if (grepl("rf", nombre)) {
      pred <- simular_random_forest(modelo, nuevos_datos)$predicciones
    } else if (grepl("red_neuronal", nombre)) {
      pred <- simular_red_neuronal(modelo, nuevos_datos)
    } else if (grepl("rnn", nombre)) {
      pred <- simular_rnn_regresion(modelo, nuevos_datos)
    } else {
      pred <- rep(NA, nrow(nuevos_datos))
    }
    resultados[[nombre]] <- pred
  }
  return(resultados)
}
