# Feature Engineering usando Random Forest
# Basado en z1311_FE_rfatributes.r

require("lightgbm")
require("data.table") 

AgregaVarRandomForest <- function(dataset, campos_buenos, meses_training, 
                                  num_trees = 100,        # increased number of trees
                                  feature_fraction = 0.5, 
                                  min_features = 50,      # minimum features to keep
                                  importance_cutoff = 0.001) { # importance threshold
  
  # Crear clase binaria temporal para el modelo
  dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1L, 0L)]
  
  # Crear dataset de entrenamiento
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[foto_mes %in% meses_training, campos_buenos, with = FALSE]),
    label = dataset[foto_mes %in% meses_training, clase01],
    free_raw_data = FALSE
  )
  
  # Parámetros para el modelo
  param <- list(
    objective = "binary",
    max_bin = 31,
    learning_rate = 0.1,
    num_iterations = num_trees,
    feature_fraction = feature_fraction,
    min_data_in_leaf = 100,
    verbose = -100
  )
  
  # Entrenar el modelo inicial para selección de features
  modelo_selection <- lgb.train(
    params = param,
    data = dtrain,
    verbose = -100
  )
  
  # Feature Selection based on importance
  importance <- lgb.importance(modelo_selection)
  selected_features <- importance[Gain >= importance_cutoff, Feature]
  
  # Ensure we keep at least min_features
  if(length(selected_features) < min_features) {
    selected_features <- importance[order(-Gain)][1:min_features, Feature]
  }
  
  cat("Selected", length(selected_features), "features out of", length(campos_buenos), "\n")
  
  # Retrain model with selected features
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[foto_mes %in% meses_training, selected_features, with = FALSE]),
    label = dataset[foto_mes %in% meses_training, clase01],
    free_raw_data = FALSE
  )
  
  modelo <- lgb.train(
    params = param,
    data = dtrain,
    verbose = -100
  )
  
  # Para cada período
  periodos <- dataset[, unique(foto_mes)]
  
  for(periodo in periodos) {
    cat("Procesando período:", periodo, "\n")
    
    # Predecir las hojas para este período
    prediccion <- predict(
      modelo,
      data.matrix(dataset[foto_mes == periodo, selected_features, with = FALSE]),
      # predleaf = TRUE
      type = 'leaf' 
    )
    
    # Para cada árbol
    for(arbolito in 1:param$num_iterations) {
      # Obtener hojas únicas para este árbol
      hojas_arbol <- unique(prediccion[, arbolito])
      
      # Crear variables binarias para cada hoja
      for(nodo_id in hojas_arbol) {
        dataset[foto_mes == periodo,
                paste0("rf_", sprintf("%03d", arbolito), "_", sprintf("%03d", nodo_id)) := 
                  as.integer(nodo_id == prediccion[, arbolito])]
      }
    }
    
    # Clean up
    gc(verbose = FALSE)
  }
  
  # Eliminar la clase temporal
  dataset[, clase01 := NULL]
  
  return(dataset)
}