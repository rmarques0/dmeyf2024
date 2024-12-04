# Linea de Muerte, Tercera Competencia

# Inicializacion
# Establezco Experimento

# limpio la memoria
format(Sys.time(), "%a %b %d %X %Y")
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

dir.create("~/buckets/b1/exp/lineademuerte/", showWarnings = FALSE)
setwd("~/buckets/b1/exp/lineademuerte/")

################ Opcional para creacion clase ternaria ################

# Creacion clase_ternaria
# Creacion de clase_ternaria a partir del dataset crudo
require("data.table")
require("lightgbm") 

download.file(
  "https://storage.googleapis.com/open-courses/dmeyf2024-b725/competencia_03_crudo.csv.gz",
  destfile = "~/buckets/b1/datasets/competencia_03_crudo.csv.gz"
)


# Leer el dataset
dataset <- fread("~/buckets/b1/datasets/competencia_03_crudo.csv.gz" )


# calculo el periodo0 consecutivo
setorder(dataset, numero_de_cliente, foto_mes)
dataset[, periodo0 := as.integer(foto_mes/100)*12 + foto_mes%%100]

# calculo topes
periodo_ultimo <- dataset[, max(periodo0)]
periodo_anteultimo <- periodo_ultimo - 1

# calculo los leads de orden 1 y 2
dataset[, c("periodo1", "periodo2") := shift(periodo0, n=1:2, fill=NA, type="lead"), numero_de_cliente]

# assign most common class values = "CONTINUA"
dataset[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]

# calculo BAJA+1
dataset[periodo0 < periodo_ultimo & (is.na(periodo1) | periodo0 + 1 < periodo1), clase_ternaria := "BAJA+1"]

# calculo BAJA+2
dataset[periodo0 < periodo_anteultimo & (periodo0+1 == periodo1) & (is.na(periodo2) | periodo0 + 2 < periodo2), clase_ternaria := "BAJA+2"]

dataset[, c("periodo0", "periodo1", "periodo2") := NULL]


# # Guardar el dataset con clase_ternaria
# fwrite(dataset, "~/buckets/b1/datasets/competencia_03.csv.gz", compress="gzip")

################################################################################


# Carga dataset
dataset <- fread("~/buckets/b1/datasets/competencia_03.csv.gz")

# Verification of clase_ternaria
tbl <- dataset[, .N, list(foto_mes, clase_ternaria)]
setorder(tbl, foto_mes, clase_ternaria)
print(tbl)

################ Preprocesamiento ################

# Cargar función de corrección
source("~/dmeyf2024/competencia_03/1201_CA_reparar_dataset.r")

# Aplicar la corrección de catástrofes usando el método MachineLearning
Corregir_Rotas(dataset, "MachineLearning")

# Feature Engineering Historico
cols_lagueables <- copy(setdiff(colnames(dataset), c("numero_de_cliente", "foto_mes", "clase_ternaria")))

dataset[, paste0(cols_lagueables, "_lag1") := shift(.SD, 1, NA, "lag"), by = numero_de_cliente, .SDcols = cols_lagueables]
dataset[, paste0(cols_lagueables, "_lag2") := shift(.SD, 2, NA, "lag"), by = numero_de_cliente, .SDcols = cols_lagueables]

# agrego los delta lags de orden 1
for (vcol in cols_lagueables) {
  dataset[, paste0(vcol, "_delta1") := get(vcol) - get(paste0(vcol, "_lag1"))]
  dataset[, paste0(vcol, "_delta2") := get(vcol) - get(paste0(vcol, "_lag2"))]
}

# Verificacion de los campos recien creados
print(ncol(dataset))
print(colnames(dataset))

# Data Drifting
source("~/dmeyf2024/competencia_03/1401_DR_corregir_drifting.r")
dataset <- Corregir_Drift(dataset, metodo="UVA")


# fwrite(dataset, "~/buckets/b1/datasets/competencia_03_DR_corregir_drifting.csv.gz", compress="gzip")
# dataset <- fread("~/buckets/b1/datasets/competencia_03_DR_corregir_drifting.csv.gz")

################################################################################


################ Modelado ################
# Training Strategy
GLOBAL_semilla <- 724939
campos_buenos <- copy(setdiff(colnames(dataset), c("clase_ternaria")))
set.seed(GLOBAL_semilla, kind = "L'Ecuyer-CMRG")
dataset[, azar := runif(nrow(dataset))]


# Definir todos los meses de entrenamiento, excluyendo meses con variables rotas
meses_training <- c(
  202107, 202106, 202105, 202104, 202103, 202102, 202101, 
  202012, 202011, 202010, 202009, 202008, 202007, 
  # 202006  Excluyo por variables rotas
  202005, 
  #202004, 202003, Excluyo por variables rotas
  202002, 202001, 201912, 201911,
  # 201910 Excluyo por variables rotas
  201909, 201908, 201907, 201906,
  # 201905  Excluyo por variables rotas
  201904, 201903
)

# Feature Engineering con Random Forest
source("~/dmeyf2024/competencia_03/1311_FE_rfatributes.r")
dataset <- AgregaVarRandomForest(dataset, 
                                 campos_buenos, 
                                 meses_training,
                                 num_trees = 50,
                                 feature_fraction = 0.5,
                                 min_features = 50,
                                 importance_cutoff = 0.001)

# undersampling de los CONTINUA al 2%
dataset[, fold_train := foto_mes <= 202107 &
          (clase_ternaria %in% c("BAJA+1", "BAJA+2") |
             azar < 0.02 )
]

# Crear clase binaria
# positivos = [ BAJA+1, BAJA+2 ]  ->  1
# negativos = [ CONTINUA ]        ->  0
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+1", "BAJA+2"), 1, 0)]

# file.remove("~/buckets/b1/datasets/competencia_03_DR_corregir_drifting.csv.gz")
# fwrite(dataset, "~/buckets/b1/datasets/competencia_03_final.csv.gz", compress="gzip")

dataset <- fread("~/buckets/b1/datasets/competencia_03_final.csv.gz")

ncol(dataset)

dtrain <- lgb.Dataset(
  data = data.matrix(dataset[fold_train == TRUE, campos_buenos, with = FALSE]),
  label = dataset[fold_train == TRUE, clase01],
  weight = dataset[fold_train == TRUE, ifelse(foto_mes<=202106, 1.0, 0.0)],
  free_raw_data = TRUE
)

dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[foto_mes == 202107, campos_buenos, with = FALSE]),
  label = dataset[foto_mes == 202107, clase01],
  free_raw_data = TRUE
)

dfuture <- dataset[foto_mes == 202109, ]

rm(dataset)
gc(full = TRUE, verbose= FALSE) # recolección de basura

# verificación de conjuntos de datos de entrenamiento
print(nrow(dtrain))
print(nrow(dvalidate))
print(nrow(dfuture))



################ Hyperparameter Tuning ################
# Clase binaria que se optimiza :  positivos = [ BAJA+1, BAJA+2 ]
# Metrica que se optimiza **Ganancia**

# GANANCIA
# Defino la función de evaluación personalizada para la ganancia
ganancia_eval <- function(preds, dtrain) {
  labels <- get_field(dtrain, "label")

  # calculo la ganancia con las constantes definidas
  gan1 <- 273000  # ganancia por acierto
  gan0 <- -7000   # perdida por error

  pred_binaria <- as.numeric(preds > 0.5)
  ganancia <- sum(pred_binaria * labels * gan1) + sum(pred_binaria * (1-labels) * gan0)

  return(list(name = "ganancia",
              value = ganancia,
              higher_better = TRUE))
}

# Parámetros base
param_base <- list(
  boosting = "gbdt",
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE,
  verbosity = -100,
  max_depth = -1L,
  min_gain_to_split = 0.0,
  min_sum_hessian_in_leaf = 0.001,
  lambda_l1 = 0.0,
  lambda_l2 = 0.0,
  max_bin = 31L,
  bagging_fraction = 1.0,
  pos_bagging_fraction = 1.0,
  neg_bagging_fraction = 1.0,
  is_unbalance = FALSE,
  scale_pos_weight = 1.0,
  learning_rate = 0.03,
  feature_fraction = 0.5,
  num_iterations = 9999,
  early_stopping_rounds = 200
)


# Función de estimación que sigue el patrón del workflow
EstimarGanancia_lightgbm <- function(x) {
  message(format(Sys.time(), "%a %b %d %X %Y"))

  # Combinar parámetros base con los de la optimización bayesiana
  param_completo <- c(param_base, x)

  # Entrenamiento del modelo
  modelo_train <- lgb.train(
    data = dtrain,
    valids = list(valid = dvalidate),
    eval = ganancia_eval,
    param = param_completo,
    verbose = -100
  )

  # Obtener la ganancia del mejor modelo
  ganancia <- modelo_train$record_evals$valid$ganancia$eval[[modelo_train$best_iter]]

  # Guardar el número de iteraciones como atributo extra
  attr(ganancia, "extras") <- list("num_iterations" = modelo_train$best_iter)

  # Limpieza
  rm(modelo_train)
  gc(full = TRUE, verbose = FALSE)

  return(ganancia)
}


# Complejo seteo de la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

configureMlr(show.learner.output = FALSE)

# Configuración de la optimización bayesiana
obj.fun <- makeSingleObjectiveFunction(
  fn = EstimarGanancia_lightgbm,
  minimize = FALSE,
  noisy = FALSE,
  par.set = makeParamSet(
    makeIntegerParam("num_leaves", lower = 8L, upper = 1024L),
    makeIntegerParam("min_data_in_leaf", lower = 64L, upper = 8192L)
  ),
  has.simple.signature = FALSE
)

# cada 600 segundos guardo el resultado intermedio
ctrl <- makeMBOControl(
  save.on.disk.at.time = 600,
  save.file.path = "lineademuerte_.RDATA"
)

# indico la cantidad de iteraciones que va a tener la Bayesian Optimization
ctrl <- setMBOControlTermination(
  ctrl,
  iters = 10  # cantidad de iteraciones inteligentes
)

# defino el método estandar para la creacion de los puntos iniciales
# los "No Inteligentes"
ctrl <- setMBOControlInfill(ctrl, crit = makeMBOInfillCritEI())

# mas configuraciones
surr.km <- makeLearner(
  "regr.km",
  predict.type = "se",
  covtype = "matern3_2",
  control = list(trace = TRUE)
)

# Corrida de la Bayesian Optimization,  aqui se hace el trabajo pesado
bayesiana_salida <- mbo(obj.fun, learner = surr.km, control = ctrl)

# obtengo los mejores hiperparametros
tb_bayesiana <- as.data.table(bayesiana_salida$opt.path)
setorder(tb_bayesiana, -y, -num_iterations) # ordeno en forma descendente por AUC = y
mejores_hiperparametros <- tb_bayesiana[1, # el primero es el de mejor AUC
                                        list(num_leaves, min_data_in_leaf, num_iterations)]

print(mejores_hiperparametros)

################ Produccion ################ 
# Final Model
# Asignar pesos al conjunto de entrenamiento
set_field(dtrain, "weight", rep(1.0, nrow(dtrain)))

# Ajustar parámetros finales usando el número de iteraciones de mejores_hiperparametros
param_final <- c(param_base, list(
  num_leaves = mejores_hiperparametros$num_leaves,
  min_data_in_leaf = mejores_hiperparametros$min_data_in_leaf,
  num_iterations = mejores_hiperparametros$num_iterations
))

# Remover early_stopping_rounds ya que no tenemos conjunto de validación
param_final$early_stopping_rounds <- NULL

# Genero el modelo final
final_model <- lgb.train(
  data = dtrain,
  param = param_final,
  verbose = -100
)

# Function to submit to Kaggle
submit_to_kaggle <- function(file_path, competition_name, message) {
  command <- sprintf("kaggle competitions submit -c %s -f %s -m \"%s\"", 
                     competition_name, file_path, message)
  system(command)
}

# Scoring and Kaggle Submission
# Aplico el modelo final a los datos del futuro
prediccion <- predict(
  final_model,
  data.matrix(dfuture[, campos_buenos, with = FALSE])
)

# Genero la salida para Kaggle
tb_entrega <- dfuture[, list(numero_de_cliente)]
tb_entrega[, prob := prediccion]

# Pruebo distintos cortes para el scoring
cortes <- seq(10500, 12050, 500)
mejores_submissions <- list()

for(corte in cortes) {
  # ordeno por probabilidad descendente
  setorder(tb_entrega, -prob)
  
  # genero la salida
  tb_submission <- copy(tb_entrega)
  tb_submission[, prob := NULL]
  tb_submission[, Predicted := 0L]
  tb_submission[1:corte, Predicted := 1L]
  
  # guardo cada versión
  file_name <- sprintf("lineademuerte_%d.csv", corte)
  fwrite(tb_submission, file = file_name)
  
  # Submit to Kaggle
  submit_to_kaggle(file_name, "dm-ey-f-2024-tercera", sprintf("Submision con corte en %d", corte))
  
  mejores_submissions[[as.character(corte)]] <- tb_submission
}

format(Sys.time(), "%a %b %d %X %Y")

# Save a copy of this script
script_content <- readLines("~/dmeyf2024/src/lineademuerte/lineademuerte_k3.R")
writeLines(script_content, "~/buckets/b1/exp/lineademuerte/competencia_03.r")
