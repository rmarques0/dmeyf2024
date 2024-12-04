# Funciones para la corrección de datos problemáticos
# Escrito por: Universidad Austral Rosario

#------------------------------------------------------------------------------
# Función para asignar NA a los valores problemáticos
AsignarNA_campomeses <- function(pcampo, pmeses) {
  if (pcampo %in% colnames(dataset)) {
    dataset[foto_mes %in% pmeses, paste0(pcampo) := NA]
  }
}

#------------------------------------------------------------------------------
# Función para interpolar valores
Corregir_interpolar <- function(pcampo, pmeses) {
  tbl <- dataset[, list(
    "v1" = shift(get(pcampo), 1, type = "lag"),
    "v2" = shift(get(pcampo), 1, type = "lead")
  ),
  by = numero_de_cliente]
  
  tbl[, numero_de_cliente := NULL]
  tbl[, promedio := rowMeans(tbl, na.rm = TRUE)]
  
  dataset[, paste0(pcampo) := ifelse(!(foto_mes %in% pmeses),
                                     get(pcampo),
                                     tbl$promedio
  )]
}

#------------------------------------------------------------------------------
# Función para usar MICE
Corregir_MICE <- function(pcampo, pmeses) {
  meth <- rep("", ncol(dataset))
  names(meth) <- colnames(dataset)
  meth[names(meth) == pcampo] <- "sample"
  
  imputacion <- mice(dataset,
                     method = meth,
                     maxit = 5,
                     m = 1,
                     seed = 7)
  
  tbl <- mice::complete(imputacion)
  dataset[, paste0(pcampo) := ifelse(foto_mes %in% pmeses, tbl[, get(pcampo)], get(pcampo))]
}

#------------------------------------------------------------------------------
# Función principal para corregir datos problemáticos
Corregir_Rotas <- function(dataset, pmetodo) {
  # Función para corregir un atributo según el método elegido
  Corregir_atributo <- function(pcampo, pmeses) {
    if (pmetodo == "MachineLearning") {
      AsignarNA_campomeses(pcampo, pmeses)
    } else if (pmetodo == "EstadisticaClasica") {
      Corregir_interpolar(pcampo, pmeses)
    } else if (pmetodo == "MICE") {
      Corregir_MICE(pcampo, pmeses)
    }
  }
  
  # Lista de campos y meses a corregir
  Corregir_atributo("active_quarter", c(202006))
  Corregir_atributo("internet", c(202006))
  Corregir_atributo("mrentabilidad", c(201905, 201910, 202006))
  Corregir_atributo("mrentabilidad_annual", c(201905, 201910, 202006))
  Corregir_atributo("mcomisiones", c(201905, 201910, 202006))
  Corregir_atributo("mactivos_margen", c(201905, 201910, 202006))
  Corregir_atributo("mpasivos_margen", c(201905, 201910, 202006))
  Corregir_atributo("mcuentas_saldo", c(202006))
  Corregir_atributo("ctarjeta_debito_transacciones", c(202006))
  Corregir_atributo("mautoservicio", c(202006))
  Corregir_atributo("ctarjeta_visa_transacciones", c(202006))
  Corregir_atributo("mtarjeta_visa_consumo", c(202006))
  Corregir_atributo("ctarjeta_master_transacciones", c(202006))
  Corregir_atributo("mtarjeta_master_consumo", c(202006))
  Corregir_atributo("ctarjeta_visa_debitos_automaticos", c(201904))
  Corregir_atributo("mttarjeta_visa_debitos_automaticos", c(201904))
  Corregir_atributo("ccajeros_propios_descuentos", c(201910, 202002, 202006, 202009, 202010, 202102))
  Corregir_atributo("mcajeros_propios_descuentos", c(201910, 202002, 202006, 202009, 202010, 202102))
  Corregir_atributo("ctarjeta_visa_descuentos", c(201910, 202002, 202006, 202009, 202010, 202102))
  Corregir_atributo("mtarjeta_visa_descuentos", c(201910, 202002, 202006, 202009, 202010, 202102))
  Corregir_atributo("ctarjeta_master_descuentos", c(201910, 202002, 202006, 202009, 202010, 202102))
  Corregir_atributo("mtarjeta_master_descuentos", c(201910, 202002, 202006, 202009, 202010, 202102))
  Corregir_atributo("ccomisiones_otras", c(201905, 201910, 202006))
  Corregir_atributo("mcomisiones_otras", c(201905, 201910, 202006))
  Corregir_atributo("cextraccion_autoservicio", c(202006))
  Corregir_atributo("mextraccion_autoservicio", c(202006))
  Corregir_atributo("ccheques_depositados", c(202006))
  Corregir_atributo("mcheques_depositados", c(202006))
  Corregir_atributo("ccheques_emitidos", c(202006))
  Corregir_atributo("mcheques_emitidos", c(202006))
  Corregir_atributo("ccheques_depositados_rechazados", c(202006))
  Corregir_atributo("mcheques_depositados_rechazados", c(202006))
  Corregir_atributo("ccheques_emitidos_rechazados", c(202006))
  Corregir_atributo("mcheques_emitidos_rechazados", c(202006))
  Corregir_atributo("tcallcenter", c(202006))
  Corregir_atributo("ccallcenter_transacciones", c(202006))
  Corregir_atributo("thomebanking", c(202006))
  Corregir_atributo("chomebanking_transacciones", c(201910, 202006))
  Corregir_atributo("ccajas_transacciones", c(202006))
  Corregir_atributo("ccajas_consultas", c(202006))
  Corregir_atributo("ccajas_depositos", c(202006, 202105))
  Corregir_atributo("ccajas_extracciones", c(202006))
  Corregir_atributo("ccajas_otras", c(202006))
  Corregir_atributo("catm_trx", c(202006))
  Corregir_atributo("matm", c(202006))
  Corregir_atributo("catm_trx_other", c(202006))
  Corregir_atributo("matm_other", c(202006))
}