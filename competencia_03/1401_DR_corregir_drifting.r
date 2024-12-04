require("data.table")

# Valores de índices financieros para cada mes
vfoto_mes <- c(
  201901, 201902, 201903, 201904, 201905, 201906,
  201907, 201908, 201909, 201910, 201911, 201912,
  202001, 202002, 202003, 202004, 202005, 202006,
  202007, 202008, 202009, 202010, 202011, 202012,
  202101, 202102, 202103, 202104, 202105, 202106,
  202107, 202108, 202109
)

# Valores UVA correspondientes a cada mes
vUVA <- c(
  2.001408838932958,  1.950325472789153,  1.89323032351521,
  1.8247220405493787, 1.746027787673673,  1.6871348409529485,
  1.6361678865622313, 1.5927529755859773, 1.5549162794128493,
  1.4949100586391746, 1.4197729500774545, 1.3678188186372326,
  1.3136508617223726, 1.2690535173062818, 1.2381595983200178,
  1.211656735577568,  1.1770808941405335, 1.1570338657445522,
  1.1388769475653255, 1.1156993751209352, 1.093638313080772,
  1.0657171590878205, 1.0362173587708712, 1.0,
  0.9669867858358365, 0.9323750098728378, 0.8958202912590305,
  0.8631993702994263, 0.8253893405524657, 0.7928918905364516,
  0.7666323845128089, 0.7428976357662823, 0.721615762047849
)

#------------------------------------------------------------------------------
# Función para aplicar la corrección por UVA
drift_UVA <- function(dataset, campos_monetarios) {
  # Crear tabla de índices UVA
  tb_indices <- data.table(UVA = vUVA, foto_mes = as.character(vfoto_mes))
  
  # Asegurar que foto_mes en el dataset sea character
  dataset[, foto_mes := as.character(foto_mes)]
  
  # Asegurar que los campos monetarios sean numéricos
  dataset[, (campos_monetarios) := lapply(.SD, as.numeric), .SDcols = campos_monetarios]
  
  # Aplicar la corrección multiplicando por el índice UVA
  dataset <- merge(dataset, tb_indices, by = "foto_mes", all.x = TRUE)
  dataset[, (campos_monetarios) := lapply(.SD, function(x) x * UVA), .SDcols = campos_monetarios]
  dataset[, UVA := NULL]  # Eliminar columna temporal de UVA
  
  return(dataset)
}

#------------------------------------------------------------------------------
# Función principal para corregir el data drifting
Corregir_Drift <- function(dataset, metodo = "UVA") {
  # Convertir el dataset a data.table si no lo es
  setDT(dataset)
  
  # Identificar campos monetarios (que empiezan con m, Visa_m, Master_m o vm_m)
  campos_monetarios <- names(dataset)[
    grepl("^(m|Visa_m|Master_m|vm_m)", names(dataset))
  ]
  
  # Aplicar corrección UVA
  dataset <- drift_UVA(dataset, campos_monetarios)
  
  return(dataset)
}