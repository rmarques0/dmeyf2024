library(data.table)

# Crear directorio datasets si no existe
dir.create("./datasets", showWarnings = FALSE)

# Descargar el dataset
# download.file(
#   "https://storage.googleapis.com/open-courses/dmeyf2024-b725/competencia_02_crudo.csv.gz",
#   destfile = "./datasets/competencia_02_crudo.csv.gz"
# )


# Leer el dataset
dt <- fread("./datasets/competencia_02_crudo.csv.gz")

# Crear dataset simple con las columnas necesarias y agregar posición
dsimple <- dt[, .(
    pos = .I,
    numero_de_cliente,
    periodo0 = as.integer(foto_mes / 100) * 12 + foto_mes %% 100
)]

# Ordenar por cliente y período
setorder(dsimple, numero_de_cliente, periodo0)

# Calcular último período y anteúltimo período
periodo_ultimo <- dsimple[, max(periodo0)]
periodo_anteultimo <- periodo_ultimo - 1

# Calcular los períodos siguientes (lead) de orden 1 y 2
dsimple[, c("periodo1", "periodo2") := 
    shift(periodo0, n = 1:2, fill = NA, type = "lead"), 
    by = numero_de_cliente]

# Clase por defecto es "CONTINUA"
dsimple[periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA"]

# Calcular BAJA+1: el cliente desaparece en el próximo período
dsimple[periodo0 < periodo_ultimo & 
        (is.na(periodo1) | periodo0 + 1 < periodo1),
        clase_ternaria := "BAJA+1"]

# Calcular BAJA+2: el cliente aparece el próximo mes pero desaparece en dos meses
dsimple[periodo0 < periodo_anteultimo & (periodo0 + 1 == periodo1) & 
        (is.na(periodo2) | periodo0 + 2 < periodo2),
        clase_ternaria := "BAJA+2"]

# Nueva lógica para manejar clientes que regresan
dsimple[is.na(clase_ternaria) & periodo0 > periodo_ultimo - 3, 
        clase_ternaria := "CONTINUA"]  # Asignar CONTINUA si hay actividad reciente

# Merge de los resultados al dataset original
setorder(dsimple, pos)
dt[, clase_ternaria := dsimple$clase_ternaria]

# Análisis de la distribución de clases
print("\nDistribución de clases:")
print(dt[, .N, by = clase_ternaria][order(-N)])

# Identificar último período
ultimo_periodo <- dt[, max(foto_mes)]
print(paste("\nÚltimo período:", ultimo_periodo))

# Analizar NAs inesperados
print("\nNAs en clase_ternaria que NO son del último período:")
unexpected_nas <- dt[is.na(clase_ternaria) & foto_mes != ultimo_periodo]

if (nrow(unexpected_nas) > 0) {
    print("\nDistribución de NAs inesperados por foto_mes:")
    print(unexpected_nas[, .N, by = foto_mes][order(foto_mes)])
    
    print("\nPrimeros 5 ejemplos de NAs inesperados:")
    print(unexpected_nas[1:5, .(foto_mes, numero_de_cliente)])
    
    clientes_ejemplo <- unexpected_nas[1:5, numero_de_cliente]
    print("\nHistoria de estos clientes:")
    print(dt[numero_de_cliente %in% clientes_ejemplo, 
            .(numero_de_cliente, foto_mes, clase_ternaria)][order(numero_de_cliente, foto_mes)])
} else {
    print("No hay NAs inesperados (todos los NAs están en el último período)")
}

# Distribución por foto_mes
print("\nDistribución de clases por foto_mes:")
print(dt[, .(
    Registros = .N,
    BAJA1 = sum(clase_ternaria == "BAJA+1", na.rm = TRUE),
    BAJA2 = sum(clase_ternaria == "BAJA+2", na.rm = TRUE),
    CONTINUA = sum(clase_ternaria == "CONTINUA", na.rm = TRUE),
    Nulos = sum(is.na(clase_ternaria))
), by = foto_mes][order(foto_mes)])

# # Exportar el dataset procesado
# fwrite(dt, "./datasets/competencia_02.csv.gz")
