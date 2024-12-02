# limpio la memoria
format(Sys.time(), "%a %b %d %X %Y")
rm(list = ls(all.names = TRUE)) # remove all objects
gc(full = TRUE, verbose= FALSE) # garbage collection

dir.create("~/buckets/b1/exp/lineademuerte/", showWarnings = FALSE)
setwd( "~/buckets/b1/exp/lineademuerte/" )


require( "data.table" )

# leo el dataset
# dataset <- fread("~/buckets/b1/datasets/competencia_03_crudo.csv.gz" )
dataset <- fread("./datasets/competencia_03.csv.gz")
# calculo el periodo0 consecutivo
setorder( dataset, numero_de_cliente, foto_mes )
dataset[, periodo0 := as.integer(foto_mes/100)*12 +  foto_mes%%100]

# calculo topes
periodo_ultimo <- dataset[, max(periodo0) ]
periodo_anteultimo <- periodo_ultimo - 1

# calculo los leads de orden 1 y 2
dataset[, c("periodo1", "periodo2") :=
    shift(periodo0, n=1:2, fill=NA, type="lead"),  numero_de_cliente ]

# assign most common class values = "CONTINUA"
dataset[ periodo0 < periodo_anteultimo, clase_ternaria := "CONTINUA" ]

# calculo BAJA+1
dataset[ periodo0 < periodo_ultimo &
    ( is.na(periodo1) | periodo0 + 1 < periodo1 ),
    clase_ternaria := "BAJA+1" ]

# calculo BAJA+2
dataset[ periodo0 < periodo_anteultimo & (periodo0+1 == periodo1 )
    & ( is.na(periodo2) | periodo0 + 2 < periodo2 ),
    clase_ternaria := "BAJA+2" ]

dataset[, c("periodo0", "periodo1", "periodo2") := NULL ]

tbl <- dataset[, .N, list(foto_mes, clase_ternaria)]
setorder(tbl, foto_mes, clase_ternaria)
tbl

# salvar dataset
fwrite(dataset, "./datasets/competencia_03.csv.gz")