# Competencia 03 - Línea de Muerte

## Envio elejido:
  - lineademuerte_ca_df_fe_50_s2_12000.csv

## Requerimientos
* Dependencias: 
  - data.table
  - lightgbm
  - DiceKriging
  - mlrMBO

## Estructura del Script

### 1. Inicialización
- Limpieza de memoria
- Configuración del directorio de trabajo
- Carga de librerías necesarias

### 2. Creación de Clase Ternaria
- Descarga del dataset crudo
- Procesamiento para crear las clases:
  - CONTINUA
  - BAJA+1
  - BAJA+2

### 3. Preprocesamiento
- Corrección de catástrofes usando método MachineLearning
- Feature Engineering Histórico:
  - Lags de orden 1 y 2
  - Deltas de orden 1 y 2
- Corrección de Data Drifting usando método UVA

### 4. Modelado
- Estrategia de entrenamiento con semilla 955841
- Feature Engineering con Random Forest de 50 árboles
- Undersampling de clase CONTINUA al 2%
- Creación de clase binaria (0/1)

### 5. Optimización Bayesiana
- Configuración de parámetros base
- Función de estimación de ganancia
- Optimización de hiperparámetros:
  - num_leaves
  - min_data_in_leaf
  - num_iterations

### 6. Producción
- Entrenamiento del modelo final
- Predicción sobre datos futuros
- Generación de múltiples submissions con diferentes cortes
- Envío automático a Kaggle

## Uso
1. Asegurarse de tener todas las dependencias instaladas
2. Verificar la estructura de directorios necesaria
3. Ejecutar el script completo
4. Los resultados se guardarán en `~/buckets/b1/exp/lineademuerte/`

## Archivos Generados
- Múltiples archivos CSV de submission con diferentes cortes
- Copia del script en la carpeta del experimento
