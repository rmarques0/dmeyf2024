# Competencia 02 - Replicabilidad

Este modelo utiliza LightGBM con las siguientes características principales:

- Catastrophe machine learning para el manejo de eventos catastróficos
- Feature engineering con lags y diferencias de lags de nivel 1 y 2
- Data drifting con método rank_cero_fijo para manejar la deriva temporal
- Análisis de tendencia de 6 meses
- Bayesian Optimization con 50 semillas y 10 iteraciones
- Modelo final con 50 semillas y 1 repetición

El código fuente se encuentra en `990_workflow_orden227_SEMI.r` dentro de la carpeta competencia_02.
