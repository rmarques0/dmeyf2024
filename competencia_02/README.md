# Competencia 02 - Replicabilidad

Este modelo utiliza LightGBM con las siguientes características principales:

- Catastrophe machine learning para el manejo de eventos catastróficos
- Feature engineering con lags y diferencias de lags de nivel 1 y 2
- Data drifting con método rank_cero_fijo para manejar la deriva temporal
- Análisis de tendencia de 6 meses
- Bayesian Optimization con 50 semillas y 10 iteraciones
- Modelo final con 50 semillas y 1 repetición

El código fuente se encuentra en `990_workflow_orden227_SEMI.r` dentro de la carpeta competencia_02.
Entrega: KA-0002 , KA-0002_01_017_r1_11000.csv

 ```
wf_SEMI_ago_orden227_rm <- function( pnombrewf )
{
  param_local <- exp_wf_init( pnombrewf ) # linea fija

  # Etapa especificacion dataset de la Segunda Competencia Kaggle
  DT_incorporar_dataset( "~/buckets/b1/datasets/competencia_02.csv.gz")

  CA_catastrophe_base( metodo="MachineLearning")
  FEintra_manual_base()
  DR_drifting_base(metodo="rank_cero_fijo")
  FEhist_base()
  ultimo <- FErf_attributes_base()

  ts8 <- TS_strategy_base8()

  # la Bayesian Optimization con el semillerio dentro
  ht <- HT_tuning_semillerio(
    semillerio = 50, # semillerio dentro de la Bayesian Optim
    bo_iteraciones = 10  # iteraciones inteligentes, apenas 10
  )


  fm <- FM_final_models_lightgbm_semillerio( 
    c(ht, ts8), # los inputs
    ranks = c(1), # 1 = el mejor de la bayesian optimization
    semillerio = 50,   # cantidad de semillas finales
    repeticiones_exp = 1  # cantidad de repeticiones del semillerio
  )

  SC_scoring_semillerio( c(fm, ts8) )
  KA_evaluate_kaggle_semillerio()
  

  return( exp_wf_end() ) # linea fija
}
```
