# Competencia 2

Lista de experimentos

## Id Exp 1

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script FE: `711_FE_basico.r`
* OB: `src/lightgbm/723_lightgbm_binaria_BO.r`
* Parametros: "20220924 195144"
* Predictor: `src/lightgbm/724_lightgbm_final.r`
* Salidas: KA7240_[5000:12000].csv (Son 15 archivos que suben de a 500)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=850107609
* SHA: 82efc6e0d0cf8736b4e581c658cc1aeb68127bd0

Problema de este experimento: La OB se hizo sobre el dataset base, no con el dataset usado para probar.

## Id Exp 2

Este experimento toma el FE basico de los scripts de Gustavo, y a partir de ahi hace de nuevo una OB

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script FE: `711_FE_basico.r`
* OB: `src/lightgbm/723_lightgbm_binaria_BO.r`
* Parametros: "20220925 141139"
* Predictor: `src/lightgbm/724_lightgbm_final.r`
* Salidas: KA7240_[5000:12000].csv (Son 15 archivos que suben de a 500)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=948240695
* SHA: a38d5140240fe6ecc732f7191d15c9b074bc09a7

## Id Exp 3

Este experimento toma el FE basico de los scripts de Gustavo, le agrega mis propias features, y a partir de ahi hace de nuevo una OB

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script FE: `711_FE_basico_mas_propias.r`
* OB: `src/lightgbm/723_lightgbm_binaria_BO.r`
* Parametros: "20220925 163456"
* Predictor: `src/lightgbm/724_lightgbm_final.r`
* Salidas: KA7240_[5000:15000].csv (Son 15 archivos que suben de a 500)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=1823664382      
* SHA: 23b253c0092c0f83557803409af8da0a209e22ff

## Id Exp 4

Agregado de Hiperparametros al OB: principalmente lambda_l1 y lambda_l2. Agregando mas HP a explorar y cambiando el rango de los explorados anteriormente. Ademas realiza mas pruebas.

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script FE: `711_FE_basico_mas_propias.r`
* OB: `src/lightgbm/723_lightgbm_binaria_BO.r`
* Parametros: "20220930 162134"
* Predictor: `src/lightgbm/724_lightgbm_final.r`
* Salidas: KA7240_[6000:10000].csv (Son 15 archivos que suben de a 250)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=1905231634
* SHA: ec8450624d6ef988e13d743e84ab4603521b37fc

## Id Exp 5

Se eliminan del dataset a predecir las variables que se considera "a ojo" que tienen data drifting. Se deja la OB de ID Exp 4.

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script para visualizar Data Drifting: `src/drifting/z613_graficar_densidades_mayo.r`
* Script FE: `711_FE_basico_mas_propias.r`
* OB: `src/lightgbm/723_lightgbm_binaria_BO.r`
* Parametros: "20220930 162134"
* Predictor: `src/lightgbm/724_lightgbm_final.r`
* Salidas: KA7240_[6000:10000].csv (Son 15 archivos que suben de a 250)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=10659219
* SHA: aaf9420f3a7cde3e6139f35016c942aed6956625

## Id Exp 6

En lugar de eliminar las variables, se aplica frank a la columna que antes se eliminaba.

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script para visualizar Data Drifting: `src/drifting/z613_graficar_densidades_mayo.r`
* Script FE: `711_FE_basico_mas_propias.r`
* OB: `src/lightgbm/723_lightgbm_binaria_BO.r`
* Parametros: "20220930 162134"
* Predictor: `src/lightgbm/724_lightgbm_final.r`
* Salidas: KA7240_[6000:12000].csv (Son 15 archivos que suben de a 100)
* NO
* SHA: e00cc96072d845b1f542a085930a7c0da3534d68

TRUNCO: No se subio nada

## Id Exp 7

Se volvió al modelo original que mejor puntaje dio en el público, y se comenzó a jugar ahí con la ganancia pública, seleccionando el peor de la meseta.

* Dataset utilizado: `./datasets/competencia2_2022.csv.gz`
* FE: NO
* Script para visualizar Data Drifting: Ninguno
* Script FE: NO
* OB: NO
* Parametros: NO
* Predictor: `src/lightgbm/724_exp_7.r`
  * Salidas: KA7240_[6000:12000].csv (Son 15 archivos que suben de a 50)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=678908947
* SHA: 7bbaab4e8847ab3adc91e27a4212fdfd23a14c2b

## Id Exp 8

Prueba con el dataset + FE que elimina Data Drifting

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script para visualizar Data Drifting: Ninguno
* Script FE: `src/FeatureEngineering/711_FE_basico_mas_propias_eliminacion_data_drifting.r`
* OB: NO
* Parametros: NO
* Predictor: `src/lightgbm/724_exp_7.r`
  * Salidas: KA7240_[6000:12000].csv (Son 15 archivos que suben de a 50)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=231939486
* SHA: ab8dd127443371ccc7c9b92208667ed981f7bf2e

## Id Exp 9

Prueba con imputación de -1 en lugar de 0

* Dataset utilizado: `dataset_7110.csv.gz`
* FE: Si
* Script para visualizar Data Drifting: Ninguno
* Script FE: `src/FeatureEngineering/711_FE_basico_mas_propias_eliminacion_data_drifting.r`
* OB: Si
* Parametros: "20221002 184524"
* Predictor: `src/lightgbm/724_exp_7.r`
  * Salidas: KA7240_[6000:12000].csv (Son 15 archivos que suben de a 50)
* https://docs.google.com/spreadsheets/d/1cozQ3RZF0fhHUgbqlxKRg6O2j0jxN5PVA9sZNwtUdks/edit#gid=1641261218
* SHA: 0d02d7388b18994a5ad90f2c26ced244c73c4f90



