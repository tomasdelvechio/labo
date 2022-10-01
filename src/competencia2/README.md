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
* 
* SHA: 23b253c0092c0f83557803409af8da0a209e22ff
