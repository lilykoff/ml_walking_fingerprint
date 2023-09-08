# Walking fingerprinting using machine learning 

This code accompanies a manuscript to identify individuals from their accelerometry-derived walking pattern

## Description

This repo contains code to fit three varieties of "fingerprinting" models on two open-source accelerometry-derived walking datasets. 

## Workflow


All code is in the `R` folder

### Downloading data

* `IU_download_data.R`: download Indiana University walking accelerometry data from Physionet
* `zju_download_data.R`: download Zhejiang University (ZJU) Gait-Acc data

### Getting predictors 
* `IU_zju_get_grid_cells.R`: get "grid cell" predictors 

### Logistic regression models 
* `IU_fit_logistic.R`
* `zju_fit_logistic.R`

### Functional regression models 
* `IU_fit_functional.R`
* `zju_fit_functional.R`

### Machine learning models 
* `IU_fit_ml.R`
* `zju_fit_ml.R`

### Correlation and multiplicity adjusted (CMA) inference
* `IU_zju_CMA.R`

### Results
* `compare_results.R` - after running all models above, use to compare results between methods
* `final_tables_figures.R`: manuscript tables and figures



## Authors

<a href="mailto:lkoffma2@jh.edu"> Lily Koffman </a>
