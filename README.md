# Walking fingerprinting using machine learning 

This code accompanies a manuscript to identify individuals from their accelerometry-derived walking pattern

## Description

This repo contains code to fit three varieties of "fingerprinting" models on two open-source accelerometry-derived walking datasets. 

## Workflow

All code is in the `R` folder. Below are the specific files for each step of the data process and the workflow consists of running each of the files in the order below. All of the code in order is also `all_code.R`; this file can be run instead of running each of the separate files below. 

### Downloading data

* `step_01_IU_download_data.R`: download Indiana University walking accelerometry data from Physionet
* `step_01_zju_download_data.R`: download Zhejiang University (ZJU) Gait-Acc data

### Getting predictors 
* `step_02_IU_zju_get_grid_cells.R`: get "grid cell" predictors for lags 15, 30, 45 
* `step_02b_IU_zju_get_grid_cells_etended.R`: get "grid cell" predictors for lags 15, 30, 45, 60, 75, 90

### Logistic regression models 
* `step_03_IU_fit_logistic.R`
* `step_03_zju_fit_logistic.R`
* `step_03b_fit_logistic_screened.R`: fit logisticc regression using only significant grid cells after CMA 

### Functional regression models 
* `step_04_IU_fit_functional.R`
* `step_04_zju_fit_functional.R`

### Machine learning models 
* `step_05_IU_fit_ML_rev.R`
* `step_05_zjus1_fit_ML_rev.R`: fit ML models for ZJU S1 task 
* `step_05b_zjus1s2_fit_ML_rev.R`: fit ML models for ZJU S1S2 task
* `step_05b_zjus1_fit_ML_extended.R`: fit ZJU S1 task with six grid cells
* `step_05b_zjus1s2_fit_ML_extended.R`: fit ZJU S1S2 task with six grid cells 

### Correlation and multiplicity adjusted (CMA) inference
* `step_06_IU_zju_CMA.R`

### Results
* `step_07_process_ML_results.R`: get predictions from ML models in same format as others
* `step_07b_process_ML_extended.R`: same but for six-grid cell predictions
* `compare_results.R` - after running all models above, use to compare results between methods
* `final_tables_figures.R`: manuscript tables and figures
* `tables_figures_revision`: changes for revision to tables and figs 


## Authors

<a href="mailto:lkoffma2@jh.edu"> Lily Koffman </a>
