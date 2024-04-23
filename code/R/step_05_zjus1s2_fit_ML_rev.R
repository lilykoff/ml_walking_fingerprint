library(tidymodels)
library(tune)
library(baguette)
library(discrim)
library(finetune)
library(readr)
library(doMC)

pckgs <- c("kknn", "dbarts", "earth", "mda", "ranger", "naivebayes")
sapply(pckgs, function(x) if(!require(x,character.only=TRUE,quietly=TRUE)) {
  install.packages(x)
  require(x, character.only=TRUE)
})

get_fold = function() {
  ifold = as.numeric(Sys.getenv("SGE_TASK_ID"))
  if (is.na(ifold)) {
    ifold = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  }
  print(paste0("fold is: ", ifold))
  ifold
}

ifold = get_fold()
tidymodels_prefer()

ncores <- parallelly::availableCores()
registerDoMC(cores = ncores - 1)


data_s1 <- readRDS(here::here("data", "grid_data_rw_zju_s1.rds")) %>% 
  mutate(
    ID = ID - 22
  ) %>% 
  janitor::clean_names() %>% 
  rename(ID = id)

data_s2 <- readRDS(here::here("data", "grid_data_rw_zju_s2.rds")) %>% 
  mutate(
    ID = ID - 22
  ) %>% 
  janitor::clean_names() %>% 
  rename(ID = id)

### all model specifications 
discrim_flex_spec <-
  discrim_flexible(num_terms = tune(), prod_degree = tune()) %>%
  set_engine("earth") %>%
  set_mode("classification")

# neural net 
nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("classification")

# radial basis function support vector machine 
svm_r_spec <- 
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

# polynomial basis function support vector machine 
svm_p_spec <- 
  svm_poly(cost = tune(), degree = tune(), scale_factor = tune(), margin = tune()) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification")

# bayesian additive regression tree 
bart_spec <- 
  parsnip::bart(trees=tune(), prior_terminal_node_coef = tune(), prior_terminal_node_expo = tune()) %>% 
  set_engine("dbarts") %>%
  set_mode("classification")

# boosted tree 
boost_spec <- 
  boost_tree(min_n = tune(), mtry = tune(), trees = tune(), tree_depth = tune(),
             learn_rate = tune(), sample_size = tune(), loss_reduction = tune()) %>% 
  set_engine("xgboost") %>%
  set_mode("classification")

# multivariate adaptive regression spline 
# mars_spec <- 
#   mars(num_terms = tune(), prod_degree = tune()) %>%
#   set_engine("earth") %>%
#   set_mode("classification")

# k nearest neighbors 
knn_spec <- 
  nearest_neighbor(neighbors = tune()) %>%
  set_engine("kknn") %>% 
  set_mode("classification")

# logistic regression 
lreg_spec <-
  logistic_reg(penalty = 0, mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# penalized logistic regression 
plreg_spec <-
  logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# naive bayes 
bayes_spec <-
  naive_Bayes(smoothness = tune()) %>%
  set_engine("naivebayes") %>%
  set_mode("classification")

# random forest 
rf_spec <- 
  rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

# race control
race_ctrl <-
  control_race(
    save_pred = F,
    parallel_over = "everything",
    save_workflow = F
  )

set.seed(123)

data = bind_rows(data_s1, data_s2)
prop = nrow(data_s1)/(nrow(data_s1) + nrow(data_s2))
initialsplit = initial_split(data, prop = prop)
initialsplit$in_id = 1:nrow(data_s1) # manually make training data only from session 1

run_models = function(subject){
  
  # make outcome binary 
  initialsplit$data = initialsplit$data %>% 
    mutate(outcome = factor(ifelse(ID == subject, 1, 0)))
  
  train = training(initialsplit)
  test = testing(initialsplit)
  set.seed(456)
  folds = vfold_cv(train, v = 5, strata = ID)
  
  normalized_transform_rec <- 
    recipe(outcome ~ ., data = train %>% select(-ID, second)) %>% 
    step_nzv(all_predictors()) %>% 
    step_YeoJohnson(all_predictors()) %>%
    step_normalize(all_predictors())
  
  no_rec <- 
    recipe(outcome ~ ., data = train %>% select(-ID, second)) %>% 
    step_nzv(all_predictors()) 
  
  # workflows 
  proc  <- workflow_set(
    preproc = list(all = normalized_transform_rec),      
    models = list(mlp = nnet_spec, svm_rf = svm_r_spec, svm_p = svm_p_spec, knn = knn_spec)
  )
  
  none <- workflow_set(
    preproc = list(none = no_rec),
    models = list(bart = bart_spec, boost = boost_spec, flexdiscrim = discrim_flex_spec,
                  rf = rf_spec, naivebayes = bayes_spec, lreg = lreg_spec, plreg = plreg_spec)
  )
  
  all_workflows <- 
    bind_rows(proc, none) 
  
  results_all <-
    all_workflows %>% 
    workflow_map(
      "tune_race_anova",
      seed = 1503,
      resamples =folds,
      grid = 25,
      control = race_ctrl
    )
  
  
  # get predictions for best config of each workflow 
  results <-
    results_all %>%
    filter(lengths(result) > 1) %>%
    workflowsets::rank_results(select_best = TRUE, rank_metric = "roc_auc") %>%
    select(wflow_id, .metric, race = mean, config_race = .config) %>%
    filter(.metric == "roc_auc")
  
  
  best_results <-
    map(.x = unique(results$wflow_id),
        .f = function(res, id){
          res %>% 
            extract_workflow_set_result(id = id) %>% 
            select_best(metric = "roc_auc")},
        res = results_all)
  
  test_results =
    map2(.x = best_results, 
         .y = unique(results$wflow_id),
         .f = function(res, params, id){
           res %>% 
             extract_workflow(id = id) %>% 
             finalize_workflow(params) %>% 
             last_fit(split = initialsplit) 
         },
         res = results_all)
  
  preds_df = 
    map2_dfr(.x = test_results, 
             .y = unique(results$wflow_id),
             .f = function(ress, name){
               ress %>% 
                 collect_predictions() %>% 
                 select(.pred_class, .pred_0, .pred_1, outcome) %>% 
                 mutate(predicted_sub = subject, model = name)
             })
  
  preds_df
  
}

res = run_models(subject = ifold)
fname = paste0("predictions_zjus1s2_subject_", ifold, ".rds")
saveRDS(res,
        here::here("results", fname))
