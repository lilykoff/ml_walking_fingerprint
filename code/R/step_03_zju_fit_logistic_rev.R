library(tidyverse)
library(purrr)
library(tidymodels)
library(magrittr)
tidymodels_prefer()
options(dplyr.summarise.inform = FALSE)

## read in data
grid_data_rw_zju_s1 <-
  readRDS(here::here("data/grid_data_rw_zju_s1.rds")) %>%
  mutate(ID = ID - 22)
grid_data_rw_zju_s2 <-
  readRDS(here::here("data/grid_data_rw_zju_s2.rds")) %>%
  mutate(ID = ID - 22)

# first: use just session 1 to predict on session 1
# split into training and testing
# 75% train, 25% test, equal proportions for ea person

set.seed(123)
initialsplit = initial_split(grid_data_rw_zju_s1, prop = 3/4, strata = ID)
ids <- unique(grid_data_rw_zju_s1$ID)



data_train = training(initialsplit)
data_test = testing(initialsplit)


# function to fit ovr logistic regression models
fit_model <- function(subject, train, test) {
  train$class <- ifelse(train$ID == subject, 1, 0)
  test$class <- ifelse(test$ID == subject, 1, 0)
  tmp <- train %>% dplyr::select(-c(ID, second))
  tmp_test <- test %>% dplyr::select(-c(ID, second))
  mod <-
    glm(class ~ ., data = tmp, family = binomial(link = "logit"))
  pred <- predict.glm(mod, newdata = tmp_test, type = "response")
  return(pred)
}

# first we want to remove columns with near zero variance
nzv_trans <-
  recipe(ID ~ ., data = data_train) %>%
  step_nzv(all_predictors())

nzv_estimates <- prep(nzv_trans)

nzv <- colnames(juice(nzv_estimates))
dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv))
dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv))


## now fit models, get predictions
t1 <- Sys.time()
all_predictions_zju_s1 <-
  map_dfc(
    .x = ids,
    .f = fit_model,
    train = dat_nzv,
    test = dat_nzv_test,
    .progress = T
  ) %>%
  janitor::clean_names()
Sys.time() - t1

# Time difference of 2.041705 mins

row_sums <- rowSums(all_predictions_zju_s1)

# normalize and add "true subject column"
all_predictions_zju_s1 %<>%
  bind_cols(sum = row_sums) %>%
  rowwise() %>%
  mutate(across(x1:x153, ~ .x / sum)) %>%
  dplyr::select(-sum) %>%
  ungroup() %>%
  bind_cols(true_subject = dat_nzv_test$ID)



if (!dir.exists("predictions")) {
  dir.create(here::here("predictions"))
}
saveRDS(
  all_predictions_zju_s1,
  here::here("predictions/zjus1_logistic_predictions_revised.rds")
)

get_summarized_predictions <- function(predictions, long = FALSE) {
  if (long == T) {
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
      mutate(correct = ifelse(true_subject == model, 1, 0))
  }
  else{
    predictions %>%
      group_by(true_subject) %>%
      mutate(sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(model = as.numeric(sub(".*x", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred == maxprob]),
        probsubj = first(mean_pred[true_subject == model])
      ) %>%
      mutate(correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0))
  }
}

get_summarized_predictions(all_predictions_zju_s1) %>% print(n = Inf)

get_prediction_stats <- function(predictions, seconds) {
  predictions %>%
    group_by(true_subject) %>%
    mutate(sec = floor(row_number() / seconds)) %>%
    pivot_longer(cols = -c("true_subject", "sec")) %>%
    mutate(model = as.numeric(sub(".*x", "", name))) %>%
    rename(pred = value) %>%
    ungroup() %>%
    group_by(true_subject, model, sec) %>%
    summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
    group_by(true_subject, sec) %>%
    summarize(maxprob = max(mean_pred),
              predicted_subject = model[mean_pred == maxprob]) %>% ungroup() %>%
    dplyr::select(c(true_subject, predicted_subject)) %>%
    mutate(across(1:2, factor, levels = as.factor(seq(1, 153, 1)))) %>%
    yardstick::conf_mat(., truth = true_subject, estimate = predicted_subject) %>%
    summary() %>%
    mutate(s = seconds)
}

results_summarized <-
  map_dfr(
    .x = seq(1, 21, 2),
    .f = get_prediction_stats,
    predictions = all_predictions_zju_s1,
    .progress = T
  ) %>%
  filter(.metric != "detection_prevalence")

supp.labs <-
  c(
    "Accuracy",
    "Kappa",
    "Sensitivity",
    "Specificity",
    "PPV",
    "NVP",
    "MCC",
    "J Index",
    "Balanced Accuracy",
    "Precision",
    "Recall",
    "F1 Score"
  )
names(supp.labs) <- unique(results_summarized$.metric)

results_summarized %>%
  ggplot(aes(x = s, y = .estimate, col = .metric)) +
  geom_point() +
  geom_line() +
  theme_light() +
  facet_wrap(. ~ .metric,
             scales = "free_y",
             labeller = labeller(.metric = supp.labs)) +
  labs(x = "Number of Seconds", y = "Estimate") +
  scale_x_continuous(breaks = seq(1, 21, 2)) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(legend.position = "none")


data_train <- grid_data_rw_zju_s1
data_test <- grid_data_rw_zju_s2


# first we want to remove columns with near zero variance
nzv_trans <-
  recipe(ID ~ ., data = data_train) %>%
  step_nzv(all_predictors())

nzv_estimates <- prep(nzv_trans)

nzv <- colnames(juice(nzv_estimates))
dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv))
dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv))
ids <- unique(grid_data_rw_zju_s1$ID)

## now fit models, get predictions
t1 <- Sys.time()
plan(multisession, workers = 8)
all_predictions_zju_s1s2 <-
  furrr::future_map_dfc(
    .x = ids,
    .f = fit_model,
    train = dat_nzv,
    test = dat_nzv_test,
    .progress = T
  ) %>%
  janitor::clean_names()

Sys.time() - t1

# Time difference of 2.71424 mins

row_sums <- rowSums(all_predictions_zju_s1s2)

# normalize and add "true subject column"
all_predictions_zju_s1s2 %<>%
  bind_cols(sum = row_sums) %>%
  rowwise() %>%
  mutate(across(x1:x153, ~ .x / sum)) %>%
  dplyr::select(-sum) %>%
  ungroup() %>%
  bind_cols(true_subject = dat_nzv_test$ID)

res = get_summarized_predictions(all_predictions_zju_s1s2) %>% print(n = Inf)

sum(res$correct)
saveRDS(
  all_predictions_zju_s1s2,
  here::here("predictions/zjus1s2_logistic_predictions_revised.rds")
)

