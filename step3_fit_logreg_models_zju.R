library(tidyverse)
library(purrr)
library(tidymodels)
tidymodels_prefer()
options(dplyr.summarise.inform=FALSE)

## read in data 
grid_data_rw_zju_s1 <- readRDS("~/Documents/ml_walking_fingerprint/grid_data_rw_zju_s1.rds")
grid_data_rw_zju_s2 <- readRDS("~/Documents/ml_walking_fingerprint/grid_data_rw_zju_s2.rds")

## first: use just session 1 to predict on session 1
grid_data_rw_zju_s1$ID <- grid_data_rw_zju_s1$ID - 22
grid_data_rw_zju_s2$ID <- grid_data_rw_zju_s2$ID - 22
# split into training and testing
# 75% train, 25% test, equal proportions for ea person 
data_split <- split(grid_data_rw_zju_s1, f = grid_data_rw_zju_s1$ID)
# function to sample percentage 
samp <- function(pct, n, ind) {
  set.seed(ind)
  sample(n, floor(pct * n), replace = F)
}
ids <- unique(grid_data_rw_zju_s1$ID)
# number of rows for each individual 
rows <- lapply(data_split, nrow) %>% unlist()
# get random 75% of seconds for training 
train_indices <- map2(pct = .75,
                      .x = rows,
                      .y = ids,
                      .f = samp)

getrows <- function(data, rows) {
  data[rows, ]
}
getrows_test <- function(data, rows) {
  data[-rows, ]
}

data_train <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows)
data_test <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows_test)

# function to fit ovr logistic regression models 
fit_model <- function(subject, train, test){
  train$class <- ifelse(train$ID == subject, 1, 0)
  test$class <- ifelse(test$ID == subject, 1, 0)
  tmp <- train %>% dplyr::select(-c(ID, second))
  tmp_test <- test %>% dplyr::select(-c(ID, second))
  mod <- glm(class ~ ., data = tmp, family = binomial(link = "logit"))
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
all_predictions_zju_s1 <- 
  map_dfc(.x = ids,
          .f = fit_model,
          train = dat_nzv,
          test = dat_nzv_test,
          .progress = T)
# column j is predicted probbaility that data in that row belong to subject j 
# append true values

all_predictions_zju_s1$true_subject <- dat_nzv_test$ID

get_summarized_predictions <- function(predictions, long = T){
  if(long == T){
    predictions %>%
      group_by(true_subject) %>%
      mutate(
        sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(
        model = as.numeric(sub(".*^...", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(
        mean_pred = mean(pred)) %>%
      mutate(
        correct = ifelse(true_subject == model, 1, 0))
  }
  else{
    predictions %>%
      group_by(true_subject) %>%
      mutate(
        sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(
        model = as.numeric(sub(".*^...", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(
        mean_pred = mean(pred)) %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = first(max(mean_pred)),
        predicted_sub = first(model[mean_pred==maxprob]),
        probsubj = first(mean_pred[true_subject==model])) %>%
      mutate(
        correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0)) 
  }
}

get_summarized_predictions(all_predictions_zju_s1, long=F) %>% print(n=Inf)

# fn takes number of seconds to average over as input, outputs classification stats 
get_prediction_stats <- function(predictions, seconds){
  predictions %>% 
    group_by(true_subject) %>% 
    mutate(
      sec = floor(row_number()/seconds)) %>% 
    pivot_longer(cols = -c("true_subject", "sec")) %>%
    mutate(
      model = as.numeric(sub(".*^...", "", name))) %>%
    rename(pred = value) %>%
    ungroup() %>%
    group_by(true_subject, model, sec) %>%
    summarize(
      mean_pred = mean(pred)) %>%
    group_by(true_subject, sec) %>%
    summarize(
      maxprob = first(max(mean_pred)),
      predicted_subject = first(model[mean_pred == maxprob])
    ) %>% ungroup() %>% 
    dplyr::select(c(true_subject, predicted_subject)) %>% 
    mutate(across(1:2, factor, levels = as.factor(seq(1, 153, 1)))) %>%
    yardstick::conf_mat(., truth = true_subject, estimate = predicted_subject) %>%
    summary() %>% 
    mutate(
      s = seconds
    )
}

dat_nzv_test %>% group_by(ID) %>% summarize(n = n()) %>% summarize(min = min(n))
results_summarized <-
  map_dfr(.x = seq(1, 21, 2),
          .f = get_prediction_stats,
          predictions = all_predictions_zju_s1,
          .progress = T) %>% 
  filter(.metric != "detection_prevalence")

supp.labs <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "PPV", "NVP", "MCC", "J Index", "Balanced Accuracy", "Precision","Recall", "F1 Score")
names(supp.labs) <- unique(results_summarized$.metric)

results_summarized %>% 
  ggplot(aes(x = s, y = .estimate, col = .metric))+
  geom_point()+
  geom_line()+
  theme_light()+
  facet_wrap(.~.metric, scales="free_y", labeller=labeller(.metric=supp.labs))+
  labs(x = "Number of Seconds", y = "Estimate")+
  scale_x_continuous(breaks=seq(1,21,2))+
  theme(axis.text.x = element_text(angle=45))+
  theme(legend.position = "none")

## train on session 2, predict on session 2
data_split <- split(grid_data_rw_zju_s2, f = grid_data_rw_zju_s2$ID)

ids <- unique(grid_data_rw_zju_s2$ID)
# number of rows for each individual 
rows <- lapply(data_split, nrow) %>% unlist()
# get random 75% of seconds for training 
train_indices <- map2(pct = .75,
                      .x = rows,
                      .y = ids,
                      .f = samp)


data_train <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows)
data_test <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows_test)


# first we want to remove columns with near zero variance 
nzv_trans <- 
  recipe(ID ~ ., data = data_train) %>% 
  step_nzv(all_predictors())

nzv_estimates <- prep(nzv_trans)

nzv <- colnames(juice(nzv_estimates))
dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv))
dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv))

## now fit models, get predictions 
all_predictions_zju_s2 <- 
  map_dfc(.x = ids,
          .f = fit_model,
          train = dat_nzv,
          test = dat_nzv_test,
          .progress = T)

all_predictions_zju_s2$true_subject <- dat_nzv_test$ID

get_summarized_predictions(all_predictions_zju_s2, long=F) %>% print(n=Inf)

# fn takes number of seconds to average over as input, outputs classification stats 

results_summarized <-
  map_dfr(.x = seq(1, 21, 2),
          .f = get_prediction_stats,
          predictions = all_predictions_zju_s2,
          .progress = T) %>% 
  filter(.metric != "detection_prevalence")

supp.labs <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "PPV", "NVP", "MCC", "J Index", "Balanced Accuracy", "Precision","Recall", "F1 Score")
names(supp.labs) <- unique(results_summarized$.metric)

results_summarized %>% 
  ggplot(aes(x = s, y = .estimate, col = .metric))+
  geom_point()+
  geom_line()+
  theme_light()+
  facet_wrap(.~.metric, scales="free_y", labeller=labeller(.metric=supp.labs))+
  labs(x = "Number of Seconds", y = "Estimate")+
  scale_x_continuous(breaks=seq(1,21,2))+
  theme(axis.text.x = element_text(angle=45))+
  theme(legend.position = "none")

## all data combined 

grid_data_all <- 
  bind_rows(grid_data_rw_zju_s1, grid_data_rw_zju_s2) %>%
  group_by(ID) %>%
  mutate(
    second = row_number())

data_split <- split(grid_data_all, f = grid_data_all$ID)

ids <- unique(grid_data_all$ID)
# number of rows for each individual 
rows <- lapply(data_split, nrow) %>% unlist()
# get random 75% of seconds for training 
train_indices <- map2(pct = .75,
                      .x = rows,
                      .y = ids,
                      .f = samp)


data_train <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows) %>% ungroup() 
data_test <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows_test) %>% ungroup()


# first we want to remove columns with near zero variance 
nzv_trans <- 
  recipe(ID ~ ., data = data_train) %>% 
  step_nzv(all_predictors())

nzv_estimates <- prep(nzv_trans)

nzv <- colnames(juice(nzv_estimates))
dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv))
dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv))

## now fit models, get predictions 
all_predictions_zju_all <- 
  map_dfc(.x = ids,
          .f = fit_model,
          train = dat_nzv,
          test = dat_nzv_test,
          .progress = T)

all_predictions_zju_all$true_subject <- dat_nzv_test$ID

get_summarized_predictions(all_predictions_zju_all, long=F) %>% print(n=Inf)

# fn takes number of seconds to average over as input, outputs classification stats 

results_summarized <-
  map_dfr(.x = seq(1, 21, 2),
          .f = get_prediction_stats,
          predictions = all_predictions_zju_all,
          .progress = T) %>% 
  filter(.metric != "detection_prevalence")

supp.labs <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "PPV", "NVP", "MCC", "J Index", "Balanced Accuracy", "Precision","Recall", "F1 Score")
names(supp.labs) <- unique(results_summarized$.metric)

results_summarized %>% 
  ggplot(aes(x = s, y = .estimate, col = .metric))+
  geom_point()+
  geom_line()+
  theme_light()+
  facet_wrap(.~.metric, scales="free_y", labeller=labeller(.metric=supp.labs))+
  labs(x = "Number of Seconds", y = "Estimate")+
  scale_x_continuous(breaks=seq(1,21,2))+
  theme(axis.text.x = element_text(angle=45))+
  theme(legend.position = "none")

## finally: train on 1, predict on 2

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

## now fit models, get predictions 
all_predictions_zju_s1s2 <- 
  map_dfc(.x = ids,
          .f = fit_model,
          train = dat_nzv,
          test = dat_nzv_test,
          .progress = T)

all_predictions_zju_s1s2$true_subject <- dat_nzv_test$ID

get_summarized_predictions(all_predictions_zju_s1s2, long=F) %>% print(n=Inf)

# fn takes number of seconds to average over as input, outputs classification stats 

results_summarized <-
  map_dfr(.x = seq(1, 21, 2),
          .f = get_prediction_stats,
          predictions = all_predictions_zju_s1s2,
          .progress = T) %>% 
  filter(.metric != "detection_prevalence")

supp.labs <- c("Accuracy", "Kappa", "Sensitivity", "Specificity", "PPV", "NVP", "MCC", "J Index", "Balanced Accuracy", "Precision","Recall", "F1 Score")
names(supp.labs) <- unique(results_summarized$.metric)

results_summarized %>% 
  ggplot(aes(x = s, y = .estimate, col = .metric))+
  geom_point()+
  geom_line()+
  theme_light()+
  facet_wrap(.~.metric, scales="free_y", labeller=labeller(.metric=supp.labs))+
  labs(x = "Number of Seconds", y = "Estimate")+
  scale_x_continuous(breaks=seq(1,21,2))+
  theme(axis.text.x = element_text(angle=45))+
  theme(legend.position = "none")

# does not work well predicting on second session - maybe need to utilize other body parts (can revisit) 
