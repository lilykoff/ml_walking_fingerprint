library(tidymodels)
library(tune)
library(baguette)
library(discrim)
library(finetune)
library(readr)

tidymodels_prefer()

data <- readRDS("grid_data_lw_IU.rds") 
# change column names to avoid bugs with some of the models 
colnames <- colnames(data)[-c(1,2)]
# create key to link back to old column names 
key <- 
  data.frame(name = colnames) %>% 
  mutate(
    number = row_number(),
    newname = paste0("col", number))
# replace column names 
colnames(data)[-c(1,2)] <- key$newname

data_split <- split(data, f = factor(data$ID))

# function to get random sample 
samp <- function(pct, n, ind) {
  set.seed(ind)
  sample(n, floor(pct * n), replace = F)
}
ids <- unique(data$ID)
rows <- lapply(data_split, nrow) %>% unlist()

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


get_original_indices <- function(subject, list){
  cbind(rep(subject, length(list[[subject]])), list[[subject]]) %>% 
    data.frame() %>%
    rename(
      sub = X1, 
      sec = X2
    )
}

orig_indices_key <- 
  map_dfr(.x = ids,
          .f = get_original_indices,
          list = train_indices)

train_inds <-
  data %>%
  full_join(orig_indices_key %>% mutate(train = 1), by = c("ID" = "sub",
                                                           "second" = "sec")) %>%
  ungroup() %>% 
  mutate(
    row = row_number()
  )

true_train <- train_inds$row[!is.na(train_inds$train)]
# create folds for cross validation 


# initial split data for eventually testing 
data_split <- initial_split(data, prop = 0.75)
data_split$in_id <- true_train

# check 
#all_equal(data_train, training(data_split2))
#all_equal(data_test, testing(data_split2))

data_split$data <- data %>% dplyr::select(-second)
data_train <- data_train %>% dplyr::select(-second)
data_test <- data_test %>% dplyr::select(-second)
data_folds <- vfold_cv(data_train, v = 5)
## model specifications 

# flexible discriminant model from multivariate adaptive regression splines 


## processing results 
rm(data)
predictions_models_2 <- readRDS("~/Documents/ml_walking_fingerprint/Results/tidyml/predictions_models_2.rds")


ranked_results <- function(id, list){
  list[[id]][2]$results %>%
    filter(lengths(result) > 1) %>%
    rank_results(rank_metric = "roc_auc", select_best = TRUE) %>%
    mutate(
      subject = id
    )
}
subs <- seq(1, 8, 1)
all_results_2 <- 
  map_dfr(
    .x = subs,
    .f = ranked_results,
    list = predictions_models_2
  ) %>% 
  mutate(
    subject = subject + 8
  )


get_preds <- function(id, list){
  list[[id]][1]$preds %>%
    arrange(.row) %>% 
    dplyr::select(.pred_1)
}

all_preds_2 <-
  map_dfc(
    .x = subs,
    .f = get_preds,
    list = predictions_models_2
  )


rm(predictions_models_2)



# now in correct format, just need to do for all 4 groups
predictions_models_3 <- readRDS("~/Documents/ml_walking_fingerprint/Results/tidyml/predictions_models_3.rds")
all_results_3 <- 
  map_dfr(
    .x = subs,
    .f = ranked_results,
    list = predictions_models_3
  ) %>% 
  mutate(
    subject = subject + 16
  )
all_preds_3 <-
  map_dfc(
    .x = subs,
    .f = get_preds,
    list = predictions_models_3
  )


rm(predictions_models_3)

predictions_models_4 <- readRDS("~/Documents/ml_walking_fingerprint/Results/tidyml/predictions_models_4.rds")

all_results_4 <- 
  map_dfr(
    .x = subs,
    .f = ranked_results,
    list = predictions_models_4
  ) %>% 
  mutate(
    subject = subject + 24
  )
all_preds_4 <-
  map_dfc(
    .x = subs,
    .f = get_preds,
    list = predictions_models_4
  )

res <- predictions_models_4[[1]][2]$results %>%
  filter(lengths(result) > 1)

autoplot(
  res,
  rank_metric = "roc_auc",  
  metric = "roc_auc",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - .02, label = wflow_id), angle = 90, hjust = 1) +
  scale_y_continuous(limits=c(.70,.99))+
  theme_light()+
  scale_x_continuous(breaks=seq(1, 13,1))+
  labs(y = "ROC AUC", title = "Models Ranked by ROC AUC in 5-fold Cross Validation", 
       subtitle = "Subject 9")+
  theme(legend.position = "none")

autoplot(
  res,
  rank_metric = "accuracy",  
  metric = "accuracy",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - .02, label = wflow_id), angle = 90, hjust = 1) +
  scale_y_continuous(limits=c(.919,.99))+
  theme_light()+
  scale_x_continuous(breaks=seq(1,12,1))+
  labs(y = "Accuracy", title = "Models Ranked by Accuracy in 5-fold Cross Validation",
       subtitle = "Subject 9")+
  theme(legend.position = "none")


rm(predictions_models_4)

## plots 
all_results_2 %>%
  filter(.metric == "roc_auc") %>%
  ggplot(aes(x = rank, y = mean, color = wflow_id))+
  geom_point()+
  geom_errorbar(aes(ymin = mean - 1.96*std_err, ymax = mean + 1.96*std_err))+
  facet_wrap(.~subject)+
  scale_x_continuous(breaks=seq(1,12,1))+
  theme_light()+
  labs(x = "Workflow Rank", y = "Mean AUC", title = "Model Performance in Cross Validation by Subject")+
  theme(legend.position="bottom")

all_results_2 %>% 
  filter(.metric == "roc_auc") %>%
  group_by(wflow_id) %>%
  summarize(
    num_best = length(rank[rank==1]),
    num_second_best = length(rank[rank==2])
  )

# process predictions 
p1 <- predictions_models_1[[1]][1]$preds %>% 
  arrange(.row)
p2 <- predictions_models_1[[2]][1]$preds %>%
  arrange(.row)

all_preds$true_subject <- data_test$ID
