library(tidyverse)
library(tidymodels) 

# data = readRDS("/users/lkoffman/paper2/data/grid_data_lw_IU.rds") %>% 
#   janitor::clean_names() %>%
#   rename("ID" = "id")

# also put in same format as before 
data = readRDS(here::here("data", "grid_data_rw_zju_s1_extended.rds")) %>% 
  mutate(
    ID = ID - 22
  ) %>% 
  janitor::clean_names() %>% 
  rename(ID = id) 


set.seed(123)
initialsplit = initial_split(data, prop = 3/4, strata = ID)
test = testing(initialsplit)
test_key = test %>% select(ID, second) %>% 
  mutate(row = row_number()) %>% 
  rename(true_subject = ID)

files = list.files(here::here("results_ml_extended"), full.names = TRUE, pattern="predictions_zju_subject_extended*")

file_list = map(.x = files, 
                .f = function(x){readRDS(x) %>% 
                    group_by(model) %>% 
                    mutate(row = row_number())})

all_files = bind_rows(file_list) %>% split(., .$model)

# we want one row per second and "true subject" column 
# temp = all_files[[1]]

process_results  = function(df){
  x = df %>% 
    select(-outcome, -.pred_class, -.pred_0) %>% 
    pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "sub_") %>% 
    left_join(test_key, by = c("row"))
  row_sums <- rowSums(x %>% ungroup() %>% select(starts_with("sub")))
  
  # normalize and add "true subject column"
  x = x %>% 
    bind_cols(sum = row_sums) %>% 
    rowwise() %>%
    mutate(across(starts_with("sub"), ~ .x / sum)) %>%
    dplyr::select(-sum) %>%
    ungroup()
  
  x %>%
    group_by(true_subject, model) %>%
    pivot_longer(cols = -c("true_subject", "second", "row", "model")) %>%
    mutate(pred_sub = as.numeric(sub(".*sub\\_", "", name))) %>%
    rename(pred = value) %>%
    ungroup() %>%
    group_by(true_subject, pred_sub, model) %>%
    summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
    group_by(true_subject, model) %>%
    summarize(
      maxprob = first(max(mean_pred)),
      predicted_sub = first(pred_sub[mean_pred == maxprob]),
      probsubj = first(mean_pred[true_subject == pred_sub])
    ) %>%
    mutate(correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0))
  
}


all_results_zjus1_ext = map_dfr(.x = all_files, .f = process_results)

all_results_zjus1_ext %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))

# files = list.files(here::here("results_ml"), full.names = TRUE, pattern="predictions_zju_sub*")
# 
# file_list = map(.x = files, 
#                 .f = function(x){readRDS(x) %>% 
#                     group_by(model) %>% 
#                     mutate(row = row_number())})
# 
# all_files = bind_rows(file_list) %>% split(., .$model)
# 
# all_results_zjus1 = map_dfr(.x = all_files, .f = process_results)

# we wan

############# s1s2 

data_s1 = readRDS(here::here("data", "grid_data_rw_zju_s1_extended.rds")) %>% 
  mutate(
    ID = ID - 22
  ) %>% 
  janitor::clean_names() %>% 
  rename(ID = id)

data_s2 = readRDS(here::here("data", "grid_data_rw_zju_s2_extended.rds")) %>% 
  mutate(
    ID = ID - 22
  ) %>% 
  janitor::clean_names() %>% 
  rename(ID = id)
data = bind_rows(data_s1, data_s2)
prop = nrow(data_s1)/(nrow(data_s1) + nrow(data_s2))
initialsplit = initial_split(data, prop = prop)
initialsplit$in_id = 1:nrow(data_s1) # manually make training data only from session 1
test = testing(initialsplit)
test_key = test %>% select(ID, second) %>% 
  mutate(row = row_number()) %>% 
  rename(true_subject = ID)

files = list.files(here::here("results_ml_extended"), full.names = TRUE, pattern="predictions_zjus1s2_subject_extended*")


file_list = map(.x = files, 
                .f = function(x){readRDS(x) %>% 
                    group_by(model) %>% 
                    mutate(row = row_number())})

all_files = bind_rows(file_list) %>% split(., .$model)

# we want one row per second and "true subject" column 

all_results_zjus1s2_ext = map_dfr(.x = all_files, .f = process_results)

all_results_zjus1s2_ext %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))


 files = list.files(here::here("results_ml"), full.names = TRUE, pattern="predictions_zjus1s2*")

file_list = map(.x = files,
                .f = function(x){readRDS(x) %>%
                    group_by(model) %>%
                    mutate(row = row_number())})

all_files = bind_rows(file_list) %>% split(., .$model)

# we want one row per second and "true subject" column

all_results_zjus1s2 = map_dfr(.x = all_files, .f = process_results)



# table 
all_results_zjus1_ext %>% 
  group_by(model) %>% 
  summarize(correct_s1 = sum(correct),
            pct_s1 = correct_s1 / n()) %>% 
  left_join(all_results_zjus1_ext %>% 
              group_by(model) %>% 
              summarize(correct_s1_ext = sum(correct),
                        pct_s1_ext = correct_s1_ext / n()),
            by = "model") %>% 
  left_join(all_results_zjus1s2 %>% 
              group_by(model) %>% 
              summarize(correct_s1s2 = sum(correct),
                        pct_s1s2 = correct_s1s2 / n()),
            by = "model") %>% 
  left_join(all_results_zjus1s2_ext %>% 
              group_by(model) %>% 
              summarize(correct_s1s2_ext = sum(correct),
                        pct_s1s2_ext = correct_s1s2_ext / n()),
            by = "model") %>% 
  filter(model != "none_lreg") %>% 
  mutate(across(starts_with("pct"), ~sprintf("%.02f", round(.x, 2)))) %>% 
  mutate(model = c("K-nearest neighbors", 
                   "Neural network", 
                   "Polynomial-basis SVM", 
                   "Radial-basis SVM", 
                   "Bayesinan Additive Regression Trees", 
                   "Boosted Tree", 
                   "Flexible Discriminant Analysis", 
                   "Naive Bayes", 
                   "Penalized logistic regression", 
                   "Random forest")) %>%
  arrange(desc(pct_s1)) %>% 
  kableExtra::kable(align = "lllll", booktabs  = TRUE,  format = "latex", col.names = 
                      c("Model", "Number Correct", "% Correct", "Number Correct", "% Correct",
                        "Number Correct", "% Correct", "Number Correct", "% Correct")) %>% 
  kableExtra::add_header_above(c(" " = 1, "3 lags" = 2, "6 lags"= 2,
                                 "3 lags" = 2, "6 lags" = 2)) %>% 
  kableExtra::add_header_above(c(" " = 1, "ZJU S1" = 4, "ZJU S1S2" = 4)) %>% 
  kableExtra::kable_styling(latex_options = "scale_down")  

# one model per person

all_results_zjus1 %>% 
  group_by(model) %>% 
  summarize(correct_s1 = sum(correct),
            pct_s1 = correct_s1 / n()) %>% 
  left_join(all_results_zjus1_ext %>% 
              group_by(model) %>% 
              summarize(correct_s1_ext = sum(correct),
                        pct_s1_ext = correct_s1_ext / n()),
            by = "model") %>% 
  left_join(all_results_zjus1s2 %>% 
              group_by(model) %>% 
              summarize(correct_s1s2 = sum(correct),
                        pct_s1s2 = correct_s1s2 / n()),
            by = "model") %>% 
  left_join(all_results_zjus1s2_ext %>% 
              group_by(model) %>% 
              summarize(correct_s1s2_ext = sum(correct),
                        pct_s1s2_ext = correct_s1s2_ext / n()),
            by = "model") %>% 
  filter(model != "none_lreg") %>% 
  mutate(across(starts_with("pct"), ~sprintf("%.02f", round(.x, 2)))) %>% 
  mutate(model = c("K-nearest neighbors", 
                   "Neural network", 
                   "Polynomial-basis SVM", 
                   "Radial-basis SVM", 
                   "Bayesinan Additive Regression Trees", 
                   "Boosted Tree", 
                   "Flexible Discriminant Analysis", 
                   "Naive Bayes", 
                   "Penalized logistic regression", 
                   "Random forest")) %>%
  arrange(desc(pct_s1)) %>% 
  kableExtra::kable(align = "lllll", booktabs  = TRUE, 
                    format = "latex",
                    col.names = 
                      c("Model", "Number Correct", "% Correct", "Number Correct", "% Correct",
                        "Number Correct", "% Correct", "Number Correct", "% Correct")) %>% 
  kableExtra::add_header_above(c(" " = 1, "3 lags" = 2, "6 lags"= 2,
                                 "3 lags" = 2, "6 lags" = 2)) %>% 
  kableExtra::add_header_above(c(" " = 1, "ZJU S1" = 4, "ZJU S1S2" = 4)) %>% 
  kableExtra::kable_styling(latex_options = "scale_down")  


