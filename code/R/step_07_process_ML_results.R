library(tidyverse)
library(tidymodels) 

# data = readRDS("/users/lkoffman/paper2/data/grid_data_lw_IU.rds") %>% 
#   janitor::clean_names() %>%
#   rename("ID" = "id")

# also put in same format as before 
data = readRDS(here::here("data", "grid_data_lw_IU.rds")) %>%
  janitor::clean_names() %>%
  rename("ID" = "id")


set.seed(123)
initialsplit = initial_split(data, prop = 3/4, strata = ID)
test = testing(initialsplit)
test_key = test %>% select(ID, second) %>% 
  mutate(row = row_number()) %>% 
  rename(true_subject = ID)

files = list.files(here::here("results_ml"), full.names = TRUE, pattern="predictions_IU*")

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


all_results_IU = map_dfr(.x = all_files, .f = process_results)

all_results_IU %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))

# for knn, get in same format as before 
knn_IU = bind_rows(file_list) %>% 
  filter(model == "all_knn")

x = knn_IU %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "x") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(x %>% ungroup() %>% select(starts_with("x")))

# normalize and add "true subject column"
x = x %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("x"), ~ .x / sum)) %>%
  ungroup() %>% 
  select(starts_with("x"), "true_subject")
saveRDS(x, here::here("predictions", "IU_ml_predictions_revised.rds"))



### ZJU 

data = readRDS(here::here("data", "grid_data_rw_zju_s1.rds")) %>% 
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

files = list.files(here::here("results_ml"), full.names = TRUE, pattern="predictions_zju_sub*")

file_list = map(.x = files, 
                .f = function(x){readRDS(x) %>% 
                    group_by(model) %>% 
                    mutate(row = row_number())})

all_files = bind_rows(file_list) %>% split(., .$model)

# we want one row per second and "true subject" column 


all_results_zjus1 = map_dfr(.x = all_files, .f = process_results)

all_results_zjus1 %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))

all_results_zjus1 %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))
# knn is second best 


knn = file_list %>% 
  bind_rows() %>% 
  filter(model == "all_knn")

knn = 
  knn %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "sub_") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(knn %>% ungroup() %>% select(starts_with("sub")))

# normalize and add "true subject column"
knn = knn %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("sub"), ~ .x / sum)) %>%
  dplyr::select(-sum) %>%
  ungroup()

knn %>%
  group_by(true_subject, model) %>%
  pivot_longer(cols = -c("true_subject", "second", "row", "model")) %>%
  mutate(pred_sub = as.numeric(sub(".*sub\\_", "", name))) %>%
  rename(pred = value) %>%
  ungroup() %>%
  group_by(true_subject, pred_sub, model) %>%
  summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
  group_by(true_subject) %>%
  mutate(
    rank = rank(-mean_pred)
  ) %>% 
  filter(pred_sub==true_subject) %>%
  mutate(
    rank1 = ifelse(rank == 1, 1, 0),
    rank5 = ifelse(rank <= 5, 1, 0)
  ) %>% 
  ungroup() %>% 
  summarize(n1 = sum(rank1),
            n5 = sum(rank5),
            acc1 =n1/n(),
            acc5 = n5/n())
            

# for knn, get in same format as before 
knn = file_list %>% 
  bind_rows() %>% 
  filter(model == "all_knn")
x = knn %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "x") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(x %>% ungroup() %>% select(starts_with("x")))

# normalize and add "true subject column"
x = x %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("x"), ~ .x / sum)) %>%
  ungroup() %>% 
  select(starts_with("x"), "true_subject")
saveRDS(x, here::here("predictions", "zjus1_ml_predictions_revised.rds"))



############# s1s2 

data_s1 = readRDS(here::here("data", "grid_data_rw_zju_s1.rds")) %>% 
  mutate(
    ID = ID - 22
  ) %>% 
  janitor::clean_names() %>% 
  rename(ID = id)

data_s2 = readRDS(here::here("data", "grid_data_rw_zju_s2.rds")) %>% 
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

files = list.files(here::here("results_ml"), full.names = TRUE, pattern="predictions_zjus1s2*")

file_list = map(.x = files, 
                .f = function(x){readRDS(x) %>% 
                    group_by(model) %>% 
                    mutate(row = row_number())})

all_files = bind_rows(file_list) %>% split(., .$model)

# we want one row per second and "true subject" column 

all_results_zjus1s2 = map_dfr(.x = all_files, .f = process_results)

all_results_zjus1s2 %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))


plr = file_list %>% 
  bind_rows() %>% 
  filter(model == "none_plreg")

plr  = 
  plr %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "sub_") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(plr %>% ungroup() %>% select(starts_with("sub")))

# normalize and add "true subject column"
plr = plr %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("sub"), ~ .x / sum)) %>%
  dplyr::select(-sum) %>%
  ungroup()

plr %>%
  group_by(true_subject, model) %>%
  pivot_longer(cols = -c("true_subject", "second", "row", "model")) %>%
  mutate(pred_sub = as.numeric(sub(".*sub\\_", "", name))) %>%
  rename(pred = value) %>%
  ungroup() %>%
  group_by(true_subject, pred_sub, model) %>%
  summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
  group_by(true_subject) %>%
  mutate(
    rank = rank(-mean_pred)
  ) %>% 
  filter(pred_sub==true_subject) %>%
  mutate(
    rank1 = ifelse(rank == 1, 1, 0),
    rank5 = ifelse(rank <= 5, 1, 0)
  ) %>% 
  ungroup() %>% 
  summarize(n1 = sum(rank1),
            n5 = sum(rank5),
            acc1 =n1/n(),
            acc5 = n5/n())


#  get in same format as before 

x = plr = file_list %>% 
  bind_rows() %>% 
  filter(model == "none_plreg") %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "x") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(x %>% ungroup() %>% select(starts_with("x")))

# normalize and add "true subject column"
x = x %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("x"), ~ .x / sum)) %>%
  ungroup() %>% 
  select(starts_with("x"), "true_subject")
saveRDS(x, here::here("predictions", "zjus1s2_ml_predictions_revised.rds"))




# table 
all_results_IU %>% 
  group_by(model) %>% 
  summarize(correct_IU = sum(correct),
            pct_IU = correct_IU / n()) %>% 
  left_join(all_results_zjus1 %>% 
              group_by(model) %>% 
              summarize(correct_s1 = sum(correct),
                        pct_s1 = correct_s1 / n()),
            by = "model") %>% 
  left_join(all_results_zjus1s2 %>% 
              group_by(model) %>% 
              summarize(correct_s1s2 = sum(correct),
                        pct_s1s2 = correct_s1s2 / n()),
            by = "model") %>% 
  filter(model != "none_lreg") %>% 
  mutate(across(starts_with("pct"), ~sprintf("%.02f", round(.x, 2)))) %>% 
  mutate(model = c("K-Nearest Neighbors", 
                     "Neural Net", 
                     "Polynomial SVM", 
                     "Radial SVM", 
                     "BART", 
                     "Boosted Tree", 
                     "Flexible Discriminant", 
                     "Naive Bayes", 
                     "Penalized logistic regression", 
                     "Random Forest")) %>%
  arrange(desc(pct_s1s2)) %>% 
  kableExtra::kable(align = "lllll", booktabs  = TRUE,  format = "latex", col.names = 
                      c("Model", "Number Correct", "% Correct", "Number Correct", "% Correct",
                        "Number Correct", "% Correct")) %>% 
  kableExtra::add_header_above(c(" " = 1, "IU" = 2, "ZJU S1" = 2, "ZJU S1S2" = 2)) %>% 
  kableExtra::kable_styling(latex_options = "scale_down")  

# one model per person

data = readRDS(here::here("data", "grid_data_rw_zju_s1.rds")) %>% 
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

files = list.files(here::here("results_ml"), full.names = TRUE, pattern="predictions_zju_subject_multimodel*")

file_list = map(.x = files, 
                .f = function(x){readRDS(x) %>% 
                    group_by(subject) %>% 
                    mutate(row = row_number())})

all_files = bind_rows(file_list) 

x = all_files %>% 
  rename(predicted_sub = subject) %>% 
  select(-outcome, -.pred_class, -.pred_0, -id, -.row, -.config, -model) %>% 
  ungroup() %>% 
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

res = x %>%
  group_by(true_subject) %>%
  pivot_longer(cols = -c("true_subject", "second", "row")) %>%
  mutate(pred_sub = as.numeric(sub(".*sub\\_", "", name))) %>%
  rename(pred = value) %>%
  ungroup() %>%
  group_by(true_subject, pred_sub) %>%
  summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
  group_by(true_subject) %>%
  summarize(
    maxprob = first(max(mean_pred)),
    predicted_sub = first(pred_sub[mean_pred == maxprob]),
    probsubj = first(mean_pred[true_subject == pred_sub])
  ) %>%
  mutate(correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0))

# we want one row per second and "true subject" column 


all_results_zjus1 = map_dfr(.x = all_files, .f = process_results)

all_results_zjus1 %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))

all_results_zjus1 %>% 
  group_by(model) %>% 
  summarize(correct = sum(correct),
            pct = correct / n()) %>% 
  arrange(desc(pct))
# knn is second best 


knn = file_list %>% 
  bind_rows() %>% 
  filter(model == "all_knn")

knn = 
  knn %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "sub_") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(knn %>% ungroup() %>% select(starts_with("sub")))

# normalize and add "true subject column"
knn = knn %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("sub"), ~ .x / sum)) %>%
  dplyr::select(-sum) %>%
  ungroup()

knn %>%
  group_by(true_subject, model) %>%
  pivot_longer(cols = -c("true_subject", "second", "row", "model")) %>%
  mutate(pred_sub = as.numeric(sub(".*sub\\_", "", name))) %>%
  rename(pred = value) %>%
  ungroup() %>%
  group_by(true_subject, pred_sub, model) %>%
  summarize(mean_pred = mean(pred, na.rm = TRUE)) %>%
  group_by(true_subject) %>%
  mutate(
    rank = rank(-mean_pred)
  ) %>% 
  filter(pred_sub==true_subject) %>%
  mutate(
    rank1 = ifelse(rank == 1, 1, 0),
    rank5 = ifelse(rank <= 5, 1, 0)
  ) %>% 
  ungroup() %>% 
  summarize(n1 = sum(rank1),
            n5 = sum(rank5),
            acc1 =n1/n(),
            acc5 = n5/n())


# for knn, get in same format as before 
knn = file_list %>% 
  bind_rows() %>% 
  filter(model == "all_knn")
x = knn %>% 
  select(-outcome, -.pred_class, -.pred_0) %>% 
  pivot_wider(names_from = predicted_sub, values_from = .pred_1, names_prefix = "x") %>% 
  left_join(test_key, by = c("row"))
row_sums <- rowSums(x %>% ungroup() %>% select(starts_with("x")))

# normalize and add "true subject column"
x = x %>% 
  bind_cols(sum = row_sums) %>% 
  rowwise() %>%
  mutate(across(starts_with("x"), ~ .x / sum)) %>%
  ungroup() %>% 
  select(starts_with("x"), "true_subject")
saveRDS(x, here::here("predictions", "zjus1_ml_predictions_revised.rds"))


