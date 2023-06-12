## process results of functional models - just session 1 
preds_functional_reg_zju_s1 <- readRDS("~/Documents/ml_walking_fingerprint/Results/functional/preds_functional_reg_zju_s1_final.rds")
library(tidyverse)
library(mgcv)
library(readr)
options(dplyr.summarise.inform=FALSE)
# just use session 1; right wrist 
df_all_zju <- read_csv("df_all_zju.csv", 
                       col_types = cols(...1 = col_skip())) %>%
  filter(session == "session_1") %>% 
  mutate(
    ID = ID - 22
  )



get_functional_data <- function(df, tlen){
  uid <- unique(df$ID)
  nid <- length(uid)
  
  nk <- (tlen*100)*(tlen*100-1)/2
  ji <- df %>% 
    group_by(ID) %>%
    summarize(
      maxtime = max(second)
    ) %>% 
    dplyr::select(maxtime) %>%
    unlist() %>%
    unname() 
  
  N <- sum(ji)
  smat_w <- umat_w <- dmat <- matrix(NA, ncol=nk, nrow=N) # matrix of NAs 
  id_vec <- time_g_vec <- rep(NA, N)
  
  inx_samp <- lapply(1:(100*tlen-1), function(x){
    ret <- data.frame("u"=x,"s"=(x+1):(100*tlen))
    ret
  })
  inx_samp <- bind_rows(inx_samp)
  inx <- 1
  
  for(i in seq_along(uid)){
    df_i <- filter(df, ID == i)
    j_i    <- unique(df_i$second)
    for(j in seq_along(j_i)){
      Y_ij <- df_i$signal_rwrist[df_i$second == j_i[j]]
      umat_w[inx,] <- Y_ij[inx_samp$u]
      smat_w[inx,] <- Y_ij[inx_samp$s]
      dmat[inx,] <- inx_samp$s-inx_samp$u
      time_g_vec[inx] <- ji[j]
      id_vec[inx] <- df_i$ID[1]
      inx <- inx + 1
    }
  }
  
  df_fit <- data.frame("ID" = id_vec, 
                       "umat" = I(umat_w),
                       "smat" = I(smat_w),
                       "dmat" = I(dmat),
                       "lmat" = I(matrix(1/nk, ncol=nk, nrow=N)))
  rm(list=c("smat_w","umat_w","dmat"))
  gc()
  
  
  df_fit <- df_fit %>% 
    group_by(ID) %>% 
    dplyr::mutate(J = 1:n()) %>% 
    ungroup() 
  
  df_fit 
}

df_fit_zju <- get_functional_data(df_all_zju, tlen = 1)

ji <- df_all_zju %>% 
  group_by(ID) %>%
  summarize(
    maxtime = max(second)
  ) %>% 
  dplyr::select(maxtime) %>%
  unlist() %>%
  unname() 
min(ji)

set.seed(222)
train_ind <- sample(seq(1:min(ji)), min(ji)*.75, replace = F)
test_ind <- seq(1:min(ji))[-train_ind]

df_train   <- subset(df_fit_zju, J %in% train_ind)
df_test    <- subset(df_fit_zju,  J %in% test_ind)

all_preds <- data.frame(preds_functional_reg_zju_s1)
expit <- function(x) 1/(1+exp(-x))
all_preds <- expit(all_preds)
rowsums <- rowSums(all_preds)

all_preds_normalized <- 
  all_preds %>%
  bind_cols(sum = rowsums) %>% 
  rowwise() %>% 
  mutate(across(X1:X153, ~.x/sum)) %>%
  ungroup()

all_preds$true_subject <- df_test$ID
all_preds_normalized$true_subject <- df_test$ID

# predictions <- all_preds
expit <- function(x) 1/(1+exp(-x))

  
  
get_summarized_predictions <- function(predictions, long = T){
  if(long == T){
    predictions %>%
      group_by(true_subject) %>%
      mutate(
        sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(
        model = as.numeric(sub(".*X", "", name)),
        value = expit(value)) %>%
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
        model = as.numeric(sub(".*X", "", name)),
        value = expit(value)) %>%
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
get_summarized_predictions <- function(predictions, long = T){
  if(long == T){
    predictions %>%
      group_by(true_subject) %>%
      mutate(
        sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(
        model = as.numeric(sub(".*X", "", name))) %>%
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
        model = as.numeric(sub(".*X", "", name))) %>%
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

summ_preds <- get_summarized_predictions(all_preds, long=F)
sum(summ_preds$correct)

summ_preds <- get_summarized_predictions(all_preds_normalized %>% 
                                           dplyr::select(-sum), long=F)
sum(summ_preds$correct)



# fn takes number of seconds to average over as input, outputs classification stats 
get_prediction_stats <- function(predictions, seconds){
  predictions %>% 
    group_by(true_subject) %>% 
    mutate(
      sec = floor(row_number()/seconds)) %>% 
    pivot_longer(cols = -c("true_subject", "sec")) %>%
    mutate(
      model = as.numeric(sub(".*X", "", name)),
      value = expit(value)) %>%
    rename(pred = value) %>%
    ungroup() %>%
    group_by(true_subject, model, sec) %>%
    summarize(
      mean_pred = mean(pred)) %>%
    group_by(true_subject, sec) %>%
    summarize(
        maxprob = first(max(mean_pred)),
        predicted_subject = first(model[mean_pred==maxprob])
    ) %>% ungroup() %>% 
    dplyr::select(c(true_subject, predicted_subject)) %>% 
    mutate(across(1:2, factor, levels=seq(1, 153, 1))) %>%
    yardstick::conf_mat(., truth = true_subject, estimate = predicted_subject) %>%
    summary() %>% 
    mutate(
      s = seconds
    )
}

results_summarized <-
  map_dfr(.x = seq(1,38, 5),
          .f = get_prediction_stats,
          predictions = all_preds,
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
  scale_x_continuous(breaks=c(1, seq(10, 100, 10)))+
  theme(axis.text.x = element_text(angle=45))+
  theme(legend.position = "none")


### session 2 


preds_functional_reg_zju_s2 <- readRDS("~/Documents/ml_walking_fingerprint/preds_functional_reg_zju_s2.rds")

# just use session 1; right wrist 
df_all_zju <- read_csv("df_all_zju.csv", 
                       col_types = cols(...1 = col_skip())) %>%
  filter(session == "session_2") %>% 
  mutate(
    ID = ID - 22
  )


df_fit_zju <- get_functional_data(df_all_zju, tlen = 1)

ji <- df_all_zju %>% 
  group_by(ID) %>%
  summarize(
    maxtime = max(second)
  ) %>% 
  dplyr::select(maxtime) %>%
  unlist() %>%
  unname() 
min(ji)

set.seed(222)
train_ind <- sample(seq(1:min(ji)), min(ji)*.75, replace = F)
test_ind <- seq(1:min(ji))[-train_ind]

df_train   <- subset(df_fit_zju, J %in% train_ind)
df_test    <- subset(df_fit_zju,  J %in% test_ind)

all_preds <- data.frame(preds_functional_reg_zju_s2)
all_preds$true_subject <- df_test$ID
predictions <- all_preds

summ_preds <- get_summarized_predictions(all_preds, long=F)
sum(summ_preds$correct[!is.na(summ_preds$correct)])


# fn takes number of seconds to average over as input, outputs classification stats 

results_summarized <-
  map_dfr(.x = seq(1,38, 5),
          .f = get_prediction_stats,
          predictions = all_preds,
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
  scale_x_continuous(breaks=c(1, seq(10, 100, 10)))+
  theme(axis.text.x = element_text(angle=45))+
  theme(legend.position = "none")
