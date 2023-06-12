rm(list=ls())
library(tidyverse)
library(purrr)
library(tidymodels)
tidymodels_prefer()
options(dplyr.summarise.inform=FALSE)

## read in data 
grid_data_lw_IU <- readRDS("~/Documents/ml_walking_fingerprint/grid_data_lw_IU.rds")
df_all_IU <- read_csv("df_all_IU.csv", 
                      col_types = cols(...1 = col_skip())) %>%
  rename(ID = ID2,
         signal_lwrist = signal_lw,
         signal_lhip = signal_lh,
         signal_lankle = signal_la,
         signal_rankle = signal_ra)


get_train_test <- function(pct, data){
  data_split <- split(data, f = data$ID)
  
  samp <- function(pct, n, ind) {
    set.seed(ind)
    sample(n, floor(pct * n), replace = F)
  }
  ids <- unique(data$ID)
  # number of rows for each individual 
  rows <- lapply(data_split, nrow) %>% unlist()
  # get random 75% of seconds for training 
  train_indices <- map2(pct = pct,
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
  return(list(train = data_train, test = data_test))
}

data_train <- get_train_test(pct = .75, data = grid_data_lw_IU)$train
data_test <- get_train_test(pct = .75, data = grid_data_lw_IU)$test

plot_sig_cells <- function(train, test, sub){
  
  # first we want to remove columns with near zero variance 
  nzv_trans <- 
    recipe(ID ~ ., data = data_train) %>% 
    step_nzv(all_predictors())
  
  nzv_estimates <- prep(nzv_trans)
  
  nzv <- colnames(juice(nzv_estimates))
  dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv), -second)
  dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv), -second)
  
  train <- dat_nzv
  train$class <- ifelse(train$ID == sub, 1, 0)
  tmp <- train %>% dplyr::select(-c(ID)) 
  mod <- glm(class ~ ., data = tmp, family=binomial(link="logit"))
  
  
  summary <-
    mod %>% 
    tidy() %>%
    arrange(p.value) %>%
    filter(term != "(Intercept)") 
  
  terms <- summary$term
  # variance covariance matrix 
  v_hat <- vcov(mod)[terms, terms]
  a <- 1/(sqrt(diag(v_hat))) 
  A <- a %*% t(a)
  C <- v_hat*A
  
  q <- mvtnorm::qmvnorm(p = .95, corr = C)$quantile
  
  summary <-
    summary %>% rowwise() %>%
    mutate(
      lb_marg = estimate - (1.96 * std.error),
      ub_marg = estimate + (1.96 * std.error),
      lb_joint = estimate - (q * std.error),
      ub_joint = estimate +  (q * std.error),
      sig_marg = ifelse(between(0, lb_marg, ub_marg), 0, 1),
      sig_joint = ifelse(between(0, lb_joint, ub_joint), 0, 1),
    )
  summary <- 
    summary %>% rowwise() %>% 
    mutate(
      lag = str_sub(term, -3, -2),
      u = sub('.', '', str_split(term, " ")[[1]][1]),
      s = str_split(term, " ")[[1]][2]
    )
  
  if(nrow(summary %>% filter(sig_marg == 1)) == 0){
    marg <- NULL
  }
  if(nrow(summary %>% filter(sig_joint == 1)) == 0){
    joint <- NULL
  }
  else{
    joint <- summary %>% mutate(
      est = ifelse(sig_joint == 1, exp(estimate), NA)) %>% 
      ggplot(aes(
        x = factor(
          u,
          levels = c(
            "[0,0.25]",
            "(0.25,0.5]",
            "(0.5,0.75]" ,
            "(0.75,1]" ,
            "(1,1.25]" ,
            "(1.25,1.5]",
            "(1.5,1.75]",
            "(1.75,2]" ,
            "(2,2.25]",
            "(2.25,2.5]",
            "(2.5,2.75]"
          )
        ),
        y = factor(
          s,
          levels = c(
            "[0,0.25]",
            "(0.25,0.5]",
            "(0.5,0.75]" ,
            "(0.75,1]" ,
            "(1,1.25]" ,
            "(1.25,1.5]",
            "(1.5,1.75]",
            "(1.75,2]" ,
            "(2,2.25]",
            "(2.25,2.5]",
            "(2.5,2.75]"
          )
        ),
        fill = est
      )) + geom_tile() + facet_grid(. ~ lag) + theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) + 
      scale_fill_gradientn(colours=c("blue","yellow","red"),na.value="white", name = "Estimate")+
      labs(title = paste("Subject", sub, "Jointly Significant Grid Cells"),
           x = "Signal",
           y = "Lag Signal")+theme(legend.position = "bottom")
    
    marg <- summary %>% mutate(
      est = ifelse(sig_marg == 1, exp(estimate), NA)) %>% 
      ggplot(aes(
        x = factor(
          u,
          levels = c(
            "[0,0.25]",
            "(0.25,0.5]",
            "(0.5,0.75]" ,
            "(0.75,1]" ,
            "(1,1.25]" ,
            "(1.25,1.5]",
            "(1.5,1.75]",
            "(1.75,2]" ,
            "(2,2.25]",
            "(2.25,2.5]",
            "(2.5,2.75]"
          )
        ),
        y = factor(
          s,
          levels = c(
            "[0,0.25]",
            "(0.25,0.5]",
            "(0.5,0.75]" ,
            "(0.75,1]" ,
            "(1,1.25]" ,
            "(1.25,1.5]",
            "(1.5,1.75]",
            "(1.75,2]" ,
            "(2,2.25]",
            "(2.25,2.5]",
            "(2.5,2.75]"
          )
        ),
        fill = est
      )) + geom_tile() + facet_grid(. ~ lag) + theme_classic() +
      theme(axis.text.x = element_text(angle = 45, vjust = .5)) + 
      scale_fill_gradientn(colours=c("blue","yellow","red"),na.value="white", name = "Estimate")+
      labs(title = paste("Subject", sub, "Marginally Significant Grid Cells"),
           x = "Signal",
           y = "Lag Signal")+theme(legend.position = "bottom")
  }
  gridExtra::grid.arrange(joint, marg)
}

plot_sig_cells(train = data_train, test = data_test, sub = 1)


plot_w_points <- function(train, test, sub, raw_data){
  nzv_trans <- 
    recipe(ID ~ ., data = data_train) %>% 
    step_nzv(all_predictors())
  
  nzv_estimates <- prep(nzv_trans)
  
  nzv <- colnames(juice(nzv_estimates))
  dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv), -second)
  dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv), -second)
  
  train <- dat_nzv
  train$class <- ifelse(train$ID == sub, 1, 0)
  tmp <- train %>% dplyr::select(-c(ID)) 
  mod <- glm(class ~ ., data = tmp, family=binomial(link="logit"))
  
  
  summary <-
    mod %>% 
    tidy() %>%
    arrange(p.value) %>%
    filter(term != "(Intercept)") 
  
  terms <- summary$term
  # variance covariance matrix 
  v_hat <- vcov(mod)[terms, terms]
  a <- 1/(sqrt(diag(v_hat))) 
  A <- a %*% t(a)
  C <- v_hat*A
  
  q <- mvtnorm::qmvnorm(p = .95, corr = C)$quantile
  
  summary <- 
    summary %>% rowwise() %>% 
    mutate(
      lb_marg = estimate - (1.96*std.error),
      ub_marg = estimate + (1.96*std.error),
      lb_joint = estimate - (q*std.error),
      ub_joint = estimate +  (q*std.error),
      sig_marg = ifelse(between(0, lb_marg, ub_marg), 0, 1),
      sig_joint = ifelse(between(0, lb_joint, ub_joint), 0, 1),
    )
  summary <- 
    summary %>% rowwise() %>% 
    mutate(
      lag = str_sub(term, -3, -2),
      u = sub('.', '', str_split(term, " ")[[1]][1]),
      s = str_split(term, " ")[[1]][2]
    )
  
  train_times <-
    data_train %>%
    dplyr::select(ID, second) %>%
    filter(ID == sub)
  
  
  pts_dat <- 
   raw_data%>% inner_join(., train_times, by = c("ID" = "ID", "second" = "second")) %>%
    group_by(ID, second) %>% mutate(
      u = signal_lwrist, 
      s_1 = dplyr::lag(u, 15),
      s_2 = dplyr::lag(signal_lwrist, 30),
      s_3 = dplyr::lag(signal_lwrist, 45)
    ) %>% dplyr::select(u, s_1, s_2, s_3, ID, second) %>% 
    pivot_longer(s_1:s_3) %>% filter(!is.na(value)) %>% 
    mutate(lag = case_when(
      name == "s_1" ~ 15,
      name == "s_2" ~ 30,
      name== "s_3" ~ 45,
    ),
    xmin = u, 
    xmax = u,
    ymin = value,
    ymax = value) 
  
  g <- summary %>% 
    mutate(
      est = ifelse(sig_joint == 1, exp(estimate), NA)) %>% 
    mutate(
      xmin = as.numeric(sub(".*\\(", "", sub(",.*", "", u))),
      xmax = as.numeric(str_sub(sub(".*,", "", u), end = -2)),
      ymin =  as.numeric(sub(".*\\(", "", sub(",.*", "", s))),
      ymax = as.numeric(str_sub(sub(".*,", "", s), end = -2))
    ) %>% ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))+geom_rect(aes(fill = est)) +
    facet_wrap(.~lag)+scale_fill_gradientn(colours=c("blue","yellow","red"),na.value="white", name = "Estimate")+
    labs(title = paste("Subject", sub, "Significant Grid Cells"),
         x = "Signal" ,
         y = "Lag Signal")+
    scale_x_continuous(limits=c(0,3))+scale_y_continuous(limits=c(0,3))+
    theme_classic()+
    theme(legend.position = "bottom")
  
  g + geom_point(data = pts_dat, aes(x = u, y = value), col = "black", 
                 alpha = 0.01, size = .4)
  
}




plot_w_points(train = data_train, test = data_test, sub = 1, raw_data = df_all_IU)
plot_w_points(train = data_train, test = data_test, sub = 2, raw_data = df_all_IU)


## apply w ZJU data 

grid_data_rw_zju_s1 <- readRDS("~/Documents/ml_walking_fingerprint/grid_data_rw_zju_s1.rds")
grid_data_rw_zju_s1$ID <- grid_data_rw_zju_s1$ID - 22
df_all_zju <- read_csv("df_all_zju.csv", 
                      col_types = cols(...1 = col_skip()))

data_train <- get_train_test(pct = .75, data = grid_data_rw_zju_s1)$train
data_test <- get_train_test(pct = .75, data = grid_data_rw_zju_s1)$test

plot_sig_cells(train = data_train, test = data_test, sub = 110)

plot_w_points_zju <- function(train, test, sub, raw_data){
  nzv_trans <- 
    recipe(ID ~ ., data = data_train) %>% 
    step_nzv(all_predictors())
  
  nzv_estimates <- prep(nzv_trans)
  
  nzv <- colnames(juice(nzv_estimates))
  dat_nzv <- data_train %>% dplyr::select(ID, all_of(nzv), -second)
  dat_nzv_test <- data_test %>% dplyr::select(ID, all_of(nzv), -second)
  
  train <- dat_nzv
  train$class <- ifelse(train$ID == sub, 1, 0)
  tmp <- train %>% dplyr::select(-c(ID)) 
  mod <- glm(class ~ ., data = tmp, family=binomial(link="logit"))
  
  
  summary <-
    mod %>% 
    tidy() %>%
    arrange(p.value) %>%
    filter(term != "(Intercept)") 
  
  terms <- summary$term
  # variance covariance matrix 
  v_hat <- vcov(mod)[terms, terms]
  a <- 1/(sqrt(diag(v_hat))) 
  A <- a %*% t(a)
  C <- v_hat*A
  
  q <- mvtnorm::qmvnorm(p = .95, corr = C)$quantile
  
  summary <- 
    summary %>% rowwise() %>% 
    mutate(
      lb_marg = estimate - (1.96*std.error),
      ub_marg = estimate + (1.96*std.error),
      lb_joint = estimate - (q*std.error),
      ub_joint = estimate +  (q*std.error),
      sig_marg = ifelse(between(0, lb_marg, ub_marg), 0, 1),
      sig_joint = ifelse(between(0, lb_joint, ub_joint), 0, 1),
    )
  summary <- 
    summary %>% rowwise() %>% 
    mutate(
      lag = str_sub(term, -3, -2),
      u = sub('.', '', str_split(term, " ")[[1]][1]),
      s = str_split(term, " ")[[1]][2]
    )
  
  if(nrow(summary %>% filter(sig_joint==1)) == 0){
    return(NULL)
  }
  else{
    train_times <-
      data_train %>%
      dplyr::select(ID, second) %>%
      filter(ID == sub)
    
    
    pts_dat <- 
      raw_data%>% inner_join(., train_times, by = c("ID" = "ID", "second" = "second")) %>%
      group_by(ID, second) %>% mutate(
        u = signal_rwrist, 
        s_1 = dplyr::lag(u, 15),
        s_2 = dplyr::lag(signal_rwrist, 30),
        s_3 = dplyr::lag(signal_rwrist, 45)
      ) %>% dplyr::select(u, s_1, s_2, s_3, ID, second) %>% 
      pivot_longer(s_1:s_3) %>% filter(!is.na(value)) %>% 
      mutate(lag = case_when(
        name == "s_1" ~ 15,
        name == "s_2" ~ 30,
        name== "s_3" ~ 45,
      ),
      xmin = u, 
      xmax = u,
      ymin = value,
      ymax = value) 
    
    g <- summary %>% 
      mutate(
        est = ifelse(sig_joint == 1, exp(estimate), NA)) %>% 
      mutate(
        xmin = as.numeric(sub(".*\\(", "", sub(",.*", "", u))),
        xmax = as.numeric(str_sub(sub(".*,", "", u), end = -2)),
        ymin =  as.numeric(sub(".*\\(", "", sub(",.*", "", s))),
        ymax = as.numeric(str_sub(sub(".*,", "", s), end = -2))
      ) %>% ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))+geom_rect(aes(fill = est)) +
      facet_wrap(.~lag)+scale_fill_gradientn(colours=c("blue","yellow","red"),na.value="white", name = "Estimate")+
      labs(title = paste("Subject", sub, "Significant Grid Cells"),
           x = "Signal" ,
           y = "Lag Signal")+
      scale_x_continuous(limits=c(0,3))+scale_y_continuous(limits=c(0,3))+
      theme_classic()+
      theme(legend.position = "bottom")
    
    g + geom_point(data = pts_dat, aes(x = u, y = value), col = "black", 
                   alpha = 0.01, size = .4)
  }
}

plot_w_points_zju(data_train, data_test, sub = 110, raw_data = df_all_zju %>% mutate(ID = ID - 22))
