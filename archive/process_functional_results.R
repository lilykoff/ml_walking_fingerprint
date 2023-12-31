library(tidyverse)
library(mgcv)
library(readr)
library(magrittr)
expit <- function(x) 1/(1+exp(-x))
`%notin%` <- Negate(`%in%`)
df_all_IU <- read_csv("df_all_IU.csv", 
                      col_types = cols(...1 = col_skip())) %>%
  rename(ID = ID2,
         signal_lwrist = signal_lw,
         signal_lhip = signal_lh,
         signal_lankle = signal_la,
         signal_rankle = signal_ra)

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
      Y_ij <- df_i$signal_lwrist[df_i$second == j_i[j]]
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

df_fit_IU <- get_functional_data(df_all_IU, tlen = 1)
data_split <- split(df_fit_IU, f = df_fit_IU$ID)

# function to sample percentage 
samp <- function(pct, n, ind) {
  set.seed(ind)
  sample(n, floor(pct * n), replace = F)
}
ids <- unique(df_fit_IU$ID)
# number of rows for each individual 
rows <- lapply(data_split, nrow) %>% unlist()
# get random 75% of seconds for training 
train_indices <- map2(pct = .75,
                      .x = rows,
                      .y = ids,
                      .f = samp)

getrows <- function(data, rows) {
  subset(data, J %in% rows)
}
getrows_test <- function(data, rows) {
  subset(data, J %notin% rows)
}

data_train <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows)
data_test <-
  map2_dfr(.x = data_split, .y = train_indices, .f = getrows_test)


# read in predictions and get back on probability scale 
preds_IU <- 
  readRDS("~/Documents/ml_walking_fingerprint/preds_funtional_reg_IU_v2.rds") %>% 
  data.frame() %>% 
  expit()

# normalize probabilities - get rowsums 
row_sums <- rowSums(preds_IU)

# normalize and add "true subject column" 

preds_IU %<>%
  bind_cols(sum = row_sums) %>%
  rowwise() %>%
  mutate(across(X1:X32, ~.x/sum)) %>%
  dplyr::select(-sum) %>%
  ungroup() %>%
  bind_cols(true_subject = data_test$ID)

# function to get prediction summaries 
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

# accuracy 
get_summarized_predictions(preds_IU, long = F) %>% print(n = Inf)

# perfectly predicted 

get_accuracies <- function(predictions, seconds){
  tmp <- 
    predictions %>%
    group_by(true_subject) %>% 
    mutate(
      sec = floor(row_number()/seconds)) %>% 
    pivot_longer(cols = -c("true_subject", "sec")) %>%
    mutate(
      model = as.numeric(sub(".*X", "", name))) %>%
    rename(pred = value) %>%
    ungroup() %>%
    group_by(true_subject, model, sec) %>%
    summarize(
      mean_pred = mean(pred)) %>%
    group_by(true_subject, sec) %>%
    mutate(
      rank = rank(-mean_pred)
    ) %>% 
    filter(model==true_subject) %>%
    mutate(
      rank1 = ifelse(rank == 1, 1, 0),
      rank5 = ifelse(rank <= 5, 1, 0)
    )
  n = nrow(tmp)
  r1 = sum(tmp$rank1)/n
  r5 = sum(tmp$rank5)/n
  return(data.frame(metric = c("rank1", "rank5"),
                    value = c(r1, r5),
                    s = c(seconds, seconds)))
  
}


secs <- c(1,2,5,10,15,20,25,30,35,50,60,70,80,90,100)
stats_fnl <- 
  map_dfr(.x = secs,
          .f = get_accuracies,
          predictions = preds_IU) %>%
  mutate(
    model = "functional"
  )


stats_fnl %>% 
  ggplot(aes(x = s, y = value, col = model))+
  geom_line()+
  theme_light()+
  facet_wrap(.~metric, nrow = 2)+
  labs(x = "Number of Seconds", y = "Estimate", title = "Classification Metrics",
       subtitle = "Averaged over Varying Number of Seconds")+
  scale_x_continuous(breaks=c(1,2,5,10,15,20,25,30,35,50,60,70,80,90,100))+
  theme(axis.text.x = element_text(angle=45, size = 4))+
  scale_color_brewer(palette = "Dark2", labels = c("Functional", "Logistic", "ML"), name = "Model")
