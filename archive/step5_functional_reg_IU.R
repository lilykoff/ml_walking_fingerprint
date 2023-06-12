## script to run functional regression 
## have to run on the cluster
## output: predictions 
# need to use same train/test split of other data 

### get data in functional format 
library(tidyverse)
library(mgcv)
library(readr)

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


ji <- df_all_IU %>% 
  group_by(ID) %>%
  summarize(
    maxtime = max(second)
  ) %>% 
  dplyr::select(maxtime) %>%
  unlist() %>%
  unname() 
min(ji)

set.seed(211)
train_ind <- sample(seq(1:min(ji)), min(ji)*.75, replace = F)
test_ind <- seq(1:min(ji))[-train_ind]

df_train   <- subset(df_fit_IU, J %in% train_ind)
df_test    <- subset(df_fit_IU,  J %in% test_ind)
tmp <- df_train$umat[df_train$ID==1]

fit_functional_model <- function(train, test){
  
  uid        <- unique(train$ID)
  nid        <- length(uid)
  
  lpmat <- matrix(NA, ncol=nid, nrow=nrow(test)) # to store predictions 
  expit <- function(x) 1/(1+exp(-x))
  for(i in 1:nid){
    train$Y <- as.numeric(train$ID == uid[i])
    fit_i <- gam(Y ~ te(umat, smat, dmat, by=lmat), method="REML", data=train, family=quasibinomial()) 
    lp_i <- predict.gam(fit_i, newdata = test, type = "link")
    lpmat[,i] <- unname(lp_i)
    rm(list=c("fit_i","lp_i"))
    train$Y <- NULL
    print(i)
  }
  lpmat
}

preds <- fit_functional_model(df_train, df_test)

saveRDS(preds, "preds_funtional_reg_IU.rds")

