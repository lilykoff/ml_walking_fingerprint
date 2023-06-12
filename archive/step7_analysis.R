## goal - see if methods do similar thing w zju ppl 

get_summarized_predictions <- function(predictions, long = T){
  if(long == T){
    predictions %>%
      group_by(true_subject) %>%
      mutate(
        sec = row_number()) %>%
      pivot_longer(cols = -c("true_subject", "sec")) %>%
      mutate(
        model = as.numeric(sub(".*\\.{2,}", "", name))) %>%
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
        model = as.numeric(sub(".*\\.{2,}", "", name))) %>%
      rename(pred = value) %>%
      ungroup() %>%
      group_by(true_subject, model) %>%
      summarize(
        mean_pred = mean(pred)) %>%
      group_by(true_subject) %>%
      summarize(
        maxprob = max(mean_pred),
        predicted_sub = model[mean_pred==maxprob],
        probsubj = mean_pred[true_subject==model]) %>%
      mutate(
        correct = ifelse(as.numeric(predicted_sub) == true_subject, 1, 0)) 
  }
}
get_num_correct <- function(preds){
  summ <- get_summarized_predictions(preds, long = F)
  n_corr = sum(summ$correct)
  acc = n_corr/nrow(summ)
  total = nrow(summ)
  return(data.frame(corr = n_corr, acc = acc, total = total))
}


zju_logreg_s1 <- readRDS("~/Documents/ml_walking_fingerprint/Results/logistic/all_predictions_zju_logreg_s1.rds")
zju_logreg_s1s2 <- readRDS("~/Documents/ml_walking_fingerprint/Results/logistic/all_predictions_zju_logreg_s1s2.rds")

# functional 
expit <- function(x) 1/(1+exp(-x))

zju_func_s1 <- readRDS("~/Documents/ml_walking_fingerprint/Results/functional/preds_functional_reg_zju_s1_final.rds") %>%
  mutate(across(1:153, ~expit(.x)))
colnames(zju_func_s1) <- colnames(zju_logreg_s1)


## zjus1s2 


# ml 
zju_ml_s1s2 <- readRDS("~/Documents/ml_walking_fingerprint/Results/tidyml/zju/all_preds_corrected.rds")
zju_ml_s1 <- readRDS("~/Documents/ml_walking_fingerprint/Results/tidyml/zju/all_preds_s1_corrected.rds")


highest_probs_s1func <-
  get_summarized_predictions(zju_func_s1, long = F) %>%
  filter(probsubj > 0.5 & correct == 1) %>%
  dplyr::select(true_subject) %>%
  unlist() %>%
  unname()

highest_probs_s1log <-
  get_summarized_predictions(zju_logreg_s1, long = F) %>%
  filter(probsubj > 0.5 & correct == 1) %>%
  dplyr::select(true_subject) %>%
  unlist() %>%
  unname()


highest_probs_s1ml <-
  get_summarized_predictions(zju_ml_s1, long = F) %>%
  filter(probsubj > 0.5 & correct == 1) %>%
  dplyr::select(true_subject) %>%
  unlist() %>%
  unname()

sum(highest_probs_s1log %in% highest_probs_s1func)
sum(highest_probs_s1ml %in% highest_probs_s1func)
sum(highest_probs_s1log %in% highest_probs_s1ml)

lowest_probs_s1func <-
  get_summarized_predictions(zju_func_s1, long = F) %>%
  filter(correct == 0) %>%
  dplyr::select(true_subject) %>%
  distinct() %>%
  unlist() %>%
  unname()

lowest_probs_s1log <-
  get_summarized_predictions(zju_logreg_s1, long = F) %>%
  filter(correct == 0) %>%
  dplyr::select(true_subject) %>%
  distinct() %>%
  unlist() %>%
  unname()


lowest_probs_s1ml <-
  get_summarized_predictions(zju_ml_s1, long = F) %>%
  filter(correct == 0) %>%
  dplyr::select(true_subject) %>%
  distinct() %>%
  unlist() %>%
  unname()

sum(lowest_probs_s1func %in% lowest_probs_s1log)
sum(lowest_probs_s1func %in% lowest_probs_s1ml)
sum(lowest_probs_s1log %in% lowest_probs_s1ml)


## example of poorly predicted subject: 19 
## example of well predicted subject: 32 

## get 

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

train <-
  data_train %>% 
  dplyr::select(ID, second)

test <- 
  data_test %>%
  dplyr::select(ID, second)

df_all_zju <- read_csv("/Users/lilykoffman/Documents/ml_walking_fingerprint/df_all_zju.csv", 
                       col_types = cols(...1 = col_skip())) %>%
  filter(session == "session_1") %>% 
  mutate(
    ID = ID - 22
  )

df_zju_train <-
  df_all_zju %>% dplyr::select(ID, second, signal_rwrist) %>%
  right_join(train, by = c("ID" = "ID", "second" = "second"))

df_zju_test <-
  df_all_zju %>% dplyr::select(ID, second, signal_rwrist) %>%
  right_join(test, by = c("ID" = "ID", "second" = "second"))


onelag <- function(subject, lag, data){
    df_dens <- 
      data %>% 
      filter(ID==subject) %>% 
      dplyr::select(signal_rwrist, second) %>% 
      group_by(second) %>%
      mutate(
      lag_signal = lag(signal_rwrist, n = lag)) %>% 
      drop_na() %>% 
      mutate(
        lag = lag 
      )
    df_dens$density <- get_density(df_dens$signal_rwrist, df_dens$lag_signal, n=100)
    df_dens
}
get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}


library(viridis)
all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=32) 
trainimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
    scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
    theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
    scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) +
    scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                       minor_breaks = seq(0, 3, 0.25)) +
    theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                         y= "Acceleration from v(s) (g)",
                                         title = "Training Data, Subject 32")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=32) 
testimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Testing Data, Subject 32")

gridExtra::grid.arrange(trainimg, testimg, nrow=2)

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=19) 
trainimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Training Data, Subject 19")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=19) 
testimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Testing Data, Subject 19")

gridExtra::grid.arrange(trainimg, testimg, nrow=2)

## repeat with session 1 and session 2 data 

zju_ml_s1s2 <- readRDS("~/Documents/ml_walking_fingerprint/Results/tidyml/zju/all_preds_corrected.rds")


highest_probs_s1s2 <-
  get_summarized_predictions(zju_logreg_s1s2, long = F) %>%
  filter(probsubj > 0.5 & correct == 1) %>%
  dplyr::select(true_subject) %>%
  unlist() %>%
  unname()

highest_probs_s1log <-
  get_summarized_predictions(zju_logreg_s1, long = F) %>%
  filter(probsubj > 0.5 & correct == 1) %>%
  dplyr::select(true_subject) %>%
  unlist() %>%
  unname()


highest_probs_s1s2ml <-
  get_summarized_predictions(zju_ml_s1s2, long = F) %>%
  filter(probsubj > 0.5 & correct == 1) %>%
  dplyr::select(true_subject) %>%
  unlist() %>%
  unname()

## highest probs: 5, 79, 107 

lowest_probs_s1func <-
  get_summarized_predictions(zju_func_s1, long = F) %>%
  filter(correct == 0) %>%
  dplyr::select(true_subject) %>%
  distinct() %>%
  unlist() %>%
  unname()

lowest_probs_s1s2log <-
  get_summarized_predictions(zju_logreg_s1s2, long = F) %>%
  filter(correct == 0) %>%
  dplyr::select(true_subject) %>%
  distinct() %>%
  unlist() %>%
  unname()


lowest_probs_s1s2ml <-
  get_summarized_predictions(zju_ml_s1s2, long = F) %>%
  filter(correct == 0) %>%
  dplyr::select(true_subject) %>%
  distinct() %>%
  unlist() %>%
  unname()

lowest_probs_s1s2ml[lowest_probs_s1s2ml %in% lowest_probs_s1s2log]

# 3,9,10,13,17, 110 

## get 
df_all_zju <- read_csv("/Users/lilykoffman/Documents/ml_walking_fingerprint/df_all_zju.csv", 
                       col_types = cols(...1 = col_skip())) %>%
  mutate(
    ID = ID - 22
  )
df_zju_train <-
  df_all_zju %>% 
  filter(session == "session_1") 

df_zju_test <-
  df_all_zju %>% 
  filter(session == "session_2")



all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=5) 
trainimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 1, Subject 5")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=5) 
testimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 2, Subject 5")

gridExtra::grid.arrange(trainimg, testimg, nrow=2)

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=79) 
trainimg2 <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 1, Subject 79")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=79) 
testimg2 <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 2, Subject 79")

gridExtra::grid.arrange(trainimg, testimg, nrow=2)

gridExtra::grid.arrange(trainimg, trainimg2, 
                        testimg, testimg2)

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=107) 
trainimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 1, Subject 107")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=107) 
testimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 2, Subject 107")

gridExtra::grid.arrange(trainimg, testimg, nrow=2)


all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=3) 
trainimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 1, Subject 3")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=3) 
testimg <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 2, Subject 3")

gridExtra::grid.arrange(trainimg, testimg, nrow=2)

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_train, subject=136) 
trainimg2 <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 1, Subject 136")

all_densities <- map_dfr(.x = seq(15,45,15), .f = onelag, data = df_zju_test, subject=136) 
testimg2 <- 
  all_densities %>% ggplot(aes(x = lag_signal, y = signal_rwrist, col = density))+geom_point() + 
  scale_color_viridis(name = "Density", limits=c(0, max(all_densities$density)))+
  theme(legend.title=element_text(size=9), legend.text=element_text(size=7)) +
  scale_x_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  scale_y_continuous(breaks = seq(0, 3, by = 0.5), limits = c(0,3),
                     minor_breaks = seq(0, 3, 0.25)) +
  theme_bw() + facet_grid(.~lag)+ labs(x = "Acceleration from v(s-u) (g)", 
                                       y= "Acceleration from v(s) (g)",
                                       title = "Session 2, Subject 136")

gridExtra::grid.arrange(trainimg2, testimg2, nrow=2)

gridExtra::grid.arrange(trainimg, trainimg2, 
                        testimg, testimg2)

