library(tidyverse)

# given data sets of optimal models, these functions make summaries plots tables less painful

# (empirical) monte carlo standard error
mcse <- function(est) {
  ese <- sd(est)
  mcse <- ese / sqrt(2 * (length(est) - 1))
  return(mcse)
} 

# obtain summary measures by N, model, alpha
get_summaries <- function(data,           # data from optimal_ targets
                          summary_metric = "mean"  # mean, median, sd, IQR, mcse
                          ) 
  {
  
  # I can't seem to get any "unquoting" arguments to work so we have this inelegance instead...
  extract_metric <- function(x, summary_metric) {
    if (!(summary_metric %in% c("mean", "median", "sd", "IQR", "mcse"))) {
      stop("Invalid summary function. \n Pick one of mean , median, sd, IQR, mcse. \n")
    } else {
      if (summary_metric == "mean") {
        return(mean(x))
      }
      if (summary_metric == "median") {
        return(median(x))
      }
      if (summary_metric == "sd") {
        return(sd(x))
      }
      if (summary_metric == "IQR") {
        return(IQR(x))
      }
      if (summary_metric == "mcse") {
        return(mcse(x))
      }
    }
  }
  
  # group and nest
  data_gn <- data %>% 
    group_by(N, model, alpha) %>%
    nest() %>%
    mutate(metric = map(data, function(x) map_df(x, function(x) extract_metric(x, summary_metric = summary_metric)))) %>%
    unnest(cols = c(N, model, alpha, metric)) %>%
    select(-data)
  
  return(data_gn)
}

# uses get_summaries() for each summary measure in choose_summaries
# returns a list containing data frames corresponding to each summary
get_all_summaries <- function(data, choose_summaries = c("mean", "median", "sd", "IQR", "mcse")) {
  
  all_summaries <- list()
  
  for (i in 1:length(choose_summaries)) {
    all_summaries[[i]] <- get_summaries(data = data, summary_metric = choose_summaries[i])
  }
  
  names(all_summaries) <- choose_summaries
  
  return(all_summaries)
}

get_summaries_by_relevance <- function(data = NULL,
                                       index.type = "ellipse",
                                       w = 6, 
                                       h = 6,
                                       row.index = 10,
                                       col.index = 24,
                                       B.values = 0.1, 
                                       im.res = c(32, 32))
{
  # which parameters are relevant?
  betas <- sim2Dpredictr::beta_builder(index.type = index.type,
                        w = w, h = h,
                        row.index = row.index, col.index = col.index,
                        B.values = B.values, im.res = im.res)
  
  # parameter indices for relevant parameters
  ti <- betas$Truth.Indices
  
  # divide data into relevant/irrelevant parameters
  model_info_mean <- data$mean %>% 
    select(N, model, alpha)
  
  model_info_sd <- data$sd %>% 
    select(N, model, alpha)
  
  # get means
  
  mean_df <- data$mean[, grep("x.*", colnames(data$mean))[-1]]
  
  relevant_mean_df <- cbind(ungroup(model_info_mean), mean_df[, ti]) 
  relevant_mean_df <- relevant_mean_df %>% 
    mutate(est = rowMeans(select(relevant_mean_df, starts_with("x"))),
           metric = "Mean",
           ss = "slab") %>%
    select(N, model, alpha, metric, est, ss)
  
  
  irrelevant_mean_df <- cbind(ungroup(model_info_mean), mean_df[, -ti])
  irrelevant_mean_df <- irrelevant_mean_df %>% 
    mutate(est = rowMeans(select(irrelevant_mean_df, starts_with("x"))),
           metric = "Mean",
           ss = "spike") %>%
    select(N, model, alpha, metric, est, ss)
  
  # get sds
  
  sd_df <- data$sd[, grep("x.*", colnames(data$sd))[-1]]
  
  relevant_sd_df <- cbind(ungroup(model_info_sd), sd_df[, ti]) 
  relevant_sd_df <- relevant_sd_df %>% 
    mutate(est = rowMeans(select(relevant_sd_df, starts_with("x"))),
           metric = "SD",
           ss = "slab") %>%
    select(N, model, alpha, metric, est, ss)
  
  
  irrelevant_sd_df <- cbind(ungroup(model_info_sd), sd_df[, -ti])
  irrelevant_sd_df <- irrelevant_sd_df %>% 
    mutate(est = rowMeans(select(irrelevant_sd_df, starts_with("x"))),
           metric = "SD",
           ss = "spike") %>%
    select(N, model, alpha, metric, est, ss)
  
  out_df <- rbind(relevant_mean_df,
                  irrelevant_mean_df,
                  relevant_sd_df,
                  irrelevant_sd_df)
  out_df <- out_df[order(out_df$metric,
                         out_df$ss,
                         out_df$N,
                         out_df$alpha,
                         out_df$model), ] %>% 
    mutate(beta = B.values,
           Model = case_when(model == "glmnet" & alpha == 0.5 ~ "EN",
                             model == "glmnet" & alpha == 1 ~ "Lasso",
                             model == "ss" & alpha == 0.5 ~ "SSEN",
                             model == "ss" & alpha == 1 ~ "SSL",
                             model == "ss_iar" & alpha == 0.5 ~ "SSEN (IAR)",
                             model == "ss_iar" & alpha == 1 ~ "SSL (IAR)",),
           N = case_when(N == "N25" ~ 25,
                         N == "N50" ~ 50,
                         N == "N100" ~ 100))
  
  return(out_df)
}

# This function allows for scale estiates to be grouped rather than taking the mean
# of the sd of each parameter estimate. Right now, just mean/sd

divide_and_conquer <- function(data, relevant_id) {
  
  relevant <- as.matrix(data[ , relevant_id])
  irr <- as.matrix(data[, -relevant_id])
  relevant_mean <- mean(relevant)
  relevant_sd <- sd(relevant)
  irrelevant_mean <- mean(irr)
  irrelevant_sd <- sd(irr)
  
  df <- data.frame(relevant_mean = relevant_mean,
                   relevant_sd = relevant_sd,
                   irrelevant_mean = irrelevant_mean,
                   irrelevant_sd = irrelevant_sd)
  
  return(df)
  
}

get_beta_ests_by_relevance <- function(data = NULL,
                                       wide = TRUE,
                                       index.type = "ellipse",
                                       w = 6, 
                                       h = 6,
                                       row.index = 10,
                                       col.index = 24,
                                       B.values = 0.1, 
                                       im.res = c(32, 32)) 
{
  
  # which parameters are relevant?
  betas <- sim2Dpredictr::beta_builder(index.type = index.type,
                                       w = w, h = h,
                                       row.index = row.index, col.index = col.index,
                                       B.values = B.values, im.res = im.res)
  
  # parameter indices for relevant parameters
  ti <- betas$Truth.Indices
  
  # group nest mutate
  data <- data[, c("N", "model", "alpha", grep("x.*", colnames(data), value = TRUE)[-1])] %>%
    group_by(N, model, alpha) %>%
    nest() %>%
    mutate(summaries = map(data, function(x) divide_and_conquer(x, relevant_id = ti))) %>%
    unnest(cols = c(N, model, alpha, summaries)) %>%
    select(-data)
  
  if (wide == TRUE) {
    return(data)
  } else {
    relevant_mean_df <- data %>%
      select(N, model, alpha, relevant_mean) %>% 
      rename(est = relevant_mean) %>%
      mutate(metric = "Mean",
             ss = "slab")
    relevant_sd_df <- data %>% 
      select(N, model, alpha, relevant_sd) %>% 
      rename(est = relevant_sd) %>%
      mutate(metric = "SD",
             ss = "slab")
    irrelevant_mean_df <- data %>% 
      select(N, model, alpha, irrelevant_mean) %>% 
      rename(est = irrelevant_mean) %>%
      mutate(metric = "Mean",
             ss = "spike")
    irrelevant_sd_df <- data %>% 
      select(N, model, alpha, irrelevant_sd) %>% 
      rename(est = irrelevant_sd) %>%
      mutate(metric = "SD",
             ss = "spike")
    
    out_df <- rbind(relevant_mean_df, 
                    irrelevant_mean_df,
                    relevant_sd_df,
                    irrelevant_sd_df) 
    
    out_df <- out_df[order(out_df$metric,
                           out_df$ss,
                           out_df$N,
                           out_df$alpha,
                           out_df$model), ] %>% 
      mutate(beta = B.values,
             Model = case_when(model == "glmnet" & alpha == 0.5 ~ "EN",
                               model == "glmnet" & alpha == 1 ~ "Lasso",
                               model == "ss" & alpha == 0.5 ~ "SSEN",
                               model == "ss" & alpha == 1 ~ "SSL",
                               model == "ss_iar" & alpha == 0.5 ~ "SSEN (IAR)",
                               model == "ss_iar" & alpha == 1 ~ "SSL (IAR)",),
             N = case_when(N == "N25" ~ 25,
                           N == "N50" ~ 50,
                           N == "N100" ~ 100))
    
    return(out_df)
  }
  
}

# test <- get_beta_ests_by_relevance(optimal_B01)
# test2 <- get_beta_ests_by_relevance(optimal_B01, wide = FALSE, B.values = 0.1)



















