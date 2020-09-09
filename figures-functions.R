# figure funtions

prepare_labels_facet <- function(data) {
  data <- data$mean %>%
    mutate(Model = case_when(model == "glmnet" & alpha == 0.5 ~ "EN",
                             model == "glmnet" & alpha == 1 ~ "Lasso",
                             model == "ss" & alpha == 0.5 ~ "SSEN",
                             model == "ss" & alpha == 1 ~ "SSL",
                             model == "ss_iar" & alpha == 0.5 ~ "SSEN (IAR)",
                             model == "ss_iar" & alpha == 1 ~ "SSL (IAR)",),
           N = case_when(N == "N25" ~ "N = 25",
                         N == "N50" ~ "N = 50",
                         N == "N100" ~ "N = 100"))
  data <- ungroup(data) %>%
    select(N, Model, grep("x.*", colnames(data))[-1])
  
  num_N <- length(unique(data$N))
  num_model <- length(unique(data$Model))
  
  only_estimates <- data %>% select(-N, -Model)
  only_N_Model <- data %>% select(N, Model) 
  only_estimates_vec <- as.vector(as.matrix(only_estimates))
  x.label <- rep(grep("x.*", colnames(data), value = T), each = num_N * num_model)
  data <- cbind(only_N_Model, only_estimates_vec, x.label) 
  
  mlabs <- c("Lasso" = "Lasso", "SSL" = "SS Lasso", "SSL (IAR)" = "SS Lasso (IAR)",
             "EN" = "Elastic Net", "SSEN" = "SS Elastic Net", "SSEN (IAR)" = "SS Elastic Net (IAR)")
  data$Model <- plyr::revalue(data$Model, mlabs)
  
  data$NF <- factor(data$N,levels = c("N = 25", "N = 50", "N = 100"))
  data$ModelF <- factor(data$Model, levels = c("Lasso", "SS Lasso", "SS Lasso (IAR)",
                                               "Elastic Net", "SS Elastic Net", "SS Elastic Net (IAR)"))
  
  return(data)
}
