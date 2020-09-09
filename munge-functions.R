library(tidyverse)

# These files munge the data to be ready for analysis
#     NOTE: many of these take arguments that depend on file names.
#           The whole idea is to use regular expressions to make the process easier.

# get combinations of priors and elastic net parameter 
get_ssnet_levels <- function(priors = c("glmnet", "ssnet", "ssnet-iar"), # prior names
                             en_levels = c("a1", "a05") # elastic net names
                             )
  {
  
  ssnet_levels <- expand.grid(priors = priors, en_levels = en_levels) %>%
    mutate(ssnet_levels = paste0(priors, "_", en_levels)) %>%
    select(ssnet_levels)
  
  return(ssnet_levels$ssnet_levels)
}


# combinations of prior specs and sample sizes with output being regular expressions
# we'll use to extract several relevant files at a time
# RETURNS: data.frame
#     1. names column is used later to split data
#     2. ssnet_levels_N column is a regular expression used to find each 
#        batch of files.
get_ssnet_levels_N <- function(ssnet_levels,                      # build with get_ssnet_levels
                               N_levels = c("N25", "N50", "N100") # sample sizes
                               ) 
  {
  
  ssnet_levels_N <- expand.grid(ssnet_levels = ssnet_levels,
                                N_levels = N_levels) %>%
                    mutate(ssnet_levels_N = paste0(ssnet_levels, ".*.", N_levels),
                           ssnet_names = paste0(ssnet_levels, "_", N_levels)) %>%
                    select(ssnet_names, ssnet_levels_N)
  
  return(ssnet_levels_N)
}

get_file_names <- function(directory = NULL,             # directory where files live
                           ssnet_levels_N = NULL,        # a column from get_ssnet_levels_N (see above)
                           ssnet_names = NULL,           # a column from get_ssnet_levels_N (see above)
                           N_files_each = NULL,          # data.frame containing columns for
                                                         #    1. ssnet_levels_N
                                                         #    2. expected number of files for each ssnet_levels_N
                           verbose = FALSE               # print intermediate information?
                           )
  {
  
  if(is.null(directory) | is.null(ssnet_levels_N)) {
    stop("Must specify a directory and ssnet_levels_N.")
  }
  
  if(is.null(N_files_each)) {
    warning("When N_files_each is NULL cannot check for unexpected missing files.")
  }
  
  if(length(ssnet_levels_N) != length(ssnet_names)) {
    stop("ssnet_levels_N and ssnet_names must be the same length")
  }
  
  # obtain file names
  file_names <- list() 
  all_files <- list.files(directory)
  for (i in 1:length(ssnet_levels_N)) {
    file_names[[i]] <- grep(ssnet_levels_N[i], all_files, value = TRUE)
  }
  names(file_names) <- ssnet_names
  
  # check that the number of files matches expected
  file_counts <- data.frame(ssnet_levels_N = ssnet_levels_N,
                            num_files = purrr::map_int(.x = file_names,
                                                       .f = length),
                            row.names = NULL)
  if (verbose) {
    print(file_counts)
  }
  
  if (!is.null(N_files_each)) {
    if(any(file_counts$num_files != N_files_each$num_files_expected)) {
      id <- which(file_counts$num_files != N_files_each$num_files_expected)
      warning(paste(N_files_each$ssnet_levels_N[id], "has ", file_counts$num_files[id], 
                     "files, but expected number of files is ",
                     N_files_each$num_files_expected[id], "\n"))
    }
  }
  
  return(file_names)
}

clean_file_names <- function(file_names,
                             include_models = NULL, # specify which models to take. R freaks out at the large size
                             # of the list, and while the default setting will include all
                             # files, in practice we have to reduce. At this point we will
                             # break up the files by sample size, e.g., include_models = "N25"
                             # "N50" and "N100" separately. This argument is more generally 
                             # a regular expression used to pick elements of the list whose
                             # names satisfy the regular expression.
                             return_list = FALSE,
                             verbose = FALSE
                             ) 
  {
  
  # take a subset of files
  if (!is.null(include_models)) {
    file_names <- file_names[grep(include_models, names(file_names))]
    if (verbose) {
      print(file_names)
    }
  }
  
  # remove empty element from file_names
  file_names0 <- file_names
  file_names <- file_names[lapply(file_names, length) > 0]
  model_names <- names(file_names)
  
  if (all(lapply(file_names, length) == 0) | length(file_names) == 0) {
    stop("No files found. \n")
  }
  
  if (length(file_names) != length(file_names0)) {
    warning("Some expected model scenarios had no files. \n")
  }
  
  file_names_df <- bind_rows(map(file_names, function(x) data.frame(file_names = x, stringsAsFactors = FALSE)))
  file_names_df$model_names <- rep(names(file_names), unlist(lapply(file_names, length)))
  
  if (return_list) {
    return(split(file_names_df, seq(nrow(file_names_df))))
  } else {
    return(file_names_df)
  }
}

get_output <- function(directory,      # directory where files live
                       file_names_df,     # generated from get_file_names(); see above
                       verbose = FALSE        # print intermediate messages?
)
{
  file_list <- list()
  
  for (i in 1:nrow(file_names_df)) {
    
    # columns to distinguish analyses
    model_info <- unlist(strsplit(file_names_df$model_names[i], "_"))
    dfmi <- data.frame(t(model_info), stringsAsFactors = FALSE)
    names(dfmi) <- c("model", "alpha", "N")
    # in many cases, need only N
    N <- dfmi$N
    
    # read data
    file_names_i <- file_names_df$file_names[i]
    
    if (verbose) {
      cat(paste0(file_names_i, " successfully loaded. \n"))
    }
    
    dat_i <- readRDS(paste0(directory, "/", file_names_i))
    inf_i <- dat_i$inference
    est_i <- dat_i$estimate %>% 
      select(-model, -alpha, -s0, -s1)
    
    file_list[[i]] <- cbind(N, inf_i, est_i) 
  }
  
  return(bind_rows(file_list))
}


# get the optimal model for a particular analysis
get_optimal_model <- function(output,                  # obtained from get_output
                              criteria = "deviance",   # criteria for model fitness (e.g., deviance, mse, mae, auc) 
                                                       # criteria is currently stuck on deviance (fix this!)
                              ties = "min",            # when multiple "optimal" models, which one to keep?
                                                       #    1. "min": choose model with smallest combination of 
                                                       #              s0 and s1.
                                                       #    2. "max": choose model with largest combination of 
                                                       #              s0 and s1.
                              verbose = FALSE)
{
  # how many spike scale parameters?
  num_s0 <- length(unique(output$s0))
  # how many slab scale parameters?
  num_s1 <- length(unique(output$s1))
  # how many simulated datasets?
  num_sim <- nrow(output) / (num_s0 * num_s1)
  
  if (verbose == TRUE) {
    cat(num_s0, "spike scale values. \n")
    cat(num_s1, "spike slab values. \n")
    cat(num_sim, "simulated datasets. \n")
  }
  
  # how many analyses per simulation?
  length_sim <- num_s0 * num_s1
  # create simulation indices
  sim_id <- sort(rep(1:num_sim, length_sim))
  output$sim_id <- sim_id
  
  # group output
  optimal <- output %>% 
    group_by(sim_id) %>%
    filter(deviance == min(deviance))
  
  if(any(table(optimal$sim_id) > 1)) {
    mult_opt <- which(table(optimal$sim_id) > 1)
    warning(paste0("Simulation ", names(mult_opt), " has ",
                   table(optimal$sim_id)[mult_opt], " optimal models. \n"))
    
    if (ties == "min") {
      optimal <- optimal %>% filter(s0 == min(s0) & s1 == min(s1))
    }
    if (ties == "max") {
      optimal <- optimal %>% filter(s0 == max(s0) & s1 == max(s1))
    }
  }
  
  # pick out optimal model
  return(optimal)
}

# This function takes a large data frame containing many simulations under many different scenarios (model, N, alpha)
# and selects the optimal model, i.e., values of s0 and s1 for each simulation. Its inital use tried to include all
# output from B01, but get_output() broke R via memory issues. See get_all_optimal_models2() for the current attempt
# to circumvent this problem without having to generate so many separate data sets.
get_all_optimal_models <- function(output,                 # list from get_output
                                   criteria = "deviance",  # same as get_optimal model
                                   unnest_to_df = FALSE    # when true unnests to a single (probably massive) data frame
                                   )
{
  output <- output %>% 
    group_by(model, N, alpha) %>%
    nest() %>%
    mutate(optimal_model = map(data, function(x) get_optimal_model(x, criteria = criteria)))
  
  if (unnest_to_df) {
    output <- unnest(output, cols = c(N, model, alpha, optimal_model)) %>%
      select(-data)
  }
  
  return(output)
}

# This function will:
#   1. get_output() 
#   2. use get_optimal_model() to obtain the optimal model for each simulated data set.
#   3. return a dataframe containing optimal models in each row
get_output_optimal_models <- function(directory, file_names_df, criteria = "deviance", verbose = FALSE) {
  
  current_output <- get_output(directory = directory, file_names_df = file_names_df)
  
  if ("glmnet" %in% current_output$model) {
    warning("glmnet optimal models obtained elsewhere. Removing rows where model is glmnet. \n")
    
    current_output <- current_output %>%
      filter(model != "glmnet")
  }
  
  current_output <- current_output %>%
    group_by(model, N, alpha) %>%
    nest() %>%
    mutate(optimal_model = map(data, function(x) get_optimal_model(output = x,
                                                                   criteria = criteria, 
                                                                   verbose = verbose)))
  
  current_output <- unnest(current_output, cols = c(N, model, alpha, optimal_model)) %>%
    select(-data, -sim_id)
  
  return(current_output)
}

# This function
#     1. clean_file_names()
#     2. get_output() for glmnet (which already has optimal models selected)
#     3. get_output_optimal_models() for ssnet
#     4. returns data frame of optimal models
# The purpose of this function is the same as get_all_optimal_models, but works differently
# so as to (hopefully) avoid the memory issues of the original function.
get_all_optimal_models2 <- function(directory,              # same as other functions
                                    file_names,             # list from get_file_names
                                    criteria = "deviance",  # same as get_optimal model
                                    verbose = FALSE
)
{
  
  if (length(file_names[grep("glmnet", names(file_names), value = TRUE)]) > 0) {
    
    file_names_df_glmnet <- clean_file_names(file_names = file_names,
                                             include_models = "glmnet",
                                             verbose = verbose)
    optimal_glmnet <- get_output(directory = directory,
                                 file_names_df = file_names_df_glmnet,
                                 verbose = verbose)
  }
  
  if (length(file_names[grep("ssnet", names(file_names), value = TRUE)]) > 0) {
    file_names_ss <- file_names[grep("ssnet", names(file_names))]
    
    file_names_df_ss <- clean_file_names(file_names = file_names_ss,
                                         return_list = TRUE)
    optimal_ss <- map_dfr(file_names_df_ss, 
                          function(x) get_output_optimal_models(directory = directory,
                                                                file_names_df = x,
                                                                criteria = criteria,
                                                                verbose = verbose))
  }
  
  if (exists("optimal_glmnet") & exists("optimal_ss")) {
    
    return(bind_rows(optimal_glmnet, optimal_ss))
    
  } else {
    if (exists("optimal_glmnet")) {
      
      return(optimal_glmnet)
      
    }
    if (exists("optimal_ss")) {
      
      return(optimal_ss)
    }
  }
}


