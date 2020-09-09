

# BE SURE TO READ THE README FILE! 

plan <- 
  drake_plan(
    priors = c("glmnet", "ssnet", "ssnet-iar"),
    en_levels = c("a1", "a05"),
    N_levels = c("N25", "N50", "N100"),
    
    ssnet_levels = get_ssnet_levels(priors = priors, en_levels = en_levels),
    ssnet_levels_N = get_ssnet_levels_N(ssnet_levels = ssnet_levels,
                                        N_levels = N_levels),
    
    # Make sure to run make(plan), not just source("make.R"), because the following 2 functions will tell
    # you whether all the files you expect are here.
    # Just change the directory as needed to wherever the files are stored.
    # note the following about directory arguments:
    #           (a) results/B01 should contain results from data sets where non-zero beta_j = 0.1
    #           (b) results/B05 should contain results from data sets where non-zero beta_j = 0.5
    # You many need to change these arguments to match the local directory where you've stored the results.
    file_names_B01 = get_file_names(directory = "results/B01",
                                    ssnet_levels_N = ssnet_levels_N$ssnet_levels_N,
                                    ssnet_names = ssnet_levels_N$ssnet_names,
                                    N_files_each = data.frame(ssnet_levels_N = ssnet_levels_N$ssnet_levels_N,
                                                              num_files_expected = c(rep(c(1, 5, 10), 5), c(1, 5, 20))),
                                    verbose = TRUE
                                    ),
    file_names_B05 = get_file_names(directory = "results/B05",
                                    ssnet_levels_N = ssnet_levels_N$ssnet_levels_N,
                                    ssnet_names = ssnet_levels_N$ssnet_names,
                                    N_files_each = data.frame(ssnet_levels_N = ssnet_levels_N$ssnet_levels_N,
                                                              num_files_expected = c(rep(c(1, 5, 10), 5), c(1, 5, 20))),
                                    verbose = TRUE
                                    ),
    
    # extract optimal models for each simulation
    optimal_B01 = get_all_optimal_models2(directory = "results/B01",              # same as other functions
                                          file_names = file_names_B01,
                                          criteria = "deviance",
                                          verbose = FALSE
                                          ),
    optimal_B05 = get_all_optimal_models2(directory = "results/B05",              # same as other functions
                                          file_names = file_names_B05,
                                          criteria = "deviance",
                                          verbose = FALSE
    ),
    
    # write optimal models to .rds file for safe keeping
    optimal_write = {
      saveRDS(optimal_B01, 
              file_out("results/optimal_B01.rds"))
      saveRDS(optimal_B05, 
              file_out("results/optimal_B05.rds"))
    },
    
    # obtain summarties (mean, median, sd, IQR, mcse) for each metric/parameter estimate within each N/model/alpha combo
    summary_B01 = get_all_summaries(data = optimal_B01),
    summary_B05 = get_all_summaries(data = optimal_B05),
    
    # write summaries to .rds file for safe keeping
    summary_write = {
      saveRDS(summary_B01, 
              file_out("results/summary_B01.rds"))
      saveRDS(summary_B05, 
              file_out("results/summary_B05.rds"))
    },
    
    summary_by_relevance_B01 = get_beta_ests_by_relevance(data = optimal_B01, B.values = 0.1, wide = FALSE),
    summary_by_relevance_B05 = get_beta_ests_by_relevance(data = optimal_B05, B.values = 0.5, wide = FALSE),
    summary_by_relevance = rbind(summary_by_relevance_B01, summary_by_relevance_B05),
    
    # for tables
    # summary_by_relevance_tables_B01 = get_beta_ests_by_relevance(data = optimal_B01, B.values = 0.1, wide = TRUE),
    # summary_by_relevance_tables_B05 = get_beta_ests_by_relevance(data = optimal_B05, B.values = 0.5, wide = TRUE),
    # summary_by_relevance_tables = rbind(summary_by_relevance_tables_B01, summary_by_relevance_tables_B05),
    
    # custom for some figures
    prepared_for_facets_B01 = prepare_labels_facet(data = summary_B01),
    prepared_for_facets_B05 = prepare_labels_facet(data = summary_B05),
    
    report = rmarkdown::render(
      knitr_in("summary_report.Rmd"),
      output_file = file_out("summary_report.html"),
      quiet = TRUE
    )
  )
