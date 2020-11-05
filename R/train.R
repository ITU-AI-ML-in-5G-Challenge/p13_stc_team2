train_and_score <- function(train, test, test_row,  combinations_num, re_train = FALSE){
  
  if(re_train == TRUE){
    
    # define models 
    svm <- 
      svm_poly(
        cost = tune(),
        degree = tune(),
        scale_factor = tune(),
        margin = tune()
      ) %>% 
      set_engine("kernlab") %>% 
      set_mode("regression") 
    
    r_forest <- 
      rand_forest(
        mtry = tune(),
        trees = tune(),
        min_n = tune()
      ) %>% 
      set_engine("ranger") %>% 
      set_mode("regression") 
    
    
    xgb <- 
      boost_tree(
        trees = tune(),
        tree_depth = tune(),
        min_n = tune(),
        loss_reduction = tune(),                     ## first three: model complexity
        sample_size = tune(),
        mtry = tune(),         ## randomness
        learn_rate = tune()                       ## step size
      ) %>%
      set_engine("xgboost") %>%
      set_mode("regression")
    
    mlp  <-
      mlp(
        epochs = tune(),
        hidden_units = tune() ,
        dropout = tune(),
        activation = "relu"
      ) %>%
      set_mode("regression") %>% 
      set_engine("keras", verbose = 0) 
    
    
    models <- list(
      svm = svm,
      r_forest = r_forest,
      xgb = xgb#,
      #mlp = mlp
    )
  
    
    # start training  -------------------------------------------------------
    
    
    start_time <- str_replace_all(Sys.time(), c("-| " = "_", ":"="_","\\+"="_"))
    log_appender(appender_tee(here("logs",paste0("training_", start_time, ".log"))))
    
    log_info('training start ')
    
    # all_cores <- parallel::detectCores(logical = FALSE)
    # plan(multiprocess(workers = 16))
    
    
    # train and score ---------------------------------------------------------
    
    # run expermeniments. On 10 volds split of the training data, find the the best hyperparamters for multiple models.
    # Then select the best model performing on the 10 volds and use it for the test data
    models_results <-
      map2(models, names(models), function(model, model_name){
        
        score_models(
          model,
          train,
          model_name,
          used_metric = "rmse", 
          combinations_num = combinations_num, # number of grid search combinations 
          save_results = TRUE,
          test_row, 
          test
        )
        
      })
    
    log_info(paste0('training done  '))
  }else{
    print("No training as re_train is set to FALSE")
  }
  
  
  
}
