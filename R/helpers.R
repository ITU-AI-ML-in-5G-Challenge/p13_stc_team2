

clean_data<- function(re_clean = FALSE){
  
  
  if(re_clean == TRUE){
    # training input data
    d_input <- clean_train_input(data_dir = here("data/input_node_files/"), file_name="input_data")  
    #training input and test data
    d <- clean_train_output(d_input, data_dir = here("data/output_simulator/"))
    # training data before preprocessing
    train <- clean_train(d, file_name="processed_data")
    #preprocess data
    preprocess_data(train_path = here("data","processed_data.csv"))
    
    
    
    #clean testing data received end of Sept
    df_test_input <- clean_test_input(here("data/input_node_files_test/"), "test_input")
    df_test_output <- clean_test_output(here("data/output_simulator_test/"), here("data/test_input.csv"))
    df_test <- clean_test(df_test_output)
    create_tests_split(df_test)
  }else{
    print("No cleaning or creation of data as re_clean is set to FALSE")
  }
  
  
}


#' train_models_input
#'
#' @param training_path 
#' @param testing_path 
#' @details this is the input to different models after the data has been processed. The difference between test and test raw is that the test data does not have the data for the access points
#' @return list with three tibbles :trraining data, testing raw and test data
#' @export
#'
#' @examples
train_models_input <- function(training_path, testing_path){
  
  
  train <- 
    read_csv(training_path) %>% 
    # removing access points
    filter(node_type_X1  == 1) %>% 
    select(-deployment, -node_code, -wlan_code, -node_type_X1)
  
  test_row <- 
    read_csv(testing_path) 
  
  
  test <- 
    read_csv(testing_path) %>%
    filter(node_type_X1  == 1) %>% 
    select(-deployment, -node_code, -wlan_code, -node_type_X1)
  
  return(
    list(
      train = train,
      test_row = test_row, 
      test = test)
  )
  
} 






#' find_best_reg_model
#'
#' @param model tidy model defintion
#' @param train training data
#' @param model_name randomforest or svm, etc
#' @param used_metric name of metric used for tuning
#' @param combinations_num # number of partamter value combinations used in tuning
#' @param save_results logical, if true will save results and add datetime to it
#'
#' @return specs of best performing model E
#' @export
#'
#' @examples
#' 
find_best_reg_model <- function(model, train, model_name , used_metric = "rmse", combinations_num = 50, save_results = TRUE){
  
  start_time <- str_replace_all(Sys.time(), c("-| " = "_", ":"="_","\\+"="_"))
  
  log_appender(appender_tee(here("logs",paste0(model_name, "_", start_time, ".log"))))
  log_info(paste0('creating gird specs and model input - ', model_name))
  
  model_params <-
    model %>% 
    parameters()
  
  if(model_name == "mlp"){
    max_hidden <- as.integer(ncol(train)/2 * 1.2)
    model_params <- 
      model_params %>% 
      update(
        hidden_units = hidden_units(c(7, max_hidden)),
        dropout = dropout(c(0.1, 0.3)),
        epochs = epochs(c(15, 15))
      )
  }else if(model_name == "xgb" | model_name == "r_forest"){
    model_params <- 
      model_params %>% 
      update(
        mtry = finalize(mtry(), train),
        min_n = min_n(c(500, 1000))
      )
  }else{
    model_params <- model_params
  }
  
  model_grid <- grid_latin_hypercube(
    model_params,
    size = combinations_num
  )
  
  
  model_wf <- workflow() %>%
    add_formula(y ~ .) %>%
    add_model(model)
  
  
  
  
  # train -------------------------------------------------------------------
  
  
  
  vb_folds <- vfold_cv(train, v = 5)
  
  log_info(paste0('tune gird -',model_name))
  
  
  all_cores <- parallel::detectCores(logical = FALSE)
  
  
  cl <- makePSOCKcluster(all_cores-4)
  registerDoParallel(cl)
  
  
  model_res <- tune_grid(
    model_wf,
    resamples = vb_folds,
    grid = model_grid,
    control = control_grid(save_pred = TRUE)
  )
  
  log_info(paste0('save results ', model_name))
  
  if(save_results) write_rds(model_res, here("expermients",paste0(start_time, "_", model_name,"__expermients.rds")))
  
  
  log_info(paste0('select best model -', model_name))
  
  best_rmse <- select_best(model_res, used_metric)
  
  
  
  final_model <- finalize_workflow(
    model_wf,
    best_rmse
  )
  
  log_info(paste0('save best model -', model_name))
  
  
  write_rds(final_model, here("tuned_models",paste0(start_time, "_","final_",model_name,".rds")))
  
  
  log_info(paste0('return best model - ', model_name))
  
  final_model
  
}

score_best_model <- function(final_fit, train, test_row, test, model_name){
  start_time <- str_replace_all(Sys.time(), c("-| " = "_", ":"="_","\\+"="_"))
  log_appender(appender_tee(here("logs",paste0("score_", model_name, "_", start_time, ".log"))))
  log_info(paste0('compute metric result for  ', model_name))
  
  
  
  device_predictions <- 
    test_row %>% 
    filter(node_type_X1 == 1) %>% 
    bind_cols(
      final_fit %>% 
        predict(test)
    )
  
  log_info(paste0('devices prediction for  ', model_name ))
  
  access_point_y <- 
    device_predictions %>% 
    group_by(deployment,wlan_code) %>% 
    summarise(.pred = sum(y)) %>% 
    ungroup()
  
  
  d_access_point <- 
    test_row %>% 
    filter(node_type_X1 == 0) 
  
  d_access_point <- 
    d_access_point %>% 
    left_join(access_point_y) 
  
  
  all_predictions <- 
    test_row %>% 
    filter(node_type_X1 == 1) %>% 
    bind_cols(
      final_fit %>% 
        predict(test)
    )  %>% 
    rbind(d_access_point) %>% 
    metrics(truth = y, estimate = .pred)
  
  log_info(paste0('overall prediction for  ', model_name, " is ", all_predictions %>% filter(.metric == "rmse") %>% pull(.estimate) ))
  
  all_predictions
}







score_models <- function(model, train, model_name, used_metric, combinations_num, save_results, test_row, test){
  final_model <- find_best_reg_model(model, train, model_name , used_metric = used_metric, combinations_num = combinations_num, save_results = save_results) 
  
  final_fit <- final_model %>% fit(train)
  test_score <- score_best_model(final_fit, train, test_row, test, model_name)
  test_score
}
