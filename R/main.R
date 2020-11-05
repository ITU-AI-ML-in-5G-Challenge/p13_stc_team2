library(tidyverse)
library(tidymodels)
library(janitor)
library(here)
library(ranger)
library(keras)
library(logger)
library(doParallel)
library(furrr)
library(kernlab)
library(xgboost)
library(reticulate)
source(here("R/clean_train_input.R"))
source(here("R/clean_train_output.R"))
source(here("R/pre_process.R"))
source(here("R/clean_test_input.R"))
source(here("R/clean_test_output.R"))
source(here("R/score.R"))
source(here("R/train.R"))
source(here("R/helpers.R"))


set.seed(234)

log_threshold(DEBUG)

#  clean and process training and testing data ----------------------------

clean_data(re_clean = TRUE) # change to true if you want to do the cleaning  

# reterive data after it is created  -------------------------------------------------------------------


if(file.exists(here("data/train.csv")) & file.exists(here("data/tests.csv"))){
  
  train_path <- here("data/train.csv")
  test_path <- here("data/tests.csv")
  
  
  data <- train_models_input(train_path, test_path) 
  
  train <- data$train  
  train_sample <- sample_n(train, nrow(train)*0.09)
  test_row <- data$test_row  
  test <- data$test   
  
}else{
  
  print("you may need to run clean_data function with re_clean set to True ")
}


# train models  ------------------------------------------------------------

# feed data and number of grid search combinations 
train_and_score(train, test, test_row, combinations_num = 300, re_train = TRUE) # change to TRUE to do the training again


# model results -----------------------------------------------------------


# get the scores from the log files
if(length(list.files(here("logs"))) > 0){
  
  d_scores <- 
    get_all_models_scores()  
  
  d_scores
}

