preprocess_data <- function(train_path){
  
  set.seed(234)
  
  df <- 
    read_csv(train_path) %>% 
    mutate(
      ap_num_12 = as.factor(ap_num_12),
      node_type = as.factor(node_type)
      
    ) %>% 
    select(-ap_num)
  
  # to make test data deployment that has not been seen in the training
  df_nest <- df %>% nest_by(deployment)
  
  
  splits <- initial_split(df_nest)
  
  test_data <- testing(splits) %>% unnest(cols = c("data")) %>% ungroup()
  train_data <- training(splits) %>% unnest(cols = c("data"))%>% ungroup()
  
  
  rec <- 
    recipe(y~., data = train_data) %>% 
    update_role(deployment, new_role = "id") %>% 
    update_role(node_code,new_role =  "id") %>% 
    update_role(wlan_code, new_role =  "id") %>% 
    step_YeoJohnson(all_numeric(), -all_outcomes()) %>% 
    step_normalize(all_numeric(), -all_outcomes()) %>% 
    # step_spatialsign(all_numeric(), -all_outcomes()) %>% 
    step_dummy(node_type, ap_num_12 ) 
  
  
  rec_prep <- 
    rec %>% 
    prep()
  
  
  write_rds(rec_prep, here("data/recipe/recipe.rds"))
  
  train <-
    juice(rec_prep) %>% 
    as_tibble() %>% 
    write_csv(here("data","train.csv"))
  
  tests <-
    rec_prep %>% 
    bake(test_data %>% select(-deployment)) %>% 
    cbind(deployment = test_data %>% pull(deployment), .) %>% 
    as_tibble() %>% 
    write_csv(here("data","tests.csv"))
  
  
  
  
}
