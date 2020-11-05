get_all_models_scores <- function(){
  score_logs <- list.files(here("logs"),"score")
  
  map_df(score_logs, function(log_file){
    
    readLines(here("logs",log_file)) %>%
      enframe() %>%
      filter(name == 3) %>%
      mutate(score = str_extract_all(value, "for.*") %>% unlist()) %>%
      mutate(score = str_replace_all(score,c("for  " = "", " is" = ""))) %>%
      separate(score, c("model", "rmse"), sep = " ") %>%
      mutate(rmse = as.numeric(rmse))
  }) %>%
    mutate(time = str_extract(value,"\\[.*\\]") %>% str_replace_all(., c("\\[" = "", "\\]" = ""))) %>%
    select(time, model, rmse) %>%
    arrange(rmse)
  
}

