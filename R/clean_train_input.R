

# clean and processinput ---------------------------------------------------------------

clean_train_input <- function(data_dir, file_name="input_data"){
  
  
  #data_dir <- here("data/input_node_files_test//")
  
  input_foldrs <- list.files(data_dir)
  
  
  d_input <- 
    map_df(input_foldrs, function(folder){
      input_files <- here("data", "input_node_files", folder, list.files(paste0(data_dir, "/", folder)))
      map_df(input_files, function(file_path){
        deployment_num <- 
          file_path %>%
          str_split("/") %>%
          unlist() %>%
          last() %>% 
          str_replace_all("input_nodes_", "") %>% 
          str_replace_all(".csv", "")
        
        
        
        
        df <- 
          read.csv(file_path, sep = ";") %>% 
          as_tibble() %>% 
          mutate(
            deployment = deployment_num,
            deployment_num = nrow(.)
          )
        
        wlan_devices_num <- 
          df %>% 
          count(wlan_code) %>% 
          mutate(wlan_devices_num = n) %>% 
          select(-n)
        
        df <- 
          df %>% 
          left_join(wlan_devices_num) 
        
        
        # calculate avg and min distance from each device ---------------------------------
        
        
        x_m <- df %>% pull(x.m.)
        y_m <- df %>% pull(y.m.)
        node_code <- df %>% pull(node_code)
        
        
        
        
        x <- 
          expand_grid(a=x_m, b=x_m) %>% 
          as_tibble() %>% 
          mutate(x = (a-b)^2) %>% 
          select(x)
        
        y <- 
          expand_grid(a=y_m, b=y_m) %>% 
          as_tibble() %>% 
          mutate(y = (a-b)^2) %>% 
          select(y)
        
        
        node_code <- 
          expand_grid(a=node_code, b=node_code) %>% 
          as_tibble() %>% 
          select(a) %>% 
          rename(node_code = a)
        
        d_avg <- 
          tibble(
            x,
            y,
            node_code
          ) %>% 
          mutate(distance = sqrt(x+y)) %>% 
          group_by(node_code) %>% 
          summarise(
            avg_deployment_distance = mean(distance),
            min_deployment_distance = quantile(distance, .25)
          ) %>% 
          ungroup()
        
        df %>% 
          left_join(d_avg)
        
      })
    })
  
  
  write_csv(d_input, here("data", paste0(file_name,".csv")))
  
}
  
 
  
