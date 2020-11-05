
clean_test_output <- function(data_dir, input_path){
  

# reteriving input --------------------------------------------------------

  d_input <- 
    read_csv(input_path) # input files
    read_csv("data/test_input.csv") 
  
  
  d_input <- 
    d_input %>% 
    mutate(
      deployment_class = str_extract_all(deployment,".*_d" ),
      deployment_class = str_remove_all(deployment_class, "_d"),
      deployment_file_num = str_extract_all(deployment,"deployment_.*" ),
      deployment_file_num = str_remove_all(deployment_file_num, "_d"),
      full_deployment = deployment,
      
      deployment = str_replace_all(deployment, "_deployment.*", "")
    ) %>% 
    clean_names() %>% 
    nest_by(full_deployment)
  
  
  

# getting and processing outpyt -------------------------------------------

  
  
  output_foldrs <- list.files(data_dir)
  
  j <- 1
  d_output <- 
    map_df(output_foldrs, function(folder){

      output_files <- here("data", "output_simulator_test/", folder, list.files(paste0(data_dir, "/", folder)))
      
      output_files <- output_files %>% 
        enframe() %>%
        mutate(k = str_extract_all(value, "test_output.*.csv")) %>%
        mutate(f = str_remove_all(k, ".*\\/")) %>% 
        mutate(j = str_remove_all(f, ".csv")) %>% 
        separate(j, c("type", "type_num"), sep = "_") %>%
        mutate(type_num = as.numeric(type_num)) %>% 
        arrange(type_num) %>% 
        nest_by(type_num)


      map(output_files[[2]], function(d_row){


        
      airtime_path <-  d_row %>% filter(type == "airtime") %>% pull(value)
      interference_path <- d_row %>% filter(type == "interference") %>% pull(value)
      sinr_path <- d_row %>% filter(type == "sinr") %>% pull(value)
      rssi_path <- d_row %>% filter(type == "rssi") %>% pull(value)
      


      d_airtime <- 
        read_file(airtime_path) %>% 
        str_replace_all(";",",") %>% 
        str_replace_all("\\n","emptry") %>% 
        str_split(",") %>%
        unlist() %>%
        enframe() %>% 
        tibble::rownames_to_column() %>% 
        filter(value != "emptry") %>% 
        rename(airtime = value) %>% 
        select(-name, -rowname)
      
      
     df_interference <- 
      read_file(interference_path) %>%
      str_replace_all(";",",") %>% 
      str_replace_all("\\n","") %>% 
      str_split(",") %>%
      unlist() %>%
      enframe() %>% 
      tibble::rownames_to_column()
    
    num_wlan_num <- df_interference  %>% filter(value == "Inf") %>% nrow()
    
    wlan_codes <- paste0("power_from_", rep(letters[1:num_wlan_num], nrow(df_interference)/num_wlan_num))
    

 
    df_interference <- 
      df_interference %>% 
      mutate(wlan_code = wlan_codes) %>% 
      mutate(wlan_code = wlan_codes) %>% 
      group_by(wlan_code) %>% 
      mutate(idx = 1:n()) %>% 
      mutate(value = str_replace_all(value, "Inf",  NA_character_)) %>% 
      mutate(value = as.numeric(value)) %>% 
      mutate(value = replace_na(value, -1000)) %>% 
      select(-name, -rowname) %>% 
      pivot_wider(
        names_from = wlan_code,
        values_from = value
      ) %>% 
      mutate_if(is.character, as.numeric) %>% 
      mutate(wlan_code = letters[1:num_wlan_num]) 
          

  # if(nrow(df_interference) == 6)    browser()
    
    
    d_sinr <- 
      read_file(sinr_path) %>%
      str_replace_all(";",",") %>% 
      str_replace_all("\\n","") %>% 
      str_split(",") %>%
      unlist() %>%
      enframe() %>% 
      # mutate(
      #   deployment = deployment_num
      # ) %>% 
      rename(sinr = value) %>% 
      mutate(
        sinr = case_when(
          sinr == "Inf" ~ "-1000",
          TRUE ~ sinr
        ),
        sinr = as.numeric(sinr)
        
      ) %>% 
      select(-name)
    

    d_rss <- 
      read_file(rssi_path) %>%
      str_replace_all(";",",") %>% 
      str_replace_all("\\n","") %>% 
      str_split(",") %>%
      unlist() %>%
      enframe() %>% 
      # mutate(
      #   deployment = deployment_num
      # ) %>% 
      rename(rss_list = value) %>% 
      mutate(
        rss_list = case_when(
          rss_list == "Inf" ~ "-1000",
          TRUE ~ rss_list
          ),
      rss_list = as.numeric(rss_list)
      
      ) %>% 
      select(-name)

    deployment_num <- nrow(d_rss)
    
      base_interference <-  paste0("power_from_", letters[(num_wlan_num+1):12])

      base_interference <- 
        tibble(base_interference) %>% 
        mutate(value = -1000) %>%
        spread( base_interference, value) 
            

     df <- tibble(
       d_sinr,
       d_rss,
       index = j,
       ap_num = num_wlan_num
     ) %>% 
       cbind(base_interference)
     
     df_input <- d_input[j,] %>% unnest(data) %>% ungroup()

     j <<- j + 1

   
       df %>% 
       cbind(df_input) %>% 
       left_join(df_interference) %>% 
         select(-idx)
 
     
   #  df <- df_interference
     
        
      })
      
     
  
  }) 
} 
  
  


  
clean_test <- function(df){
  
  # remove colums which are known to help not in prediction
  df %>% 
    select(
      -z_m , -min_channel_allowed, -capture_effect_model, -capture_effect_thr,
      -pifs_activated, -packet_length, -cont_wind, -cont_wind_stage, -num_packets_aggregated,
      -central_freq_g_hz, -channel_bonding_model, -constant_per, -traffic_load_pkt_s, -tpc_default_d_bm, 
      -cca_default_d_bm, -traffic_model, -cw_adaptation, -deployment
    ) %>% 
    rename(deployment = full_deployment)  %>% 
    mutate(y = 1) %>% 
    mutate(
      deployment = str_remove_all(deployment, "test_" ),
      node_type = as.factor(node_type),
      node_code = as.factor(node_code),
      deployment = as.factor(deployment),
      wlan_code = as.factor(wlan_code),
      ap_num_12 = as.factor(if_else(ap_num > 8, 1, 0)),
      min_deployment_distance = replace_na(min_deployment_distance, 1000)
    ) %>% 
    mutate(
      power_from_a = replace_na(power_from_a, -1000),
      power_from_b = replace_na(power_from_b, -1000),
      power_from_c = replace_na(power_from_c, -1000),
      power_from_d = replace_na(power_from_d, -1000),
      power_from_e = replace_na(power_from_e, -1000),
      power_from_f = replace_na(power_from_f, -1000),
      power_from_g = replace_na(power_from_g, -1000),
      power_from_h = replace_na(power_from_h, -1000),
      power_from_i = replace_na(power_from_i, -1000),
      power_from_j = replace_na(power_from_j, -1000),
      power_from_k = replace_na(power_from_k, -1000),
      power_from_l = replace_na(power_from_l, -1000),
      
      sinr = replace_na(sinr, -1000)
    ) %>% 
    as_tibble()
  
  
  
}


  
  # write files -------------------------------------------------------------
  
  
  create_tests_split <- function(d){
    df_tests <- 
      d %>% 
      select(-index) %>% 
      rename(index = deployment_class) %>% 
      select(-deployment_file_num) %>% 
      nest_by(index) 
    map2(df_tests$data, df_tests$index, function(data,file_name){
      write_rds(data , here("data/test_data", paste0(file_name, ".rds")))
    })
  }  
  
  

