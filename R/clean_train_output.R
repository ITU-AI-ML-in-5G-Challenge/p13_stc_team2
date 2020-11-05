# clean and process output ------------------------------------------------------------


clean_train_output <- function(d_input, data_dir) {
  
  output_files <- list.files(data_dir)
  
  d_output <- map_df(output_files, function(file) {
    output_file <- read_file(paste0(data_dir, file))
    col_names <-
      rep(
        c(
          "deployment",
          "sta_throughput",
          "airtime",
          "rss_list",
          "interference_map",
          "sinr"
        ),
        100
      )
    
    
    #clean data
    
    df <-
      output_file %>%
      str_replace_all("1992\\)", "1992\\}") %>%
      enframe() %>%
      separate_rows(value, sep = "\\}") %>%
      head(-1) %>%
      mutate(var = col_names) %>%
      mutate(
        value = str_replace_all(value, '\\}', ''),
        value = str_replace_all(value, '\\{', ''),
        value = str_replace_all(value, " KOMONDOR SIMULATION \\'", ''),
        value = str_replace_all(value, '\\\n', ''),
        value = str_replace_all(value, '\\.csv', ''),
        value = str_replace_all(value, "\\' \\(seed 1992", ""),
        value = trimws(value)
      ) %>%
      select(var, value) %>%
      rownames_to_column()
    
    
    # create column for each file for rbinding and left joining
    primary_key <-
      df %>%
      filter(var == "deployment") %>%
      pull(value) %>%
      rep(., 6) %>%
      sort() %>%
      enframe() %>%
      rename(rowname = name,
             deployment = value) %>%
      mutate(rowname = as.character(rowname))
    
    
    df <-
      df %>%
      left_join(primary_key) %>%
      filter(var != "deployment") %>%
      select(-rowname)  %>%
      mutate(deployment = str_replace_all(deployment, "sim_input_nodes_", ""))
    
    
  })
  
  d_output_wide <-
    d_output %>%
    spread(var, value)
  
  
  
  d_output_lookup <-
    d_output_wide %>%
    pivot_longer(-deployment) %>%
    mutate(val_length =  str_split(value, ",") ,
           val_length2 = map_int(val_length, length)) %>%
    select(deployment, name , matrix_length = val_length2) %>%
    spread(name, matrix_length)
  
  names(d_output_lookup)[-1] <-
    paste0("length_", names(d_output_lookup)[-1])
  
  
  d_output_wide <-
    d_output_wide %>%
    left_join(d_output_lookup)
  i <- 0
  
  d <-
    d_input %>%
    left_join(d_output_wide) %>%
    nest(data = c(-deployment)) %>%
    mutate(df = map(data, function(d_row) {
      d_row <-
        d_row %>%
        tibble::rownames_to_column()
      
      long_rss_list <-
        d_row %>%
        head(1) %>%
        select(rss_list) %>%
        separate_rows(rss_list, sep = ",") %>%
        rename(long_rss_list = rss_list) %>%
        mutate(long_rss_list = str_replace_all(long_rss_list, "Inf", "-1000")) %>%
        mutate(long_rss_list = as.numeric(long_rss_list))
      

      long_sinr <-
        d_row %>%
        head(1) %>%
        select(sinr) %>%
        separate_rows(sinr, sep = ",") %>%
        rename(long_sinr = sinr) %>%
        mutate(long_sinr = str_replace_all(long_sinr, "Inf", "-1000")) %>%
        mutate(long_sinr = as.numeric(long_sinr))
      
      
      long_sta_throughput <-
        d_row %>%
        head(1) %>%
        select(sta_throughput) %>%
        separate_rows(sta_throughput, sep = ",") %>%
        rename(y = sta_throughput)
      
      i <<- i + 1
      
      if (nrow(long_rss_list) == nrow(long_sta_throughput)) {
        
        d_wlan_code <-
          d_row %>%
          distinct(wlan_code) %>%
          mutate(node_type = 0)
        
        wlan_code_num <- nrow(d_wlan_code)
        
        
        # long_airtime <-
        #   d_row %>%
        #   head(1) %>%
        #   select(airtime) %>%
        #   str_split(",") %>%
        #   unlist() %>%
        #   tibble(long_airtime = .)
        
        # wlan_code_num <- nrow(long_airtime)
        
        # long_airtime <-
        #   long_airtime %>%
        #   mutate(wlan_code = LETTERS[1:wlan_code_num],
        #          node_type = 0)
        # 
        
        
        long_interfence <-
          d_row %>%
          head(1) %>%
          select(interference_map) %>%
          str_split(";") %>%
          unlist() %>%
          tibble(received_energy = .) %>%
          mutate(received_energy = str_replace_all(received_energy, "Inf", "-1000")) %>%
          mutate(wlan_code = LETTERS[1:wlan_code_num]) %>%
          nest(data = -wlan_code) %>%
          mutate(energy = map(data, function(node_energy) {
            sensed_power_cols <- paste0("power_from_", LETTERS[1:wlan_code_num])
            node_energy %>%
              separate(received_energy, sep = ",", into = sensed_power_cols)
          })) %>%
          unnest(energy) %>%
          select(-data)
        
        wlan_code <- long_interfence %>% select(wlan_code)
        
        
        long_interfence <-
          long_interfence %>%
          select(-wlan_code) %>%
          mutate(across(where(is.character), as.numeric)) %>%
          bind_cols(wlan_code)
        #mutate(node_type = 0)
        
        setup_type <-
          d_row %>%
          summarise(ap_num = n_distinct(wlan_code))
        
        d_row %>%
          bind_cols(list(long_rss_list, long_sinr, long_sta_throughput, setup_type)) %>%
          #left_join(long_airtime) %>%
          left_join(long_interfence) %>%
          select(
            -starts_with("length"),
            -rss_list,
            -sinr,
            -sta_throughput,
            -interference_map,
            -airtime,
            -rowname
          ) %>%
          mutate(empty_deployment = FALSE) %>%
          relocate(y, everything()) %>%
          clean_names()
      } else{
        tibble(
          y = NA,
          node_code = NA,
          node_type = NA,
          wlan_code = NA,
          x_m = NA,
          y_m = NA,
          z_m = NA,
          central_freq_g_hz = NA,
          channel_bonding_model = NA,
          primary_channel = NA,
          min_channel_allowed = NA,
          max_channel_allowed = NA,
          tpc_default_d_bm = NA,
          cca_default_d_bm = NA,
          traffic_model = NA,
          traffic_load_pkt_s = NA,
          packet_length = NA,
          num_packets_aggregated = NA,
          capture_effect_model = NA,
          capture_effect_thr = NA,
          constant_per = NA,
          pifs_activated = NA,
          cw_adaptation = NA,
          cont_wind = NA,
          cont_wind_stage = NA,
          long_rss_list = NA,
          sinr = NA,
         # long_airtime = NA,
          power_from_a = NA,
          power_from_b = NA,
          power_from_c = NA,
          power_from_d = NA,
          power_from_e = NA,
          power_from_f = NA,
          power_from_g = NA,
          power_from_h = NA,
          power_from_i = NA,
          power_from_j = NA,
          power_from_k = NA,
          power_from_l = NA,
          empty_deployment = TRUE,
          ap_num = NA
          
        )
        
      }
      
      
      
      
    }))
}



clean_train <- function(d, file_name = "processed_data") {
  d %>%
    select(-data) %>%
    unnest(df) %>%
    filter(empty_deployment != TRUE) %>%
    select(-empty_deployment, rss_list = long_rss_list, sinr = long_sinr) %>%
    mutate(
      node_type = as.factor(node_type),
      node_code = as.factor(node_code),
      deployment = as.factor(deployment),
      wlan_code = as.factor(wlan_code),
      ap_num_12 = as.factor(if_else(ap_num == 12, 1, 0)),
      
      min_deployment_distance = replace_na(min_deployment_distance, 1000),
      power_from_i = replace_na(power_from_i,-1000),
      power_from_j = replace_na(power_from_j,-1000),
      power_from_k = replace_na(power_from_k,-1000),
      power_from_l = replace_na(power_from_l,-1000),
      sinr = replace_na(sinr,-1000)
      
      
      
    ) %>%
    select(
      -z_m ,
      -min_channel_allowed,
      -capture_effect_model,
      -capture_effect_thr,-pifs_activated,
      -packet_length,
      -cont_wind,
      -cont_wind_stage,
      -num_packets_aggregated,-central_freq_g_hz,
      -channel_bonding_model,
      -constant_per,
      -traffic_load_pkt_s,
      -tpc_default_d_bm,-cca_default_d_bm,
      -traffic_model,
      -cw_adaptation
      
    ) %>%
    write_csv(here("data", paste0(file_name, ".csv")))
  
}
