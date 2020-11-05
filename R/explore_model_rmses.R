library(here)
library(tidyverse)
expermmints <- list.files(here("expermients/"))

models_results <-
  map_df(expermmints,function(file){
    model <- file %>% str_split("_") %>% unlist()  
    model <- model[7]
    if(model == "r") model <- "r_forest"
    d <- read_rds(here("expermients",file))
    d %>% 
      select(.metrics) %>% 
      unnest(cols = c(.metrics)) %>% 
      filter(.metric == "rmse") %>%
      mutate(model = model)
})

models_results %>% 
  ggplot(aes(x = model, y  =  .estimate)) +
  geom_point()+
  lims(y = c(1,5))
