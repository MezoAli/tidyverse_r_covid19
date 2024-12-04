# function to count NAs and their percentage


count.na <- function(df){
  
  require(tidyverse)
  require("cowplot")
  df.count <- map(df,.f = ~sum(is.na(.))) %>% 
  unlist() %>% 
  tibble(col = names(.),
         NA_count = .,
         num_rows = nrow(df)) %>% 
  mutate(NA_percent = round(NA_count/nrow(df) * 100,2))
  
  print(df.count)
  
  p1 <- ggplot(df.count,aes(x = col,
                            y = NA_count)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))
  
  p2 <-  ggplot(df.count,aes(x = col,
                             y = NA_percent)) +
    geom_col() +
    scale_y_continuous(limits = c(0,100)) +
    theme(axis.text.x = element_text(angle = 90))
  
  plot_grid(p1,p2,nrow = 2)
}

check_time_span <- function(df){
  require(tidyverse)
  
  df.time <- df %>% 
    summarise(distinct_dates = n_distinct(date),
              min_date = min(date,na.rm = T),
              max_date = max(date,na.rm = T),
              range_days = max(date,na.rm = T) - min(date,na.rm = T))
  
  print(df.time)
  
  df %>% 
    group_by(state) %>% 
    summarise(distinct_dates = n_distinct(date)) %>% 
    ggplot(.,aes(x = state,
                 y = distinct_dates)) +
    geom_col() +
    theme(axis.text.x = element_text(angle = 90))
    
}


plot_confirmed_death_per_region <- function(region){
  require(tidyverse)
  require(cowplot)
  
  #region <- rlang::enquo(region)
  p1 <- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = confirmed,
                 color = state,
                 group = state)) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Confirmed cases + ",region))
  
  p2 <- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = deaths,
                 color = state,
                 group = state)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    ggtitle(paste0("Death cases + ",region))
  
  plot_grid(p1,p2,nrow = 2)
}
