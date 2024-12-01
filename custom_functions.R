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
