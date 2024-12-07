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


plot_confirmed_death_per_state <- function(state){
  require(tidyverse)
  require(cowplot)
  
  p1 <- main.df %>% 
    filter(state == !!state) %>% 
    ggplot(.,aes(x = date,
                 y = confirmed,
                 color = state,
                )) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Confirmed cases + ",state))
  
  p2 <- main.df %>% 
    filter(state == !!state) %>% 
    ggplot(.,aes(x = date,
                 y = deaths,
                 color = state,
                 )) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    ggtitle(paste0("Death cases + ",state))
  
  plot_grid(p1,p2,nrow = 2)
}


plot_confirmed_death_per_region_7d_average <- function(region){
  require(tidyverse)
  require(cowplot)
  
  p1 <- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = confirmed_daily_cases_7d_avg,
                 color = state,
                 group = state)) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Confirmed cases 7 days average + ",region))
  
  p2 <- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = deaths_daily_cases_7d_avg,
                 color = state,
                 group = state)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    ggtitle(paste0("Death cases 7 days average + ",region))
  
  plot <- plot_grid(p1,p2,nrow = 2)
  ggsave(filename = paste0("confirmed_death_cases_7d_avg_",region,".png"),
         plot = plot,
         width = 30,
         height = 20,
         dpi = 600,
         units = "cm"
         )
}

plot_confirmed_death_per_region_vacc_doses <- function(region){
  require(tidyverse)
  require(cowplot)
  
  p1 <- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = confirmed_daily_cases_7d_avg,
                 color = state,
                 group = state)) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Confirmed cases 7 days average + ",region))
  
  p2 <- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = deaths_daily_cases_7d_avg,
                 color = state,
                 group = state)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    ggtitle(paste0("Death cases 7 days average + ",region))
  
  p3<- main.df %>% 
    filter(region == !!region) %>% 
    ggplot(.,aes(x = date,
                 y = daily_vaccinations,
                 color = state,
                 group = state)) +
    geom_line() +
    geom_point() +
    scale_color_viridis_d() +
    ggtitle(paste0("Daily vaccination + ",region))
  
  plot <- plot_grid(p1,p2,p3,nrow = 3)
  plot
  ggsave(filename = paste0("confirmed_death_cases_vacc_doses_",region,".png"),
         plot = plot,
         width = 30,
         height = 20,
         dpi = 600,
         units = "cm"
  )
}

plot_confirmed_death_per_state_vacc_doses <- function(state){
  require(tidyverse)
  require(cowplot)
  
  p1 <- main.df %>% 
    filter(state == !!state) %>% 
    ggplot(.,aes(x = date,
                 y = confirmed_daily_cases_7d_avg,
                 color = region
                )) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Confirmed cases 7 days average + ",state))
  
  p2 <- main.df %>% 
    filter(state == !!state) %>% 
    ggplot(.,aes(x = date,
                 y = deaths_daily_cases_7d_avg,
                 color = region
                 )) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Death cases 7 days average + ",state))
  
  p3<- main.df %>% 
    filter(state == !!state) %>% 
    ggplot(.,aes(x = date,
                 y = daily_vaccinations,
                 color = region,
                 )) +
    geom_line(show.legend = F) +
    geom_point(show.legend = F) +
    scale_color_viridis_d() +
    ggtitle(paste0("Daily vaccination + ",state))
  
  plot <- plot_grid(p1,p2,p3,nrow = 3)
  print(plot)
  ggsave(filename = paste0("confirmed_death_cases_vacc_doses_",state,".png"),
         plot = plot,
         width = 30,
         height = 20,
         dpi = 600,
         units = "cm"
  )
}


plot_covid_indicator_state_level <- function(state){
  require(tidyverse)
  
  state.data <- main.df %>% 
    filter(state == !!state)
  
  p1 <- state.data %>% 
    select(state,confirmed,deaths,daily_vaccinations,date) %>% 
    pivot_longer(.,cols = c("confirmed","deaths","daily_vaccinations"),
                 names_to = "indicator",
                 values_to = "values") %>%
    mutate(indicator = as_factor(indicator)) %>% 
    ggplot(.,aes(x = date,
                 y = values,
                 fill = indicator)) +
    geom_area(alpha = 0.5) +
    labs(x = "Date",
         y = "Number of Cases") +
    ggtitle(paste0("COVID-19 indicators (Total Cases ) for ",state)) +
    theme(plot.title = element_text(face = "bold",hjust = 0.5))
  
  p2 <- state.data %>% 
    select(state,confirmed_daily_cases_7d_avg,deaths_daily_cases_7d_avg,vaccine_doses_7d_avg,date) %>% 
    pivot_longer(.,cols = c("confirmed_daily_cases_7d_avg","deaths_daily_cases_7d_avg","vaccine_doses_7d_avg"),
                 names_to = "indicator",
                 values_to = "values") %>%
    mutate(indicator = as_factor(indicator),
           values = na_if(values,0)) %>% 
    ggplot(.,aes(x = date,
                 y = values,
                 color = indicator,
                 group = indicator)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.2) +
    scale_y_log10() +
    labs(x = "Date",
         y = "Number of Cases") +
    ggtitle(paste0("COVID-19 indicator (7 Days Average) for ",state)) +
    theme(plot.title = element_text(face = "bold",hjust = 0.5))
  
  p3 <- state.data %>% 
    select(state,stringency_index_for_display,
           government_response_index_for_display,
           containment_health_index_for_display,
           economic_support_index_for_display
           ,date) %>% 
    pivot_longer(.,cols = c("stringency_index_for_display","economic_support_index_for_display","government_response_index_for_display","containment_health_index_for_display"),
                 names_to = "indicator",
                 values_to = "values") %>%
    mutate(indicator = as_factor(indicator)) %>% 
    ggplot(.,aes(x = date,
                 y = values,
                 fill = indicator)) +
    geom_area(alpha = 0.5) +
    labs(x = "Date",
         y = "Government Response") +
    ggtitle(paste0("COVID-19 indicators ( State Response ) for ",state)) +
    theme(plot.title = element_text(face = "bold",hjust = 0.5))
  
  plots = plot_grid(p1,p2,p3,nrow = 3)
  
  print(plots)
  ggsave(filename = paste0("COVID-19_indicator_",state,".png"),
         plot = plots,
         width = 35,
         height = 25,
         dpi = 600,
         units = "cm"
  )
  
  
}

