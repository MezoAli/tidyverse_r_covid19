rm(list=ls())
graphics.off()
install.packages("janitor")
install.packages("rio")

library("tidyverse")
library("janitor")
library("rio")
source("./custom_functions.R")

# 1) importing all Data

directory <- "./data/csse_covid_19_daily_reports_us/"
daily.cases.df <- list.files("./data/csse_covid_19_daily_reports_us/") %>% 
  as_tibble() %>% 
  mutate(full_path = paste0(directory,value)) %>% 
  mutate(content = map(.x = full_path,
                       .f = ~rio::import(.))) %>% 
  pull(content) %>% 
  bind_rows(.)

daily.cases.df <- clean_names(daily.cases.df) %>% 
  select(state = province_state,
         everything()) %>% 
  mutate(last_update = ymd_hms(last_update)) %>% 
  mutate(date = date(last_update))

summary(daily.cases.df$date)

getwd()


GDP_data <- rio::import(file = "./data/GDP_USA_states.xlsx",
                        sheet = "clean data")




GDP_data <- GDP_data %>% 
  clean_names(.) %>% 
  select(state = state_or_territory,
         gdp_nominal = nominal_gdp_2020,
         gdp_per_capita = gdp_per_capita_2020)

GDP_data <- GDP_data %>% 
  mutate_all(.,.f = str_remove_all,",") %>% 
  mutate_all(.,.f = str_remove_all,"\\$") %>% 
  mutate_at(.,.vars = colnames(.)[2:3],.f = as.numeric)


population.data <-  rio::import(file = "./data/Population_USA_states.xlsx") %>% 
                                  clean_names(.)

population.data <- population.data %>% 
  select(state = name,
         pop = pop_2019)


daily.vaccination <-  rio::import(file = "./data/us-daily-covid-vaccine-doses-administered.csv") %>% 
  clean_names(.)

daily.vaccination <- daily.vaccination %>% 
  select(state = entity,
         date = day,
         daily_vaccinations)




covid.responses.df <-  rio::import(file = "./data/OxCGRT_US_latest.csv") %>% 
  clean_names(.)

covid.responses.df <- covid.responses.df %>% 
  mutate(date = ymd(date)) %>% 
  select(state = region_name ,
         date,
         contains("index")) %>% 
  mutate_at(.,.vars = colnames(.)[3:ncol(.)],
            .funs = as.numeric)


# 2) check data for validity

# check for na values

count.na(covid.responses.df)
count.na(daily.cases.df)
count.na(daily.vaccination)
count.na(GDP_data)
count.na(population.data)

# check for time series
check_time_span(covid.responses.df)
check_time_span(daily.cases.df)
check_time_span(daily.vaccination)



 


