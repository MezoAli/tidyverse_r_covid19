rm(list=ls())
graphics.off()
install.packages("janitor")
install.packages("rio")

library("tidyverse")
library("janitor")
library("rio")

directory <- "./data/csse_covid_19_daily_reports_us/"
daily.cases.df <- list.files("./data/csse_covid_19_daily_reports_us/") %>% 
  as_tibble() %>% 
  mutate(full_path = paste0(directory,value)) %>% 
  mutate(content = map(.x = full_path,
                       .f = ~read_csv(.))) %>% 
  pull(content) %>% 
  bind_rows(.)

daily.cases.df <- clean_names(daily.cases.df)
getwd()


GDP_data <- rio::import(file = "./data/GDP_USA_states.xlsx",
                        sheet = "clean data")




GPD_data <- GPD_data %>% 
  select(state = state_or_territory,
         gdp_nominal = nominal_gdp_2020,
         gdp_per_capita)
