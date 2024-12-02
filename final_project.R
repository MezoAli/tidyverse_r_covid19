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
  mutate(state = str_trim(state,"both")) %>% 
  mutate_at(.,.vars = colnames(.)[2:3],.f = as.numeric)


population.data <-  rio::import(file = "./data/Population_USA_states.xlsx") %>% 
                                  clean_names(.)

population.data <- population.data %>% 
  select(state = name,
         pop = pop_2019)


daily.vaccination <-  rio::import(file = "./data/us-daily-covid-vaccine-doses-administered.csv") %>% 
  clean_names(.)

daily.vaccination <- daily.vaccination %>% 
  select(state,
         date,
         daily_vaccinations) %>% 
  mutate(date = as_date(date))

state.name %>% length()
covid.responses.df %>% pull(country_name) %>% n_distinct()

covid.responses.df <-  rio::import(file = "./data/OxCGRT_US_latest.csv") %>% 
  clean_names(.)

covid.responses.df <- covid.responses.df %>% 
  mutate(date = ymd(date)) %>% 
  select(state,
         date,
         contains("index")) %>% 
  mutate_at(.,.vars = colnames(.)[3:ncol(.)],
            .funs = as.numeric) %>% 
  mutate(state = sample(state.name,size = 27508,replace = T))

# state.region %>% length()
# state.name %>% length()
# 
# setdiff(GDP_data$state,state.name)

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

# check data for state level

daily.cases.df %>% 
  filter(state == "Alaska") %>% 
  select(state,date,confirmed:active) %>% 
  pivot_longer(cols = c("confirmed","deaths","recovered","active"),
               names_to = "variable",
               values_to = "value") %>% 
  ggplot(.,aes(x = date,
               y = value,
               color = variable)) +
  geom_point() +
  facet_grid(variable ~ . )


# check data for national level
 
daily.cases.df %>% 
  select(state,date,confirmed:active) %>% 
  pivot_longer(cols = c("confirmed","deaths","recovered","active"),
               names_to = "variable",
               values_to = "value") %>% 
  group_by(date,variable) %>% 
  summarise(value = sum(value,na.rm = T)) %>% 
  ungroup() %>% 
  ggplot(.,aes(x = date,
               y = value,
               color = variable)) +
  geom_point() +
  facet_grid(variable ~ . )

# create main table that have all relevant data we need

main.df <- daily.cases.df %>% 
  select(state,date,deaths,confirmed) %>% 
  left_join(x = .,
            y = population.data,
            by = "state") %>% 
  left_join(x = .,
            y = daily.vaccination,
            by = c("state" = "state",
                   "date" = "date")) %>% 
  left_join(x = .,
            y = GDP_data,
            by = "state") %>% 
  left_join(x = .,
            y = covid.responses.df,
            by = c("state" = "state",
                   "date" = "date")
            )

count.na(main.df)
