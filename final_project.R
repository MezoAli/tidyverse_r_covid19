rm(list=ls())
graphics.off()
install.packages("janitor")
install.packages("rio")
install.packages("zoo")

library("tidyverse")
library("janitor")
library("rio")
library("zoo")
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
  filter(state != "District of Columbia") %>% 
  filter(state != "United States") %>% 
  arrange(state) %>% 
  mutate(region = state.region) %>% 
  mutate_at(.,.vars = colnames(.)[2:3],.f = as.numeric)

# covid.responses.df %>%  distinct(state) %>% pull(state) %>% length()
# daily.cases.df%>%  distinct(state) %>% pull(state) %>% length()
# daily.vaccination %>%  distinct(state) %>% pull(state) %>% length()


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
         daily_vaccinations) %>% 
  mutate(date = as_date(date))

state.name %>% length()
covid.responses.df %>% pull(country_name) %>% n_distinct()

covid.responses.df <-  rio::import(file = "./data/OxCGRT_US_latest.csv") %>% 
  clean_names(.)

covid.responses.df <- covid.responses.df %>% 
  mutate(date = ymd(date)) %>% 
  select(state = region_name,
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

# main.df %>% 
#   filter(state == "California") %>% 
#   summarise(confirmed_total = sum(confirmed,na.rm = T))


# check daily vaccinations and replace na with 0

main.df %>% 
  filter(is.na(daily_vaccinations)) %>% 
  nrow()

main.df <- main.df %>% 
  mutate(daily_vaccinations = replace_na(daily_vaccinations,0))

# add coulmn that have confirmed daily cases

main.df <- main.df %>% 
  group_by(state) %>% 
  mutate(confirmed_daily_cases = confirmed - lag(confirmed,1),
         deaths_daily_cases = deaths - lag(deaths,1)) %>% 
  ungroup()

# check if confirmed_daily or deaths daily have negative values

main.df %>% 
  filter(confirmed_daily_cases < 0)

main.df %>% 
  filter(deaths_daily_cases < 0)

# fix the negative issues and replave negatives with 0

main.df <- main.df %>% 
  mutate(confirmed_daily_cases = case_when(confirmed_daily_cases >= 0 ~ confirmed_daily_cases,
                                           T ~ 0),
         deaths_daily_cases = case_when(deaths_daily_cases >= 0 ~ deaths_daily_cases,
                                        T ~ 0))


# states per region

main.df %>% 
  filter(!is.na(region)) %>% 
  filter(!is.na(date)) %>% 
  nrow()

main.df %>% 
  filter(!is.na(region)) %>% 
  group_by(region) %>% 
  summarise(state_per_region = n_distinct(state)) %>% 
  ungroup()


main.df <- main.df %>% 
  mutate(state_ = str_to_lower(state)) %>% 
  filter(!is.na(region))

max_date <- main.df %>% pull(date) %>% max(.)
# show map
  

basic_map <- main.df %>% 
  filter(date == max_date) %>% 
  left_join(x = .,
            y = map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill = region)) +
    geom_polygon(color = "black") +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave(filename = "basic_map.png",
       plot = basic_map,
       width = 29,
       height = 21,
       units = "cm",
       dpi = 600)


# show confirmed,death cases per each region

regions <- main.df %>% distinct(region) %>% pull(.)
num_states_per_region <- main.df %>% 
  group_by(region) %>% 
  summarise(n_state = n_distinct(state)) %>% 
  ungroup()

plot_confirmed_death_per_region("Northeast")
plot_confirmed_death_per_region("South")


# show confirmed,death cases per each region

states <- main.df %>% distinct(state) %>% pull(.)

plot_confirmed_death_per_state("Alabama")
plot_confirmed_death_per_state("California")

# which state paid the heights confirmed and deaths cases per population plot
setdiff(population.data$state,GDP_data$state)



main.df <- main.df %>% 
  mutate(relative_confirmed = (confirmed / pop) * 100,
         relative_death = (deaths / pop) * 100)


relative_confirmed_total_plot <- main.df %>% 
  filter(date == max_date) %>% 
  arrange(relative_confirmed) %>% 
  mutate(state = as.factor(state),
         state = fct_inorder(state)) %>% 
  ggplot(.,aes(x = relative_confirmed,
              y = state,
              fill = region)) +
  geom_col() +
  ggtitle("Relative Confirmed Cases per State") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))

relative_death_total_plot <- main.df %>% 
  filter(date == max_date) %>% 
  arrange(relative_death) %>% 
  mutate(state = as.factor(state),
         state = fct_inorder(state)) %>% 
  ggplot(.,aes(x = relative_death,
               y = state,
               fill = region)) +
  geom_col() +
  ggtitle("Relative Death Cases per State") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))

plot_total <- plot_grid(relative_confirmed_total_plot,
                      relative_death_total_plot,
                      ncol = 2)

ggsave("./total_relative_confirmed_deaths_per_state-plot.png",
       plot = plot_total,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)

# which state paid the heights confirmed and deaths cases per population map

relative_confirmed_total_map <- main.df %>% 
  filter(date == max_date) %>% 
  left_join(x = .,
            y =  map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill = relative_confirmed)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white",
                      high = "red") +
  ggtitle("Relative Confirmed Cases per State") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  labs(x = "" , y = "")


relative_death_total_map <- main.df %>% 
  filter(date == max_date) %>% 
  left_join(x = .,
            y =  map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill = relative_death)) +
  geom_polygon(color = "black") +
  scale_fill_gradient(low = "white",
                      high = "black") +
  ggtitle("Relative Death Cases per State") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  labs(x = "" , y = "")

map_total <- plot_grid(relative_confirmed_total_map,
                        relative_death_total_map,
                        nrow = 2)

ggsave("./total_relative_confirmed_deaths_per_state_map.png",
       plot = map_total,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600)



# plot confirmed/deaths cases per 7 days average


main.df <- main.df %>% 
  arrange(state,date) %>% 
  group_by(state) %>% 
  mutate(confirmed_daily_cases_7d_avg = rollapply(confirmed_daily_cases,FUN = mean,width = 7,align = "right",fill = NA),
         deaths_daily_cases_7d_avg = rollapply(deaths_daily_cases,FUN = mean,width = 7,align = "right",fill = NA),
         vaccine_doses_7d_avg = rollapply(daily_vaccinations,FUN = mean,width = 7,align = "right",fill = NA)) %>% 
  ungroup()


regions
plot_confirmed_death_per_region_7d_average("North Central")  
plot_confirmed_death_per_region_7d_average("South")  
plot_confirmed_death_per_region_7d_average("Northeast")  

# does state wealth and population affects confirmed and deaths cases

main.df %>%
  filter(date == max_date) %>% 
  ggplot(.,aes(x = confirmed,
               y = deaths,
               color = gdp_per_capita,
               size = pop)) +
  geom_jitter(alpha = 0.7) +
  scale_color_gradient(low = "brown1",high = "green")+
  scale_size_area(max_size = 40) +
  xlab("Confirmed cases") +
  ylab("Deaths Cases") +
  ggtitle("Total Confirmed and Death Cases vs Population and GDP")+
  theme(plot.title = element_text(face = "bold",hjust = 0.5)) +
  facet_wrap(. ~ region)

ggsave(filename ="confirmed_death_cases_vs_GDP_pop.png",
       plot = last_plot(),
       width = 30,
       height = 20,
       dpi = 600,
       units = "cm"
)


# does vaccination affect confirmed cases and deaths per region

regions
plot_confirmed_death_per_region_vacc_doses("South")
plot_confirmed_death_per_region_vacc_doses("West")


# does vaccination affect confirmed cases and deaths per state

states <- main.df %>% 
  pull(state) %>% 
  unique(.)

states
plot_confirmed_death_per_state_vacc_doses("Alabama")
plot_confirmed_death_per_state_vacc_doses("California")
plot_confirmed_death_per_state_vacc_doses("Pennsylvania")


# show confirmed cases on map over time

main.df <- main.df %>% 
  arrange(state,date) %>% 
  group_by(state) %>%
  distinct(date,.keep_all = T) %>% 
  mutate(date_id = row_number()) %>% 
  ungroup() %>% 
  mutate(days_30_flag = case_when(date_id == 1 ~ T,
                                  date == max_date ~ T,
                                  date_id %% 30 == 0 ~ T,
                                  T ~ F))
  

relative_confirmed_total_map_per_month <- main.df %>% 
  filter(days_30_flag) %>%
  filter(state != "Florida") %>% 
  select(state,state_,region,confirmed,date,days_30_flag) %>% 
  left_join(x = .,
            y =  map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill =confirmed)) +
  geom_polygon(color = "black") +
  facet_wrap(~ date) +
  scale_fill_gradient(low = "white",
                      high = "red") +
  ggtitle("Relative Confirmed Cases per State Over Time") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  labs(x = "" , y = "")

ggsave(filename = "relative_confirmed_total_map_over_time.png",
       plot = relative_confirmed_total_map_per_month,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600
       )


# show deaths cases on map over time

relative_deaths_total_map_over_time <- main.df %>% 
  filter(days_30_flag) %>%
  filter(state != "Florida") %>% 
  select(state,state_,region,confirmed,deaths,date,days_30_flag) %>% 
  left_join(x = .,
            y =  map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill =deaths)) +
  geom_polygon(color = "black") +
  facet_wrap(~ date) +
  scale_fill_gradient(low = "white",
                      high = "black") +
  ggtitle("Relative Deaths Cases per State Over Time") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  labs(x = "" , y = "")

ggsave(filename = "relative_deaths_total_map_over_time.png",
       plot = relative_deaths_total_map_over_time,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600
)

# show vaccination on map over time

relative_vacc_on_map_over_time <- main.df %>% 
  filter(days_30_flag) %>%
  filter(state != "Florida") %>% 
  select(state,state_,region,daily_vaccinations,date,days_30_flag) %>% 
  left_join(x = .,
            y =  map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill =daily_vaccinations)) +
  geom_polygon(color = "black") +
  facet_wrap(~ date) +
  scale_fill_gradient(low = "white",
                      high = "green") +
  ggtitle("Vaccination per State Over Time") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  labs(x = "" , y = "")

ggsave(filename = "vaccination_over_time.png",
       plot = relative_vacc_on_map_over_time,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600
)


# show each state response rate over time

state_response_rate_over_time <- main.df %>% 
  filter(days_30_flag) %>%
  filter(state != "Florida") %>% 
  select(state,state_,region,,date,days_30_flag,stringency_index_for_display) %>% 
  left_join(x = .,
            y =  map_data("state"),
            by = c("state_"="region")) %>% 
  ggplot(.,aes(x = long,
               y = lat,
               group = group,
               fill =stringency_index_for_display)) +
  geom_polygon(color = "black") +
  facet_wrap(~ date) +
 scale_fill_viridis_c(option = "magma") +
  ggtitle("Response Rate per State Over Time") +
  theme(axis.ticks = element_blank(),
        axis.text = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) +
  labs(x = "" , y = "")

ggsave(filename = "response_rate_over_time.png",
       plot = state_response_rate_over_time,
       width = 30,
       height = 20,
       units = "cm",
       dpi = 600
)


# COVID 19 indicators per state level
states

plot_covid_indicator_state_level("California")
plot_covid_indicator_state_level("Florida")
plot_covid_indicator_state_level("Nevada")
plot_covid_indicator_state_level("New Mexico")

