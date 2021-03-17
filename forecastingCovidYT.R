# Library
library(tidyverse)
library(lubridate)
library(plotly)
# set minimal theme
theme_set(theme_minimal())
covid19<-read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

covid19 %>%
  pivot_longer(-c('Province/State','Country/Region',Lat,Long),
               names_to= "date",
               values_to= "confirmed_n") %>%
  select(-c(Lat,Long)) %>%
  rename(province_state='Province/State',
         country_region='Country/Region') %>%
  mutate(date=mdy(date)) %>%
  group_by(country_region,date) %>%
  arrange(date) %>%
  group_by(country_region) %>%
  mutate(new_cases_n=confirmed_n-lag(confirmed_n, default = 0)) %>%
  ungroup() %>%
  filter(country_region %in% c("US", "Brazil", "India")) %>%
  ggplot(aes(x=date, y=new_cases_n, color=country_region)) +
  geom_line(show.legend = FALSE) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y") +
  scale_y_continuous(labels = scales::comma) +
  facet_wrap(~country_region, ncol=1, scales="free_y") +
  labs(x="Date",y="New Cases",
       title= "New confirmed COVID-19 cases in the United States, Brazil, and India (Feb 20-Mar 21)")
  
 
  #summarise(confirmed_n=sum(confirmed_n)) %>%
  #ungroup() %>%
  #filter(country_region=="US") %>%
  #ggplot(aes(x=date, y=confirmed_n)) +
  #geom_line()