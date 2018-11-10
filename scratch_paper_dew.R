library(tidyverse)
library(lubridate)
#Original data from the website
incident_dat <-  
  read_csv('data/Incidents_Responded_to_by_Fire_Companies.csv',
  col_types = "cicccicciccccccccccciccccccc")

subset <- incident_dat %>% slice(1:100) %>% 
  mutate(INCIDENT_DATE_TIME = mdy_hms(INCIDENT_DATE_TIME),
        ARRIVAL_DATE_TIME = mdy_hms(ARRIVAL_DATE_TIME),
        LAST_UNIT_CLEARED_DATE_TIME = mdy_hms(LAST_UNIT_CLEARED_DATE_TIME))

incident_dat_2017 <- incident_dat %>% 
  #recode date/time
  mutate(INCIDENT_DATE_TIME = mdy_hms(INCIDENT_DATE_TIME),
         ARRIVAL_DATE_TIME = mdy_hms(ARRIVAL_DATE_TIME),
         LAST_UNIT_CLEARED_DATE_TIME = mdy_hms(LAST_UNIT_CLEARED_DATE_TIME)) %>% 
  #select year 2017
  filter(year(INCIDENT_DATE_TIME) == 2017)

#save 2017 data in data folder
# save(incident_dat_2017, file = "data/incident_dat_2017.RData")

load('data/incident_dat_2017.Rdata')

#create frequency by day for the whole year (x label as numeric)
incident_dat_2017 %>% mutate(day = yday(INCIDENT_DATE_TIME)) %>% 
  group_by(day) %>% count() %>% 
  ggplot(aes(x = day, y = n)) + geom_line() + labs(y = 'Frequency')
 # x label as date
incident_dat_2017 %>% mutate(date = date(INCIDENT_DATE_TIME)) %>% 
  group_by(date) %>% count() %>% 
ggplot(aes(x = date, y = n)) + geom_line() + labs(y = 'Frequency')

#Monthly trend
incident_dat_2017 %>% mutate(month = month(INCIDENT_DATE_TIME)) %>% 
  group_by(month) %>% count() %>% 
  ggplot(aes(x = month, y = n)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 45)) +  labs(y = 'Frequency') +
  scale_x_continuous(breaks = 1:12, labels = month.name)

#By the hour
incident_dat_2017 %>% mutate(hour = hour(INCIDENT_DATE_TIME)) %>% 
  group_by(hour) %>% count() %>% 
  ggplot(aes(x = hour, y = n)) + geom_line() + labs(y = 'Frequency')


#Calculate lag time (response time)
#And look at lag time (average) by the hour
incident_dat_2017 %>% mutate(response_time = 
    difftime(ARRIVAL_DATE_TIME, INCIDENT_DATE_TIME, units = 'mins'),
    hour = hour(INCIDENT_DATE_TIME)) %>% 
    group_by(hour) %>% 
    summarise(mean_response_time = mean(response_time, na.rm = TRUE)) %>% 
  ggplot(aes(x = hour, y = mean_response_time)) + geom_line() + 
  labs(y = 'Mean response time (minutes)')
  



