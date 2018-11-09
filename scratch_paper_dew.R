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

#create 
incident_dat_2017 %>% ggplot(aes(x = yday(INCIDENT_DATE_TIME)))

