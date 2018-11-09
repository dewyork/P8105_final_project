library(tidyverse)
library(lubridate)
incident_dat <- 
  read_csv('data/Incidents_Responded_to_by_Fire_Companies.csv',
  col_types = "cicccicciccccccccccciccccccc")
incident_dat_2017 <- incident_dat %>% 
  mutate(INCIDENT_DATE_TIME = mdy_hms(INCIDENT_DATE_TIME),
         ARRIVAL_DATE_TIME = mdy_hms(ARRIVAL_DATE_TIME),
         LAST_UNIT_CLEARED_DATE_TIME = mdy_hms(LAST_UNIT_CLEARED_DATE_TIME)) %>% 
  filter(year(incident_dat$INCIDENT_DATE_TIME) == 2017)


# incident_dat <- 
  # read_csv('data/Incidents_Responded_to_by_Fire_Companies.csv')
subset <- incident_dat %>% slice(1:100)
subset <- subset %>% 
  mutate(INCIDENT_DATE_TIME = mdy_hms(INCIDENT_DATE_TIME),
  ARRIVAL_DATE_TIME = mdy_hms(ARRIVAL_DATE_TIME),
  LAST_UNIT_CLEARED_DATE_TIME = mdy_hms(LAST_UNIT_CLEARED_DATE_TIME))


g <- mdy_hms(subset$INCIDENT_DATE_TIME)
