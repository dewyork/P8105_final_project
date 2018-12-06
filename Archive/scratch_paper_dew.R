library(tidyverse)
library(lubridate)
library(readxl)
#Original data from the website
incident_dat <-  
  read_csv('data/Incidents_Responded_to_by_Fire_Companies.csv',
  col_types = "cicccicciccccccccccciccccccc")

# subset <- incident_dat %>% slice(1:100) %>% 
#   mutate(INCIDENT_DATE_TIME = mdy_hms(INCIDENT_DATE_TIME),
#         ARRIVAL_DATE_TIME = mdy_hms(ARRIVAL_DATE_TIME),
#         LAST_UNIT_CLEARED_DATE_TIME = mdy_hms(LAST_UNIT_CLEARED_DATE_TIME))

incident_dat_2017 <- incident_dat %>% 
  #recode date/time
  mutate(INCIDENT_DATE_TIME = mdy_hms(INCIDENT_DATE_TIME),
         ARRIVAL_DATE_TIME = mdy_hms(ARRIVAL_DATE_TIME),
         LAST_UNIT_CLEARED_DATE_TIME = mdy_hms(LAST_UNIT_CLEARED_DATE_TIME)) %>% 
  #select year 2017
  filter(year(INCIDENT_DATE_TIME) == 2017,
         INCIDENT_TYPE_DESC == "300 - Rescue, EMS incident, other") 

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
  

#Streat closure data
street_closure_2017 <-  
  read_csv('data/Street_Closures_due_to_construction_activities_by_Intersection.csv') %>% 
  janitor::clean_names() %>% 
  #recode date/time
  mutate(work_start_date = mdy_hms(work_start_date),
         work_end_date = mdy_hms(work_end_date),
         work_time = round(difftime(work_end_date, work_start_date, 
                                    units = 'days'), 0)) %>% 
  #select year 2017
  filter(year(work_start_date) == 2017) %>% 
  select(-purpose) %>% 
  na.omit() %>% 
  mutate(A = toupper(str_replace_all(onstreetname, fixed(" "), "")),
         B = toupper(str_replace_all(fromstreetname, fixed(" "), "")))


#list of streets of intereste
street_zip <- read_excel('street_junctions_zipcode.xlsx') %>% 
  #delete white space from street name 
  mutate(A = toupper(str_replace_all(street_A, fixed(" "), "")),
         B = toupper(str_replace_all(street_B, fixed(" "), "")))

# Extract street closure data based on street_zip

#Merge A to A and B to B
first_set <- street_zip %>% 
  inner_join(street_closure_2017, by=c("A", "B"))


#Merge A to B and B to A
second_set <- street_zip %>% 
  inner_join(street_closure_2017, by=c("A" = 'B', 'B' = "A"))

street_closure_washington_height <- first_set %>% 
  bind_rows(second_set) %>% 
  select(street_A:neighborhood, work_start_date:work_time)


#Create function that checks the closure duration whether it happens while the call was made

check_street_closure <- function(datetime, zip){
  #Filter only the zipcode of interest
  zipcode_closure <- street_closure_washington_height %>% 
    filter(zipcode == zip) %>% 
    mutate(street_closed = ifelse(work_start_date < datetime & work_end_date > datetime, 1, 0))
  
  if(sum(zipcode_closure$street_closed) > 0){
    return(1)
  } else {return(0)}
  # zipcode_closure %>% mutate
}

#Then create variable street closure (y/n) in the incident_dat_2017 focusing on Washington heights
incident_height <- incident_dat_2017 %>% mutate(zipcode = as.numeric(ZIP_CODE)) %>% 
  filter(zipcode %in% c(10032, 10034, 10040)) 
# %>%   mutate(street_closed = check_street_closure(datetime = INCIDENT_DATE_TIME, zip = zipcode))

incident_height$street_closed <- NA
for (i in 1:nrow(incident_height)) {
  incident_height$street_closed[i] <- 
    check_street_closure(incident_height$INCIDENT_DATE_TIME[i], zip = incident_height$zipcode[i])
}



