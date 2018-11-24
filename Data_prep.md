Download data
================
Team
November 10, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------------------------------------------------------------------------------------------------- tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts ------------------------------------------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(xml2)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
library(rnoaa)
library(patchwork)
```

The purpose of this file is to download the datasets and create the 'interim' dataset which will be used in the main analysis. Since some datasets are large, the final products after running this code is the `RData` files for further use.
==============================================================================================================================================================================================================================================

Incident data (response from Fire company)
------------------------------------------

``` r
incident_dat_2017 <-  
  read_csv('data/Incidents_Responded_to_by_Fire_Companies.csv',
  col_types = "cicccicciccccccccccciccccccc") %>% 
  janitor::clean_names() %>% 
  #recode date/time
  mutate(incident_date_time = mdy_hms(incident_date_time),
         arrival_date_time = mdy_hms(arrival_date_time)) %>% 
  #select year 2017
  filter(year(incident_date_time) == 2017,
         incident_type_desc == "300 - Rescue, EMS incident, other") %>% 
  select(im_incident_key, incident_date_time, arrival_date_time,
         street_highway:borough_desc) %>% 
  na.omit() %>% 

  # Added response time(minute) variable
  # mutate(response_time = arrival_date_time - incident_date_time) %>%
  mutate(response_time = difftime(arrival_date_time, incident_date_time, units = 'mins'),
  # Added hour variable
  hour = hour(incident_date_time),
  date = date(incident_date_time),
  # Added incident_month and incident_day variables from incident_date_time
  incident_date = as.Date(incident_date_time)) %>% 
  separate(incident_date, 
           into = c("incident_year", "incident_month", "incident_day"), 
           sep = "-") %>% 
  select(-incident_year) %>% 
  mutate(incident_month = as.numeric(incident_month),
         incident_day = as.numeric(incident_day))
```

    ## Warning: Unnamed `col_types` should have the same length as `col_names`.
    ## Using smaller of the two.

    ## Warning in rbind(names(probs), probs_f): number of columns of result is not
    ## a multiple of vector length (arg 1)

    ## Warning: 13119 parsing failures.
    ## row # A tibble: 5 x 5 col     row col        expected       actual            file                   expected   <int> <chr>      <chr>          <chr>             <chr>                  actual 1    91 FIRE_SPRE~ no trailing c~ " - Beyond build~ 'data/Incidents_Respo~ file 2   199 FIRE_SPRE~ no trailing c~ " - Confined to ~ 'data/Incidents_Respo~ row 3   440 FIRE_SPRE~ no trailing c~ " - Confined to ~ 'data/Incidents_Respo~ col 4   492 FIRE_SPRE~ no trailing c~ " - Confined to ~ 'data/Incidents_Respo~ expected 5   569 FIRE_SPRE~ no trailing c~ " - Confined to ~ 'data/Incidents_Respo~
    ## ... ................. ... .......................................................................... ........ .......................................................................... ...... .......................................................................... .... .......................................................................... ... .......................................................................... ... .......................................................................... ........ ..........................................................................
    ## See problems(...) for more details.

``` r
save(incident_dat_2017, file = "data/incident_dat_2017.RData")
```

Neighborhood variable (by zipcode) dataset
------------------------------------------

``` r
url = "https://www.health.ny.gov/statistics/cancer/registry/appendix/neighborhoods.htm?fbclid=IwAR3N4VlKC1OehRZyEuDYPEAE7AFAEXXIRC11seIBKxA-0fd3g4hL0QvnV20"

xml = read_html(url)

zip_code_table = (xml %>% html_nodes(css = "table")) %>% 
  .[[1]] %>%
  html_table() %>% 
  janitor::clean_names() %>%  
  select(neighborhood, zip_codes) %>% 
  separate(zip_codes, c("a", "b", "c", "d", "e", "f", "g", "h", "i"), 
           sep = ",") %>% 
  gather(key = to_remove, value = zip_code, a:i) %>% 
  select(-to_remove) %>% 
  na.omit() %>% 
  distinct() %>% 
  mutate(zip_code = as.numeric(zip_code))
```

    ## Warning: Expected 9 pieces. Missing pieces filled with `NA` in 41 rows [1,
    ## 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].

``` r
save(zip_code_table, file = "data/zipcode.RData")
```

Add weather-related dataset
---------------------------

``` r
library(rnoaa)

nyc_weather_2017 = 
  rnoaa::meteo_pull_monitors("USW00094728", 
                             var = c("PRCP", "TMIN", "TMAX", "SNOW", "SNWD"), 
                             date_min = "2017-01-01", 
                             date_max = "2017-12-31"
                             ) %>% 
  mutate(tmin = tmin/10, tmax = tmax/10, prcp = prcp/10) %>% 
  select(-id)

save(nyc_weather_2017, file = "data/nyc_weather_2017.RData")
```

Street closure data
-------------------

``` r
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
```

    ## Parsed with column specification:
    ## cols(
    ##   NODEID = col_integer(),
    ##   ONSTREETNAME = col_character(),
    ##   FROMSTREETNAME = col_character(),
    ##   BOROUGH_CODE = col_character(),
    ##   WORK_START_DATE = col_character(),
    ##   WORK_END_DATE = col_character(),
    ##   PURPOSE = col_character()
    ## )

``` r
save(street_closure_2017, file = "data/street_closure_2017.RData")
```
