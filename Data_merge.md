Create final dataset
================
Team
November 10, 2018

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 3.0.0     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.0
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------- tidyverse_conflicts() --
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

The purpose of this file is to create final datsets based on 'interim dataset' created by the `data_prep.Rmd`
=============================================================================================================

Incident data
-------------

``` r
load("data/incident_dat_2017.RData")

#Create log(response time)
incident_dat_2017 <- incident_dat_2017 %>% 
  mutate(log_response_time = log(as.numeric(response_time)),
         zip_code = as.numeric(zip_code))
```

Neighborhood variable (by zipcode) dataset
------------------------------------------

``` r
load("data/zipcode.RData")
```

Weather data
------------

``` r
load("data/nyc_weather_2017.RData") 
nyc_weather_2017 <- nyc_weather_2017 %>% 
    mutate(prcp_ctg = 
           ifelse(prcp == 0, "NA",
           ifelse((prcp > 0 & prcp <= 25), "low",
           ifelse((prcp > 25 & prcp <= 60), "medium", "high"))))
```

Street closure
--------------

``` r
load("data/street_closure_2017.RData")
```

Merge first three for final dataset
-----------------------------------

``` r
# incident_dat_2017 
finaldat =  
  left_join(incident_dat_2017, zip_code_table, by = "zip_code") %>% 
  inner_join(., nyc_weather_2017, by = "date")
```

Add additional variables and save finaldat in /data
---------------------------------------------------

``` r
finaldat = 
  finaldat %>%
  mutate(season = 
           ifelse(incident_month %in% 9:11, "Fall",
           ifelse(incident_month %in% c(12,1,2), "Winter",
           ifelse(incident_month %in% 3:5, "Spring", "Summer"))), 
         hour_of_day = 
           ifelse(hour %in% 6:12, "morning",
           ifelse(hour %in% 13:17, "afternoon",
           ifelse(hour %in% 18:23, "night","dawn"))), 
         over_5min = ifelse(response_time > 5, "5min+", "5min-"))

save(finaldat, file = "data/finaldat.RData")
```
