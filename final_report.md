---
title: "Final report"
author: "Youn Kyeong Chang (yc3242), Pengxuan Chen(pc2853), Anusorn Thanataveerat (at2710), Apoorva Srinivasan (as5697), Abhishek Ajay (aa4266)"
output:
  html_document:
    keep_md: yes
---

##Motivation

A major concern in emergency medical services (EMS) planning has been the need to minimize response time to better patient outcome, translated by some EMS operations into a goal of a response time of 8 minutes or less for advanced life support (ALS) units responding to life-threatening events. Several research results suggested there may be a small beneficial effect of response ≤7 minutes 59 seconds for those who survived to become an inpatient. More information on this can be found [here](https://www.ncbi.nlm.nih.gov/pubmed/11927452) and [here](https://www.ncbi.nlm.nih.gov/pubmed/22026820)

Therefore, to find out what factors are assoicated response time could be the first step to reduce the response time. Due to its large population, New York City requires a powerful, quick, well-equipped and effective Emergency Medical Service System. Hence, we explored the data of EMS response time in New York City in the year 2017 with respect to weather, time and street blockage.

### Initial Questions

We tried to answer the factors assoicated with EMS response time. We assumed rainy or snowy conditions, season and hour of the day will affect the response time. 

Some of the questions that drove our analysis were:

* How do season, hour of the day, rainy or snowly conditions affected the response time in 2017?

* What other factors affect response time?

[This](https://www.ncbi.nlm.nih.gov/pubmed/29381111) study done in California county early this year challenges the commonly held assumption that ambulances are later to poor neighborhoods. This got us interested in exploring the relationship in emergency response time between a economically well off area - Upper West Side and a relatively poorer area such as Washington Heigths. 


##Data Source

__Incidents Responded to by Fire Companies:__ [This](https://data.cityofnewyork.us/Public-Safety/Incidents-Responded-to-by-Fire-Companies/tm6d-hbzd) dataset contains detailed information on incidents handled by FDNY Fire units and includes fire, medical and non-fire emergencies. For this project, we have filtered only EMS related data. The data is collected in the New York Fire Incident Reporting System (NYFIRS), which is structured by the FDNY to provide data to the National Fire Incident Reporting System (NFIRS). NFIRS is a modular all-incident reporting system designed by the U.S. Fire Administration. The original dataset contains 24 columns from which our aim outcome variable is response time which was obtained by subtracting incident time and arrival time. We are also interested in area(borough and zipcode) and street highway.

__The New York City Weather Data__ 

[This](https://www.ncdc.noaa.gov/cdo-web/datasets) dataset includes weather records of the city from 2014- 2018. We have restrcited our analysis to 2017 so the data was filtered accordingly. From this dataset, we have selected precipication, snow, snow depth and maximum and minimum temperatures everyday throughout the year.

__Street Closure due to Construction data__ 

We were interested in how [street closures](https://data.cityofnewyork.us/Transportation/Street-Closures-due-to-construction-activities-by-/478a-yykk) in NYC affects the response time. For this, we acquired DOT Street Closure Data. DOT Street Closure data identifies locations in the New York City Street Closure map where a street is subject to a full closure, restricting through traffic, for the purpose of conducting construction related activity on a City street. From this datset of 7 columns, we have filtered out work state and end date in 2017, on and from street and borough code for our interest.





## Exploratory Analysis



###Step 1: Data Preparation

####1. Incident data (response from Fire company)


```r
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
         incident_day = as.numeric(incident_day),
         zip_code = as.numeric(zip_code))

save(incident_dat_2017, file = "data/incident_dat_2017.RData")
```

####2. Neighborhood variable (by zipcode) dataset


```r
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

save(zip_code_table, file = "data/zipcode.RData")
```

####3. Add weather-related dataset


```r
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

####4. Merge first three for final dataset


```r
# incident_dat_2017 
finaldat =  
  left_join(incident_dat_2017, zip_code_table, by = "zip_code") %>% 
  inner_join(., nyc_weather_2017, by = "date") %>% 
  mutate(response_time = as.numeric(response_time))

save(finaldat, file = "data/finaldat.RData")

load('data/finaldat.RData')
```

Given that data of response time is right skewed, for visulization, we took a mean of response time as our outcome for exploratory analysis And then performed exploratory data analysis. 


###Step 2: Initial Analysis (Obsolete)

####Step 2-1: EDA

####1. Hourly trend of response time

```r
finaldat %>%
  group_by(hour) %>% 
  ggplot(aes(x = hour, y = response_time)) + 
  geom_smooth(se = FALSE) 
```

![](final_report_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

#####2. Response time in rainy and snowy days

```r
finaldat %>% 
  mutate(prcp = prcp > 0) %>%
  group_by(date, prcp) %>% 
  summarise(mean_resp_time = mean(response_time)) %>% 
  ggplot(aes(x = prcp, y = mean_resp_time)) +
  geom_violin(aes(fill = factor(prcp)), alpha = .5) +
  stat_summary(fun.y = mean, geom = "point", size = 4, color = "blue") +
  labs(
    title = "Mean Response time in rainy conditions",
    x = "Precipitation",
    y = "Response time"
  ) +
  viridis::scale_fill_viridis(
    name = "Precipitation",
    discrete = TRUE) +
  theme(plot.title = element_text(size = 12),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold"),
        legend.position = "None") 
```

![](final_report_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
finaldat %>% 
  mutate(snow = snow > 0) %>%
  group_by(date, snow) %>% 
  summarise(mean_resp_time = mean(response_time)) %>% 
  ggplot(aes(x = snow, y = mean_resp_time)) +
  geom_violin(aes(fill = factor(snow)), alpha = .5) +
  stat_summary(fun.y = mean, geom = "point", size = 4, color = "blue") +
  labs(
    title = "Mean response time in snowy conditions",
    y = "Mean response time",
    x = "Snow"
  ) +
  viridis::scale_fill_viridis(
    name = "Snow",
    discrete = TRUE) +
  theme(plot.title = element_text(size = 12),
        strip.background = element_rect(fill = "black"),
        strip.text = element_text(color = "white", face = "bold"),
        legend.position = "None") 
```

![](final_report_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

We showed some trends by hour of the day and by snowy conditions. Based on this result, we fit linear regression. 

####Step 2-2: Linear regression 

####1. hour of the day vs response time 


```r
fit_hour = lm(response_time ~ hour, data = finaldat)
fit_hour %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 5.470 </td>
   <td style="text-align:right;"> 0.017 </td>
   <td style="text-align:right;"> 317.714 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> hour </td>
   <td style="text-align:right;"> -0.034 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> -29.176 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

Ho: different hour of the day makes no difference in logs response time.
p-value: very small.
conclusion: There is a difference of log response time in different time/hour of the day.

#####2. snow & prcp vs response time ----- linear regression

```r
fit_snow = lm(response_time~snow, data = finaldat)
fit_snow %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 5.009 </td>
   <td style="text-align:right;"> 0.007 </td>
   <td style="text-align:right;"> 678.552 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 7.456 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

```r
fit_prcp = lm(response_time~prcp, data = finaldat)
fit_prcp %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 5.005 </td>
   <td style="text-align:right;"> 0.008 </td>
   <td style="text-align:right;"> 647.000 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 4.014 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>

H0: There is no correlation between snow and log_response_time
p-value: very small
estimate: positive
conclusion: There is a positive correlation between snow and log response time. 

####3. MLR based on snow, prcp, tmax, tmin(factors of nature environment) in different hour of the day

```r
fit_mlr = lm(response_time~snow + prcp + tmax + tmin, data = finaldat)

fit_mlr %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> statistic </th>
   <th style="text-align:right;"> p.value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (Intercept) </td>
   <td style="text-align:right;"> 5.077 </td>
   <td style="text-align:right;"> 0.022 </td>
   <td style="text-align:right;"> 226.683 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 4.539 </td>
   <td style="text-align:right;"> 0.000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp </td>
   <td style="text-align:right;"> 0.002 </td>
   <td style="text-align:right;"> 0.001 </td>
   <td style="text-align:right;"> 2.830 </td>
   <td style="text-align:right;"> 0.005 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tmax </td>
   <td style="text-align:right;"> 0.000 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> 0.123 </td>
   <td style="text-align:right;"> 0.902 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> tmin </td>
   <td style="text-align:right;"> -0.008 </td>
   <td style="text-align:right;"> 0.003 </td>
   <td style="text-align:right;"> -2.742 </td>
   <td style="text-align:right;"> 0.006 </td>
  </tr>
</tbody>
</table>

Even though we could get the statistically significant results with p-values less than 0.05, the estimates are too small so explanations from this was not meaningful. 

###Step 3: Final Analysis

####Step 3-1: Modify Variables 

Next, we categorized explanatory variables since we wanted to show the change of response time by the trend of precipitation, snow, season and time rather than by detailed amount of precipitation and snow or specific time. With these modified variables, we performed exploratory data analysis.



####Step 3-2:EDA based on modified variables

####1. precipitation and snow

```r
prcp_eda =
  finaldat %>% 
  mutate(prcp_ctg = fct_relevel(prcp_ctg, c("no_prcp", "low", "high"))) %>%   
  group_by(date, prcp_ctg) %>% 
  summarise(mean_resp_time = mean(response_time)) %>% 
  ggplot(aes(x = prcp_ctg, y = mean_resp_time)) +
  geom_violin(aes(fill = prcp_ctg), alpha = .3) +
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "blue") +
  labs(
    title = "Rainy conditions",
    y = "Mean response time(min)",
    x = " "
  ) +
  scale_x_discrete(labels = c("0(mm)", "0-25(mm)", "25(mm)+")) +
  viridis::scale_fill_viridis(
    name = "Precipitation",
    discrete = TRUE) +
  theme(plot.title = element_text(size = 12),
        axis.title.y = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        legend.position = "None") 

snow_eda =
  finaldat %>% 
  mutate(snow_ctg = fct_relevel(snow_ctg, c("no_snow", "low", "high"))) %>%   group_by(date, snow_ctg) %>% 
  summarise(mean_resp_time = mean(response_time)) %>% 
  ggplot(aes(x = snow_ctg, y = mean_resp_time)) +
  geom_violin(aes(fill = snow_ctg), alpha = .3) +
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "blue") +
  labs(
    title = "Snowy conditions",
    y = "",
    x = " "
  ) +
  scale_x_discrete(labels = c("0(mm)", "0-50(mm)", "50(mm)+")) +
  viridis::scale_fill_viridis(
    name = "Snow",
    discrete = TRUE) +
  theme(plot.title = element_text(size = 12),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(size = 8),
        legend.position = "None") 

prcp_snow = prcp_eda + snow_eda 
ggsave("prcp_snow.png", plot = prcp_snow)
```

####2. Season

```r
season_eda =
finaldat %>% 
  mutate(season = fct_relevel(season, c("Spring", "Summer", "Fall", "Winter"))) %>% 
  group_by(date, season) %>% 
  summarise(mean_resp_time = mean(response_time)) %>% 
  ggplot(aes(x = season, y = mean_resp_time)) +
  geom_violin(aes(fill = season), alpha = .3) +
  stat_summary(fun.y = mean, geom = "point", size = 2, color = "blue") +
  labs(
    y = "Mean response time (min)",
    x = " "
  ) +
  viridis::scale_fill_viridis(
    name = "season",
    discrete = TRUE) +
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 9),
        legend.position = "None") 

ggsave("season.png", plot = season_eda)
```

####3. Hour

```r
hour_eda =
finaldat %>%
  group_by(hour, season) %>% 
  ggplot(aes(x = hour, y = response_time, color = season)) + 
  geom_smooth(se = FALSE) +
  scale_x_continuous(breaks = c(6, 12, 18),
                     labels = c("6am", "12pm", "18pm")) +
  geom_vline(xintercept = c(6, 12, 18), color = "darkred") +
  labs(
    x = " ", 
    y = "Mean response time (min)"
    ) +
   viridis::scale_color_viridis(
    name = "Season",
    discrete = TRUE)
  theme(plot.title = element_text(size = 13),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 12),
        legend.position = "None") 
```

```
## List of 4
##  $ axis.title.y   :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : num 10
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ axis.text.x    :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : num 12
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  $ legend.position: chr "None"
##  $ plot.title     :List of 11
##   ..$ family       : NULL
##   ..$ face         : NULL
##   ..$ colour       : NULL
##   ..$ size         : num 13
##   ..$ hjust        : NULL
##   ..$ vjust        : NULL
##   ..$ angle        : NULL
##   ..$ lineheight   : NULL
##   ..$ margin       : NULL
##   ..$ debug        : NULL
##   ..$ inherit.blank: logi FALSE
##   ..- attr(*, "class")= chr [1:2] "element_text" "element"
##  - attr(*, "class")= chr [1:2] "theme" "gg"
##  - attr(*, "complete")= logi FALSE
##  - attr(*, "validate")= logi TRUE
```

```r
ggsave("hour.png", plot = hour_eda)
```

Accordingly, based on medical research, the ideal response time is 8mins. Hence, we categorized response time into binary outcome as less than 8min and over 8min.   

####Step 3-3:Logistic regression

```r
finaldat = 
  finaldat %>% 
  mutate(prcp_ctg = fct_relevel(prcp_ctg, "no_prcp"),
         snow_ctg = fct_relevel(snow_ctg, "no_snow"),
         season = fct_relevel(season, "Spring"),
         hour_of_day = fct_relevel(hour_of_day, "night"))

fit_logistic =
  glm(over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg, 
      family = binomial(), data = finaldat)

summary(fit_logistic)
```

```
## 
## Call:
## glm(formula = over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg, 
##     family = binomial(), data = finaldat)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.4561  -0.3762  -0.3658  -0.3271   2.4710  
## 
## Coefficients:
##                       Estimate Std. Error  z value Pr(>|z|)    
## (Intercept)          -2.959451   0.028609 -103.443  < 2e-16 ***
## seasonFall            0.058271   0.028900    2.016  0.04377 *  
## seasonSummer         -0.002128   0.028837   -0.074  0.94117    
## seasonWinter          0.070643   0.029485    2.396  0.01658 *  
## hour_of_dayafternoon  0.300029   0.026713   11.231  < 2e-16 ***
## hour_of_daydawn       0.230470   0.034634    6.654 2.84e-11 ***
## hour_of_daymorning    0.314235   0.028550   11.006  < 2e-16 ***
## snow_ctghigh          0.387983   0.084315    4.602 4.19e-06 ***
## snow_ctglow           0.212878   0.077437    2.749  0.00598 ** 
## prcp_ctghigh         -0.042873   0.057984   -0.739  0.45967    
## prcp_ctglow          -0.024086   0.023208   -1.038  0.29936    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 78187  on 167589  degrees of freedom
## Residual deviance: 77979  on 167579  degrees of freedom
## AIC: 78001
## 
## Number of Fisher Scoring iterations: 5
```

```r
fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate),
         lower_CI_OR = exp(estimate - 1.96*std.error),
         upper_CI_OR = exp(estimate + 1.96*std.error)) %>% 
  select(term, OR, lower_CI_OR, upper_CI_OR) %>% 
  mutate(term = c("intercept", "Fall", "Summer", "Winter", "Afternoon", "Dawn", "Morning", "snow 50+", "snow 50-", "prcp 25+", "prcp 25-")) %>%   knitr::kable(digits = 3, "html") %>% kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> OR </th>
   <th style="text-align:right;"> lower_CI_OR </th>
   <th style="text-align:right;"> upper_CI_OR </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> intercept </td>
   <td style="text-align:right;"> 0.052 </td>
   <td style="text-align:right;"> 0.049 </td>
   <td style="text-align:right;"> 0.055 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Fall </td>
   <td style="text-align:right;"> 1.060 </td>
   <td style="text-align:right;"> 1.002 </td>
   <td style="text-align:right;"> 1.122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Summer </td>
   <td style="text-align:right;"> 0.998 </td>
   <td style="text-align:right;"> 0.943 </td>
   <td style="text-align:right;"> 1.056 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Winter </td>
   <td style="text-align:right;"> 1.073 </td>
   <td style="text-align:right;"> 1.013 </td>
   <td style="text-align:right;"> 1.137 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Afternoon </td>
   <td style="text-align:right;"> 1.350 </td>
   <td style="text-align:right;"> 1.281 </td>
   <td style="text-align:right;"> 1.422 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dawn </td>
   <td style="text-align:right;"> 1.259 </td>
   <td style="text-align:right;"> 1.177 </td>
   <td style="text-align:right;"> 1.348 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Morning </td>
   <td style="text-align:right;"> 1.369 </td>
   <td style="text-align:right;"> 1.295 </td>
   <td style="text-align:right;"> 1.448 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow 50+ </td>
   <td style="text-align:right;"> 1.474 </td>
   <td style="text-align:right;"> 1.249 </td>
   <td style="text-align:right;"> 1.739 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow 50- </td>
   <td style="text-align:right;"> 1.237 </td>
   <td style="text-align:right;"> 1.063 </td>
   <td style="text-align:right;"> 1.440 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp 25+ </td>
   <td style="text-align:right;"> 0.958 </td>
   <td style="text-align:right;"> 0.855 </td>
   <td style="text-align:right;"> 1.073 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp 25- </td>
   <td style="text-align:right;"> 0.976 </td>
   <td style="text-align:right;"> 0.933 </td>
   <td style="text-align:right;"> 1.022 </td>
  </tr>
</tbody>
</table>

```r
OR_total_df = 
fit_logistic %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate),
         lower_CI_OR = exp(estimate - 1.96*std.error),
         upper_CI_OR = exp(estimate + 1.96*std.error)) %>% 
  select(term, OR, lower_CI_OR, upper_CI_OR) %>%
  as.tibble() %>% 
  mutate(ctg = c("intercept", 
                 "Season", "Season", "Season", 
                 "Hour of the day", "Hour of the day", "Hour of the day",
                 "Snow", "Snow",
                 "Rain", "Rain"),
         sub_ctg_chr = c("intercept",
                     "Fall", "Summer", "Winter",
                     "Afternoon", "Dawn", "Morning",
                     "50(mm)+", "0-50(mm)",
                     "25(mm)+", "0-25(mm)"),
         sub_ctg = c("1", "2", "1", "3", "8", "6", "7", "5", "4", "5", "4")) %>%   
  filter(ctg != "intercept")  

adj = .2 # This is used in position_nudge to move the dots

OR =
  ggplot(OR_total_df, aes(x = OR, y = ctg, color = sub_ctg, 
                        label = sub_ctg_chr)) +
  geom_vline(aes(xintercept = 1), linetype = "dashed") +
  geom_vline(aes(xintercept = 1.5), linetype = "dashed") +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "1"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), height = .1, 
                 color = "gray50", 
                 position = position_nudge(y = adj)) +
  geom_text(data = filter(OR_total_df, sub_ctg == "1"),
            aes(label = sub_ctg_chr), colour = "seagreen4", 
            nudge_x = -.1, nudge_y = +.2, check_overlap = TRUE) +
  geom_point(data = filter(OR_total_df, sub_ctg == "1"), 
             color = "seagreen4",
             position = position_nudge(y = adj)) +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "2"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), height = .1, 
                 color = "gray50") +
  geom_text(data = filter(OR_total_df, sub_ctg == "2"),
            aes(label = sub_ctg_chr), colour = "indianred4", 
            nudge_x = -.1, nudge_y = 0, check_overlap = TRUE) +
  geom_point(data = filter(OR_total_df, sub_ctg == "2"), color = "indianred4") +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "3"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), height = .1, 
                 color = "gray50", position = position_nudge(y = -adj)) +
  geom_text(data = filter(OR_total_df, sub_ctg == "3"),
            aes(label = sub_ctg_chr), colour = "lavenderblush4", 
            nudge_x = -.1, nudge_y = -.2, check_overlap = TRUE) +  
  geom_point(data = filter(OR_total_df, sub_ctg == "3"), color = "lavenderblush4",
             position = position_nudge(y = -adj)) +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "4"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), height = .1, 
                 color = "gray50", position = position_nudge(y = +.1)) +
  geom_text(data = filter(OR_total_df, sub_ctg == "4"),
            aes(label = sub_ctg_chr), colour = "skyblue2", 
            nudge_x = -.15, nudge_y = +.1, check_overlap = TRUE) +
  geom_point(data = filter(OR_total_df, sub_ctg == "4"), 
             color = "skyblue2",
             position = position_nudge(y = +.1)) +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "5"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), 
                  height = .1, 
                 color = "gray50", position = position_nudge(y = -.1)) +
  geom_text(data = filter(OR_total_df, sub_ctg == "5"),
            aes(label = sub_ctg_chr), colour = "skyblue4", 
            nudge_x = -.15, nudge_y = -.1) +
  geom_point(data = filter(OR_total_df, sub_ctg == "5"), 
             color = "skyblue4", 
             position = position_nudge(y = -.1)) +
 geom_errorbarh(data = filter(OR_total_df, sub_ctg == "6"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), 
                 height = .1, 
                 color = "gray50", 
                 position = position_nudge(y = adj)) +
  geom_text(data = filter(OR_total_df, sub_ctg == "6"),
            aes(label = sub_ctg_chr), colour = "seagreen4", 
            nudge_x = -.13, nudge_y = +.3, check_overlap = TRUE) +
  geom_point(data = filter(OR_total_df, sub_ctg == "6"), 
              color = "seagreen4",
             position = position_nudge(y = adj)) +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "7"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), 
                  height = .1, 
                 color = "gray50") +
  geom_text(data = filter(OR_total_df, sub_ctg == "7"),
            aes(label = sub_ctg_chr), colour = "indianred4", 
            nudge_x = -.13, nudge_y = 0, check_overlap = TRUE) +
  geom_point(data = filter(OR_total_df, sub_ctg == "7"), 
             color = "indianred4") +
  geom_errorbarh(data = filter(OR_total_df, sub_ctg == "8"), 
                 aes(xmax = upper_CI_OR, xmin = lower_CI_OR), height = .1, 
                 color = "gray50", position = position_nudge(y = -adj)) +
  geom_text(data = filter(OR_total_df, sub_ctg == "8"),
            aes(label = sub_ctg_chr), colour = "lavenderblush4", 
            nudge_x = -.13, nudge_y = -.27, check_overlap = TRUE) +  
  geom_point(data = filter(OR_total_df, sub_ctg == "8"), color = "lavenderblush4",
             position = position_nudge(y = -adj)) +
  scale_x_continuous(breaks = c(1, 1.5),
                     limits = c(.7, 1.8)) +
  labs(x = "Odds Ratio",
       y = " ") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 20),
        legend.position = "None") 

ggsave("OR.png", plot = OR, width = 45, height = 25, units = "cm")
OR
```

![](final_report_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Based on EDA, to get statistical analysis, we fit logistic regression and results are followings: 
- Snow: As expected from EDA, compared to no snow conditions, odds of over 8min response time is increased 24% by 0~50mm snow and 47% by over 50mm snow.
- Season: Compared to Spring, Summer does not affect odds of over 8min response time but odds is increased in Fall by 6% and in Winter by 7%.
- Rain: Statistically significant result was not obtained from rain variable and it matches the result of EDA.
- Hour of the day: In reference to night, odds of over 8min response time is increased at dawn by 26%, in the morning by 37% and in the afternoon by 35%.

###Step 4

Next, we were interested in exploring how street closure affects response time.  Since the original street closure data is recorded based on street junctions, and there is not a zipcode variable included, we needed to connect zipcode to every street junction in the dataset in order to merge it with the incident data. This had to be done manually. Hence, we considered two neighborhood areas: the Upper West Side and Washington and Inwood Heights. We then matched the neighbourhood with the zip codes, for us to filter the selected neighborhood in both the incident and the street closure data to create a subset.

####Step 4-1 data cleaning for original street closure data 

```r
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
save(street_closure_2017, file = "data/street_closure_2017.RData")
```

####Step 4-2 Add street closure data and create subset data for UWS and WH

```r
#list of streets of intereste
street_zip <- read_excel('./data/street_junctions_zipcode.xlsx') %>% 
  #delete white space from street name 
  mutate(A = toupper(str_replace_all(street_A, fixed(" "), "")),
         B = toupper(str_replace_all(street_B, fixed(" "), "")))
# Extract street closure data based on street_zip
#Merge A to A and B to B
first_set <- street_zip %>% 
  inner_join(street_closure_2017, by = c("A", "B"))
#Merge A to B and B to A
second_set <- street_zip %>% 
  inner_join(street_closure_2017, by = c("A" = 'B', 'B' = "A"))
street_closure_UWS_WH <- first_set %>% 
  bind_rows(second_set) %>% 
  select(street_A:neighborhood, work_start_date:work_time)
#Create function that checks the closure duration whether it happens while the call was made
check_street_closure <- function(datetime, zip){
  #Filter only the zipcode of interest
  zipcode_closure <- street_closure_UWS_WH %>% 
    filter(zipcode == zip) %>% 
    mutate(street_closed = ifelse(work_start_date < datetime & work_end_date > datetime, 1, 0))

  if (sum(zipcode_closure$street_closed) > 0) {
    return(1)
  } else {return(0)}
}
#Then create variable street closure (y/n) in the incident_dat_2017 focusing on Washington heights and UWS
dat_subset <- finaldat %>%
  filter(zip_code %in% c(10023, 10024, 10025, 10032, 10033, 10040)) 
#Down to 6065 observations
dat_subset$street_closed <- NA
for (i in 1:nrow(dat_subset)) {
  dat_subset$street_closed[i] <- 
    check_street_closure(dat_subset$incident_date_time[i], zip = dat_subset$zip_code[i])
}
save(dat_subset, file = "data/subset_dat.RData")
```

####Step 4-3 Logistic regression and visualization with among UWS and Washington Heights

```r
load('data/subset_dat.RData') 

subset <- dat_subset %>% 
  #Only retain variables of interest
  select(response_time, neighborhood, season:street_closed) %>% 
  #recode neightborhood (washington as opposed to Inwood + washington heights)
  mutate(neighborhood = ifelse(neighborhood == 'Inwood and Washington Heights',
      'Washington Heights', neighborhood)) %>% 
  mutate(prcp_ctg = fct_relevel(prcp_ctg, "no_prcp"),
         snow_ctg = fct_relevel(snow_ctg, "no_snow"),
         season = fct_relevel(season, "Spring"),
         hour_of_day = fct_relevel(hour_of_day, "night"))

uws_dat <- subset %>% filter(neighborhood == 'Upper West Side')
fit_logistic_uws =
  glm(over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg + street_closed, 
      family = binomial(), data = uws_dat)
summary(fit_logistic_uws)
```

```
## 
## Call:
## glm(formula = over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg + 
##     street_closed, family = binomial(), data = uws_dat)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.5427  -0.3986  -0.3577  -0.3198   2.8739  
## 
## Coefficients:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)          -2.83346    0.19867 -14.262   <2e-16 ***
## seasonFall           -0.47027    0.24973  -1.883   0.0597 .  
## seasonSummer         -0.22236    0.20141  -1.104   0.2696    
## seasonWinter         -0.01566    0.21217  -0.074   0.9412    
## hour_of_dayafternoon  0.35631    0.18714   1.904   0.0569 .  
## hour_of_daydawn       0.45071    0.25250   1.785   0.0743 .  
## hour_of_daymorning    0.38077    0.20086   1.896   0.0580 .  
## snow_ctghigh          0.44003    0.55488   0.793   0.4278    
## snow_ctglow           0.28654    0.51052   0.561   0.5746    
## prcp_ctghigh         -1.27998    0.72319  -1.770   0.0767 .  
## prcp_ctglow          -0.01853    0.16096  -0.115   0.9084    
## street_closed         0.20596    0.20845   0.988   0.3231    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1607  on 3386  degrees of freedom
## Residual deviance: 1589  on 3375  degrees of freedom
## AIC: 1613
## 
## Number of Fisher Scoring iterations: 6
```



```r
wash_dat <- subset %>% filter(neighborhood == 'Washington Heights')

fit_logistic_wash =
  glm(over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg + street_closed, 
      family = binomial(), data = wash_dat)

summary(fit_logistic_wash)
```

```
## 
## Call:
## glm(formula = over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg + 
##     street_closed, family = binomial(), data = wash_dat)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6915  -0.3962  -0.3538  -0.3066   2.6222  
## 
## Coefficients:
##                      Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -2.8675     0.2322 -12.351  < 2e-16 ***
## seasonFall            -0.2942     0.2297  -1.281  0.20016    
## seasonSummer          -0.1007     0.2254  -0.447  0.65490    
## seasonWinter           0.1149     0.2244   0.512  0.60866    
## hour_of_dayafternoon   0.4628     0.2221   2.084  0.03716 *  
## hour_of_daydawn        0.2508     0.2926   0.857  0.39135    
## hour_of_daymorning     0.5856     0.2269   2.581  0.00986 ** 
## snow_ctghigh           1.3078     0.6896   1.896  0.05791 .  
## snow_ctglow           -0.1308     0.6369  -0.205  0.83726    
## prcp_ctghigh          -0.8082     0.5062  -1.597  0.11035    
## prcp_ctglow           -0.3349     0.1971  -1.699  0.08934 .  
## street_closed         -0.1022     0.4760  -0.215  0.82994    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1277.1  on 2677  degrees of freedom
## Residual deviance: 1258.6  on 2666  degrees of freedom
## AIC: 1282.6
## 
## Number of Fisher Scoring iterations: 5
```


```r
#Prepare table for visualization
table_results_uws <-  fit_logistic_uws %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  bind_cols(.,  exp(confint_tidy(fit_logistic_uws))) %>% 
  select(term, OR, conf.low, conf.high) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = c("Fall", "Summer", "Winter", "Afternoon", "Dawn", "Morning", "snow 50+", "snow 50-", "prcp 25+", "prcp 25-", "Street closed"), 
         neightborhood = 'UWS') %>% 
   mutate(term = factor(term, levels = as.ordered(term)))
 
table_results_uws %>% 
   knitr::kable(digits = 3, "html") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> OR </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:left;"> neightborhood </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Fall </td>
   <td style="text-align:right;"> 0.625 </td>
   <td style="text-align:right;"> 0.380 </td>
   <td style="text-align:right;"> 1.013 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Summer </td>
   <td style="text-align:right;"> 0.801 </td>
   <td style="text-align:right;"> 0.538 </td>
   <td style="text-align:right;"> 1.187 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Winter </td>
   <td style="text-align:right;"> 0.984 </td>
   <td style="text-align:right;"> 0.647 </td>
   <td style="text-align:right;"> 1.489 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Afternoon </td>
   <td style="text-align:right;"> 1.428 </td>
   <td style="text-align:right;"> 0.994 </td>
   <td style="text-align:right;"> 2.074 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dawn </td>
   <td style="text-align:right;"> 1.569 </td>
   <td style="text-align:right;"> 0.945 </td>
   <td style="text-align:right;"> 2.553 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Morning </td>
   <td style="text-align:right;"> 1.463 </td>
   <td style="text-align:right;"> 0.988 </td>
   <td style="text-align:right;"> 2.177 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow 50+ </td>
   <td style="text-align:right;"> 1.553 </td>
   <td style="text-align:right;"> 0.446 </td>
   <td style="text-align:right;"> 4.154 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow 50- </td>
   <td style="text-align:right;"> 1.332 </td>
   <td style="text-align:right;"> 0.435 </td>
   <td style="text-align:right;"> 3.348 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp 25+ </td>
   <td style="text-align:right;"> 0.278 </td>
   <td style="text-align:right;"> 0.045 </td>
   <td style="text-align:right;"> 0.899 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp 25- </td>
   <td style="text-align:right;"> 0.982 </td>
   <td style="text-align:right;"> 0.711 </td>
   <td style="text-align:right;"> 1.338 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Street closed </td>
   <td style="text-align:right;"> 1.229 </td>
   <td style="text-align:right;"> 0.818 </td>
   <td style="text-align:right;"> 1.854 </td>
   <td style="text-align:left;"> UWS </td>
  </tr>
</tbody>
</table>


```r
table_results_wash =  
  fit_logistic_wash %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  bind_cols(.,  exp(confint_tidy(fit_logistic_wash))) %>% 
  select(term, OR, conf.low, conf.high) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = c("Fall", "Summer", "Winter", "Afternoon", "Dawn", "Morning", "snow 50+", "snow 50-", "prcp 25+", "prcp 25-", "Street closed"),
         neightborhood = 'Washington Heights') %>% 
  mutate(term = factor(term, levels = as.ordered(term)))
 
table_results_wash %>% 
   knitr::kable(digits = 3, "html") %>% 
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> term </th>
   <th style="text-align:right;"> OR </th>
   <th style="text-align:right;"> conf.low </th>
   <th style="text-align:right;"> conf.high </th>
   <th style="text-align:left;"> neightborhood </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Fall </td>
   <td style="text-align:right;"> 0.745 </td>
   <td style="text-align:right;"> 0.473 </td>
   <td style="text-align:right;"> 1.168 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Summer </td>
   <td style="text-align:right;"> 0.904 </td>
   <td style="text-align:right;"> 0.580 </td>
   <td style="text-align:right;"> 1.408 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Winter </td>
   <td style="text-align:right;"> 1.122 </td>
   <td style="text-align:right;"> 0.721 </td>
   <td style="text-align:right;"> 1.743 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Afternoon </td>
   <td style="text-align:right;"> 1.589 </td>
   <td style="text-align:right;"> 1.035 </td>
   <td style="text-align:right;"> 2.480 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Dawn </td>
   <td style="text-align:right;"> 1.285 </td>
   <td style="text-align:right;"> 0.713 </td>
   <td style="text-align:right;"> 2.261 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Morning </td>
   <td style="text-align:right;"> 1.796 </td>
   <td style="text-align:right;"> 1.158 </td>
   <td style="text-align:right;"> 2.828 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow 50+ </td>
   <td style="text-align:right;"> 3.698 </td>
   <td style="text-align:right;"> 0.790 </td>
   <td style="text-align:right;"> 12.905 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> snow 50- </td>
   <td style="text-align:right;"> 0.877 </td>
   <td style="text-align:right;"> 0.202 </td>
   <td style="text-align:right;"> 2.666 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp 25+ </td>
   <td style="text-align:right;"> 0.446 </td>
   <td style="text-align:right;"> 0.144 </td>
   <td style="text-align:right;"> 1.083 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> prcp 25- </td>
   <td style="text-align:right;"> 0.715 </td>
   <td style="text-align:right;"> 0.480 </td>
   <td style="text-align:right;"> 1.041 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Street closed </td>
   <td style="text-align:right;"> 0.903 </td>
   <td style="text-align:right;"> 0.311 </td>
   <td style="text-align:right;"> 2.087 </td>
   <td style="text-align:left;"> Washington Heights </td>
  </tr>
</tbody>
</table>


```r
OR_UWS =
 ggplot(table_results_uws, aes(x = term, y = OR)) +
  geom_hline(aes(yintercept = 1), size = 1, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip()

OR_WASH =
  ggplot(table_results_wash, aes(x = term, y = OR)) +
  geom_hline(aes(yintercept = 1), size = 1, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high)) +
  coord_flip()
  
OR_all <- 
  table_results_uws %>% bind_rows(table_results_wash) %>% 
  ggplot(., aes(x = term, y = OR, colour = neightborhood)) +
  geom_hline(aes(yintercept = 1), size = 1, linetype = "dashed") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                  position = position_dodge(width = 0.30)) +
  coord_flip() + 
  xlab('') + 
  ylab('Odds ratio') 

ggsave("OR_all.png", plot = OR_all, width = 45, height = 25, units = "cm")
OR_all
```

![](final_report_files/figure-html/plot_OR-1.png)<!-- -->

####Step 4-4 Fitting regression model by neighborhood



```r
fit_logistic_neighbor =
   glm(over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg + street_closed + neighborhood, family = binomial(), data = subset)

summary(fit_logistic_neighbor)
```

```
## 
## Call:
## glm(formula = over_8min ~ season + hour_of_day + snow_ctg + prcp_ctg + 
##     street_closed + neighborhood, family = binomial(), data = subset)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -0.6117  -0.3945  -0.3562  -0.3119   2.7517  
## 
## Coefficients:
##                                Estimate Std. Error z value Pr(>|z|)    
## (Intercept)                    -2.86464    0.15912 -18.003  < 2e-16 ***
## seasonFall                     -0.37896    0.16624  -2.280  0.02263 *  
## seasonSummer                   -0.18311    0.14944  -1.225  0.22048    
## seasonWinter                    0.05325    0.15168   0.351  0.72553    
## hour_of_dayafternoon            0.40097    0.14289   2.806  0.00501 ** 
## hour_of_daydawn                 0.36221    0.19103   1.896  0.05795 .  
## hour_of_daymorning              0.47460    0.14988   3.167  0.00154 ** 
## snow_ctghigh                    0.77128    0.42623   1.810  0.07036 .  
## snow_ctglow                     0.09757    0.39631   0.246  0.80553    
## prcp_ctghigh                   -0.89831    0.39781  -2.258  0.02394 *  
## prcp_ctglow                    -0.15267    0.12436  -1.228  0.21959    
## street_closed                   0.13713    0.16726   0.820  0.41232    
## neighborhoodWashington Heights  0.04894    0.11289   0.434  0.66464    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2884.1  on 6064  degrees of freedom
## Residual deviance: 2853.0  on 6052  degrees of freedom
## AIC: 2879
## 
## Number of Fisher Scoring iterations: 6
```

```r
fit_logistic_neighbor %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  bind_cols(.,  exp(confint_tidy(fit_logistic_neighbor))) %>% 
  select(term, OR, conf.low, conf.high) %>% 
  filter(term != '(Intercept)') %>% 
  mutate(term = c("Fall", "Summer", "Winter", "Afternoon", "Dawn", "Morning", "snow 50+", "snow 50-", "prcp 25+", "prcp 25-", "Street closed", "Washington Heights"))
```

```
## # A tibble: 12 x 4
##    term                  OR conf.low conf.high
##    <chr>              <dbl>    <dbl>     <dbl>
##  1 Fall               0.685    0.493     0.947
##  2 Summer             0.833    0.621     1.12 
##  3 Winter             1.05     0.783     1.42 
##  4 Afternoon          1.49     1.13      1.98 
##  5 Dawn               1.44     0.981     2.08 
##  6 Morning            1.61     1.20      2.16 
##  7 snow 50+           2.16     0.863     4.70 
##  8 snow 50-           1.10     0.471     2.27 
##  9 prcp 25+           0.407    0.170     0.825
## 10 prcp 25-           0.858    0.670     1.09 
## 11 Street closed      1.15     0.822     1.58 
## 12 Washington Heights 1.05     0.841     1.31
```

Washington Heights has 1.05 times the odds of response time greater than 8 minutes compared to UWS. However the observed effect is not statistically significant.


##Additional Statistical Analysis

As we first tried the linear regression we found out that despite the significant p value, the estimate of the variables are very small. We wanted to change to another statistical method to get relatively meaningful result. We summarized the continuous response time in to binary outcomes, about whether it is higher than 8 mins or not. We also categorized the snow and precipitation columns into binary variables. The model we chose is here: 

$$
\begin{aligned}
&logit(P(response\space time > 8 min))\\
&= \beta_{0} + \beta_{1}I(season = Fall) + \beta_{2}I(season = Summer) + \beta_{3}I(season = Winter) \\
& + \beta_{4}I(hour\space of \space day = afternoon) + \beta_{5}I(hour\space of \space day = dawn) + \beta_{6}I(hour\space of \space day = morning) \\
& + \beta_{7}I(snow = 50mm+) + \beta_{8}I(snow = 50mm-) + \beta_{9}I(prcp = 25mm+) + \beta_{10}I(prcp = 25mm-) 
\end{aligned}
$$

In the Odd Ratio table generated from the code in the above section, we found out that 
* As expected from EDA, compared to no snow conditions, odds of over 8min response time is increased 24% by 0~50mm snow and 47% by over 50mm snow. 
* In summary of the effect of seasons, compared to Spring, Summer does not affect odds of over 8min response time but odds is increased in Fall by 6% and in Winter by 7%.
* For the rainy condition, statistically significant result was not obtained from prcp variable and it matches the result of EDA.
* Furthermore, in reference to night, odds of over 8min response time is increased at dawn by 26%, in the morning by 37% and in the afternoon by 35%. It seems like the emergency service in our city is more or less affected by snowy condition, and somewhat delayed in the morning.

On the other hand, we have constructed logistic analysis on our subset filtered with neighborhood area variables, UMS, and Washington Heights in specific based on the [paper published earlier this year in California](https://www.ncbi.nlm.nih.gov/pubmed/29381111)  county that challenges the commonly held assumption that ambulances are later to poor neighborhoods. This got us interested in exploring the relationship in emergency response time between a economically well off area - Upper West Side and a relatively poorer area such as Washington Heigths.  Interestingly, In Upper West Side, high precipitation is associated with lower odds of response times greater than 8 minutes. Street closure is associated with the outcome but not statistically significant. Washington Heights, on the other hand, saw afternoon and dawn time, compared to the night time as reference, as contributing factors to the response time greater than 8 minutes with ORs 1.6 and 1.8, respectively. Again street closure is not predictive of the outcome. The null result from street closure could be due to either linking street closure to the zip code in this case is too broad or street closure did not really play a role in our response time. 

We also fitted the model with neighborhood as predictor variable and found that Washington Heights had 1.05 times the odds of response time greater than 8 minutes compared to the Upper West Side. However the observed effect is not statistically significant.


##Discussion

From our exploratory and logistic regression analysis, we found out that in snowy conditions, the odds of over 8min response increases while rainy condition does not effect the outcome in New York City in 2017. Along with this result, the odds of over 8min response increased in winter and it may be linked to snow factor. From this analysis, we can recommend a plan which assigns more ambulances in snowy conditions and during early hours.   

While comparing Upper West Side and Washington & Inwood Heights, the odds of getting different response time wasn't statistically significant. Hence, we can be rest assured that where we live, we will receive equal treatment from the emergency dispatcher.

Further investigation can be done using traffic as a variable. This can also be specifically used to determine if holiday events such as Thanksgiving or Hawlloween Parades affects the response time.

__Limitations of our analysis:__

* Response time ‘clock’ almost universally stops when the unit arrives in front of the address; in large office or apartment buildings, actually accessing the patient may take several minutes longer, but this is not considered in response time calculation or reporting.
* Missing traffic variable due to inability to match traffic with the location of the event. 
* Inability to map geographic map of each borough in interactive visualization plot
