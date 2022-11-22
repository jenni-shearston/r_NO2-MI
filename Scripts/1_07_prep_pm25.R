# Load and Explore PM2.5 Data
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 11/22/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Prep Variables for API Pull
# 2: Create Function to Pull from API
# 3: Pull from API
# 4: Clean Data Before Saving


####***********
#### Notes #### 
####***********

# In this script we pull data from the EPA AQS API for a sensitivity analysis
# where we adjust for PM2.5 (hourly / daily options) for cities in which there are 
# PM2.5 monitors at any point during the study period. This includes a total of 5    
# cities: Buffalo, Cheektowaga, Corning, New York, and Rochester. 


####*****************
#### Preparation #### 
####*****************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages and passwords
source(paste0(project.folder, 'Scripts/', 'packages.R'))
source(paste0(project.folder, 'Scripts/', 'passwords.R'))

# 0c Set up filepath(s)
data_path <- '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data/Intermediate_Data/'


####*************************************
#### 1. Prep Variables for API Pull  #### 
####*************************************

# 1a Create tibble of PM2.5 monitors 
#    Notes: Data from EPA AirData Air Quality Monitors map
monitors <- tribble(
  ~monitor_id, ~b_year, ~e_year, ~monitor_name, ~ city,
  "36-029-0023", 2014, 2016, "Buffalo Near-Road", 'Cheektowaga',
  "36-029-0005", 1999, 2016, "Buffalo", 'Buffalo',
  "36-055-6001", 1999, 2003, "", "Rochester",
  "36-055-2002", 2000, 2003, "", "Rochester",
  "36-055-0015", 2015, 2016, "Rochester Near-Road", "Rochester",
  "36-055-1007", 2004, 2016, "Rochester 2", "Rochester",
  "36-101-0003", 2014, 2016, "Pinnacle State Park", "Corning",
  "36-081-0097", 1999, 2000, "Queensboro Community College", "New York",
  "36-081-0094", 1999, 2003, "PS 29", "New York",
  "36-081-0096", 2000, 2003, "PS 214", "New York",
  "36-047-0011", 1999, 2000, "Greenpoint", "New York",
  "36-047-0122", 2001, 2016, "JHS 126", "New York",
  "36-047-0076", 1999, 2003, "", "New York",
  "36-047-0052", 2000, 2003, "PS 314", "New York",
  "36-085-0055", 1999, 2016, "Richmond Post Office", "New York",
  "36-085-0067", 1999, 2011, "Susan Wagner HS", "New York",
  "36-061-0134", 2007, 2016, "Division Street", "New York",
  "36-061-0062", 1999, 2007, "Canal Street", "New York",
  "36-061-0128", 2001, 2016, "PS 19", "New York",
  "36-061-0010", 1999, 2001, "", "New York",
  "36-061-0056", 1999, 2008, "PS 59", "New York",
  "36-061-0079", 2000, 2016, "IS 45", "New York",
  "36-005-0080", 1999, 2012, "Morrisania", "New York",
  "36-005-0110", 1999, 2016, "IS 52", "New York",
  "36-005-0083", 1999, 2016, "Botanical Garden", "New York",
  "36-005-0133", 2008, 2016, "Pfizer Lab Site", "New York")
  
# 1b Create variable equal to the number of times to replicate 
# each row (monitor), based on how many years of data are available. 
# We want each row to correspond to one year of data from one monitor
# because the API can only pull one year of data at a time 
# from a given monitor.
monitors <- monitors %>% 
  mutate(numb_years = e_year-b_year+1)

# 1c Replicate rows number of times in numb_years variable
monitors <- monitors[rep(seq_len(nrow(monitors)), monitors$numb_years), 1:6]

# 1d Create sequence variable of number of years of data
monitors <- monitors %>% 
  group_by(monitor_id) %>% 
  mutate(yr_count = seq(1:numb_years)-1) %>% 
  ungroup()

# 1e Create variables for accessing API 
#    Notes: Set email and key to your own AQS email and key
monitors <- monitors %>% 
  mutate(pull_year = b_year+yr_count,
         b_date = paste0(pull_year, "0101"),
         e_date = paste0(pull_year, "1231"),
         county = substr(monitor_id, 4, 6),
         site = substr(monitor_id, 8, 11),
         email = aqs_email,
         key = aqs_key,
         data = list(NA))


####*****************************************
#### 2: Create Function to Pull from API #### 
####*****************************************

# 2a Create function to pull PM2.5 for each monitor 
pull_PM2.5 = function(x) {
  #x=1
  #monitors$data[[x]]<-  
  GET(paste0("https://aqs.epa.gov/data/api/sampleData/bySite?email=",monitors$email[x],"&key=",monitors$key[x],"&param=88101&bdate=",monitors$b_date[x],"&edate=",monitors$e_date[x],"&state=36&county=",monitors$county[x],"&site=",monitors$site[x])) %>% 
    content("text") %>%
    jsonlite::fromJSON() %>% 
    as_tibble() %>% 
    purrr::map_if(., is.data.frame, list) %>% 
    as_tibble() %>% 
    unnest(cols = c(Header, Data))
}


####**********************
#### 3: Pull from API #### 
####**********************

# 3a Pull from API
# Note: pull ten at a time, and wait 10 sec between each pull,
# so as not to overwhelm the API.
for (x in 1:10){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 11:20){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 21:30){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 31:40){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 41:50){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 51:60){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 61:70){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 71:80){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 81:90){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 91:100){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 101:110){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 111:120){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 121:130){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 131:140){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 141:150){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 151:160){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 161:170){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 171:180){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 181:190){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 191:200){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 201:210){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 211:220){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 221:230){
  monitors$data[[x]] = pull_PM2.5(x)
}

for (x in 231:241){
  monitors$data[[x]] = pull_PM2.5(x)
}


####*********************************
#### 4: Clean Data Before Saving #### 
####*********************************

# 4a Unnest
monitors2 <- monitors %>% unnest(cols = c(data), names_repair = "unique")

# 4b Check vars
glimpse(monitors2)

# 4c Manage 2 "county" vars
monitors2 <- monitors2 %>% 
  rename(county_name = county...45) %>% 
  dplyr::select(-county...11)

# 4d Drop unneeded vars  
monitors2 <- monitors2 %>% 
  dplyr::select(-yr_count, -b_date, -e_date, -site, -email, -key, -state, 
                -Header, -Data) 

# 4e Create datetime variables
monitors2 <- monitors2 %>% 
  mutate(datetime_gmt1 = paste0(date_gmt, " ", time_gmt),
         date_local1 = lubridate::ymd(date_local, tz = 'EST')) %>% 
  mutate(datetime_gmt = lubridate::ymd_hm(datetime_gmt1, tz = 'GMT'),
         date_any = lubridate::with_tz(date_local1, tzone = 'America/New_York')) %>% 
  mutate(date_any = date(date_any))

# 4f Review some variables
table(monitors2$parameter, useNA = c("always")) # 0 NA, all PM2.5 Local Conditions
table(monitors2$units_of_measure, useNA = c("always")) # 0 NA, 103162 micrograms/m3
table(monitors2$sample_duration, useNA = c("always")) # 0 NA, 68399 1h, 34763 24h
table(monitors2$sample_frequency, useNA = c("always")) # 0 NA, hourly, every day, 
                                                       # every 3rd day, every 6th day
table(monitors2$method, useNA = c("always")) # 0 NA, 3 different gravimetric methods
table(monitors2$state_code, useNA = c("always")) # all obs "36"
table(monitors2$status, useNA = c("always")) # all obs "Success"

# 4g Look over time
monitors2 %>% 
  ggplot(aes(x = datetime_gmt, y = sample_measurement)) +
  geom_line(color = 'darkgray') + 
  geom_smooth() +
  facet_wrap(~monitor_id, nrow = 6, ncol = 5)

# 4h Review POCs
#    Notes: POCs are 'Parameter Occurrence Codes' that distinguish different 
#           instruments that measure the same parameter at the same site
#           When we average by city-date, we will not restrict to specific
#           POCs or ensure only one POC per datetime
#           When we average by city-hour, we will restrict to the POC that
#           reflects an hourly method (POC 4 for these monitors)
table(monitors2$poc, useNA = c("always"))

# 4i Aggregate to city/date (unique city/date combinations)
#    Notes: Aggregating to date rather than datetime because several PM2.5 
#           monitors only do 24 hour (daily) measures
# 4i.i Aggregate to city/date
pm25_city_day <- monitors2 %>% group_by(city, date_any) %>% 
  summarise(pm25_daily = mean(sample_measurement, na.rm = T))
# 4i.ii Confirm only one observation per city/date
check <- pm25_city_day %>% mutate(city_date = paste0(city, date_any))
check$city_date[duplicated(check$city_date)] # Check for duplicates --> should be 0

# 4j Aggregate to city/hour (unique city/hour combinations)
#    Notes: Aggregate to hour in order to do sensitivity analysis with
#           hourly pm2.5 for NYC (and maybe Rochester and Corning)
# 4j.i Aggregate to city/hour
pm25_city_hour <- monitors2 %>% filter(sample_duration == '1 HOUR') %>% 
  group_by(city, datetime_gmt) %>% 
  summarise(pm25_hourly = mean(sample_measurement, na.rm = T))
# 4j.ii Confirm only one observation per city/hour
check2 <- pm25_city_hour %>% mutate(city_datetime = paste0(city, datetime_gmt))
check2$city_datetime[duplicated(check2$city_datetime)] # Check for duplicates --> should be 0

# 4k Look over time again 
pm25_city_day %>% 
  ggplot(aes(x = date_any, y = pm25_daily)) +
  geom_line(color = 'darkgray') + 
  geom_smooth() +
  facet_wrap(~city, nrow = 2, ncol = 3)
pm25_city_hour %>% 
  ggplot(aes(x = datetime_gmt, y = pm25_hourly)) +
  geom_line(color = 'darkgray') + 
  geom_smooth() +
  facet_wrap(~city, nrow = 2, ncol = 3)

# 4k Save out data
pm25_city_day %>% write_fst(paste0(data_path, 'pm25_city_day.fst'))
pm25_city_hour %>% write_fst(paste0(data_path, 'pm25_city_hour.fst'))


