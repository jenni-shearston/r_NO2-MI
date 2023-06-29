# Load and Explore O3 Data
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 06/27/2023

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
# where we adjust for hourly ozone for NYC. There were also hourly ozone monitors
# during the study period for Rochester (2015), Holtsville (2000-2015) and 
# Amherst (2000-2012), but we chose only NYC so that the sensitivity analysis is
# comparable to our secondary NYC analysis, as we did not have data for similar
# years and cities as the main analysis. 


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

# 1a Create tibble of O3 monitors 
#    Notes: Data from EPA AirData Air Quality Monitors map
monitors <- tribble(
  ~monitor_id, ~b_year, ~e_year, ~monitor_name, ~ city,
  "36-085-0067", 1999, 2016, "Susan Wagner HS", "New York",
  "36-081-0097", 1999, 2001, "Queensboro Community College", "New York",
  "36-081-0124", 2001, 2016, "Queens College 2", "New York",
  "36-081-0098", 1999, 2006, "College Point Post Office", "New York",
  "36-061-0010", 1999, 2001, "", "New York",
  "36-061-0135", 2007, 2016, "CCNY", "New York",
  "36-005-0110", 1999, 2016, "IS 52", "New York",
  "36-005-0080", 1999, 2000, "Morrisania", "New York",
  "36-005-0133", 2007, 2016, "Pfizer Lab Site", "New York",
  "36-005-0083", 1999, 2007, "Botanical Garden", "New York")
  
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

# 2a Create function to pull O3 for each monitor 
pull_O3 = function(x) {
  #x=1
  #monitors$data[[x]]<-  
  GET(paste0("https://aqs.epa.gov/data/api/sampleData/bySite?email=",monitors$email[x],"&key=",monitors$key[x],"&param=44201&bdate=",monitors$b_date[x],"&edate=",monitors$e_date[x],"&state=36&county=",monitors$county[x],"&site=",monitors$site[x])) %>% 
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
  monitors$data[[x]] = pull_O3(x)
}

for (x in 11:20){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 21:30){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 31:40){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 41:50){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 51:60){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 61:70){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 71:80){
  monitors$data[[x]] = pull_O3(x)
}

for (x in 81:90){
  monitors$data[[x]] = pull_O3(x)
}


for (x in 91:97){
  monitors$data[[x]] = pull_O3(x)
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
  dplyr::select(-yr_count, -b_date, -e_date, -site, -email, -key, -state) 

# 4e Create datetime variables
monitors2 <- monitors2 %>% 
  mutate(datetime_gmt1 = paste0(date_gmt, " ", time_gmt),
         date_local1 = lubridate::ymd(date_local, tz = 'EST')) %>% 
  mutate(datetime_gmt = lubridate::ymd_hm(datetime_gmt1, tz = 'GMT'),
         date_any = lubridate::with_tz(date_local1, tzone = 'America/New_York')) %>% 
  mutate(date_any = date(date_any))

# 4f Review some variables
table(monitors2$parameter, useNA = c("always")) # 0 NA, all Ozone
table(monitors2$units_of_measure, useNA = c("always")) # 0 NA, 769136 ppm
table(monitors2$sample_duration, useNA = c("always")) # 0 NA, 769136 1 HOUR
table(monitors2$sample_frequency, useNA = c("always")) # 0 NA, 769136 hourly
table(monitors2$method, useNA = c("always")) # 0 NA, 2 ultraviolet methods
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
#           In this case, all POCs are a value of 1
table(monitors2$poc, useNA = c("always"))

# 4i Aggregate to city/hour (unique city/hour combinations)
#    Notes: Aggregate to hour in order to do sensitivity analysis with
#           hourly Ozone for NYC
# 4i.i Aggregate to city/hour
ozone_city_hour <- monitors2 %>%  
  group_by(city, datetime_gmt) %>% 
  summarise(ozone_hourly = mean(sample_measurement, na.rm = T))
# 4i.ii Confirm only one observation per city/hour
check2 <- ozone_city_hour %>% mutate(city_datetime = paste0(city, datetime_gmt))
check2$city_datetime[duplicated(check2$city_datetime)] # Check for duplicates --> should be 0

# 4j Look over time again 
ozone_city_hour %>% 
  ggplot(aes(x = datetime_gmt, y = ozone_hourly)) +
  geom_line(color = 'darkgray') + 
  geom_smooth() +
  facet_wrap(~city, nrow = 2, ncol = 3)

# 4k Save out data
ozone_city_hour %>% write_fst(paste0(data_path, 'ozone_city_hour.fst'))

