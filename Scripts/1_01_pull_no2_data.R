# Pull NO2 Data from AQS
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 12/17/2020

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Enter monitor information
# 2: Replicate rows to create monitor-year specific pulls
# 3: Define variables needed for API pull
# 4: Create function to pull data from API
# 5: Pull data
# 6: Clean data before saving
# 7: Save out data

####**************
#### N: Notes ####
####**************

# Na Description
# We use this script to pull data from the EPA AQS API, AirData 
# for the time period 2000 - 2018
# Instructions for API use: https://aqs.epa.gov/aqsweb/documents/data_api.html#email

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load packages and passwords
source(paste0(project.folder, 'Scripts/', 'packages.R'))
source(paste0(project.folder, 'Scripts/', 'passwords.R'))

# 0c Set email
#    Note: Set your own AQS email
email = aqs_email

# 0d Register email and get key
#GET(paste0('https://aqs.epa.gov/data/api/signup?email=', email)

# 0e Set key
#    Note: Set your own AQS password here
key = aqs_key

# 0f Set data folder location
data_path = '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data'

####***************************
#### 1: Enter Monitor Info #### 
####***************************

# 1a Create tibble of monitor data
# Data entered from EPA's Interactive Map of Air Quality Monitors
# All NO2 monitors, active or inactive, with data any years from 
# 2000-2018 are included
monitors <- tribble(
  ~monitor_id, ~b_year, ~e_year, ~monitor_name,
  "36-033-7003", 2009, 2017, "Y001",
  "36-055-0015", 2014, 2018, "Rochester Near-Road",
  "36-055-1007", 2011, 2011, "Rochester 2",
  "36-029-0002", 2000, 2012, "Amherst",
  "36-029-0023", 2014, 2018, "Buffalo Near-Road",
  "36-029-0005", 2000, 2018, "Buffalo",
  "36-101-0003", 2007, 2011, "Pinnacle State Park",
  "36-005-0133", 2007, 2018, "Pfizer Lab Site",
  "36-005-0083", 2000, 2007, "Botanical Garden",
  "36-005-0080", 2000, 2000, "Morrisania",
  "36-005-0110", 2000, 2018, "IS 52",
  "36-081-0098", 2000, 2006, "College Point Post Office",
  "36-061-0056", 2000, 2008, "PS 59",
  "36-061-0010", 2000, 2001, "",
  "36-081-0124", 2001, 2018, "Queens College 2",
  "36-081-0125", 2017, 2018, "Queens College Near Road",
  "36-081-0097", 2000, 2001, "Queensboro Community College",
  "36-059-0005", 2000, 2010, "Eisenhower Park",
  "36-103-0009", 2000, 2010, "Holtsville"
)

####***********************
#### 2: Replicate Rows #### 
####***********************

# 2a Create variable equal to the number of times to replicate 
# each row (monitor), based on how many years of data are available. 
# We want each row to correspond to one year of data from one monitor
# because the API can only pull one year of data at a time 
# from a given monitor.
monitors <- monitors %>% 
  mutate(numb_years = e_year-b_year+1)

# 2b Replicate rows number of times in numb_years variable
monitors <- monitors[rep(seq_len(nrow(monitors)), monitors$numb_years), 1:5]

####**************************************
#### 3: Define Variables for API Pull #### 
####**************************************

# 3a Create sequence variable of number of years of data
monitors <- monitors %>% 
  group_by(monitor_id) %>% 
  mutate(yr_count = seq(1:numb_years)-1) %>% 
  ungroup()
  
# 3b Create data pull year variable
monitors <- monitors %>% 
  mutate(pull_year = b_year+yr_count)

# 3c Create b_date and e_date variables for API pull
# Format should be "yearmonthday" 
monitors <- monitors %>% 
  mutate(b_date = paste0(pull_year, "0101"),
         e_date = paste0(pull_year, "1231"))

# 3d Define county and site variables
monitors <- monitors %>% 
  mutate(county = substr(monitor_id, 4, 6),
         site = substr(monitor_id, 8, 11))

# 3e Add email and key variables to "monitors" tibble
monitors <- monitors %>% 
  mutate(email = email,
         key = key)

# 3f Add data variable to hold lists of data
monitors <- monitors %>% 
  mutate(data = list(NA))

####*****************************************
#### 4: Create Function to Pull from API #### 
####*****************************************

# Create function to pull NO2 for each monitor 
pull_NO2 = function(x) {
  #x=1
  #monitors$data[[x]]<-  
  GET(paste0("https://aqs.epa.gov/data/api/sampleData/bySite?email=",monitors$email[x],"&key=",monitors$key[x],"&param=42602&bdate=",monitors$b_date[x],"&edate=",monitors$e_date[x],"&state=36&county=",monitors$county[x],"&site=",monitors$site[x])) %>% 
    content("text") %>%
    jsonlite::fromJSON() %>% 
    as_tibble() %>% 
    purrr::map_if(., is.data.frame, list) %>% 
    as_tibble() %>% 
    unnest(cols = c(Header, Data))
}

####**********************
#### 5: Pull from API #### 
####**********************

# 5a Pull from API
# Note: pull ten at a time, and wait 10 sec between each pull,
# so as not to overwhelm the API. Otherwise it stops around x=14.
for (x in 1:10){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 11:20){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 21:30){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 31:40){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 41:50){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 51:60){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 61:70){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 71:80){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 81:90){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 91:100){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 101:110){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 111:120){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 121:130){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 131:140){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 141:150){
  monitors$data[[x]] = pull_NO2(x)
}

for (x in 151:159){
  monitors$data[[x]] = pull_NO2(x)
}

####*********************************
#### 6: Clean Data Before Saving #### 
####*********************************

# 6a Unnest
monitors2 <- monitors %>% unnest(cols = c(data), names_repair = "unique")

# 6b Check vars
glimpse(monitors2)

# 6c Manage 2 "county" vars
monitors2 <- monitors2 %>% 
  rename(county_name = county...44) %>% 
  dplyr::select(-county...10)

# 6d Drop unneeded vars  
monitors2 <- monitors2 %>% 
    dplyr::select(-yr_count, -b_date, -e_date, -site, -email, -key, -data) 

####**********************
#### 7: Save Out Data #### 
####**********************

# 7a Save out data as csv
# note: large df, may take some time to save
# do not use write.csv or write_csv - not fast enough
fwrite(monitors2, paste0(data_path, "/Intermediate_Data/no2.csv"))

# 7b Clean environment
rm(monitors, monitors2, email, key, x, pull_NO2)


