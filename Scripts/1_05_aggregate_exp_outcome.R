# Aggregate exp/outcome to cities and zips
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 08/09/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Aggregate exposure (NO2) data to city and zip code
# 2: Aggregate MI data to cities & restrict to zip codes with monitors
# 3: Exclude Months of NO2 Observations Missing x% of Observations


####***********
#### Notes #### 
####***********

# Multiple monitors in same city or zip
# There could be multiple monitors in the same city or zip. We will average these such
# that each city or zip and datetime combination has only one NO2 value (city-datetime 
# or zip-datetime). 

# Run time
# Note that there are millions of rows in some of the dataframes 
# used in this script, and that some computations may be time-intensive. I have 
# tried to note these steps in the script below. 

# Timezones
# NO2 data was collected in Eastern Standard Time
# https://aqs.epa.gov/aqsweb/documents/about_aqs_data.html
# MI data is in America/New_York. 
# When we merge in a later script we will make everything America/New_York so that 
# it is consistent with the time a case experienced when they had an MI


####*****************
#### Preparation #### 
####*****************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'Scripts/', 'packages.R'))

# 0c Set up filepath(s)
data_path <- paste0(print(here::here('Data', 'Intermediate_Data')),'/')
data_path_external <- '/Users/jennishearston/Desktop/SPARCCS_MI/'

# 0d Load data
# 0d.i Load crosswalk for city-level analysis
city_crosswalk <- read_fst(paste0(data_path, 'city_analysis_spatial_crosswalk'))
# 0d.ii Load crosswalk for zip code-level analysis
zipcode_crosswalk <- read_fst(paste0(data_path, 'zipcode_analysis_spatial_crosswalk'))
# 0d.iii Load mi data
mi <- read_fst(paste0(data_path_external, 'MI_for_NO2_Analysis.fst'))
# 0d.iv Load no2 data
setwd(data_path)
no2 <- tibble(file_name = list.files(pattern = "36-."),
              monitor_id = str_sub(file_name, end = -5L),
              exp_data = lapply(list.files(pattern = "36-."), fread)) %>% 
  dplyr::select(-file_name, -monitor_id) %>% 
  unnest(cols = "exp_data")

# 0e Set no2 datetime variables to correct timezones and create ET time variable
#    fread has read in both POSIXct variables as UTC
no2 <- no2 %>% 
  mutate(datetime_gmt = ymd_hms(datetime_gmt, tz = 'GMT'),
         datetime_est = with_tz(datetime_est, tzone = 'EST'),
         datetime_et = with_tz(datetime_est, tzone = 'America/New_York'))


####***********************************************************
#### 1. Aggregate exposure (NO2) data to city and zip code #### 
####***********************************************************

# Notes: This analysis includes any city that contains a NO2 monitor (n = 9)
#        A sensitivity analysis includes any zip code whose centroid is within
#        5km of a monitor (n = 322). 

####***********************************************************City

# 1a Add city variable to NO2 dataset 
# Notes: The NO2 dataset has a current spatial unit of points (monitor_id) 
#        Rows in the NO2 df refer to monitor-datetime observations
no2_city <- city_crosswalk %>% dplyr::select(monitor_id, city) %>% distinct() %>% 
  left_join(no2, by = "monitor_id") 

# 1b Average no2 by city and datetime
no2_city <- no2_city %>% 
  mutate(city_activeLag = paste0(city, datetime_et)) %>% 
  group_by(city_activeLag) %>% 
  summarize(no2_avg = mean(sample_measurement, na.rm = T)) 

# 1c Add 'activeLag' variable to use in 2_03 when creating lagged exposures
no2_city <- no2_city %>% ungroup() %>%
  mutate(activeLag = str_sub(city_activeLag, -19L)) %>% 
  mutate(activeLag = ymd_hms(activeLag, tz = 'America/New_York'))

####***********************************************************Zip code

# 1d Restrict zipcode_crosswalk to distinct zip code / monitor id combinations
zipcode_crosswalk_no2 <- zipcode_crosswalk %>% 
  dplyr::select(zip_code, monitor_id) %>% distinct()

# 1e Add zip code variable to NO2 dataset
no2_zc <- no2 %>% left_join(zipcode_crosswalk_no2, by = 'monitor_id') %>% 
  filter(!is.na(zip_code))

# 1d Average no2 by zip code and datetime (run time ~ 5-10 mins)
no2_zc <- no2_zc %>% 
  group_by(zip_code, datetime_gmt) %>% 
  summarize(no2_avg = mean(sample_measurement, na.rm = T))

# 1d Add activeLag variable to use in function below (~ 2 min to run)
no2_zc <- no2_zc %>% ungroup %>% 
  mutate(activeLag = with_tz(datetime_gmt, tzone = "America/New_York")) 

# 1e Save out aggregated NO2 files
write_fst(no2_city, paste0(data_path, 'no2_city'))
write_fst(no2_zc, paste0(data_path, 'no2_zipcode'))


####**************************************************************************
#### 2: Aggregate MI data to cities & restrict to zip codes with monitors ####
####**************************************************************************

####***********************************************************City

# 2a Rename zip code variable (in the MI dataset zip code is labeled zcta, but
#    actually is zip code and NOT zcta)
mi <- mi %>% rename(zip_code = zcta)

# 2b Add city variable to MI dataset
mi_city <- city_crosswalk %>% dplyr::select(zip_code, city) %>% distinct() %>%
  left_join(mi, by = "zip_code") 

# 2c Check for missing MI data
#    n = 35 missing observations
# 2c.i Identify missing observations
MIS_mi_city <- mi_city %>% filter(is.na(MI_count_Prim))
# 2c.ii Check zip code type of missing observations
#       Most missing zips are 'post office or large volume customer'
#       14642 is missing, has population under 500 people 
#       14260 is missing, covers the University at Buffalo North Campus
MIS_mi_city <- MIS_mi_city %>% left_join(city_crosswalk, by = 'zip_code') %>% 
  dplyr::select(-monitor_id) %>% distinct()

# 2c Sum MI cases by city / datetime
mi_city <- mi_city %>%   
  group_by(city, CaseDateRaw) %>% 
  summarize(MI_count_Prim = sum(MI_count_Prim, na.rm = T),
            MI_count_DXA410X1 = sum(MI_count_DXA410X1, na.rm = T)) 

# 2d Save aggregated MI file
write_fst(mi_city, paste0(data_path_external, 'mi_city'))

####***********************************************************Zip code

# 2e Restrict MI cases to zip codes we have NO2 data for
mi_zc <- zipcode_crosswalk %>% dplyr::select(zip_code) %>% distinct() %>%
  left_join(mi, by = "zip_code") 
  
# 2f Check for missing MI data
#    n = 139 missing observations
# 2f.i Identify missing observations
MIS_mi_zc <- mi_zc %>% filter(is.na(MI_count_Prim))
# 2c.ii Check zip code type of missing observations
#       Most missing zips are 'post office or large volume customer'
#       14642 is missing, has population under 500 people 
#       14260 is missing, covers the University at Buffalo North Campus
#       11249 is missing, not sure why...
#       11251 is missing, no current population
#       10168 is missing, no current population
#       11439 is missing, covers the Saint John's University Queens Campus
#       11549 is missing, covers Hofstra University
MIS_mi_zc <- MIS_mi_zc %>% left_join(zipcode_crosswalk, by = 'zip_code') %>% 
  dplyr::select(-monitor_id, -ZCTA5CE10, -city) %>% distinct()

# 2f Save filtered MI file
write_fst(mi_zc, paste0(data_path_external, 'mi_zc'))


####**********************************************************************
#### 3: Exclude Months of NO2 Observations Missing x% of Observations ####
####**********************************************************************

# 3a Create hourly time series for each city
all_hours_dataframe <- 
  tibble(datetime_ANY = rep(seq(ymd_hms('2000-01-01 00:00:00', tz = "America/New_York"), 
                                ymd_hms('2015-12-31 23:00:00', tz = "America/New_York"), 
                                by = '1 hour'), times = 9),
         city = rep(c('Amherst', 'Buffalo', 'Cheektowaga','Corning', 'East Meadow', 
                      'Hogansburg','Holtsville', 'New York', 'Rochester'), 
                    each = 140256),
         city_activeLag = paste0(city, datetime_ANY)) %>% 
  mutate(dup = duplicated(city_activeLag)) %>% 
  filter(dup == FALSE) # to remove duplicated 1am obs created during Fall Back

# 3b Merge NO2 data with complete hourly time series
# 3b.i Add merge variable to NO2_city dataframe
no2_city <- no2_city %>% 
  filter(activeLag < ymd_hms('2016-01-01 00:00:00', tz = 'America/New_York')) %>% 
  filter(!is.na(no2_avg)) 
# 3b.ii Join NO2_city and complete time series
all_hours_dataframe <- all_hours_dataframe %>% 
  left_join(no2_city, by = 'city_activeLag')

# 3c Create variables to calculate missingness
all_hours_dataframe <- all_hours_dataframe %>% 
    mutate(year = year(datetime_ANY),
           month = month(datetime_ANY),
           hour = hour(datetime_ANY),
           hours_in_month = days_in_month(datetime_ANY)*24,
           year_month = paste0(year, '_', month)) %>% 
    mutate(missing = ifelse(is.na(no2_avg), 1, 0)) %>% 
    group_by(year, month, city) %>% 
    mutate(tot_mis_bymonth = sum(missing),
           prop_mis_bymonth = tot_mis_bymonth/hours_in_month,
           exclude_month_5per = ifelse(prop_mis_bymonth >= .05, 1, 0),
           exclude_month_25per = ifelse(prop_mis_bymonth >= .25, 1, 0)) %>% 
  ungroup()

# 3d Calculate percent of all datetimes in included months that are missing
# 3d.i After removing months with more than 5% of hours missing
#      About 2-3% missing, still plenty of months
all_hours_dataframe %>% 
  group_by(city, exclude_month_5per) %>% 
  summarise(total = n(), missing = sum(missing), prop_miss = missing/total, 
            tot_months = length(unique(year_month)))
# 3d.ii After removing months with more than 25% of hours missing
#       Now about 3-8% missing and still plenty of months 
all_hours_dataframe %>% 
  group_by(city, exclude_month_25per) %>% 
  summarise(total = n(), missing = sum(missing), prop_miss = missing/total, 
            tot_months = length(unique(year_month)))

# 3e Review which months are included for each city
# 3e.i After removing months with more than 5% of hours missing
#      Cheektowaga has more Sep-Dec months, but still full coverage
#      Corning is missing Jan, Feb, April and November
#      All others look pretty good!
all_hours_dataframe %>% filter(exclude_month_5per == 0) %>% 
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = month)) + geom_bar() + 
  facet_grid(vars(city)) + scale_x_continuous(breaks = 1:12)
# 3e.ii After removing months with more than 25% of hours missing
#       Cheektowaga has less coverage Jan-March, but still pretty good
#       All others are good
all_hours_dataframe %>% filter(exclude_month_25per == 0) %>% 
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = month)) + geom_bar() + 
  facet_grid(vars(city)) + scale_x_continuous(breaks = 1:12)

# 3f Select needed variables, then save
all_hours_dataframe %>% dplyr::select(-dup, -year, -month, -hour) %>% 
  write_fst(., paste0(data_path, 'no2_city_allhours'))


