# Create Control Date-Hours
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 09/15/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create City-level MI + Weather + PM2.5 Dataset
# 2: Create City-level MI + Weather Dataset with 48 Lags
# 3: Create Zipcode-level MI + Weather Dataset


####**************
#### N: Notes #### 
####**************

# We must match all timezones. PM2.5 is in EST (no adjustment for daylight 
# savings) while MI data is in America/New_York (adjusted for daylight savings).
# Weather data is in the number of hours since 1990/01/01 00:00:00, tz = America/New_York


####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'Scripts/', 'packages.R'))

# 0c Set up filepath(s)
data_path <- paste0(print(here::here('Data', 'Intermediate_Data')),'/')
data_path_external <- '/Users/jennishearston/Desktop/SPARCCS_MI/'

# 0b Load data
# 0b.i Load weather data
w_city <- read_fst(paste0(data_path, 'weather_city.fst'))
w_zc <- read_fst(paste0(data_path, 'weather_zipcode.fst'))
# 0b.ii Load pm2.5 data
#       Note: pm data only needs to be added to the city dataset because it is
#             only used in a sensitivity analysis
pm_city <- read_fst(paste0(data_path, 'pm25_city.fst'))
# 0b.iii Load MI data (city level only - will load zip level later)
setwd(data_path_external)
mi_city <- tibble(file_name = list.files(pattern = "controlhours_city."),                              
                  year = str_sub(file_name, start = -8L, end = -5L),                              
                  outcome_data = lapply(list.files(pattern = "controlhours_city."), 
                                        read_fst))  %>%
  dplyr::select(-file_name) %>% 
  unnest(cols = "outcome_data")


####*******************************************************
#### 1: Create City-level MI + Weather + PM2.5 Dataset #### 
####*******************************************************

# 1a Transform all datetime variables to America/New_York
#    Note: YYYY does not match the year in the datetime variable for the first
#          five observations of the year in w_city because YYYY was defined on
#          UTC, but the date was then transformed to America/New_York (ET).
#          When we use tz = 'America/New_York' the as_datetime function transforms
#          all times from UTC to ET, however, we actually started at ET, so this
#          incorrectly adds five hours to the datetimes. Thus, we add five hours
#          to the hour index before converting. We must set the timezone as
#          'America/New_York' to ensure Daylight Savings is parsed correctly.
#          We will need to average the "fall back" hours
#          because two 1:00am hours are induced on every fall back day.
mi_city <- mi_city %>% mutate(DayDateTime = ymd_hms(DayDateTime, tz = "America/New_York"))
w_city <- w_city %>% mutate(HourIndex = HourIndex + 5)
w_city <- w_city %>% mutate(datetime = as_datetime(HourIndex*3600, 
                                                   origin = '1990/01/01 00:00:00',
                                                   tz = 'America/New_York'))
pm_city <- pm_city %>% mutate(date_any = ymd_hms(date_any, tz = "America/New_York")) %>% 
  mutate(date_any = date(date_any))

# 1b Create unique city/datetime id variable (city_activeLag) to merge mi data 
#    with weather data (only create for weather data, is created in lag function
#    for MI data)
# 1b.i Create variables
w_city <- w_city %>% mutate(city_activeLag = paste0(city, datetime)) %>% 
  dplyr::select(city_activeLag, rh, temp)
# 1b.ii Average weather vars by city_activeLag variable to remove duplicate Daylight
#       Savings ending hours in the Fall
w_city <- w_city %>% group_by(city_activeLag) %>% 
  summarize(rh = mean(rh, na.rm = T), temp = mean(temp, na.rm = T))
# 1b.iii Confirm only one weather observation per unique city_datetime
length(unique(w_city$city_activeLag)) # equal to df length

# 1c Create unique city/date id variable to merge mi data with pm data
mi_city <- mi_city %>% mutate(DayDate = date(DayDateTime)) %>%
  mutate(city_date = paste0(city, DayDate))
pm_city <- pm_city %>% mutate(city_date = paste0(city, date_any)) %>% 
  dplyr::select(-date_any, -city)

# 1d Merge pm with MI dataset
# city_outcome_covar <- mi_city %>% 
#   left_join(w_city, by = 'city_datetime')
city_outcome_pm <- mi_city %>% 
  left_join(pm_city, by = 'city_date')

# 1e Initialize function to assign lagged weather variables
assign_laggedWeather_city <- function(dta_outcome, dta_weather, numLag){
  #dta_outcome <- mi[1:1000,]
  #dta_exp <- no2_city
  #numLag <- 12
  
  # 1.1b Create variable name 
  VarName1 <- paste0('rh_lag', str_pad(numLag, 2, 'left', '0'))
  VarName2 <- paste0('temp_lag', str_pad(numLag, 2, 'left', '0'))
  
  # 1.1c Create column of lag 
  dta_outcome <- dta_outcome %>% 
    mutate(DayDateTime = ymd_hms(DayDateTime, tz = "America/New_York")) %>% 
    mutate(activeLag := DayDateTime - as.period(1 * numLag, 'hour')) %>% 
    mutate(city_activeLag = paste0(city, activeLag))
  
  # 1.1d Join with exposure data 
  dta_outcome <- dta_outcome %>%
    left_join(dta_weather, by = 'city_activeLag')
  
  # 1.1e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dta_outcome <- dta_outcome %>% 
    mutate(!!VarName1 := rh,
           !!VarName2 := temp) %>% 
    dplyr::select(-rh, -temp, -activeLag, -city_activeLag)
}

# 1f Assign city-wide weather covariates via a loop
city_outcome_covar <- city_outcome_pm
maxLag <- 24
for(l in 0:(maxLag-1)){
  city_outcome_covar <- assign_laggedWeather_city(city_outcome_covar, w_city, l)
}

# 1e Check missing
# there are two missing rh/temp observations, resulting in 67 to 111 missing obs
# for each lag
# there are 296,673 missing pm observations
# a large number of missing pm2.5 is expected, given that we only had pm monitor
# coverage in 5 out of 9 cities, many monitors record every 3rd or 6th day, and
# there seems to be a lot of issues with data collection where measures were
# not reported because of measurement issues
summary(city_outcome_covar)

# 1f Save merged mi + covariates dataset
city_outcome_covar %>% write_fst(paste0(data_path_external, 'mi_covars_city.fst'))

# 1g Clear environment to preserve memory for zipcode level work
rm(city_outcome_covar, city_outcome_pm, maxLag, pm_city)


####*********************************************************
#### 2: Create City-level MI + Weather Dataset w 48 Lags #### 
####*********************************************************

# 2a Assign city-wide weather covariates in a loop, 48 lags
#    Note: This takes ~ 10 min to run
city_outcome_covar_48Lags <- mi_city
maxLag <- 48
for(l in 0:(maxLag-1)){
  city_outcome_covar_48Lags <- 
    assign_laggedWeather_city(city_outcome_covar_48Lags, w_city, l)
}

# 2b Save merged mi + covariates dataset
city_outcome_covar_48Lags %>% write_fst(paste0(data_path_external, 'mi_covars_city_48Lags.fst'))

# 2c Clear environment to preserve memory for zipcode level work
rm(city_outcome_covar_48Lags, maxLag, mi_city, w_city,
   assign_laggedWeather_city, l)


####**************************************************
#### 3: Create Zipcode-level MI + Weather Dataset #### 
####**************************************************

# 3a Transform all datetime variables to America/New_York
#    Note: YYYY does not match the year in the datetime variable for the first
#          five observations of the year in w_zc because YYYY was defined on
#          UTC, but the date was then transformed to America/New_York (ET).
#          When we use tz = 'America/New_York' the as_datetime function transforms
#          all times from UTC to ET, however, we actually started at ET, so this
#          incorrectly adds five hours to the datetimes. Thus, we add five hours
#          to the hour index before converting. We must set the timezone as
#          'America/New_York' to ensure Daylight Savings is parsed correctly.
#          We will need to average the "fall back" hours
#          because two 1:00am hours are induced on every fall back day.
w_zc <- w_zc %>% mutate(HourIndex = HourIndex + 5)
w_zc <- w_zc %>% mutate(datetime = as_datetime(HourIndex*3600, 
                                                   origin = '1990/01/01 00:00:00',
                                                   tz = 'America/New_York'))

# 3b Create unique zip code/datetime id variable to merge mi data with weather data
#    (only create for weather data, is created in lag function for MI data)
# 3b.i Create variables
w_zc <- w_zc %>% mutate(zc_activeLag = paste0(zip_code, datetime)) %>% 
  dplyr::select(zc_activeLag, rh, temp)
# 3b.ii Average weather vars by zc_activeLag variable to remove duplicate Daylight
#       Savings ending hours in the Fall
#       This may take ~30 min to run
w_zc <- w_zc %>% group_by(zc_activeLag) %>% 
  summarize(rh = mean(rh, na.rm = T), temp = mean(temp, na.rm = T))
# 3b.iii Confirm only one weather observation per unique zc_datetime
length(unique(w_zc$zc_activeLag)) # equal to df length

# 3c Save weather dataset with one observation per unique zc_datetime (zc_activeLag)
w_zc %>% write_fst(paste0(data_path, 'weather_zipcode_unique_datetime.fst'))

# 3d Load MI data at zc level
#    Note: We waited to load this data so that there was less data in the global
#          environment when the previous averaging was done, to save memory
setwd(data_path_external)
mi_zc <- tibble(file_name = list.files(pattern = "controlhours_zc."),                              
                year = str_sub(file_name, start = -8L, end = -5L),                              
                outcome_data = lapply(list.files(pattern = "controlhours_zc."), 
                                      read_fst))  %>%
  dplyr::select(-file_name) %>% 
  unnest(cols = "outcome_data")

# 3e Split MI data into a list of yearly dataframes
years.list_mizc <- mi_zc %>% split(mi_zc$YYYY)

# 3f Initialize function to assign lagged weather variables for a single lag
assign_laggedWeather_zc <- function(dta_outcome, dta_weather, numLag){
  #dta_outcome <- mi[1:1000,]
  #dta_exp <- no2_city
  #numLag <- 12
  
  # 3f.i Create variable name 
  VarName1 <- paste0('rh_lag', str_pad(numLag, 2, 'left', '0'))
  VarName2 <- paste0('temp_lag', str_pad(numLag, 2, 'left', '0'))
  
  # 3f.ii Create column of lag 
  dta_outcome <- dta_outcome %>% 
    mutate(DayDateTime = ymd_hms(DayDateTime, tz = "America/New_York")) %>% 
    mutate(activeLag := DayDateTime - as.period(1 * numLag, 'hour')) %>% 
    mutate(zc_activeLag = paste0(zip_code, activeLag))
  
  # 3f.iii Join with exposure data 
  dta_outcome <- dta_outcome %>%
    left_join(dta_weather, by = 'zc_activeLag')
  
  # 3f.iv Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dta_outcome <- dta_outcome %>% 
    mutate(!!VarName1 := rh,
           !!VarName2 := temp) %>% 
    dplyr::select(-rh, -temp, -activeLag, -zc_activeLag)
}

# Note: I attempted to use parallelization, but it was still taking quite a 
# long time and my computer ran out of memory, so I switched to the forloop below.

# # 3g Set up parallelization
# # 3g.i Get number of cores
# #      Note: We subtract one to reserve a core for other tasks
# n.cores <- parallel::detectCores() - 1
# # 3g.ii Create the cluster
# my.cluster <- parallel::makeCluster(
#   n.cores,
#   type = 'FORK')
# # 3g.iii Register cluster to be used by %dopar%
# doParallel::registerDoParallel(cl = my.cluster)

# 3h Assign 24 weather lags to zc MI data, for each year
# foreach(
#     i = 1:length(years.list_mizc)
#   ) %dopar% {
#     df <- years.list_mizc[[i]]
#     ActiveYear <- unique(df$YYYY)
#     maxLag <- 24
#     for(l in 0:(maxLag-1)){
#       df <- assign_laggedWeather_zc(df, w_zc, l)
#     }
#     df %>% 
#       fst::write_fst(paste0(data_path_external, 'zc_outcome_covar_',
#                             ActiveYear, '.fst'))
#   }
# 
# stopCluster(my.cluster)

# 3g Assign zc-wide 24 lags of weather covariates via a loop
#    Notes: This takes ~2 hours to run
for(x in 1:length(years.list_mizc)){
  df <- years.list_mizc[[x]]
  ActiveYear <- unique(df$YYYY)
  maxLag <- 24
  for(l in 0:(maxLag-1)){
    df <- assign_laggedWeather_zc(df, w_zc, l)
  }
  df %>% 
    fst::write_fst(paste0(data_path_external, 'zc_outcome_covar_',
                          ActiveYear, '.fst'))
  print(ActiveYear)
}



