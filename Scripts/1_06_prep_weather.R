# Load and Explore Weather Data
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 07/21/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Aggregate weather by city
# 2: Aggregate weather by zip code
# 3: Descriptive statistics / plots


####***********
#### Notes #### 
####***********

# Weather data was originally processed by Sebastian Rowland, following this script:
# https://github.com/s-rowland/HourlyTemp_MI/blob/master/A_04_Compute_weather_var.R

# Weather data is from NLDAS
# Temperature is in degrees Celsius
# Relative humidity was calculated from specific humidity, temperature, and pressure
# HourIndex represents the number of hours since 1990/01/01 00:00:00, 
#   tz = America/New_York


####*****************
#### Preparation #### 
####*****************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'Scripts/', 'packages.R'))

# 0c Set up filepath(s)
data_path <- '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data/Intermediate_Data/'
weather_path <- paste0(data_path, 'weather/')

# 0d Load data
# 0d.i Load crosswalk for city-level analysis
city_crosswalk <- read_fst(paste0(data_path, 'city_analysis_spatial_crosswalk'))
# 0d.ii Load crosswalk for zip code-level analysis
zipcode_crosswalk <- read_fst(paste0(data_path, 'zipcode_analysis_spatial_crosswalk'))


####**********************************
#### 1. Aggregate weather by city #### 
####**********************************

# 1a Initialize function to aggregate weather for city/datetimes, by year
prep_weather_city <- function(year, city_crosswalk){
  # year <- 2012
  # city_crosswalk <- city_crosswalk
  
  # 1a.i Set active year
  YYYY <- year
  
  # 1a.ii Load weather data for active yer
  w <- read_fst(paste0(weather_path, 'weather_long_Pop_', YYYY, '.fst'))
  
  # 1a.iii Add zip codes to weather data
  w_city_int <- city_crosswalk %>% dplyr::select(city, zip_code, zcta) %>% 
    distinct() %>% 
    left_join(w, by = 'zcta') %>% 
    dplyr::select(-zcta)
  
  # 1a.iv Average weather by city/datetime
  w_city_int <- w_city_int %>% 
    group_by(city, HourIndex) %>% 
    summarise(rh = mean(rh, na.rm = T),
              temp = mean(temp, na.rm = T))
  
}

# 1b Run function for all years of data of interest
#    Note: We include 1999 and 2016 because of timezone differences that may
#          require a few hours of data from either year. 
# 1b.i Initialize dataframe to hold aggregated weather data
w_city <- tibble(YYYY = 1999:2016,
                 data = list(NA))
# 1b.ii Run weather aggregation function in a for loop
for (i in 1:length(w_city$YYYY)){
  w_city$data[[i]] <- prep_weather_city(w_city$YYYY[[i]], city_crosswalk)
}
# 1b.iii Unlist dataframe
w_city <- w_city %>% unnest(cols = data)

# 1c Confirm one observation per city/datetime
#    Both datasets have the same number of obs, confirmed one obs per city/datetime
one_obs <- w_city %>% dplyr::select(city, HourIndex) %>% distinct()
rm(one_obs)

# 1d Save aggregated dataset
write_fst(w_city, paste0(data_path, 'weather_city.fst'))


####**************************************
#### 2. Aggregate weather by zip code #### 
####**************************************

# 2a Initialize function to aggregate weather for zipcode/datetimes, by year
prep_weather_zipcode <- function(year, zipcode_crosswalk){
  # year <- 2012
  # zipcode_crosswalk <- zipcode_crosswalk
  
  # 2a.i Set active year
  YYYY <- year
  
  # 2a.ii Load weather data for active yer
  w <- read_fst(paste0(weather_path, 'weather_long_Pop_', YYYY, '.fst'))
  
  # 2a.iii Add zip codes to weather data
  w_zc_int <- zipcode_crosswalk %>% dplyr::select(ZCTA5CE10, zip_code) %>% 
    distinct() %>% 
    left_join(w, by = c('ZCTA5CE10' = 'zcta')) 
  
  # 2a.iv Average weather by zipcode/datetime
  w_zc_int <- w_zc_int %>% 
    group_by(zip_code, HourIndex) %>% 
    summarise(rh = mean(rh, na.rm = T),
              temp = mean(temp, na.rm = T))
  
}

# 2b Run function for all years of data of interest
#    Note: We include 1999 and 2016 because of timezone differences that may
#          require a few hours of data from either year. 
# 2b.i Initialize dataframe to hold aggregated weather data
w_zc <- tibble(YYYY = 1999:2016,
               data = list(NA))
# 2b.ii Run weather aggregation function in a for loop (~12 mins to run)
for (i in 1:length(w_zc$YYYY)){
  w_zc$data[[i]] <- prep_weather_zipcode(w_zc$YYYY[[i]], zipcode_crosswalk)
}
# 2b.iii Unlist dataframe
w_zc <- w_zc %>% unnest(cols = data)

# 2c Confirm one observation per city/datetime
#    Both datasets have the same number of obs, confirmed one obs per city/datetime
one_obs <- w_zc %>% dplyr::select(zip_code, HourIndex) %>% distinct()
rm(one_obs)

# 2d Save aggregated dataset
write_fst(w_zc, paste0(data_path, 'weather_zipcode.fst'))


####***************************************
#### 3. Descriptive statistics / plots #### 
####***************************************

# 3a Data summary
summary(w_city)
mis <- w_city %>% filter(is.na(temp))

# 3b Time plot for relative humidity
w_city %>% ggplot(aes(x = HourIndex, y = rh)) + 
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ city, nrow = 3, ncol = 3)

# 3c Time plot for temperature
w_city %>% ggplot(aes(x = HourIndex, y = temp)) + 
  geom_line() +
  geom_smooth(method = 'lm') +
  facet_wrap(~ city, nrow = 3, ncol = 3)

# 3d Descriptive stats by city
w_desc <- w_city %>% group_by(city) %>% 
  summarise(min_rh = min(rh, na.rm = T),
            median_rh = median(rh, na.rm = T),
            iqr_rh = IQR(rh, na.rm = T),
            mean_rh = mean(rh, na.rm = T),
            sd_rh = sd(rh, na.rm = T),
            max_rh = max(rh, na.rm = T),
            min_temp = min(temp, na.rm = T),
            median_temp = median(temp, na.rm = T),
            iqr_temp = IQR(temp, na.rm = T),
            mean_temp = mean(temp, na.rm = T),
            sd_temp = sd(temp, na.rm = T),
            max_temp = max(temp, na.rm = T))



