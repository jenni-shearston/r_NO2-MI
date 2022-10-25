# Do Calculations for Table 1
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 09/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Calculations for Table 1

####**************
#### N: Notes #### 
####**************

# In this script, we use the dataset for the main models (with rows = observation
# for a single case or control) to calculate descriptive statistics for Table 1.


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
output_path <- paste0(print(here::here('Outputs')),'/')

# 0d Load data
# 0d.i Load data used in main models
data4casecross_city <- read_fst(paste0(data_path_external, 'data4casecross_city_5per.fst'))


####*********************************
#### 1: Calculations for Table 1 #### 
####*********************************

# 1a Identify years included in study
data4casecross_city %>% 
  mutate(DayDateTime = ymd_hms(DayDateTime, tz = 'America/New_York'),
         year = year(DayDateTime)) %>% 
  dplyr::select(city, year) %>% 
  table()
  
# 1b Calculate variables needed for table: MI case count, 
#    NO2 mean/sd, temp mean/sd, rh mean/sd
#    Used data for all cases and controls
data4casecross_city %>% group_by(city) %>% 
  summarise(MI_case_count = sum(MI_count_Prim, na.rm = T),
            NO2_mean = mean(no2_lag00, na.rm = T),
            NO2_sd = sd(no2_lag00, na.rm = T),
            temp_mean = mean(temp_lag00, na.rm = T),
            temp_sd = sd(temp_lag00, na.rm = T),
            rh_mean = mean(rh_lag00, na.rm = T),
            rh_sd = sd(rh_lag00, na.rm = T))

# 1c Add a total row to the table
data4casecross_city %>%
  summarise(MI_case_count = sum(MI_count_Prim, na.rm = T),
            NO2_mean = mean(no2_lag00, na.rm = T),
            NO2_sd = sd(no2_lag00, na.rm = T),
            temp_mean = mean(temp_lag00, na.rm = T),
            temp_sd = sd(temp_lag00, na.rm = T),
            rh_mean = mean(rh_lag00, na.rm = T),
            rh_sd = sd(rh_lag00, na.rm = T))

# 1d Percent of cases in NYC
tot_cases <- sum(data4casecross_city$MI_count_Prim, na.rm = T)
nyc_cases <- data4casecross_city %>% filter(city == 'New York') %>% 
  summarise(nyc_cases = sum(MI_count_Prim, na.rm = T))
prop_nyc_cases <- (nyc_cases$nyc_cases/tot_cases)*100

# 1e NO2 for cases vs control hours, entire study period
data4casecross_city %>% group_by(case_control) %>%
  summarise(NO2_mean = mean(no2_lag00, na.rm = T),
            NO2_sd = sd(no2_lag00, na.rm = T))
  
