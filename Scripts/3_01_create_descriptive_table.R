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
data_path <- '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data/Intermediate_Data/'
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
  
# 1b Calculate variables needed for table:  
#    NO2 mean/sd, temp mean/sd, rh mean/sd
#    Used data for all cases and controls
data4casecross_city %>% group_by(city) %>% 
  summarise(NO2_mean = mean(no2_lag00, na.rm = T),
            NO2_sd = sd(no2_lag00, na.rm = T),
            temp_mean = mean(temp_lag00, na.rm = T),
            temp_sd = sd(temp_lag00, na.rm = T),
            rh_mean = mean(rh_lag00, na.rm = T),
            rh_sd = sd(rh_lag00, na.rm = T))

# 1c Add a total row to the table
data4casecross_city %>%
  summarise(NO2_mean = mean(no2_lag00, na.rm = T),
            NO2_sd = sd(no2_lag00, na.rm = T),
            temp_mean = mean(temp_lag00, na.rm = T),
            temp_sd = sd(temp_lag00, na.rm = T),
            rh_mean = mean(rh_lag00, na.rm = T),
            rh_sd = sd(rh_lag00, na.rm = T))

# 1d Calculate MI case count by city and MI case count total
#    Note: include only rows marked "cases" so as not to quadruple
#          count by also adding in the MIs on control days
data4casecross_city %>% group_by(city, case_control) %>% 
  summarise(MI_case_count = sum(MI_count_Prim, na.rm = T))
data4casecross_city %>% group_by(case_control) %>% 
  summarise(MI_case_count = sum(MI_count_Prim, na.rm = T))

# 1e Percent of cases in NYC
tot_cases <- data4casecross_city %>% filter(case_control == 'case') %>% 
  summarise(tot = sum(MI_count_Prim, na.rm = T))
nyc_cases <- data4casecross_city %>% filter(city == 'New York' &
                                              case_control == 'case') %>% 
  summarise(nyc = sum(MI_count_Prim, na.rm = T))
prop_nyc_cases <- (nyc_cases$nyc/tot_cases$tot)*100

# 1f NO2 for cases vs control hours, entire study period
data4casecross_city %>% group_by(case_control) %>%
  summarise(NO2_mean = mean(no2_lag00, na.rm = T),
            NO2_sd = sd(no2_lag00, na.rm = T))
  
