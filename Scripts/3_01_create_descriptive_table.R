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
# 2: Calculating EPA Standard
# 3: Near-Road / Non-Near-Road Fraction

####**************
#### N: Notes #### 
####**************

# In this script, we use the dataset for the main models (with rows = observation
# for a single case or control) to calculate descriptive statistics for Table 1.

# We use all NO2 data to calculate the NO2 NAAQS standards during our study period,  
# and the road-side/non-road-side fraction for the Buffalo/Cheektowaga area from 
# 2014-2015.


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
# 0d.ii NO2 data
city_no2 <- read_fst(paste0(data_path, 'no2_city_allhours')) %>% 
  mutate(datetime_ANY = ymd_hms(datetime_ANY, tz = 'America/New_York'))


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
  

####*********************************
#### 2: Calculating EPA Standard #### 
####*********************************

# 2a Calculate 1-hour daily max values 
epa <- city_no2 %>% 
  group_by(datetime_ANY) %>% 
  summarise(max_no2 = max(no2_avg, na.rm = T))

# 2b Identify the 98th percentile max concentrations for each year
# 2b.i Create year variable
epa <- epa %>% ungroup() %>% 
  mutate(year = year(datetime_ANY))
# 2b.ii Calculate 98th percentile for each year
epa <- epa %>% group_by(year) %>% 
  mutate(p98 = quantile(max_no2, c(.98), na.rm = T))

# 2c Create 3-year assignments
epa <- epa %>% ungroup() %>% 
  mutate(yr_avg_period = 
           case_when(
           year == 2000 | year == 2001 | year == 2002 ~ 'ga',
           year == 2001 | year == 2002 | year == 2003 ~ 'gb',
           year == 2002 | year == 2003 | year == 2004 ~ 'gc',
           year == 2003 | year == 2004 | year == 2005 ~ 'gd',
           year == 2004 | year == 2005 | year == 2006 ~ 'ge',
           year == 2005 | year == 2006 | year == 2007 ~ 'gf',
           year == 2006 | year == 2007 | year == 2008 ~ 'gh',
           year == 2007 | year == 2008 | year == 2009 ~ 'gi',
           year == 2008 | year == 2009 | year == 2010 ~ 'gj',
           year == 2009 | year == 2010 | year == 2011 ~ 'gk',
           year == 2010 | year == 2011 | year == 2012 ~ 'gl',
           year == 2011 | year == 2012 | year == 2013 ~ 'gm',
           year == 2012 | year == 2013 | year == 2014 ~ 'gn',
           year == 2013 | year == 2014 | year == 2015 ~ 'go',
         ))

# 2d Average 98 percentile values by 3-year averages
epa <- epa %>% group_by(yr_avg_period) %>% 
  summarise(epa_mean = mean(p98))


####*******************************************
#### 3: Near-Road / Non-Near-Road Fraction #### 
####*******************************************

# 3a Restrict to Buffalo and Cheektowaga, from 2014-2015
#    Note: There is only one monitor in each place
#          Cheektowaga is near-road, Buffalo is non-near-road
#          Buffalo and Cheektowaga are part of the same metro area
road_fraction <- city_no2 %>% 
  filter(city == 'Buffalo' | city == 'Cheektowaga') %>% 
  mutate(year = year(datetime_ANY)) %>% 
  filter(year == 2014 | year == 2015) %>% 
  dplyr::select(city, datetime_ANY, no2_avg)

# 3b Pivot wider
road_fraction <- road_fraction %>% 
  pivot_wider(names_from = city,
              values_from = no2_avg)

# 3c Calculate hourly fraction
road_fraction <- road_fraction %>% 
  mutate(fraction = Cheektowaga/Buffalo)

# 3e Calculate mean hourly fraction
road_fraction %>% filter(!is.infinite(fraction)) %>% 
  summarise(mean(fraction, na.rm = TRUE))



