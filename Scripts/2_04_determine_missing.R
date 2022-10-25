# Evaluate missingness by case / control status
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 09/02/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Add variables to calculate missingness
# 2: Calculate missing by case and control status
# 3: Calculate missing by case and control status for the first day of the month
# 4: Explore missingness for months/years and days of the month
# 5: Check that weather missingness is not differential by case/control
# 6: Exclude cities and/or year-months


####**************
#### N: Notes ####
####**************

# In this script, we evaluate the proportion of MI observations missing
# NO2 concentrations, and consider dropping months where more than 5% or 25% of 
# NO2 obs are missing. We also evaluate missingness in exposure by case and 
# control status, and ensure that the majority of days of the month and months
# are represented in the analysis. 


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

# 0d Load data for case-crossover analyses
data4casecross_city <- read_fst(paste0(data_path_external, 'data4casecross_city_allhours.fst'))
data4casecross_zip <- read_fst(paste0(data_path_external, 'data4casecross_zip.fst'))
data4casecross_city_48Lags <- read_fst(paste0(data_path_external, 'data4casecross_city_48Lags.fst'))

# 0e Load NO2 data with missingness variables
city_no2 <- read_fst(paste0(data_path, 'no2_city_allhours')) %>% 
  dplyr::select(-city, -no2_avg, -datetime_ANY, -activeLag)


####***********************************************
#### 1: Add variables to calculate missingness #### 
####***********************************************

# 1a Add missingness variables to data from case-crossover analyses
cases_missing_no2_city <- data4casecross_city %>% 
  mutate(city_activeLag = paste0(city, DayDateTime)) %>% 
  left_join(city_no2, by = 'city_activeLag')

# 1b Create variables indicating if an observation is (a) missing any of its
#    24 NO2 observations, (b) missing any of its 48 weather obs, and (c) if it 
#    is a case or control
cases_missing_no2_city <- cases_missing_no2_city %>% 
  mutate(missing_byrow = case_when(
    is.na(no2_lag00) | is.na(no2_lag01) | is.na(no2_lag02) |
      is.na(no2_lag03) | is.na(no2_lag04) | is.na(no2_lag05) |
      is.na(no2_lag06) | is.na(no2_lag07) | is.na(no2_lag08) |
      is.na(no2_lag09) | is.na(no2_lag10) | is.na(no2_lag11) |
      is.na(no2_lag12) | is.na(no2_lag13) | is.na(no2_lag14) |
      is.na(no2_lag15) | is.na(no2_lag16) | is.na(no2_lag17) |
      is.na(no2_lag18) | is.na(no2_lag19) | is.na(no2_lag20) |
      is.na(no2_lag21) | is.na(no2_lag22) | is.na(no2_lag23) ~ 1,
    !is.na(no2_lag00) & !is.na(no2_lag01) & !is.na(no2_lag02) &
      !is.na(no2_lag03) & !is.na(no2_lag04) & !is.na(no2_lag05) &
      !is.na(no2_lag06) & !is.na(no2_lag07) & !is.na(no2_lag08) &
      !is.na(no2_lag09) & !is.na(no2_lag10) & !is.na(no2_lag11) &
      !is.na(no2_lag12) & !is.na(no2_lag13) & !is.na(no2_lag14) &
      !is.na(no2_lag15) & !is.na(no2_lag16) & !is.na(no2_lag17) &
      !is.na(no2_lag18) & !is.na(no2_lag19) & !is.na(no2_lag20) &
      !is.na(no2_lag21) & !is.na(no2_lag22) & !is.na(no2_lag23) ~ 0),
    case_control = ifelse(HourName == 'CaseHour_0', 'case', 'control'),
    missing_by_row_weather = case_when(
      is.na(rh_lag00) | is.na(rh_lag01) | is.na(rh_lag02) |
        is.na(rh_lag03) | is.na(rh_lag04) | is.na(rh_lag05) |
        is.na(rh_lag06) | is.na(rh_lag07) | is.na(rh_lag08) |
        is.na(rh_lag09) | is.na(rh_lag10) | is.na(rh_lag11) |
        is.na(rh_lag12) | is.na(rh_lag13) | is.na(rh_lag14) |
        is.na(rh_lag15) | is.na(rh_lag16) | is.na(rh_lag17) |
        is.na(rh_lag18) | is.na(rh_lag19) | is.na(rh_lag20) |
        is.na(rh_lag21) | is.na(rh_lag22) | is.na(rh_lag23) |
        is.na(temp_lag00) | is.na(temp_lag01) | is.na(temp_lag02) |
        is.na(temp_lag03) | is.na(temp_lag04) | is.na(temp_lag05) |
        is.na(temp_lag06) | is.na(temp_lag07) | is.na(temp_lag08) |
        is.na(temp_lag09) | is.na(temp_lag10) | is.na(temp_lag11) |
        is.na(temp_lag12) | is.na(temp_lag13) | is.na(temp_lag14) |
        is.na(temp_lag15) | is.na(temp_lag16) | is.na(temp_lag17) |
        is.na(temp_lag18) | is.na(temp_lag19) | is.na(temp_lag20) |
        is.na(temp_lag21) | is.na(temp_lag22) | is.na(temp_lag23)~ 1,
      !is.na(rh_lag00) & !is.na(rh_lag01) & !is.na(rh_lag02) &
        !is.na(rh_lag03) & !is.na(rh_lag04) & !is.na(rh_lag05) &
        !is.na(rh_lag06) & !is.na(rh_lag07) & !is.na(rh_lag08) &
        !is.na(rh_lag09) & !is.na(rh_lag10) & !is.na(rh_lag11) &
        !is.na(rh_lag12) & !is.na(rh_lag13) & !is.na(rh_lag14) &
        !is.na(rh_lag15) & !is.na(rh_lag16) & !is.na(rh_lag17) &
        !is.na(rh_lag18) & !is.na(rh_lag19) & !is.na(rh_lag20) &
        !is.na(rh_lag21) & !is.na(rh_lag22) & !is.na(rh_lag23) &
        !is.na(temp_lag00) & !is.na(temp_lag01) & !is.na(temp_lag02) &
        !is.na(temp_lag03) & !is.na(temp_lag04) & !is.na(temp_lag05) &
        !is.na(temp_lag06) & !is.na(temp_lag07) & !is.na(temp_lag08) &
        !is.na(temp_lag09) & !is.na(temp_lag10) & !is.na(temp_lag11) &
        !is.na(temp_lag12) & !is.na(temp_lag13) & !is.na(temp_lag14) &
        !is.na(temp_lag15) & !is.na(temp_lag16) & !is.na(temp_lag17) &
        !is.na(temp_lag18) & !is.na(temp_lag19) & !is.na(temp_lag20) &
        !is.na(temp_lag21) & !is.na(temp_lag22) & !is.na(temp_lag23) ~ 0))


####*****************************************************
#### 2: Calculate missing by case and control status #### 
####*****************************************************

# 2a Determine missing NO2 values for MI cases and controls 
#    A case or control is marked missing if it is missing any one of the 24
#    hour lags of NO2 data
# 2a.i Calculated separately for months missing more and less than 5% of NO2 obs 
missing_5per <- cases_missing_no2_city %>% 
  dplyr::select(-MI_count_DXA410X1, -YYYY, -spatialUnit, -DayDate, -city_date) %>% 
  group_by(case_control, city, exclude_month_5per) %>% 
  summarise(missing_anyno2_bycc_bycity = sum(missing_byrow),
            total_bycc_bycity = n(),
            prop_missing_anyno2_bycc_bycity = missing_anyno2_bycc_bycity/total_bycc_bycity) %>% 
  ungroup()

# 2a.ii Calculated separately for months missing more and less than 25% of NO2 obs 
missing_25per <- cases_missing_no2_city %>% 
  dplyr::select(-MI_count_DXA410X1, -YYYY, -spatialUnit, -DayDate, -city_date) %>% 
  group_by(case_control, city, exclude_month_25per) %>% 
  summarise(missing_anyno2_bycc_bycity = sum(missing_byrow),
            total_bycc_bycity = n(),
            prop_missing_anyno2_bycc_bycity = missing_anyno2_bycc_bycity/total_bycc_bycity) %>% 
  ungroup()

# 2b Run regression to confirm no difference by case/control status
summary(glm(prop_missing_anyno2_bycc_bycity ~ exclude_month_5per 
            + case_control, data = missing_5per))


####************************************************************************************
#### 3: Calculate missing by case and control status for the first day of the month #### 
####************************************************************************************

# 3a Determine missing NO2 values for MI cases and controls for the first day of the month
#    A case or control is marked missing if it is missing any one of the 24
#    hour lags of NO2 data
# 3a.i Calculated separately for months missing more and less than 5% of NO2 obs 
missing_5per_monthday1 <- cases_missing_no2_city %>% 
  dplyr::select(-MI_count_DXA410X1, -YYYY, -spatialUnit, -DayDate, -city_date) %>% 
  mutate(monthday1 = mday(DayDateTime),
         missing_month_day1 = ifelse(monthday1 == 1 & missing_byrow == 1, 1, 0)) %>% 
  group_by(case_control, city, exclude_month_5per) %>% 
  summarise(missing_monthday1_bycc_bycity = sum(missing_month_day1),
            total_bycc_bycity = n(),
            prop_missing_monthday_bycc_bycity = missing_monthday1_bycc_bycity/total_bycc_bycity) %>% 
  ungroup()

# 3b Run regression to confirm no difference by case/control status
summary(glm(prop_missing_monthday_bycc_bycity ~ exclude_month_5per + 
              case_control, data = missing_5per_monthday1))


####*******************************************************************
#### 4: Explore missingness for months/years and days of the month #### 
####*******************************************************************

# Create heatmaps, by city and for cases and controls separately
# x axis is 1-31 for days of month
# y axis is months in study period

# 4a Add day of month variable and convert missing variable to factor
cases_missing_no2_city <- cases_missing_no2_city %>% 
  mutate(missing_byrow = factor(missing_byrow),
         day_of_month = mday(DayDateTime))

# 4b Amherst
# 4b.i All datehours
cases_missing_no2_city %>% filter(city == 'Amherst') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Amherst all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4b.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Amherst') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Amherst months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4c Buffalo
# 4c.i All datehours
cases_missing_no2_city %>% filter(city == 'Buffalo') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Buffalo all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4c.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Buffalo') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Buffalo months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4d Cheektowaga
# 4d.i All datehours
cases_missing_no2_city %>% filter(city == 'Cheektowaga') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Cheektowaga all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4d.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Cheektowaga') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Cheektowaga months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4e Corning
# 4e.i All datehours
cases_missing_no2_city %>% filter(city == 'Corning') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Corning all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4e.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Corning') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Corning months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4f East Meadow
# 4f.i All datehours
cases_missing_no2_city %>% filter(city == 'East Meadow') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('East Meadow all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4f.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'East Meadow') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('East Meadow months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4g Hogansburg
# 4g.i All datehours
cases_missing_no2_city %>% filter(city == 'Hogansburg') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Hogansburg all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4g.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Hogansburg') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Hogansburg months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4h Holtsville
# 4h.i All datehours
cases_missing_no2_city %>% filter(city == 'Holtsville') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Holtsville all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4h.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Holtsville') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Holtsville months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4i New York
# 4i.i All datehours
cases_missing_no2_city %>% filter(city == 'New York') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('New York all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4i.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'New York') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('New York months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))

# 4j Rochester
# 4j.i All datehours
cases_missing_no2_city %>% filter(city == 'Rochester') %>%   
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Rochester all hours') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))
# 4j.ii Including only months with less than 5% missing NO2 observations
cases_missing_no2_city %>% filter(city == 'Rochester') %>% filter(exclude_month_5per == 0) %>%  
  mutate(month = as.numeric(str_sub(year_month, start = 6L))) %>% 
  ggplot(aes(x = day_of_month, y = year_month, fill = missing_byrow)) + 
  geom_tile() + facet_grid(~case_control) + ggtitle('Rochester months < 5% missing') +
  scale_x_continuous(breaks = 1:31) +
  scale_y_discrete(breaks = c('2000_1', '2001_1', '2002_1', '2003_1', '2004_1', 
                              '2005_1', '2006_1', '2007_1', '2008_1', '2009_1',
                              '2010_1', '2011_1', '2012_1', '2013_1', '2014_1',
                              '2015_1'))


####***************************************************************************
#### 5: Check that weather missingness is not differential by case/control #### 
####***************************************************************************

# 5a Determine missing weather values for MI cases and controls 
#    A case or control is marked missing if it is missing any one of the 48
#    hour lags of weather data
# 5a.i Calculated separately for months missing more and less than 5% of NO2 obs 
missing_5per_weather <- cases_missing_no2_city %>% 
  dplyr::select(-MI_count_DXA410X1, -YYYY, -spatialUnit, -DayDate, -city_date) %>% 
  group_by(case_control, city, exclude_month_5per) %>% 
  summarise(missing_anywea_bycc_bycity = sum(missing_by_row_weather),
            total_bycc_bycity = n(),
            prop_missing_anywea_bycc_bycity = missing_anywea_bycc_bycity/total_bycc_bycity) %>% 
  ungroup()

####******************************************
#### 6: Exclude cities and/or year-months #### 
####******************************************

# 6a Keep only months missing less than 5% of NO2 data and save
cases_missing_no2_city %>% filter(exclude_month_5per == 0) %>% 
  write_fst(paste0(data_path_external, 'data4casecross_city_5per.fst'))

# 6b Create list of city/year/month combos missing less than 5% of NO2 data
cityYearMonths_to_keep <- cases_missing_no2_city %>% filter(exclude_month_5per == 0) %>% 
  mutate(city_year_month = paste0(city, '_', year_month)) %>% 
  dplyr::select(city_year_month) %>% distinct()

# 6c Filter 48Lags dataset to include only city/year/month combos missing less than
#    5% of NO2 data, to match the main analysis
# 6c.i Add city_year_month variable
data4casecross_city_48Lags <- data4casecross_city_48Lags %>% 
  mutate(DayDateTime = ymd_hms(DayDateTime, tz = 'America/New_York')) %>% 
  mutate(month = month(DayDateTime),
         city_year_month = paste0(city, '_', year, '_', month))
# 6c.ii Filter and save
data4casecross_city_48Lags %>% 
  filter(city_year_month %in% cityYearMonths_to_keep$city_year_month) %>% 
  write_fst(paste0(data_path_external, 'data4casecross_city_48Lags_5per.fst'))

# 6d Filter zip code dataset to include only city/year/month combos missing less than
#    5% of NO2 data, to match the main analysis
# 6d.i Load zip-city crosswalk
city_analysis_spatial_crosswalk <- 
  read_fst(paste0(data_path, 'city_analysis_spatial_crosswalk')) %>% 
  dplyr::select(city, zip_code) %>% distinct() 
# 6d.ii Add city variable to zip dataset
data4casecross_zip2 <- data4casecross_zip %>% 
  left_join(city_analysis_spatial_crosswalk, by = 'zip_code')
# 6d.iii Create city_year_month variable
data4casecross_zip2 <- data4casecross_zip2 %>% 
  mutate(DayDateTime = ymd_hms(DayDateTime, tz = 'America/New_York')) %>%
  mutate(month = month(DayDateTime),
         city_year_month = paste0(city, '_', year, '_', month))
# 6d.iv Filter and save
data4casecross_zip2 %>% 
  filter(city_year_month %in% cityYearMonths_to_keep$city_year_month) %>% 
  write_fst(paste0(data_path_external, 'data4casecross_zip_5per.fst'))




