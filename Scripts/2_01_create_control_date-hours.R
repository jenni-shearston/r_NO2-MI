# Create Control Date-Hours
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 08/04/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define functions to create control hours
# 2: Test function with single year
# 3: Create control hours in parallel


####**************
#### N: Notes #### 
####**************

# We choose control hours via time-stratified bidirectional matching 
# We match by year, month, day of the week, and hour of the day 
# We also use the function to generate the case hour, 
# so that all of the hours have parallel format.


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

# 0b Load MI data
mi_city <- read_fst(paste0(data_path_external, 'mi_city'))
mi_zc <- read_fst(paste0(data_path_external, 'mi_zc'))

# 0c Confirm each row is a unique spatial unit / datetime observation 
mi_city %>% mutate(id = paste0(city, CaseDateRaw)) %>% summarize(unique_obs = length(unique(id)))
mi_zc %>% mutate(id = paste0(zip_code, CaseDateRaw)) %>% summarize(unique_obs = length(unique(id)))

# 0d Convert case date to posixct
mi_city$CaseDateTime = parse_date_time(mi_city$CaseDateRaw, "ymd H", tz = "America/New_York")
mi_zc$CaseDateTime = parse_date_time(mi_zc$CaseDateRaw, "ymd H", tz = "America/New_York")

# 0e Add year variable to split dataset by year (for computational speed)
mi_city$YYYY <- year(mi_city$CaseDateTime)
mi_zc$YYYY <- year(mi_zc$CaseDateTime)

# 0f Filter to 2000 and onward
mi_city <- mi_city %>% filter(YYYY > 1999)
mi_zc <- mi_zc %>% filter(YYYY > 1999)

# 0g Add a unique id variable & spatial unit variable
mi_city <- mi_city %>% mutate(id = 1:n(), spatialUnit = 'city')
mi_zc <- mi_zc %>% mutate(id = 1:n(), spatialUnit = 'zc')

# 0h Split data into a list of yearly dataframes
years.list_city <- mi_city %>% split(mi_city$YYYY)
years.list_zc <- mi_zc %>% split(mi_zc$YYYY)


####*************************************************
#### 1: Define Functions to Create Control Hours ####
####*************************************************

# Here I edit functions developed by Sebastian Rowland

# 1a Define function to create potentially matching datehours
# Note: our data is in Eastern Time (ET)
#       as such, we are matching on the socially-recognized time that the case would have experienced
#       In particular, due to daylight savings time, certain hours do not 'exist' in ET
#       This will lead to some NA's in the dataset 
#       While converting to UTC would avoid these NA's, 
#       the cases experienced time according to ET 
#       and their time-varying factors would follow ET, not UTC 
#       so if we match on UTC we would not be matching on hour of the day 

make_control_hr <- function(hours1, BeforeAfter, WK){ 
  # hours1 <- df.YYYY; BeforeAfter <- 'Before'; WK <- 4
  # The name of the hour; accounts for if control or case
  VarName <- paste0(BeforeAfter, '_', str_trunc(WK, 1, 'left', ''))    
  # adds WKs number of weeks, preserves hour of day even in daylight saving
  hours1 %>% mutate(!!VarName := CaseDateTime + as.period(7 * WK, 'day'))  
}

# 1b Define function to create control hours
create_control_hours_by_year <- function(df.YYYY){
  #df.YYYY <- years.list[[6]]
  
  # 1c Add progress bar
  #progressr::p()
  
  # 1d Use function to create bidirectionally symmetric datehours 
  hours1 <-  df.YYYY
  hours1 <- hours1 %>% 
    mutate(CaseDateTime = parse_date_time(CaseDateRaw, 'ymd H', tz = 'America/New_York')) 
  ActiveYYYY <- hours1$YYYY[1]
  hours1 <- make_control_hr(hours1, 'Before', -4)
  hours1 <- make_control_hr(hours1, 'Before', -3)
  hours1 <- make_control_hr(hours1, 'Before', -2)
  hours1 <- make_control_hr(hours1, 'Before', -1)
  hours1 <- make_control_hr(hours1, 'CaseHour', 0)
  hours1 <- make_control_hr(hours1, 'After', 1)
  hours1 <- make_control_hr(hours1, 'After', 2)
  hours1 <- make_control_hr(hours1, 'After', 3)
  hours1 <- make_control_hr(hours1, 'After', 4)
  
  # 1e Put in long format by HourName
  hours2 <- hours1 %>% 
    gather('HourName', 'DayDateTime', contains('CaseHour_'),
           contains('Before_'), contains('After_') ) 
  
  # 1f Stratify by month of event 
  hours3 <- hours2 %>% filter(month(CaseDateTime) == month(DayDateTime))
  
  # double Check timezone
  #tz(hours3$DayDateTime[1])
  
  # 1g Identify spatial unit
  spatialUnit <- ifelse(hours3$spatialUnit[1] == 'city', 'city', 'zc')
  
  # 1h Save results 
  hours3 %>% 
    fst::write_fst(paste0(data_path_external, 'controlhours_', spatialUnit,
                          '_', ActiveYYYY, '.fst'))
}


####************************************
#### 2: Test Function w Single Year ####
####************************************

test <- create_control_hours_by_year(years.list_city[[1]])


####*****************************************
#### 3: Create Control Hours in Parallel ####
####*****************************************

# 3a Set up future to run in parallel
plan(multisession)

# 3b Actually run the function, in parallel, with a progress bar
# Notes: Run time < 30 sec
# 3b.i City level
with_progress({
  p <- progressor(steps = length(years.list_city)*2)
  
  result_city <- future_map(years.list_city, create_control_hours_by_year,
                            .options = furrr_options(seed = TRUE))
})
# 3b.ii Zip code level
with_progress({
  p <- progressor(steps = length(years.list_zc)*2)
  
  result_zc <- future_map(years.list_zc, create_control_hours_by_year,
                          .options = furrr_options(seed = TRUE))
})

# 3c View one year's data to check
summary(result_city[[1]])
View(result_city[[1]])
summary(result_zc[[5]])
View(result_zc[[5]])



