# Assign Exposure with Lags to MI Data
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 08/09/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Define Functions for Assigning Lagged Exposures
# 2: Assign Lagged Exposures
# 3: Save Out New Datasets


####**************
#### N: Notes #### 
####**************

# In this script, the final goal is to create a dataset with lagged NO2
# exposures assigned to MI cases and controls, to be used in later 
# case-crossover analyses. 


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

# 0d Load NO2 data
# 0d.i Load city level data 
city_no2 <- read_fst(paste0(data_path, 'no2_city_allhours')) %>% 
  dplyr::select(city_activeLag, no2_avg)
# 0d.ii Load zip code level data
zc_no2 <- read_fst(paste0(data_path, 'no2_zipcode')) %>% 
  mutate(activeLag = ymd_hms(activeLag, tz = "America/New_York")) %>% 
  mutate(zc_activeLag = paste0(zip_code, activeLag)) %>% 
  dplyr::select(zc_activeLag, no2_avg)

# 0e Load MI+covariate data
#    Note: We will load the zc level data later, inside the function
# 0e.i Load data for main analysis
city_outcome_covar <- read_fst(paste0(data_path_external, 'mi_covars_city.fst'))
# 0e.ii Load data for 48 hour lag sensitivity analysis
city_outcome_covar_48Lags <- read_fst(paste0(data_path_external, 'mi_covars_city_48Lags.fst'))


####***************************************************
#### 1: Define Functions to Assign Lagged Exposure ####
####***************************************************

# Notes: We define two functions, one for city as a spatial unit, 
#        and one for zip code as a spatial unit 

# 1.1a Name function for assigning lagged exposure to cities
assign_laggedExp_city <- function(dta_outcome, dta_exp, numLag){
   #dta_outcome <- mi[1:1000,]
   #dta_exp <- no2_city
   #numLag <- 12
  
  # 1.1b Create variable name 
  VarName <- paste0('no2_lag', str_pad(numLag, 2, 'left', '0'))
  
  # 1.1c Create column of lag 
  dta_outcome <- dta_outcome %>% 
    mutate(DayDateTime = ymd_hms(DayDateTime, tz = "America/New_York")) %>% 
    mutate(activeLag := DayDateTime - as.period(1 * numLag, 'hour')) %>% 
    mutate(city_activeLag = paste0(city, activeLag))
  
  # 1.1d Join with exposure data 
  dta_outcome <- dta_outcome %>%
    left_join(dta_exp, by = 'city_activeLag')
  
  # 1.1e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dta_outcome <- dta_outcome %>% 
    mutate(!!VarName := no2_avg) %>% 
    dplyr::select(-no2_avg, -activeLag, -city_activeLag)
}

# 1.2a Name function for assigning lagged exposure to zip codes
assign_laggedExp_zip <- function(dta_outcome, dta_exp, numLag){
  #dta_outcome <- mi[1:1000,]
  #dta_exp <- no2_zc
  #numLag <- 12
  
  # 1.2b Create variable name 
  VarName <- paste0('no2_lag', str_pad(numLag, 2, 'left', '0'))
  
  # 1.2c Create column of lag 
  dta_outcome <- dta_outcome %>% 
    mutate(DayDateTime = ymd_hms(DayDateTime, tz = "America/New_York")) %>% 
    mutate(activeLag := DayDateTime - as.period(1 * numLag, 'hour')) %>% 
    mutate(zc_activeLag = paste0(zip_code, activeLag))
  
  # 1.2d Join with exposure data 
  dta_outcome <- dta_outcome %>% 
    left_join(dta_exp, by = 'zc_activeLag')
  
  # 1.2e Rename lagged exposure column
  # this is done in two steps because it is tricky to do dynamic variable naming 
  # with the rename() function. mutate + select does the same thing.
  dta_outcome <- dta_outcome %>% 
    mutate(!!VarName := no2_avg) %>% 
    dplyr::select(-no2_avg, -activeLag, -zc_activeLag)
}


####*******************************
#### 2: Assign Lagged Exposure ####
####*******************************

# Notes: city-wide loop takes approximately 6min to run
#        zip-wide loop takes approximately 1 hour to run
#        48-lag city-wide loop takes a few minutes to run

# 2a Assign city-wide exposures via a loop
#    Note: All months in NO2 data included (no months have been excluded yet)
data4casecross_city <- city_outcome_covar
maxLag <- 24
for(l in 0:(maxLag-1)){
  data4casecross_city <- assign_laggedExp_city(data4casecross_city, city_no2, l)
}

# 2b Assign zip-wide exposures via a loop for sensitivity analysis
#    Will use which NO2 dataset that was decided for the main analysis
# 2b.i Create years list
years.list <- list.files(path = data_path_external,
                         pattern = "zc_outcome_covar_")
# 2b.ii Loop over years list
for(x in 1:length(years.list)){
  df <- read_fst(paste0(data_path_external, years.list[[x]]))
  ActiveYear <- unique(df$YYYY)
  maxLag <- 24
  for(l in 0:(maxLag-1)){
    df <- assign_laggedExp_zip(df, zc_no2, l)
  }
  df %>% 
    fst::write_fst(paste0(data_path_external, 'data4casecross_zip_',
                          ActiveYear, '.fst'))
  print(ActiveYear)
}
 
# 2c  Assign city-wide exposures via a loop for 48 hour lag sensitivity analysis
#     Note: All months in NO2 data included; no months excluded yet
data4casecross_city_48Lags <- city_outcome_covar_48Lags
maxLag <- 48
for(l in 0:(maxLag-1)){
  data4casecross_city_48Lags <- 
    assign_laggedExp_city(data4casecross_city_48Lags, city_no2, l)
}   


####**************************
#### 3: Save New Datasets ####
####**************************

# 3a Save city datasets
data4casecross_city %>% write_fst(paste0(data_path_external, 'data4casecross_city_allhours.fst'))
data4casecross_city_5per %>% write_fst(paste0(data_path_external, 'data4casecross_city_5per.fst'))
data4casecross_city_25per %>% write_fst(paste0(data_path_external, 'data4casecross_city_25per.fst'))

# 3b Save 48 hour lag dataset
data4casecross_city_48Lags %>% write_fst(paste0(data_path_external, 'data4casecross_city_48Lags.fst'))

# 3c Merge all zip code files into one and save
# 3c.i Read in data
setwd(data_path_external)
data4casecross_zip <- tibble(file_name = list.files(pattern = "data4casecross_zip_."),
                             data = lapply(list.files(pattern = "data4casecross_zip_."),
                                                   read_fst))  %>% 
  dplyr::select(-file_name) %>% 
  unnest(cols = "data")
# 3c.ii Save out full dataset
data4casecross_zip %>% write_fst(paste0(data_path_external, 'data4casecross_zip.fst'))




