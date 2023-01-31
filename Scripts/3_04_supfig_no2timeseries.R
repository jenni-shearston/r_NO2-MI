# Create Supplemental Figures Not Relating to Model Outputs
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 01/30/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Sup Fig 2 - NO2 Time Series by City
# 2: Calculate n for Flowchart of Included Cases (Sup Fig 1) 


####**************
#### N: Notes #### 
####**************

# In this script, we create non-model-related supplemental figures. 


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
model_path <- paste0(print(here::here('Outputs/Models')),'/')

# 0d Load data
# 0d.i NO2 data
city_no2 <- read_fst(paste0(data_path, 'no2_city_allhours')) %>% 
  mutate(datetime_ANY = ymd_hms(datetime_ANY, tz = 'America/New_York'))
# 0d.ii Load MI data
mi <- read_fst(paste0(data_path_external, 'MI_for_NO2_Analysis.fst'))
# 0d.iii Load crosswalk for city-level analysis
city_crosswalk <- read_fst(paste0(data_path, 'city_analysis_spatial_crosswalk'))
# 0d.iv Load NO2 dataframe with all possible hours
all_hours_dataframe <- read_fst(paste0(data_path, 'no2_city_allhours'))
# 0d.v Load main model
main_mod <- readRDS(paste0(model_path, 'HourlyNO2_MIcountPrim_Main_ERlin_LR4dfevenknots.RDS'))
# 0d.vi Load dataset used for main model
data4casecross_city <- read_fst(paste0(data_path_external, 'data4casecross_city_5per.fst')) %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0))


####********************************************
#### 1: Sup Fig 2 - NO2 Time Series by City #### 
####********************************************

# 1a Create and save plot
tiff(paste0(output_path, 'Plots/', 'figS2_no2_timeseries.tif'),
     units = "in", width = 12, height = 10, res = 300)

city_no2 %>% ggplot() +
  geom_line(aes(x = datetime_ANY, y = no2_avg),
            alpha = 0.3) +
  #geom_point(aes(x = datetime_ANY, y = no2_avg), 
  #           alpha = 0.3, shape = 'circle open') +
  geom_smooth(aes(x = datetime_ANY, y = no2_avg), 
              size = 1, color = "#800000FF") +
  facet_wrap(~city, nrow = 3, ncol = 3) +
  xlab("Date") + ylab(expression('NO'[2]*' (ppb)')) +
  theme_bw(base_size = 16)

dev.off()

# 1b Run lm to determine trend direction
city_no2_lm <- city_no2 %>% 
  group_by(city) %>% 
  nest() %>% 
  mutate(no2_lm = map(data, ~lm(no2_avg ~ datetime_ANY, data = .x)))

summary(city_no2_lm$no2_lm[[1]]) # negative -2.3^-8 (Amherst)
summary(city_no2_lm$no2_lm[[2]]) # negative -2.7^-8 (Buffalo)
summary(city_no2_lm$no2_lm[[3]]) # positive 5.0^-8 (Cheektowaga)
summary(city_no2_lm$no2_lm[[4]]) # positive 4.56^-9 (Corning)
summary(city_no2_lm$no2_lm[[5]]) # negative -3.0^-8 (East Meadow)
summary(city_no2_lm$no2_lm[[6]]) # negative -7.4^-9 (Hogansburg)
summary(city_no2_lm$no2_lm[[7]]) # negative -2.9^-8 (Holtsville)
summary(city_no2_lm$no2_lm[[8]]) # negative -3.4^-8 (New York)
summary(city_no2_lm$no2_lm[[9]]) # positive 1.2^-8 (Rochester)


####****************************************************
#### 2: Calculate n for Flowchart of Included Cases #### 
####****************************************************

# Supplemental Figure 1

# 2a Identify original number of MI cases in study cities
# 2a.i Rename zip code variable 
#      Note: (in the MI dataset zip code is labeled zcta, but
#      actually is zip code and NOT zcta)
mi <- mi %>% rename(zip_code = zcta)
# 2a.ii Add city variable and restrict to cities in study period
mi_city <- city_crosswalk %>% dplyr::select(zip_code, city) %>% distinct() %>%
  left_join(mi, by = "zip_code") 
# 2a.iii Restrict to years in study period
mi_city <- mi_city %>% 
  mutate(CaseDateTime = parse_date_time(CaseDateRaw, 'ymd H', tz = 'America/New_York'),
         year = year(CaseDateTime)) %>% 
  filter(year > 1999 & year < 2016)
# 2a.iv Calculate number of cases
mi_city %>% summarise(sum(MI_count_Prim))

# 2b Identify number of cases remaining and removed after excluding city-months
#    missing >= 5% of hourly NO2 observations
# 2b.i Add case complete NO2 dataset to MI dataset
mi_city <- mi_city %>% 
  mutate(city_activeLag = paste0(city, CaseDateTime)) %>% 
  left_join(all_hours_dataframe, by = 'city_activeLag')
# 2b.ii Determine number of cases removed by city
mi_city %>% filter(exclude_month_5per == 1) %>% 
  group_by(city.x) %>% 
  summarise(sum(MI_count_Prim))
# 2b.iii Determine number of cases remaining after city-months missing >= 5% of
#        hourly NO2 observations are removed
mi_city %>% filter(exclude_month_5per == 0) %>% summarise(sum(MI_count_Prim))
# 2b.iv Determine start/end dates for each city
mi_city %>% filter(missing == 0) %>% group_by(city.x) %>% 
  summarise(min = min(CaseDateTime),
            max = max(CaseDateTime))

# 2c Determine total number of cases included in main model
# 2c.i Pull final n from model
main_mod$n # 619,244
# 2c.ii Identify cases included in main model
data4casecross_city %>% 
  dplyr::select(Case, id, MI_count_Prim, contains(c('temp_lag', 'rh_lag', 'no2_lag'))) %>%  
  na.omit() %>% 
  group_by(Case) %>% 
  summarise(sum(MI_count_Prim))


