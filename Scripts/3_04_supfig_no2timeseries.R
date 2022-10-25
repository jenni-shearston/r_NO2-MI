# Create Supplemental Figures of NO2 Time Series
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 09/01/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: 


####**************
#### N: Notes #### 
####**************

# In this script,  


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
# 0d.i NO2 data
city_no2 <- read_fst(paste0(data_path, 'no2_city_allhours')) %>% 
  mutate(datetime_ANY = ymd_hms(datetime_ANY, tz = 'America/New_York'))


####********************************************
#### 1: Sup Fig 1 - NO2 Time Series by City #### 
####********************************************

# 1a Create and save plot
tiff(paste0(output_path, 'Plots/', 'figS1_no2_timeseries.tif'),
     units = "in", width = 12, height = 10, res = 300)

city_no2 %>% ggplot() +
  geom_point(aes(x = datetime_ANY, y = no2_avg), 
             alpha = 0.3, shape = 'circle open') +
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


