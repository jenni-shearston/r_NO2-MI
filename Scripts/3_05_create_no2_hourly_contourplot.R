# Create NO2 Hourly Contourplot
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 01/27/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create contour plot of NO2 and time of day


####**************
#### N: Notes #### 
####**************

# This script is based on code created by Vivian Do. 
# In this script, we create a contour plot showing within-day frequency of hourly 
# NO2 concentrations. 

# Helpful links:
# contour plot link: https://plotly.com/ggplot2/contour-plots/
# https://r-graphics.org/recipe-legend-label-text


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
# 0d.i NO2 data
data4casecross_city <- read_fst(paste0(data_path_external, 'data4casecross_city_5per.fst'))

# 0e Set options
options(scipen = 999)


####***************************************************
#### 1: Create contour plot of NO2 and time of day #### 
####***************************************************

# 1a Prepare dataset
df_contour <- data4casecross_city %>% 
  dplyr::select(DayDateTime, no2_lag00) %>% 
  distinct(DayDateTime, .keep_all = TRUE) %>% 
  mutate(hour = hour(DayDateTime),
         no2_lag00 = round(no2_lag00, digits = 0)) %>% 
  group_by(hour, no2_lag00) %>% 
  summarise(count_no2 = n())

# 1b Create and save plot
tiff(paste0(output_path, 'Plots/', 'fig2_no2_contourplot.tif'),
     units = "in", width = 12, height = 8, res = 300)

df_contour %>% 
  ggplot(aes(x = hour, y = no2_lag00)) +
  stat_contour_filled(aes(z = count_no2)) +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu") +
  labs(fill = 'Count of \n Obs.') +
  xlab("Hour of Day") + ylab(expression('NO'[2]*' (ppb)')) +
  scale_x_continuous(limits = c(0, 23)) +
  theme_bw(base_size = 20)

dev.off()


