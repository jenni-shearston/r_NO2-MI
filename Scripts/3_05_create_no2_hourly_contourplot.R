# Create NO2 Hourly Contourplot
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 06/15/2023

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

# 1a Make dataframe containing all unique datetime-city-no2 observations 
df_contour <- data4casecross_city %>% 
  dplyr::select(DayDateTime, no2_lag00, city) %>% 
  distinct() %>% 
  dplyr::select(-city)

# 1x Create y axis bin (NO2 conc bins) and z variable (count of hours for given
#    NO2 conc bin and hour)
df_contour <- df_contour %>% 
  mutate(hour = hour(DayDateTime),
         no2_lag00 = 5 * round(no2_lag00/5)) %>%
  group_by(hour, no2_lag00) %>% 
  summarise(count_no2 = n())

# 1b Create case-complete grid with all possible hour and NO2 concentration
#    combinations
no2_max = max(df_contour$no2_lag00, na.rm = T)
cc <- 
  tibble(no2_lag00 = rep(seq(from = 0, to = no2_max, by = 5), times = 24),
         hour = rep(seq(from = 0, to = 23), each = (no2_max/5)+1))

# 1c Merge case-complete with observed data for plotting
cc <- cc %>% 
  left_join(df_contour, by = c('no2_lag00', 'hour'))

# 1d Convert missing counts to 0 values
#    If there is no count for a specific hour/conc combo, this means
#    there are 0 observations of this
cc <- cc %>%
  mutate(count_no2 = ifelse(is.na(count_no2), 0, count_no2))

# 1e Create function to add commas to Count of Hours legend
comma_format <- function(x){
  scales::comma(x, digits = 0)
}

# 1f Create function to add ':00' to the end of x-axis labels
time_format <- function(x){
  paste0(x, ":00")
}
         
# 1f Create and save plot
tiff(paste0(output_path, 'Plots/', 'fig2_no2_contourplot.tif'),
     units = "in", width = 12, height = 8, res = 300)

cc %>% 
  ggplot(aes(x = hour, y = no2_lag00)) +
  stat_contour_filled(aes(z = count_no2)) +
  scale_fill_distiller(super = metR::ScaleDiscretised, palette = "RdYlBu",
                       breaks = c(0, 400, 800, 1200, 1600, 2000, 2400, 2800),
                       labels = comma_format) +
  labs(fill = 'Count of \n Hours') +
  xlab("Hour of Day") + ylab(expression('NO'[2]*' (ppb)')) +
  scale_x_continuous(limits = c(0, 23),
                     labels = time_format) +
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 60, 10)) +
  theme_bw(base_size = 20) +
  theme(legend.text = element_text(size = 14))

dev.off()


