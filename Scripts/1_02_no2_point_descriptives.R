# NO2 Descriptives
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 07/11/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Look over time
# 2: Central tendency
# 3: Correlations

####**************
#### N: Notes ####
####**************

# Na Description
# We use this script to conduct descriptive analyses 
# for the NO2 data

####********************
#### 0: Preparation #### 
####********************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'Scripts/', 'packages.R'))

# 0c Load Data
no2_path <- '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data/Intermediate_Data/'
no2 <- fread(paste0(no2_path, 'no2.csv'))

# 0d Get a Feel for the Data
glimpse(no2)
names(no2)
summary(no2)

# 0e Review Some Variables
# If we can drop vars that are the same for all obs
# this will decrease dataset size and increase speed
table(no2$poc, useNA = c("always")) # 0 NA, 1 is most common
table(no2$datum, useNA = c("always")) # 15360 NAD83, 0 NA, 1301691 WGS84
table(no2$parameter, useNA = c("always")) # 0 NA, 1317051 Nitrogen dioxide (NO2)
table(no2$parameter_code, useNA = c("always")) # 0 NA, 1317051 "42602"
table(no2$units_of_measure, useNA = c("always")) # 0 NA, 1317051 Parts per billion
table(no2$units_of_measure_code, useNA = c("always")) # 0 NA, 1317051 "8"
table(no2$sample_duration, useNA = c("always")) # 0 NA, 1317051 1 HOUR
table(no2$sample_duration_code, useNA = c("always")) # 0 NA, 1317051 "1"
table(no2$sample_frequency, useNA = c("always")) # 0 NA, 1317051 HOURLY
table(no2$detection_limit, useNA = c("always")) # 0 NA, varied limits from 0.05 to 5
table(no2$uncertainty, useNA = c("always")) # all NA
table(no2$qualifier, useNA = c("always")) # 0 NA, super useful, with details like "high winds"
table(no2$method_type, useNA = c("always")) # 0 NA, 1306517 FRM, 10534 FEM
table(no2$method, useNA = c("always")) # 0 NA, four different method types
table(no2$method_code, useNA = c("always")) # 0 NA, six different method codes
table(no2$cbsa_code, useNA = c("always")) # 0 NA, five CBSAs covered
table(no2$sample_measurement, useNA = c("always")) # 114740 NA values, some negative values
table(no2$county_code, useNA = c("always")) # 0 NA, nine counties total (correct number)
table(no2$county_name, useNA = c("always")) # 0 NA, nine counties total
table(no2$state, useNA = c("always")) # all obs "New York"
table(no2$state_code, useNA = c("always")) # all obs "36"
table(no2$status, useNA = c("always")) # all obs "Success"
table(no2$request_time, useNA = c("always")) # leave as it might be useful is something is wrong w data
table(no2$url, useNA = c("always")) # leave as it might be useful is something is wrong w data

# 0f Drop vars that are not needed
# Notes: units_of_measure, units_of_measure_code, sample_duration, sample_frequency, parameter,
# parameter_code, state, state_code, status, sample_duration_code are the same for all obs; 
# uncertainty is NA for all obs
no2_clean <- no2 %>% 
  dplyr::select(-units_of_measure, -sample_duration, -sample_frequency, -parameter,
                -parameter_code, -state, -state_code, -status, -uncertainty, 
                -units_of_measure_code, -sample_duration_code)
rm(no2)

# 0g Create datetime gmt and est variables
no2_clean <- no2_clean %>% 
  mutate(datetime_gmt1 = paste0(date_gmt, " ", time_gmt),
         datetime_est1 = paste0(date_local, " ", time_local)) %>% 
  mutate(datetime_gmt = lubridate::ymd_hm(datetime_gmt1, tz = 'GMT'),
         datetime_est = lubridate::ymd_hm(datetime_est1, tz = 'EST')) %>% 
  dplyr::select(-datetime_gmt1, -datetime_est1)

# 0h Time series plot with all data
no2_clean %>% 
  ggplot(aes(x = datetime_gmt, y = sample_measurement)) +
  geom_line(color = 'darkgray') + 
  geom_smooth()

# Notes: Things are running slow. I'm going to save out individual files for each 
# monitor, and then am going to use the "chunk and pull" strategy from 
# https://rviews.rstudio.com/2019/07/17/3-big-data-strategies-for-r/ to run 
# descriptives for each monitor separately.

# 0i Save separate csv files for each monitor
monitor_list <- unique(no2_clean$monitor_id) # vector of monitor ids
m = 0                                        # counting variable to increase index
for (i in monitor_list){
  df <- no2_clean %>% filter(monitor_id == monitor_list[1+m])
  fwrite(df, paste0(no2_path, monitor_list[1+m], '.csv'))
  m = m+1
}

# Oj Save unique monitor list
write(monitor_list, paste0(no2_path, 'monitor_list.csv'))

# 0k Remove full dataset and clean environment
rm(no2_clean, df)


####***********************
#### 1: Look over time #### 
####***********************

# 1a Monitor-specific time series plots
for (i in 1:length(monitor_list)){
  #i = 1
  df <- fread(paste0(no2_path, monitor_list[i], '.csv'))
  plot.title = monitor_list[i]
  p = df %>% 
  ggplot(aes(x = datetime_gmt, y = sample_measurement)) +
  geom_line() +
  ggtitle(bquote(atop(.(plot.title))))
  print(p)
  rm(df,p,plot.title)
}

# 1b Linear regressions over time
# Interpretation: for an additional datetime hour (hour in time) NO2 levels 
# increased/decreased by estimate amount
lin_reg <- tibble(monitor = monitor_list,
                  reg_output = list(NA))
for (i in 1:length(monitor_list)){
  #i=1
  df <- fread(paste0(no2_path, monitor_list[i], '.csv'))
  lin_reg$reg_output[[i]] = broom::tidy(lm(sample_measurement ~ datetime_gmt, data = df))
  rm(df)
}
lin_reg <- lin_reg %>% unnest(cols = c(reg_output))

# 1c Plot to show regression coefficients
lin_reg %>% 
  filter(term == "datetime_gmt") %>% 
  mutate(monitor = fct_reorder(monitor, estimate)) %>% 
  ggplot(aes(x = monitor, y = estimate)) +
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


####*****************************
#### 2: Central Tendency #### 
####*****************************

# 2a Central Tendency of NO2 levels for each monitor, over full time period
cent_tend <- tibble(monitor = monitor_list, mean = NA, sd = NA, median = NA,
                    iqr = NA, b_year = NA, e_year = NA, monitor_name = NA)
for (i in 1:length(monitor_list)){
  #i=1
  df <- fread(paste0(no2_path, monitor_list[i], '.csv'))
  cent_tend$mean[[i]] = mean(df$sample_measurement, na.rm = T)
  cent_tend$sd[[i]] = sd(df$sample_measurement, na.rm = T)
  cent_tend$median[[i]] = median(df$sample_measurement, na.rm = T)
  cent_tend$iqr[[i]] = IQR(df$sample_measurement, na.rm = T)
  cent_tend$b_year[[i]] = unique(df$b_year)
  cent_tend$e_year[[i]] = unique(df$e_year)
  cent_tend$monitor_name[[i]] = unique(df$monitor_name)
  rm(df)
}

# 2b Central Tendency of NO2 levels for each monitor, by year
cent_tend_yr <- tibble(monitor_id = character(), monitor_name = character(), 
                       year = numeric(), mean = numeric(), sd = numeric(),
                       median = numeric(), iqr = numeric())
for (i in 1:length(monitor_list)){
  #i=1
  df <- fread(paste0(no2_path, monitor_list[i], '.csv'))
  df <- df %>% 
    rename(year = pull_year) %>% 
    group_by(monitor_id, monitor_name, year) %>% 
    summarise(mean = mean(sample_measurement, na.rm = T),
              sd = sd(sample_measurement, na.rm = T),
              median = median(sample_measurement, na.rm = T),
              iqr = IQR(sample_measurement, na.rm = T))
  cent_tend_yr <- cent_tend_yr %>% bind_rows(df)
  rm(df)
}


####*********************
#### 3: Correlations #### 
####*********************

# 3a Correlations between monitors (yearly averages)
# 3a.i Make data wide and calculate correlations
monitor_yr_corrs <- cent_tend_yr %>% 
  dplyr::select(monitor_id, year, mean) %>% 
  pivot_wider(names_from = monitor_id, values_from = mean) %>% 
  dplyr::select(-year) %>% 
  cor(use = "pairwise.complete.obs")
# 3a.ii Create plot
corrplot::corrplot(monitor_yr_corrs, type = "upper",
                   tl.col = "black", tl.srt = 45)

# 3b Correlations between monitors (all years, no averages)
# 3b.i Set up df with all possible datetimes
#      Note: the sequence starts at 05:00 because GMT is +5 hours from NY local
#            time and the API pull uses local time
monitor_corrs_df <- tibble(datetime_gmt = 
                           seq(lubridate::ymd_hm('2000-01-01 05:00', tz = 'GMT'), 
                               lubridate::ymd_hm('2019-01-01 04:00', tz = 'GMT'), 
                               60*60))
# 3b.ii Add NO2 observations as monitor-specific columns  
for (i in 1:length(monitor_list)){
  #i=1
  df <- fread(paste0(no2_path, monitor_list[i], '.csv'))
  df <- df %>% 
    dplyr::select(datetime_gmt, sample_measurement) %>% 
    rename(!!monitor_list[i]:= sample_measurement)
  monitor_corrs_df <- monitor_corrs_df %>% 
    left_join(df, by = 'datetime_gmt')
  rm(df)
}
# 3b.iii Calculate correlations
monitor_corrs <- monitor_corrs_df %>% 
  dplyr::select(-datetime_gmt) %>% 
  cor(use = "pairwise.complete.obs")
# 3b.iv Create plot
corrplot::corrplot(monitor_corrs, type = "upper",
                   tl.col = "black", tl.srt = 45)





