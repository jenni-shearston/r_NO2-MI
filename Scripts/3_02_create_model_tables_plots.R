# Create Tables and Plots to Show Model Results
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 06/28/2023

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Start function to create estimate table and manuscript plots for model results
# 2: Clean data 
# 3: Create table
# 4: Create plots
# 5: Run function
# 6: Combine NYC Plots
# 7: PM2.5 Sensitivity Analysis Plots
# 8: Create sens analysis plots w overlay of main model results


####**************
#### N: Notes #### 
####**************

# In this script, plots that show the exposure-response relationship across lags
# and NO2 concentrations are created for the manuscript and supplementary
# material. 


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

# 0d Load model estimates
# 0d.i Main model estimates
ind_main <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_Main_ERlin_LR4dfevenknots.csv'))
cumul_main <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_Main_ERlin_LR4dfevenknots.csv'))
# 0d.ii Secondary model estimates (NYC alone)
ind_main_nyc_city <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_NYC_ERlin_LR4dfevenknots.csv'))
cumul_main_nyc_city <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_NYC_ERlin_LR4dfevenknots.csv'))
# 0d.iii Sensitivity: no rh model estimates
ind_sens_NoRH <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_NoRH_ERlin_LR4dfevenknots.csv'))
cumul_sens_NoRH <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_NoRH_ERlin_LR4dfevenknots.csv'))
# 0d.iv Sensitivity: zip codes
ind_sens_zip <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_Zips_ERlin_LR4dfevenknots.csv'))
cumul_sens_zip <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_Zips_ERlin_LR4dfevenknots.csv'))
# 0d.v Sensitivity: MI in the first diagnostic position only
ind_sens_DXA410X1 <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountDXA410X1_MI_count_DXA410X1_ERlin_LR4dfevenknots.csv'))
cumul_sens_DXA410X1 <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountDXA410X1_MI_count_DXA410X1_ERlin_LR4dfevenknots.csv'))
# 0d.vi Sensitivity: 48 hourly lags
ind_sens_48Lags <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_48Lags_ERlin_LR4dfevenknots.csv'))
cumul_sens_48Lags <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_48Lags_ERlin_LR4dfevenknots.csv'))
# 0d.vii Secondary model estimates (all but NYC)
ind_main_not_nyc <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_notNYC_ERlin_LR4dfevenknots.csv'))
cumul_main_not_nyc <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_notNYC_ERlin_LR4dfevenknots.csv'))
# 0d.viii Sensitivity: 2014-2015 NYC 
ind_sens_1415NoPM <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_1415NoPM_ERlin_LR4dfevenknots.csv'))
cumul_sens_1415NoPM <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_1415NoPM_ERlin_LR4dfevenknots.csv'))
# 0d.ix Sensitivity: 2014-2015 NYC adjusted for non-traffic PM2.5
ind_sens_1415PM <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_1415PM_ERlin_LR4dfevenknots.csv'))
cumul_sens_1415PM <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_1415PM_ERlin_LR4dfevenknots.csv'))
# 0d.x Sensitivity: 2014-2015 NYC adjusted for full PM2.5
ind_sens_1415PMfull <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_1415PMfull_ERlin_LR4dfevenknots.csv'))
cumul_sens_1415PMfull <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_1415PMfull_ERlin_LR4dfevenknots.csv'))
# 0d.xi Sensitivity: NYC adjusted for ozone
ind_sens_ozone <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_NYCozone_ERlin_LR4dfevenknots.csv'))
cumul_sens_ozone <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_NYCozone_ERlin_LR4dfevenknots.csv'))
# 0d.xii Sensitivity: Remove Cheektowaga, Corning, Rochester
ind_sens_RemoveSomeCities <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_RemoveSomeCities_ERlin_LR4dfevenknots.csv'))
cumul_sens_RemoveSomeCities <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_RemoveSomeCities_ERlin_LR4dfevenknots.csv'))
# 0d.xiii Secondary: Rush hour
ind_sec_RushHour <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_RushHour_ERlin_LR4dfevenknots.csv'))
cumul_sens_RushHour <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_RushHour_ERlin_LR4dfevenknots.csv'))
# 0d.xiv Secondary: Not rush hour
ind_sec_NotRushHour <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstInd_HourlyNO2_MIcountPrim_NotRushHour_ERlin_LR4dfevenknots.csv'))
cumul_sens_NotRushHour <- read_csv(
  paste0(output_path, 'Estimates/',
         'EstCumul_HourlyNO2_MIcountPrim_NotRushHour_ERlin_LR4dfevenknots.csv'))


####*********************************************************************************
#### 1: Function to create estimate table and manuscript plots for model results #### 
####*********************************************************************************

####****************************************************************************** BEGIN FUNCTION

####*******************
#### 2: Clean data #### 
####*******************

#2a Initialize function
table_plot <- function(dataset, CumulInd, Analysis, FigNum){
  #dataset <- city_est_ind; CumulInd = "Ind"
  #Analysis = 'Main'; FigNum = '2'
  
  # 2b Add Label variable for linear models
  if(!'Label' %in% dataset){
    dataset <- dataset %>% mutate(Label = 'Linear')
  }
  
  if(str_detect(Analysis, '48Lags', negate = TRUE)){
  # 2c Convert from wide to long format
  dataset <- dataset %>% 
    dplyr::select(-...1) %>% 
    dplyr::select(Label, CounterfactualNO2, everything()) %>% 
    pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
  }

  if(str_detect(Analysis, '48Lags')){
  # 2c Convert from wide long format 
  dataset <- dataset %>% 
    dplyr::select(-...1) %>% 
    dplyr::select(Label, CounterfactualNO2, everything()) %>% 
    pivot_longer(fit.or.lag0:uci.or.lag47, names_to = "lag", values_to = "estimate")
  }
  
  # 2d Separate lag variable into lag and estimate type, then convert back to wide
  dataset <- dataset %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate)


####*********************
#### 3: Create table #### 
####*********************

  # 3a Table of effect estimates
  est_table <- dataset %>% 
    filter(Label != "ERValues") %>% 
    mutate(sig = case_when(
      lci < 1 & uci < 1 ~ "sig",
      lci > 1 & uci > 1 ~ "sig",
      lci < 1 & uci > 1 ~ "not sig"
    ))

  # 3b rename table according to cumul/ind
  if(str_detect(CumulInd, 'Cumul')){
    .GlobalEnv$est_table_cumul <- est_table}
  if(str_detect(CumulInd, 'Ind')){
    .GlobalEnv$est_table_ind <- est_table}
  rm(est_table)
  

####*********************
#### 4: Create plots #### 
####*********************

  if(str_detect(CumulInd, 'Cumul')){
    
    # 4a Plot of exposure response relationship, across lags (figure 4)
    expRespLags_cumul <- dataset %>% 
      #filter(Label == "per95") %>% 
      filter(CounterfactualNO2 == 10) %>% 
      ggplot(aes(x = lag, y = fit)) +
      geom_line(alpha = .9, color = "#800000FF", size = 1) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") +         
      geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
      ylab('MI Rate Ratio') + xlab('Hourly Lag') +
      scale_y_continuous(breaks = seq(0.995, 1.025, by = 0.005)) +
      theme_bw() +
      theme(text = element_text(size = 16))

    tiff(paste0(output_path, 'Plots/', 'fig', FigNum, '_expRespLags_cumul_',
                Analysis, '.tif'),
         units = "in", width = 8, height = 6, res = 300)
    print(expRespLags_cumul)
    dev.off()
    
  }
  
  if(str_detect(CumulInd, 'Ind')){
    
    FigNum1 = str_split(FigNum, '_')[[1]][1]
    FigNum2 = str_split(FigNum, '_')[[1]][2]
    
    # 4b Plot of exposure response relationship, across lags (figure 2)
    expRespLags_ind <- dataset %>% 
      #filter(Label == "per95") %>% 
      filter(CounterfactualNO2 == 10) %>% 
      ggplot(aes(x = lag, y = fit)) +
      geom_line(alpha = .9, color = "#800000FF", size = 1) +
      geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") + 
      geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
      ylab('MI Rate Ratio') + xlab('Hourly Lag') +
      scale_y_continuous(limits = c(0.995, 1.008),
                         breaks = seq(1, 1.02, by = 0.005)) +
      theme_bw() +
      theme(text = element_text(size = 16))
    
    tiff(paste0(output_path, 'Plots/', 'fig', FigNum1, '_expRespLags_ind_',
                Analysis, '.tif'),
         units = "in", width = 8, height = 6, res = 300)
    print(expRespLags_ind)
    dev.off()
    
    # 4c Plot of exposure response relationship, across NO2 (figure 3)
    expRespNO2_ind <- dataset %>% 
      #filter(Label == "ERValues" & (lag == 3 | lag == 6 | lag == 9)) %>% 
      filter(lag == 0 | lag == 3 | lag == 6) %>% 
      filter(CounterfactualNO2 < 75) %>% 
      mutate(Lag = factor(lag)) %>% 
      ggplot(aes(x = CounterfactualNO2, y = fit)) +
      geom_line(aes(color = Lag), alpha = .9, size = 1) +
      geom_ribbon(aes(ymin = lci, ymax = uci, fill = Lag), alpha = .1) +
      scale_fill_manual(values = wes_palette(n = 3, name = 'BottleRocket2')) +
      scale_color_manual(values = wes_palette(n = 3, name = 'BottleRocket2')) +
      geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
      labs(y = 'MI Rate Ratio', x = expression(NO[2]~Concentration~(ppb))) +
      scale_y_continuous(breaks = seq(0.985, 1.025, by = 0.005)) +
      theme_bw()+
      theme(text = element_text(size = 16))
    
    tiff(paste0(output_path, 'Plots/', 'fig', FigNum2, '_expRespNO2_ind_',
                Analysis, '.tif'),
         units = "in", width = 9, height = 6, res = 300)
    print(expRespNO2_ind)
    dev.off()
    
  }

  
}


####****************************************************************************** END FUNCTION


####*********************
#### 5: Run function #### 
####*********************

# 5a Create estimate table and manuscript plots for main analysis
table_plot(cumul_main, "Cumul", "Main", '5')
table_plot(ind_main, "Ind", "Main", '3_4')

## The following table/plots in this section were run to create all three
## plot options, but we ended up using different plot selections for the 
## manuscript, done in later sections of this script.

# 5b Create estimate table and manuscript plots for secondary analysis (NYC alone)
table_plot(cumul_main_nyc_city, "Cumul", "NYC", 'S4.2')
table_plot(ind_main_nyc_city, "Ind", "NYC", 'S4.1_X')

# 5c Sensitivity analysis: no rh
table_plot(cumul_sens_NoRH, "Cumul", "NoRH", 'S5.3')
table_plot(ind_sens_NoRH, "Ind", "NoRH", 'S5.1_S5.2')

# 5d Sensitivity analysis: zip codes
table_plot(cumul_sens_zip, "Cumul", "Zips", 'S6.3')
table_plot(ind_sens_zip, "Ind", "Zips", 'S6.1_S6.2')

# 5e Sensitivity analysis:  MI in the first diagnostic position only
table_plot(cumul_sens_DXA410X1, "Cumul", "DXA410X1", 'S7.3')
table_plot(ind_sens_DXA410X1, "Ind", "DXA410X1", 'S7.1_S7.2')

# 5f Sensitivity analysis: 48 Lags
table_plot(cumul_sens_48Lags, "Cumul", "48Lags", 'S8.3')
table_plot(ind_sens_48Lags, "Ind", "48Lags", 'S8.1_S8.2')

# 5g Create estimate table and manuscript plots for secondary analysis (all but NYC)
table_plot(cumul_main_not_nyc, "Cumul", "notNYC", 'X')
table_plot(ind_main_not_nyc, "Ind", "notNYC", 'X_X')

# 5h Sensitivity analysis: 2014-2015 NYC not adjusted for PM2.5
table_plot(cumul_sens_1415NoPM, "Cumul", "1415NoPM", 'S9.3')
table_plot(ind_sens_1415NoPM, "Ind", "1415NoPM", 'S9.1_S9.2')

# 5i Sensitivity analysis: 2014-2015 NYC adjusted for non-traffic PM2.5
table_plot(cumul_sens_1415PM, "Cumul", "1415PM", 'S9.3')
table_plot(ind_sens_1415PM, "Ind", "1415PM", 'S9.1_S9.2')


####**************************
#### 6: Combine NYC plots #### 
####**************************

# 6a Cumulative: Plot of exposure response relationship, across lags, for NYC
# 6a.i Add 'Linear' as label value & convert from long to wide
nyc_expRespLags_cumul <- cumul_main_nyc_city %>% 
  mutate(Label = 'Linear') %>% 
  dplyr::select(-...1) %>% 
  dplyr::select(Label, CounterfactualNO2, everything()) %>% 
  pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
# 6a.ii Separate lag variable into lag and estimate type, then convert back to wide
nyc_expRespLags_cumul <- nyc_expRespLags_cumul %>% 
  mutate(est_type = str_sub(lag, start = 1, end = 3),
         lag = str_replace(lag, "fit.or.lag", ""),
         lag = str_replace(lag, "lci.or.lag", ""),
         lag = str_replace(lag, "uci.or.lag", ""),
         lag = as.numeric(lag)) %>% 
  pivot_wider(names_from = est_type, values_from = estimate)
# 6a.iii Create plot
nyc_expRespLags_cumul <- nyc_expRespLags_cumul %>% 
  filter(CounterfactualNO2 == 10) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_line(alpha = .9, color = "#800000FF", size = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") +         
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
  ylab('MI Rate Ratio') + xlab('Hourly Lag') +
  scale_y_continuous(breaks = seq(0.995, 1.025, by = 0.005)) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  ggtitle('B')
nyc_expRespLags_cumul

# 6b Individual lags: Plot of exposure response relationship, across lags, for NYC
# 6b.i Add 'Linear' as label value & convert from long to wide
nyc_expRespLags_ind <- ind_main_nyc_city %>% 
  mutate(Label = 'Linear') %>% 
  dplyr::select(-...1) %>% 
  dplyr::select(Label, CounterfactualNO2, everything()) %>% 
  pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
# 6b.ii Separate lag variable into lag and estimate type, then convert back to wide
nyc_expRespLags_ind <- nyc_expRespLags_ind %>% 
  mutate(est_type = str_sub(lag, start = 1, end = 3),
         lag = str_replace(lag, "fit.or.lag", ""),
         lag = str_replace(lag, "lci.or.lag", ""),
         lag = str_replace(lag, "uci.or.lag", ""),
         lag = as.numeric(lag)) %>% 
  pivot_wider(names_from = est_type, values_from = estimate)
# 6b.iii Create plot
nyc_expRespLags_ind <- nyc_expRespLags_ind %>% 
  filter(CounterfactualNO2 == 10) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_line(alpha = .9, color = "#800000FF", size = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") + 
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
  ylab('MI Rate Ratio') + xlab('Hourly Lag') +
  scale_y_continuous(limits = c(0.995, 1.008),
                     breaks = seq(1, 1.02, by = 0.005)) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  ggtitle('A')
nyc_expRespLags_ind

# 6c Combine plots
nyc_plot <- cowplot::plot_grid(nyc_expRespLags_ind, nyc_expRespLags_cumul)
nyc_plot

# 6d Save combined plot
tiff(paste0(output_path, 'Plots/figS4_expRespLags_ind+cumul_NYC.tif'),
     units = "in", width = 12, height = 6, res = 300)
print(nyc_plot)
dev.off()


####*****************************************
#### 7: PM2.5 Sensitivity Analysis Plots #### 
####*****************************************

# Notes: Need to re-run these plots separately because they require
#        a different y axis as the confidence intervals are wider than
#        those from other models

# 7a Individual lags: Plot of exposure response relationship, across lags,
#                     for NYC 2014-2015, no PM adjustment
# 7a.i Add 'Linear' as label value & convert from long to wide
nyc1415NoPM_expRespLags_ind <- ind_sens_1415NoPM %>% 
  mutate(Label = 'Linear') %>% 
  dplyr::select(-...1) %>% 
  dplyr::select(Label, CounterfactualNO2, everything()) %>% 
  pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
# 7a.ii Separate lag variable into lag and estimate type, then convert back to wide
nyc1415NoPM_expRespLags_ind <- nyc1415NoPM_expRespLags_ind %>% 
  mutate(est_type = str_sub(lag, start = 1, end = 3),
         lag = str_replace(lag, "fit.or.lag", ""),
         lag = str_replace(lag, "lci.or.lag", ""),
         lag = str_replace(lag, "uci.or.lag", ""),
         lag = as.numeric(lag)) %>% 
  pivot_wider(names_from = est_type, values_from = estimate)
# 7a.iii Create plot
nyc1415NoPM_expRespLags_ind <- nyc1415NoPM_expRespLags_ind %>% 
  filter(CounterfactualNO2 == 10) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_line(alpha = .9, color = "#800000FF", size = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") + 
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
  ylab('MI Rate Ratio') + xlab('Hourly Lag') +
  scale_y_continuous(limits = c(0.990, 1.016),
                     breaks = seq(0.990, 1.02, by = 0.005)) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  labs(title = expression(A:~Not~PM[2.5]~Adjusted))
nyc1415NoPM_expRespLags_ind

# 7b Individual lags: Plot of exposure response relationship, across lags,
#                     for NYC 2014-2015, with PM adjustment (non-traffic PM)
# 7b.i Add 'Linear' as label value & convert from long to wide
nyc1415PM_expRespLags_ind <- ind_sens_1415PM %>% 
  mutate(Label = 'Linear') %>% 
  dplyr::select(-...1) %>% 
  dplyr::select(Label, CounterfactualNO2, everything()) %>% 
  pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
# 7b.ii Separate lag variable into lag and estimate type, then convert back to wide
nyc1415PM_expRespLags_ind <- nyc1415PM_expRespLags_ind %>% 
  mutate(est_type = str_sub(lag, start = 1, end = 3),
         lag = str_replace(lag, "fit.or.lag", ""),
         lag = str_replace(lag, "lci.or.lag", ""),
         lag = str_replace(lag, "uci.or.lag", ""),
         lag = as.numeric(lag)) %>% 
  pivot_wider(names_from = est_type, values_from = estimate)
# 7b.iii Create plot
nyc1415PM_expRespLags_ind <- nyc1415PM_expRespLags_ind %>% 
  filter(CounterfactualNO2 == 10) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_line(alpha = .9, color = "#800000FF", size = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") + 
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
  ylab('MI Rate Ratio') + xlab('Hourly Lag') +
  scale_y_continuous(limits = c(0.990, 1.016),
                     breaks = seq(0.990, 1.02, by = 0.005)) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  labs(title = expression(B:~Non-traffic~PM[2.5]~Adjusted))
nyc1415PM_expRespLags_ind

# 7c Combine plots for NYC 2014-2015 and non-traffic PM
nyc1415_plot <- cowplot::plot_grid(nyc1415NoPM_expRespLags_ind, 
                                   nyc1415PM_expRespLags_ind)
nyc1415_plot

# 7d Save combined plot for NYC 2014-2015 and non-traffic PM
tiff(paste0(output_path, 'Plots/figS9_expRespLags_ind_NYC1415PM+NoPM.tif'),
     units = "in", width = 12, height = 6, res = 300)
print(nyc1415_plot)
dev.off()

# 7e Individual lags: Plot of exposure response relationship, across lags,
#                     for NYC 2014-2015, with PM adjustment (total-PM)
# 7b.i Add 'Linear' as label value & convert from long to wide
nyc1415PMfull_expRespLags_ind <- ind_sens_1415PMfull %>% 
  mutate(Label = 'Linear') %>% 
  dplyr::select(-...1) %>% 
  dplyr::select(Label, CounterfactualNO2, everything()) %>% 
  pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
# 7b.ii Separate lag variable into lag and estimate type, then convert back to wide
nyc1415PMfull_expRespLags_ind <- nyc1415PMfull_expRespLags_ind %>% 
  mutate(est_type = str_sub(lag, start = 1, end = 3),
         lag = str_replace(lag, "fit.or.lag", ""),
         lag = str_replace(lag, "lci.or.lag", ""),
         lag = str_replace(lag, "uci.or.lag", ""),
         lag = as.numeric(lag)) %>% 
  pivot_wider(names_from = est_type, values_from = estimate)
# 7b.iii Create plot
nyc1415PMfull_expRespLags_ind <- nyc1415PMfull_expRespLags_ind %>% 
  filter(CounterfactualNO2 == 10) %>% 
  ggplot(aes(x = lag, y = fit)) +
  geom_line(alpha = .9, color = "#800000FF", size = 1) +
  geom_ribbon(aes(ymin = lci, ymax = uci), alpha = .5, fill = "gray75") + 
  geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
  ylab('MI Rate Ratio') + xlab('Hourly Lag') +
  scale_y_continuous(limits = c(0.990, 1.016),
                     breaks = seq(0.990, 1.02, by = 0.005)) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  labs(title = expression(B:~Total~PM[2.5]~Adjusted))
nyc1415PMfull_expRespLags_ind

# 7c Combine plots
nyc1415_plot_fullPM <- cowplot::plot_grid(nyc1415NoPM_expRespLags_ind, 
                                          nyc1415PMfull_expRespLags_ind)
nyc1415_plot_fullPM

# 7d Save combined plot
tiff(paste0(output_path, 'Plots/figS10_expRespLags_ind_NYC1415fullPM+NoPM.tif'),
     units = "in", width = 12, height = 6, res = 300)
print(nyc1415_plot_fullPM)
dev.off()


####*******************************************************************
#### 8: Create sens analysis plots w overlay of main model results #### 
####*******************************************************************

# 8a Create function
# 8a.i Initialize function
table_plot_sens <- function(sens_dataset, main_dataset, Analysis, FigNum){
  #sens_dataset <- ind_sens_NoRH; main_dataset = ind_main
  #Analysis = 'NoRH'; FigNum = '2'
  
  # 8a.ii Add Label variable for linear models
  if(!'Label' %in% sens_dataset){
    sens_dataset <- sens_dataset %>% mutate(Label = 'Linear')
  }
  if(!'Label' %in% main_dataset){
    main_dataset <- main_dataset %>% mutate(Label = 'Linear')
  }
  
  # 8a.iii Convert from wide to long format
  if(str_detect(Analysis, '48Lags', negate = TRUE)){
    sens_dataset <- sens_dataset %>% 
      dplyr::select(-...1) %>% 
      dplyr::select(Label, CounterfactualNO2, everything()) %>% 
      pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
    main_dataset <- main_dataset %>% 
      dplyr::select(-...1) %>% 
      dplyr::select(Label, CounterfactualNO2, everything()) %>% 
      pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
  }
  if(str_detect(Analysis, '48Lags')){
    sens_dataset <- sens_dataset %>% 
      dplyr::select(-...1) %>% 
      dplyr::select(Label, CounterfactualNO2, everything()) %>% 
      pivot_longer(fit.or.lag0:uci.or.lag47, names_to = "lag", values_to = "estimate")
    main_dataset <- main_dataset %>% 
      dplyr::select(-...1) %>% 
      dplyr::select(Label, CounterfactualNO2, everything()) %>% 
      pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
  }
  
  # 8a.iv Separate lag variable into lag and estimate type, then convert back to wide
  main_dataset <- main_dataset %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate) %>% 
    filter(CounterfactualNO2 == 10)
  sens_dataset <- sens_dataset %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate) %>% 
    filter(CounterfactualNO2 == 10)
    
  # 8a.v Plot of exposure response relationship, across lags, w main
  #      model overlaid
    expRespLagsWOverlay_ind <- 
      ggplot() +
      geom_line(aes(x = sens_dataset$lag, y = sens_dataset$fit),
                alpha = 1, color = "blue3", size = 1) +
      geom_ribbon(aes(x = sens_dataset$lag, y = sens_dataset$fit,
                      ymin = sens_dataset$lci, ymax = sens_dataset$uci),
                  alpha = .3, fill = "lightskyblue1") +
      geom_line(aes(x = main_dataset$lag, y = main_dataset$fit),
                alpha = 1, color = "gray40", size = 1) +
      geom_ribbon(aes(x = main_dataset$lag, y = main_dataset$fit,
                      ymin = main_dataset$lci, ymax = main_dataset$uci),
                  alpha = .3, fill = "gray80") +
      geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
      ylab('MI Rate Ratio') + xlab('Hourly Lag') +
      scale_y_continuous(limits = c(0.995, 1.008),
                         breaks = seq(1, 1.02, by = 0.005)) +
      theme_bw() +
      theme(text = element_text(size = 16))
   
  # 8a.vi Save out plot   
    tiff(paste0(output_path, 'Plots/', 'fig', FigNum, '_expRespLagsWOverlay_ind_',
                Analysis, '.tif'),
         units = "in", width = 8, height = 6, res = 300)
    print(expRespLagsWOverlay_ind)
    dev.off()
}

# 8b Run sensitivity analysis plot w main model overlay: no rh
table_plot_sens(ind_sens_NoRH, ind_main, "NoRH", 'S5.4')

# 8c Run sensitivity analysis plot w main model overlay: ZIP Codes
table_plot_sens(ind_sens_zip, ind_main, "Zips", 'S6.4')

# 8c Run sensitivity analysis plot w main model overlay: MI in first diag pos only
table_plot_sens(ind_sens_DXA410X1, ind_main, "DXA410X1", 'S7.4')

# 8c Run sensitivity analysis plot w main model overlay: 48 Lags
table_plot_sens(ind_sens_48Lags, ind_main, "48Lags", 'S8.4')

# 8d Run sensitivity analysis plot w main model overlay: removing Cheektowaga,
#    Corning, Rochester
table_plot_sens(ind_sens_RemoveSomeCities, ind_main, "RemoveSomeCities", 'S11.4')

# 8e Run sensitivity analysis plot w NYC model overlay: adjusting for ozone
table_plot_sens(ind_sens_ozone, ind_main_nyc_city, "NYCozone", 'S12.4')


####***************************************************************
#### 9: Create rush hour plots w overlay of main model results #### 
####***************************************************************

# 9a Create function
# 9a.i Initialize function
table_plot_sec <- function(secondary_dataset1, secondary_dataset2, main_dataset, Analysis, FigNum){
  
  # 9a.ii Add Label variable for linear models
  if(!'Label' %in% secondary_dataset1){
    secondary_dataset1 <- secondary_dataset1 %>% mutate(Label = 'Linear')
  }
  if(!'Label' %in% secondary_dataset2){
    secondary_dataset2 <- secondary_dataset2 %>% mutate(Label = 'Linear')
  }
  if(!'Label' %in% main_dataset){
    main_dataset <- main_dataset %>% mutate(Label = 'Linear')
  }
  
  # 9a.iii Convert from wide to long format
  secondary_dataset1 <- secondary_dataset1 %>% 
    dplyr::select(-...1) %>% 
    dplyr::select(Label, CounterfactualNO2, everything()) %>% 
    pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
  secondary_dataset2 <- secondary_dataset2 %>% 
    dplyr::select(-...1) %>% 
    dplyr::select(Label, CounterfactualNO2, everything()) %>% 
    pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
  main_dataset <- main_dataset %>% 
    dplyr::select(-...1) %>% 
    dplyr::select(Label, CounterfactualNO2, everything()) %>% 
    pivot_longer(fit.or.lag0:uci.or.lag23, names_to = "lag", values_to = "estimate")
  
  # 9a.iv Separate lag variable into lag and estimate type, then convert back to wide
  main_dataset <- main_dataset %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate) %>% 
    filter(CounterfactualNO2 == 10)
  secondary_dataset1 <- secondary_dataset1 %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate) %>% 
    filter(CounterfactualNO2 == 10)
  secondary_dataset2 <- secondary_dataset2 %>% 
    mutate(est_type = str_sub(lag, start = 1, end = 3),
           lag = str_replace(lag, "fit.or.lag", ""),
           lag = str_replace(lag, "lci.or.lag", ""),
           lag = str_replace(lag, "uci.or.lag", ""),
           lag = as.numeric(lag)) %>% 
    pivot_wider(names_from = est_type, values_from = estimate) %>% 
    filter(CounterfactualNO2 == 10)
  
  # 9a.v Plot of exposure response relationship, across lags, w main
  #      model overlaid
  expRespLagsWOverlay_ind_rushHourStrat <- 
    ggplot() +
    geom_line(aes(x = secondary_dataset1$lag, y = secondary_dataset1$fit),
              alpha = 1, color = "#D81B60", size = 1) +
    geom_ribbon(aes(x = secondary_dataset1$lag, y = secondary_dataset1$fit,
                    ymin = secondary_dataset1$lci, ymax = secondary_dataset1$uci),
                alpha = .2, fill = "#D81B60") +
    geom_line(aes(x = secondary_dataset2$lag, y = secondary_dataset2$fit),
              alpha = 1, color = "#FFC107", size = 1) +
    geom_ribbon(aes(x = secondary_dataset2$lag, y = secondary_dataset2$fit,
                    ymin = secondary_dataset2$lci, ymax = secondary_dataset2$uci),
                alpha = .2, fill = "#FFC107") +
    # geom_line(aes(x = main_dataset$lag, y = main_dataset$fit),
    #           alpha = 1, color = "gray40", size = 1) +
    # geom_ribbon(aes(x = main_dataset$lag, y = main_dataset$fit,
    #                 ymin = main_dataset$lci, ymax = main_dataset$uci),
    #             alpha = .2, fill = "gray40") +
    geom_hline(yintercept = 1, color = "black", linetype = "dashed") + 
    ylab('MI Rate Ratio') + xlab('Hourly Lag') +
    scale_y_continuous(limits = c(0.995, 1.009),
                       breaks = seq(1, 1.02, by = 0.005)) +
    theme_bw() +
    theme(text = element_text(size = 16))
  
  # 9a.vi Save out plot   
  tiff(paste0(output_path, 'Plots/', 'fig', FigNum, '_expRespLags_ind_',
              Analysis, '.tif'),
       units = "in", width = 8, height = 6, res = 300)
  print(expRespLagsWOverlay_ind_rushHourStrat)
  dev.off()
}

# 9b Run secondary analysis plot w main model overlay: rush hour stratification
table_plot_sec(ind_sec_NotRushHour, ind_sec_RushHour, ind_main, "RushHourStrat", '6')


