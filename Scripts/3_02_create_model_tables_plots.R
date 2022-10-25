# Create Tables and Plots to Show Model Results
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 08/12/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Clean data
# 2: Create table
# 3: Create plot(s)
# 4: Run function


####**************
#### N: Notes #### 
####**************

# In this script, plots that show the exposure-response relationship across lags
# and NO2 concentrations are created for the manuscript. 


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
      ylab('Rate Ratio of MI Hospitalization') + xlab('Hourly Lag') +
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
      ylab('Rate Ratio of MI Hospitalization') + xlab('Hourly Lag') +
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
      labs(y = 'Rate Ratio of MI Hospitalization', x = expression(NO[2]~Concentration~(ppb))) +
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
table_plot(cumul_main, "Cumul", "Main", '4')
table_plot(ind_main, "Ind", "Main", '2_3')

# 5b Create estimate table and manuscript plots for secondary analysis (NYC alone)
table_plot(cumul_main_nyc_city, "Cumul", "NYC", '5.2')
table_plot(ind_main_nyc_city, "Ind", "NYC", '5.1_X')

# 5c Sensitivity analysis: no rh
table_plot(cumul_sens_NoRH, "Cumul", "NoRH", 'S2.3')
table_plot(ind_sens_NoRH, "Ind", "NoRH", 'S2.1_S2.2')

# 5d Sensitivity analysis: zip codes
table_plot(cumul_sens_zip, "Cumul", "Zips", 'S3.3')
table_plot(ind_sens_zip, "Ind", "Zips", 'S3.1_S3.2')

# 5e Sensitivity analysis:  MI in the first diagnostic position only
table_plot(cumul_sens_DXA410X1, "Cumul", "DXA410X1", 'S4.3')
table_plot(ind_sens_DXA410X1, "Ind", "DXA410X1", 'S4.1_S4.2')

# 5f Sensitivity analysis: 48 Lags
table_plot(cumul_sens_48Lags, "Cumul", "48Lags", 'S5.3')
table_plot(ind_sens_48Lags, "Ind", "48Lags", 'S5.1_S5.2')


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
  ylab('Rate Ratio of MI Hospitalization') + xlab('Hourly Lag') +
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
  ylab('Rate Ratio of MI Hospitalization') + xlab('Hourly Lag') +
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
tiff(paste0(output_path, 'Plots/fig5_expRespLags_ind+cumul_NYC.tif'),
     units = "in", width = 12, height = 6, res = 300)
print(nyc_plot)
dev.off()







# 4c Sensitivity analysis: no rh
table_plot(cumul_sens_NoRH, "Cumul", "NoRH", 'S2.3')
table_plot(ind_sens_NoRH, "Ind", "NoRH", 'S2.1_S2.2')

# 4d Sensitivity analysis: zip codes
table_plot(cumul_sens_zip, "Cumul", "Zips", 'S3.3')
table_plot(ind_sens_zip, "Ind", "Zips", 'S3.1_S3.2')

# 4e Sensitivity analysis:  MI in the first diagnostic position only
table_plot(cumul_sens_DXA410X1, "Cumul", "DXA410X1", 'S4.3')
table_plot(ind_sens_DXA410X1, "Ind", "DXA410X1", 'S4.1_S4.2')

# 4f Sensitivity analysis: 48 Lags
table_plot(cumul_sens_48Lags, "Cumul", "48Lags", 'S5.3')
table_plot(ind_sens_48Lags, "Ind", "48Lags", 'S5.1_S5.2')
