# Conduct DLNM Case-crossover Analyses
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 11/22/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Calculate summary stats for use in DLNM crosspreds
# 2: Create counterfactual exposure vector for use in DLNM crosspreds
# 3: Add case and time-to-event variables for models
# 4: Create function to conduct dlnm case-crossover
# 5: Create cross basis
# 6: Create health model
# 7: Save model and create crosspredictions
# 8: Save model AIC for grid search
# 9: Grid search to determine df for ER and lR
# 10: Run main and sensitivity analyses


####**************
#### N: Notes #### 
####**************

# In this script, we create distributed lag non-linear models using a case-crossover
# study design, for the main analysis, secondary analysis, and all sensitivity 
# analyses. Functions in this script were originally developed by Sebastian Rowland.

# Main analysis: City-level NO2 and MI (MI_count_Prim) using all months of data for
#                which less than 5% of cases and controls were missing NO2 data, for
#                all cities with hourly NO2 data

# Secondary analysis: 
#  1. Main analysis restricted to New York City only
#  2. Main analysis restricted to all cities except NYC                   

# Sensitivity analyses:
#  1. Instead of 24 hourly lags, evaluating 48 hours of lags
#  2. Excluding relative humidity from the model
#  3. Conducting a zip code level analysis rather than city level
#  4. Restricting to MI cases that were in the first diagnostic position (MI_count_DXA410X1) 
#  5. In years 2014-2015 for NYC, adjusting for hourly PM2.5
#  We considered, but chose NOT to do an additional sensitivity analysis where we
#    adjusted for daily PM2.5 in the full dataset, because there was so much missingness 
#    that the results would not be comparable to the main analysis


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

# 0d Load data for case-crossover analyses
# 0d.i Dataset for main analysis, NYC alone analysis, sens analysis excluding
#      rh, sens analysis using MI in first position
data4casecross_city <- read_fst(paste0(data_path_external, 'data4casecross_city_5per.fst'))
# 0d.ii Dataset for sens analysis using zip codes
data4casecross_zip <- read_fst(paste0(data_path_external, 'data4casecross_zip_5per.fst'))
# 0d.iii Dataset for sens analysis using 48 lags
data4casecross_48lags <- read_fst(paste0(data_path_external, 'data4casecross_city_48Lags_5per.fst'))


####************************************************************
#### 1: Calculate summary stats for use in DLNM crosspreds  #### 
####************************************************************

# 1a Create vector of observed hourly NO2 concentrations 
alldata <- data4casecross_city %>%
  dplyr::select(contains('no2_lag'))
alldata <- as.matrix(alldata)[,1:24]
obsHourlyNO2 <- as.vector(alldata)

# 1b Calculate summary statistics
HourlyNO2.mean <- mean(obsHourlyNO2, na.rm = TRUE)
HourlyNO2.sd <- sd(obsHourlyNO2, na.rm = TRUE)
HourlyNO2.min <- min(obsHourlyNO2, na.rm = TRUE)
HourlyNO2.max <- max(obsHourlyNO2, na.rm = TRUE)
HourlyNO2.per05 <- quantile(obsHourlyNO2, 0.05, type = 1, na.rm = TRUE)
HourlyNO2.per95 <- quantile(obsHourlyNO2, 0.95, type = 1, na.rm = TRUE)


####**************************************************************************
#### 2: Create counterfactual exposure vector for use in DLNM crosspreds  #### 
####**************************************************************************

# 2a Create counterfactual exposure vector
# Notes: We use this exposure vector for creating the estimates from the dlnm model. 
#        These are the counterfactual exposure levels, and we generate effect estimates
#        for a contrast between the reference level (the mean) and these levels. 
#        We likely won't need all of these, but making a big exposure contrast table 
#        makes it much easier to deal with any changes to analysis, e.g., reporting a different effect estimate
expContrasts <- data.frame(  
  CounterfactualNO2 = c(seq(HourlyNO2.min, HourlyNO2.max, length.out = 100), 
                         quantile(obsHourlyNO2, 0.01, type = 1, na.rm = T), quantile(obsHourlyNO2, 0.99, type = 1, na.rm = T), 
                         quantile(obsHourlyNO2, 0.05, type = 1, na.rm = T), quantile(obsHourlyNO2, 0.95, type = 1, na.rm = T), 
                         quantile(obsHourlyNO2, 0.10, type = 1, na.rm = T), quantile(obsHourlyNO2, 0.90, type = 1, na.rm = T),  
                         quantile(obsHourlyNO2, 0.15, type = 1, na.rm = T), quantile(obsHourlyNO2, 0.85, type = 1, na.rm = T),
                         quantile(obsHourlyNO2, 0.20, type = 1, na.rm = T), quantile(obsHourlyNO2, 0.80, type = 1, na.rm = T), 
                         quantile(obsHourlyNO2, 0.25, type = 1, na.rm = T), quantile(obsHourlyNO2, 0.75, type = 1, na.rm = T), 
                        HourlyNO2.mean - HourlyNO2.sd,  HourlyNO2.mean + HourlyNO2.sd, 
                        HourlyNO2.mean - 10,  HourlyNO2.mean + 10),
  Label = c(rep('ERValues', 100), 'per01','per99', 'per05', 'per95', 'per10', 'per90',
            'per15', 'per85', 'per20', 'per80', 'per25', 'per75', 'MeanMinusSD', 'MeanPlusSD', 
            'MeanMinus10', 'MeanPlus10')) %>% 
  mutate(CounterfactualNO2 = round(CounterfactualNO2, 7))

# 2b Clean up 
rm(obsHourlyNO2, alldata)


####********************************************************
#### 3: Add case and time-to-event variables for models #### 
####********************************************************

# 3a Add needed vars for city analysis
data4casecross_city <- data4casecross_city %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    

# 3b Add needed vars for zip code sens analysis
data4casecross_zip <- data4casecross_zip %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    

# 3c Add needed vars for increased lag sens analysis
data4casecross_48lags <- data4casecross_48lags %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    

# 3d Add needed vars for MI_count_DXA410X1 sens analysis
data4casecross_DXA410X1 <- data4casecross_city %>% 
  filter(MI_count_DXA410X1 == 1 | MI_count_DXA410X1 == 2 | 
           MI_count_DXA410X1 == 3 | MI_count_DXA410X1 == 4 |
           MI_count_DXA410X1 == 5 | MI_count_DXA410X1 == 6 | 
           MI_count_DXA410X1 == 7) %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    

# 3e Add needed vars for NYC only secondary analysis
data4casecross_NYC <- data4casecross_city %>% 
  filter(city == 'New York') %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    

# 3f Add needed vars for all cities except NYC secondary analysis
data4casecross_NotNYC <- data4casecross_city %>% 
  filter(!city == 'New York') %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    

# 3g Add needed vars for pm25 sens analysis
data4casecross_1415nyc <- data4casecross_city %>% 
  filter(city == 'New York' & (year == '2014' | year == '2015')) %>% 
  mutate(Case = ifelse(HourName == 'CaseHour_0', 1, 0),         # bivariate Case variable
         TimetoEvent = if_else(HourName == 'CaseHour_0', 1, 2)) # bivariate Case variable for coxph model (if using instead of clogit)    


####****************************************************************************** BEGIN FUNCTION
# Here I edit functions developed by Sebastian Rowland


####*******************************************************
#### 4: Create function to conduct dlnm case-crossover #### 
####*******************************************************

# 4a Name function
analyze_dlnmNO2 <- function(ExpTerm, CaseType, Sensitivity,  
                            ERConstraint, LRConstraint, SaveModel, dta){
 #ExpTerm <- 'HourlyNO2'; CaseType <- 'MIcountPrim';  
 #Sensitivity <- 'Main'; ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots';
 #SaveModel <- 'SaveModel'; dta <- data4casecross_city

# 4b Create ModelName
ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)
ExpConstraints <- paste0('ER', ERConstraint, '_LR', LRConstraint)
ModelName <- paste0(ModelIdentifier,'_', ExpConstraints)

# 4c Determine number of lag hours to include 
if(str_detect(Sensitivity, 'Main') | str_detect(Sensitivity, 'NYC')
   | str_detect(Sensitivity, 'NoRH') | str_detect(Sensitivity, 'Zips')
   | str_detect(Sensitivity, 'MI_count_DXA410X1') | str_detect(Sensitivity, 'notNYC')
   | str_detect(Sensitivity, '1415NoPM') | str_detect(Sensitivity, '1415PM')){NumLag <- 24} 
if(str_detect(Sensitivity, '48Lags')){NumLag <- 48}


####***************************
#### 5: Create cross basis #### 
####***************************


# 5a Set ER (exposure response) and LR (lagged response) constraints
ERdf <- as.numeric(str_remove_all(ERConstraint, '[A-z]')) # Remove all letters, leaving only number of df (degrees of freedom)
LRdf <- as.numeric(str_remove_all(LRConstraint, '[A-z]'))

# 5b Create cross basis for temperature
cb.hrlytemp <- crossbasis(
  as.matrix(dplyr::select(dta, contains('temp_lag')))[,1:NumLag], 
  lag=c(0,(NumLag-1)),
  argvar=list(fun='ns', df = 3),
  arglag=list(fun='ns', df = 4))

# 5c Create cross basis for relative humidity
cb.hrlyrh <- crossbasis(
  as.matrix(dplyr::select(dta, contains('rh_lag')))[,1:NumLag], 
  lag=c(0,(NumLag-1)),
  argvar=list(fun='ns', df = 3),
  arglag=list(fun='ns', df = 4))

# 5d Create cross basis for NO2 (~ 1min)
if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){
  cb.hrlyNO2 <- crossbasis(
    as.matrix(dplyr::select(dta, contains('no2_lag')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='ns', df = ERdf),
    arglag=list(fun='ns', df = LRdf))}

if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){
  cb.hrlyNO2 <- crossbasis(
    as.matrix(dplyr::select(dta, contains('no2_lag')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='lin'),
    arglag=list(fun='ns', df = LRdf))}

# 5e Create cross basis for PM2.5 
if(str_detect(Sensitivity, '1415PM')){
  cb.hrlyPM25 <- crossbasis(
    as.matrix(dplyr::select(dta, contains('pm25resids_lag')))[,1:NumLag], 
    lag=c(0,(NumLag-1)),
    argvar=list(fun='lin'),
    arglag=list(fun='ns', df = LRdf))}


####****************************
#### 6: Create health model #### 
####****************************

# 6a Health model for main analysis, NYC sub analysis, zip code sens analysis,
#    48 hr lag sens analysis, all but NYC sub analysis, and 2014-2015 NYC analysis
#    (conducted to compare with 2014-2015 NYC adjusted for PM analysis) 
if(str_detect(Sensitivity, 'Main') | str_detect(Sensitivity, 'NYC')
   | str_detect(Sensitivity, 'Zips') | str_detect(Sensitivity, '48Lags')
   | str_detect(Sensitivity, 'notNYC') | str_detect(Sensitivity, '1415NoPM')){
  mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 +
                 #ns(temp, df = 3) + 
                 #ns(rh, df = 3) +
                 cb.hrlytemp +
                 cb.hrlyrh +
                 strata(id),              # each case id is a strata
               weights = MI_count_Prim,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
               method = "efron",          # the method tells the model how to deal with ties
               data = dta)}

# 6b Health model for No Rel Hum sens analysis
if(str_detect(Sensitivity, 'NoRH')){
  mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 + 
                 cb.hrlytemp +
                 strata(id),              # each case id is a strata
               weights = MI_count_Prim,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
               method = "efron",          # the method tells the model how to deal with ties
               data = dta)}

# 6c Health model for MI_count_DXA410X1 sens analysis
if(str_detect(Sensitivity, 'MI_count_DXA410X1')){
  mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 + 
                 cb.hrlytemp +
                 cb.hrlyrh +
                 strata(id),                  # each case id is a strata
               weights = MI_count_DXA410X1,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
               method = "efron",              # the method tells the model how to deal with ties
               data = dta)}

# 6d Health model for 2014-2015 NYC PM2.5 sens analysis 
if(str_detect(Sensitivity, '1415PM')){
  mod <- coxph(Surv(TimetoEvent, Case) ~ cb.hrlyNO2 +
                 #ns(temp, df = 3) + 
                 #ns(rh, df = 3) +
                 cb.hrlytemp +
                 cb.hrlyrh +
                 cb.hrlyPM25 +
                 strata(id),              # each case id is a strata
               weights = MI_count_Prim,   # num of events in each hour (could only use if exposure is same for all cases/controls in a given hour)
               method = "efron",          # the method tells the model how to deal with ties
               data = dta)}


####***********************************************
#### 7: Save model and create crosspredictions #### 
####***********************************************

# 7a Begin option  
if(str_detect(SaveModel, 'SaveModel')){
  
  # 7b Save the model 
  mod %>% saveRDS(paste0(output_path, 'Models/', ModelName, '.RDS'))
    
  # 7c Begin option for nonlinear ER and LR 
  if(str_detect(ERConstraint, 'evenknots') & str_detect(LRConstraint, 'evenknots')){ 
    
    # 7d Generate estimates for nonlinear ER and LR   
    # the cen argument sets the reference exposure level for our effect estimates (set to the mean NO2 value)
    # crosspred() will yield estimates for 100 exposure levels plus those exposure levels we set
    est <- crosspred(cb.hrlyNO2,                                   # exposure crossbasis
                     mod,                                          # health model
                     cen = HourlyNO2.mean,                         # center at mean NO2
                     at = expContrasts$CounterfactualNO2,          # compute estimated association for each integer value of NO2 in CounterfactualNO2 vector
                     cumul = TRUE,                                 # also compute cumulative associations
                     bylag = 1)                                    # estimates association along each lag
  
    # 7e Extract coefficient fit and CI 
    fit.table <- as.data.frame(est$matRRfit)  
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%  
      mutate(CounterfactualNO2 = as.numeric(row.names(fit.table)))
  
    lci.table <- as.data.frame(est$matRRlow)  
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
  
    uci.table <- as.data.frame(est$matRRhigh)  
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
  
    # 7f Combine fit and se for individual lags 
    #    Notes: All OR are relative to the mean NO2 
    est.table <- bind_cols(fit.table, lci.table, uci.table)
  
    # 7g Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'CounterfactualNO2')
  
    # 7h Save estimate table for individual lags 
    est.table %>%
      write.csv(paste0(output_path, 'Estimates/', 
                       'EstInd_', ModelName, '.csv'))
  
    # 7i Extract cumulative coefficient fit and ci  
    fit.table <- as.data.frame(est$cumRRfit)  
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%  
      mutate(CounterfactualNO2 = as.numeric(row.names(fit.table)))
  
    lci.table <- as.data.frame(est$cumRRlow)  
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
  
    uci.table <- as.data.frame(est$cumRRhigh)  
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
  
    # 7j Combine fit and se for individual lags 
    #    Notes: All OR are relative to the mean NO2  
    est.table <- bind_cols(fit.table, lci.table, uci.table)
  
    # 7k Attach the labels of the exposure contrasts
    est.table <- est.table %>% full_join(expContrasts, by = 'CounterfactualNO2')
  
    # 7l Save cumulative estimates table 
    est.table %>% 
      write.csv(paste0(output_path, 'Estimates/', 
                       'EstCumul_', ModelName, '.csv'))
  }

  # 7m Begin option for linear ER and nonlinear LR 
  if(str_detect(ERConstraint, 'lin') & str_detect(LRConstraint, 'evenknots')){ 
    
    # 7n Generate estimates for nonlinear ER and LR   
    est <- crosspred(cb.hrlyNO2,                    # exposure crossbasis
                     mod,                           # health model
                     at = 0:HourlyNO2.max,          # compute estimated association for each integer value of NO2
                     cumul = TRUE,                  # also compute cumulative associations
                     bylag = 1)                     # estimates association along each lag
    
    # 7o Extract coefficient fit and CI 
    fit.table <- as.data.frame(est$matRRfit)  
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%  
      mutate(CounterfactualNO2 = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$matRRlow)  
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$matRRhigh)  
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # 7p Combine fit and se for individual lags 
    #    Notes: All OR are relative 0 ppb
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 7q Save estimate table for individual lags 
    est.table %>%
      write.csv(paste0(output_path, 'Estimates/', 
                       'EstInd_', ModelName, '.csv'))
    
    # 7r Extract cumulative coefficient fit and ci  
    fit.table <- as.data.frame(est$cumRRfit)  
    colnames(fit.table) <- paste0('fit.or.', colnames(fit.table))
    fit.table <- fit.table %>%  
      mutate(CounterfactualNO2 = as.numeric(row.names(fit.table)))
    
    lci.table <- as.data.frame(est$cumRRlow)  
    colnames(lci.table) <- paste0('lci.or.', colnames(lci.table))
    
    uci.table <- as.data.frame(est$cumRRhigh)  
    colnames(uci.table) <- paste0('uci.or.', colnames(uci.table))
    
    # 7s Combine fit and se for individual lags 
    #    Notes: All OR are relative to the mean NO2  
    est.table <- bind_cols(fit.table, lci.table, uci.table)
    
    # 7t Save cumulative estimates table 
    est.table %>% 
      write.csv(paste0(output_path, 'Estimates/', 
                       'EstCumul_', ModelName, '.csv'))
  }
  
} 

####***************************************
#### 8: Save model AIC for grid search #### 
####***************************************

if(str_detect(SaveModel, 'StoreAIC')){
  
  # 8a Readin the table of AIC's
  aic.table <- read_csv(paste0(output_path, 'Tables/', 'Model_AIC.csv'), 
                        col_types = 'cccdT')
  
  # 8b Add this aic to the set of AIC's
  aic.table[1+nrow(aic.table),] <- list(ModelIdentifier,
                                        ERConstraint, LRConstraint, AIC(mod), Sys.time())
  
  # 8c Remove any old AICs and then save
  # at the slice step you keep only the earliest AIC for each model-constraint combo
  aic.table %>% 
    group_by(ModelIdentifier,ERConstraint, LRConstraint) %>% 
    arrange(desc(RunDate)) %>% 
    slice(0:1) %>% 
    filter(!is.na(ModelIdentifier)) %>%
    write_csv(paste0(output_path, 'Tables/', 'Model_AIC.csv'))
}

}

####****************************************************************************** END FUNCTION


####**************************************************
#### 9: Grid search to determine df for ER and lR #### 
####**************************************************

# 9a Set dlnm / case-crossover function inputs
ExpTerm <- 'HourlyNO2'
CaseType <- 'MIcountPrim'
Sensitivity <- 'Main' 
SaveModel <- 'StoreAIC'
dta <- data4casecross_city
ModelIdentifier <- paste0(ExpTerm, '_', CaseType, '_', Sensitivity)
  
# 9b Build constraint grid
CandidateConstraintsGrid <- data.frame(
  ERConstraint = rep(c('lin', '3dfevenknots','4dfevenknots', '5dfevenknots'), 3), 
  LRConstraint = c(rep('4dfevenknots',4), rep('5dfevenknots',4), rep('6dfevenknots',4)))

# 9c Initialize loop over grid cells
for(i in 1:nrow(CandidateConstraintsGrid)){
  # i <- 1
  # 2b Fit model with candidate constraints
  analyze_dlnmNO2(ExpTerm, CaseType, Sensitivity, 
                  CandidateConstraintsGrid$ERConstraint[i], CandidateConstraintsGrid$LRConstraint[i], 
                  SaveModel, dta)
}

# 9d Read in table of model AICs
aic.table0 <- read_csv(paste0(output_path, 'Tables/', 'Model_AIC.csv'), 
                       col_types = 'cccdT')

# 9e Filter by model identifier 
aic.table <- aic.table0 %>% filter(ModelIdentifier == !!ModelIdentifier) 

# 9f Find minAIC
AIC.min <- min(aic.table$AIC)

# 9g Identify selected model
SelectedModel <- aic.table %>% 
  filter(AIC == AIC.min) 

# 9h Remove old terms from environment
rm(ExpTerm, CaseType, Sensitivity, SaveModel, dta, ModelIdentifier,
   aic.table, aic.table0, CandidateConstraintsGrid, AIC.min)


####*******************************************
#### 10: Run main and sensitivity analyses #### 
####*******************************************

# after best parameters determined, actually run the health model

# 10a Run main city-level analysis (~ 1min)
#    Notes: Model use -- analyze_dlnmNO2(ExpTerm, CaseType, Sensitivity, ERConstraint, 
#                                LRConstraint, SaveModel, dta)
#           Example arguments -- ExpTerm <- 'HourlyNO2'; CaseType <- 'MIcountPrim'; 
#                                Sensitivity <- 'Main'; 
#                                ERConstraint <- '4dfevenknots'; LRConstraint <- '3dfevenknots';
#                                SaveModel <- 'SaveModel'; dta <- data4casecross_city
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', 'Main', 'lin', '4dfevenknots', 
                'SaveModel', data4casecross_city)

# 10b Run secondary NYC alone analysis
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', 'NYC', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_NYC)

# 10c Run sensitivity analysis: no relative humidity
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', 'NoRH', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_city)

# 10d Run sensitivity analysis: zip codes
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', 'Zips', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_zip)

# 10e Run sensitivity analysis: MI in the first diagnostic position only
analyze_dlnmNO2('HourlyNO2', 'MIcountDXA410X1', 'MI_count_DXA410X1', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_DXA410X1)

# 10f Run sensitivity analysis: 48 lags
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', '48Lags', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_48lags)

# 10g Run secondary all but NYC analysis
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', 'notNYC', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_NotNYC)

# 10h Run sensitivity analysis: 2014-2015 NYC not adjusted for hourly PM2.5
#     Note: this analysis is only run to compare with the model that is 
#           adjusted for hourly PM2.5
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', '1415NoPM', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_1415nyc)

# 10i Run sensitivity analysis: 2014-2015 NYC adjusted for hourly PM2.5
analyze_dlnmNO2('HourlyNO2', 'MIcountPrim', '1415PM', 'lin', '4dfevenknots',
                'SaveModel', data4casecross_1415nyc)




