# Packages to Load
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 09/15/2022

# List of packages to use
list.of.packages = c('broom',
                     'corrplot',
                     'data.table',
                     'doParallel',
                     'dlnm',
                     'egg',
                     'fst',
                     'furrr',
                     'future',
                     'here',
                     'httr',
                     'jsonlite',
                     'lubridate',
                     'magrittr',
                     'nlme',
                     'parallel',
                     'progress',
                     'progressr',
                     'purrr',
                     'rvest',
                     'sf',
                     'splines',
                     'stringr',
                     'survival',
                     'tidycensus',
                     'tidyverse', 
                     'wesanderson'
                     )

# Check if packages in list are installed, and if not, install 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load packages
lapply(list.of.packages, require, character.only = TRUE)


