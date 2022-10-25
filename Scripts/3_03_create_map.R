# Create Map of Study Area
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 08/12/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Dissolve zctas by city and add labels
# 2: Create map of study area


####**************
#### N: Notes #### 
####**************

# In this script, a map of the study area is created. 


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
# 0d.i Load city-analysis spatial crosswalk
city_crosswalk <- read_fst(paste0(data_path, 'city_analysis_spatial_crosswalk')) %>% 
  dplyr::select(city, zcta) %>% distinct()
# 0d.ii Load NY state as sf object
ny_state <- get_acs(geography = 'state', state = 'NY', 
                    variables = 'B25077_001', geometry = TRUE)
# 0d.iii Load NY state zctas as sf object
ny_zctas <- get_acs(geography = 'zcta', state = 'NY', 
                    variables = 'B25077_001', geometry = TRUE) 
# 0d.iv Load monitors with lat/longs & convert to sf objcect
monitors <- read_csv(paste0(data_path, 'monitor_list_geo.csv')) %>% 
  st_as_sf(., coords = c("long", "lat"), remove = FALSE, 
                       crs = 4326, agr = "constant")


####**********************************************
#### 1: Dissolve zctas by city and add labels #### 
####**********************************************

# 1a Restrict zctas to those in analytic cities and add city variable 
study_zctas <- ny_zctas %>% 
  filter(GEOID %in% city_crosswalk$zcta) %>% 
  left_join(city_crosswalk, by = c('GEOID' = 'zcta'))

# 1b Dissolve by city
study_zctas <- study_zctas %>% 
  group_by(city) %>% summarize()

# 1c Add centroids with X and Y locations to be used in geom_text for labels
study_zctas <- cbind(study_zctas, st_coordinates(st_centroid(study_zctas)))

# 1d Nudge centroids to move label locations
study_zctas$nudge_x <- 0
study_zctas$nudge_y <- 0
study_zctas$nudge_x[study_zctas$city == 'Amherst'] <- -.5
study_zctas$nudge_y[study_zctas$city == 'Amherst'] <- .05
study_zctas$nudge_x[study_zctas$city == 'Cheektowaga'] <- .75
study_zctas$nudge_y[study_zctas$city == 'Buffalo'] <- -.1
study_zctas$nudge_x[study_zctas$city == 'Buffalo'] <- -.4
study_zctas$nudge_x[study_zctas$city == 'Corning'] <- .5
study_zctas$nudge_x[study_zctas$city == 'East Meadow'] <- .65
study_zctas$nudge_x[study_zctas$city == 'Hogansburg'] <- .7
study_zctas$nudge_x[study_zctas$city == 'Holtsville'] <- .4
study_zctas$nudge_y[study_zctas$city == 'Holtsville'] <- .1
study_zctas$nudge_y[study_zctas$city == 'New York'] <- -.25
study_zctas$nudge_x[study_zctas$city == 'New York'] <- .4 
study_zctas$nudge_x[study_zctas$city == 'Rochester'] <- .65


####*********************************
#### 2: Create map of study area #### 
####*********************************  
  
# 2a Create map
tiff(paste0(output_path, 'Plots/', 'fig1_study_map.tif'),
     units = "in", width = 12, height = 10, res = 300)

ggplot() +
  geom_sf(data = ny_state, color = 'gray85') +
  geom_sf(data = study_zctas, aes(fill = city, color = city)) +
  geom_sf(data = monitors, size = 2) +
  scale_fill_manual(values = rainbow(9)) +
  scale_color_manual(values = rainbow(9)) +
  geom_text(data = study_zctas, aes(X,Y, label = city), size = 6,
            nudge_x = study_zctas$nudge_x, nudge_y = study_zctas$nudge_y) +
  theme_minimal() +
  xlab('') + ylab('') +
  theme(legend.position = "none", 
        text = element_text(size = 16))

dev.off()




