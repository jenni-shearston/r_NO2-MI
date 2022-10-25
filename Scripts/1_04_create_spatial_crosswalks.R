# Create spatial crosswalks for city and zip code analyses
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 07/25/2022

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Create Spatial Crosswalk for City Level Analysis (n = 9)
# 2: Create Spatial Crosswalk for Zip Code Level Analysis (n = 322)
# 3: Sanity Check: Make Sure All Zip Codes are Included


####***********
#### Notes #### 
####***********

# There are five spatial units at play: points/monitors, 5km buffers around
# each point, zctas, zip codes, and cities

# NO2 monitors: points. In QGIS we built 5km buffers around each point
# MI data: zip code

# There are two analytic spatial units: cities and zip codes. 
# Cities were identified by the city name assigned to each monitor in AirData 
# (n = 9). All zip codes in each of the 9 cities were included in the analysis.
# Zip codes were identified in a more complex way. We drew 5km buffers around
# each NO2 monitor, and then identified all zctas with a centroid inside a 
# buffer. We converted each of these zctas to zip codes, and included them in
# a sensitivity analysis (n = 322).

# Spatially, we started with 5km buffers around NO2 monitors (which are points). 
# Buffers could overlap with each other, and often did, especially in NYC. 
# We then identified zctas with centroids inside a buffer. It is possible that a 
# single zcta is assigned to more than one monitor, as its centroid could have
# fallen within more than one monitor's buffer. We will later average the NO2
# concentrations so that each zip code has one NO2 value per datetime. Additionally, 
# zctas may be matched with more than one zip code, but because we will 
# average NO2 by zip code, this is okay.


####*****************
#### Preparation #### 
####*****************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'Scripts/', 'packages.R'))

# 0c Set up filepath(s)
data_path <- '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data/Intermediate_Data/'

# 0d Load zcta to zip code crosswalk
#    Source: https://udsmapper.org/zip-code-to-zcta-crosswalk/
zcta_zip_match <- readxl::read_xlsx(paste0(data_path, 'zip_to_zcta_crosswalk_2020.xlsx')) %>% 
  janitor::clean_names() %>% filter(state == "NY")

# 0e Load city to monitor crosswalk
city_monitor_match <- read_fst(paste0(data_path, 'city_monitor_match'))


####*********************************************************
#### 1: Create Spatial Crosswalk for City Level Analysis #### 
####*********************************************************

# 1a Create datasets for zip codes for each city
#    Notes: Includes some zips that are for a specific company, organization, or PO box
#           Some zips (eg 14261) are assigned to more than one city
#    Source: https://tools.usps.com/zip-code-lookup.htm
amherst_zips <- tibble(zip_code = as.character(c(14068, 14221, 14226, 14228, 14231, 
                                                 14260, 14261)))
buffalo_zips <- tibble(zip_code = as.character(c(14201:14228, 14231, 14233, 14240, 
                                                 14241, 14260, 14261, 14263, 14264, 
                                                 14265, 14267, 14269, 14270, 14272, 
                                                 14273, 14276, 14280)))
cheektowaga_zips <- tibble(zip_code = as.character(c(14225, 14206, 14211, 14215, 14227)))
corning_zips <- tibble(zip_code = as.character(c(14830, 14831)))
eastmeadow_zips <- tibble(zip_code = as.character(c(11554)))
hogansburg_zips <- tibble(zip_code = as.character(c(13655)))
holtsville_zips <- tibble(zip_code = as.character(c('00501', '00544', '11742')))
rochester_zips <- tibble(zip_code = as.character(c(14602:14627, 14638, 14639, 14642, 
                                                   14643, 14644, 14646, 14647, 14649, 
                                                   14650, 14651, 14652, 14653, 14692, 14694)))

# 1b Create dataset for NYC zipcodes (all five boroughs)
#    Notes: I don't believe this includes zips for specific companies/organizations, 
#           given the smaller n. 
#    Source: https://data.beta.nyc/en/dataset/pediacities-nyc-neighborhoods/resource/7caac650-d082-4aea-9f9b-3681d568e8a5
nyc_zips <- read_csv(paste0(data_path, 'nyc_zip_borough_neighborhoods_pop.csv'),
                     col_types = 'ccccnn') %>% 
  dplyr::select(zip) %>% 
  rename(zip_code = zip)

# 1c Create dataset with cities and zip codes for the city-level 
#    analysis
city_cityZip <- tribble(
  ~city, ~zip_code,
  "Hogansburg", hogansburg_zips$zip_code,
  "Rochester", rochester_zips$zip_code,
  "Amherst", amherst_zips$zip_code,
  "Cheektowaga", cheektowaga_zips$zip_code,
  "Buffalo", buffalo_zips$zip_code,
  "Corning", corning_zips$zip_code,
  "New York", nyc_zips$zip_code,
  "East Meadow", eastmeadow_zips$zip_code,
  "Holtsville", holtsville_zips$zip_code
) %>% unnest(cols = zip_code)

# 1d Identify zip codes assigned to more than one city 
# Notes: There are 11 zipcodes occurring in two cities
# 1d.i Create unique city/zip_code combination dataset
city_cityZip <- city_cityZip %>% distinct()
# 1d.ii Count the number of times each zip code appears
n_occur <- data.frame(table(city_cityZip$zip_code))
# 1d.iii Identify the zip codes that are assigned to more than one city
n_occur[n_occur$Freq > 1,] # n = 11

# 1e Assign each zip code to only one city
#    Notes: In QGIS, I identified the monitor closest to the zcta centroid for
#           each of the 11 zip codes, and assigned the zip code to that monitor's city
# 1e.i Assign 11 zip codes to only one city
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14206" & city_cityZip$city == "Cheektowaga"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14211" & city_cityZip$city == "Cheektowaga"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14215" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14221" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14225" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14226" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14227" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14228" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14231" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14260" & city_cityZip$city == "Buffalo"),]
city_cityZip <- city_cityZip[!(city_cityZip$zip_code == "14261" & city_cityZip$city == "Buffalo"),]
# 1e.ii Confirm that there is now only one city assigned to each zip code
n_occur <- data.frame(table(city_cityZip$zip_code))
n_occur[n_occur$Freq > 1,] # fixed now, n = 0

# 1f Add zctas to city-level city/zip dataset 
city_cityZipZcta <- city_cityZip %>% 
  left_join(zcta_zip_match, by = c('zip_code')) 

# 1g Add monitors to city-level city/zip/zcta dataset
city_cityZipZctaMonitor <- city_cityZipZcta %>% 
  left_join(city_monitor_match, by = 'city')

# 1h Confirm all unique spatial units are present
# 1h.i Count unique zip codes
length(unique(city_cityZipZctaMonitor$zip_code)) # n = 269
# 1h.ii Count unique zctas
length(unique(city_cityZipZctaMonitor$zcta)) # n = 235
# 1h.iii Count unique monitors
length(unique(city_cityZipZctaMonitor$monitor_id)) # n = 19
# 1h Count unique cities
#    This is also the final spatial n for city level analysis
length(unique(city_cityZipZctaMonitor$city)) # n = 9

# 1i Rename and save
city_analysis_spatial_crosswalk <- city_cityZipZctaMonitor
write_fst(city_analysis_spatial_crosswalk, 
          paste0(data_path, 'city_analysis_spatial_crosswalk'))


####*************************************************************
#### 2: Create Spatial Crosswalk for Zip Code Level Analysis #### 
####*************************************************************

# 2a Load dataset with zctas connected to monitors
zip_zctaMonitor <- read_csv(paste0(data_path, 'centroids_in_buffers.csv'))

# 2b Filter to observations that are inside a monitor's 5km buffer and keep
#    only needed variables (n = 396 zcta/monitors)
zip_zctaMonitor <- zip_zctaMonitor %>% filter(!is.na(monitor_id)) %>% 
  dplyr::select(ZCTA5CE10, monitor_id)

# 2c Add zip code variable to zcta/monitor dataset
#    Notes: Dropped one zcta (07020) that was in New Jersey
zip_zctaMonitorZip <- zip_zctaMonitor %>% 
  left_join(zcta_zip_match, by = c('ZCTA5CE10' = 'zcta')) %>% 
  na.omit()

# 2d Add cities to zcta/monitor/zip dataset
#    Notes: We used the city connected to each monitor on AirData
# 2d.i Add city/monitor dataset
city_monitor_match <- tribble(
  ~monitor_id, ~city,
  "36-033-7003", "Hogansburg", 
  "36-055-0015", "Rochester", 
  "36-055-1007", "Rochester", 
  "36-029-0002", "Amherst", 
  "36-029-0023", "Cheektowaga", 
  "36-029-0005", "Buffalo", 
  "36-101-0003", "Corning", 
  "36-005-0133", "New York", 
  "36-005-0083", "New York",
  "36-005-0080", "New York", 
  "36-005-0110", "New York", 
  "36-081-0098", "New York", 
  "36-061-0056", "New York", 
  "36-061-0010", "New York", 
  "36-081-0124", "New York", 
  "36-081-0125", "New York",
  "36-081-0097", "New York", 
  "36-059-0005", "East Meadow",
  "36-103-0009", "Holtsville"
)
# 2d.ii Merge city/monitor dataset with zcta/monitor/zip dataset
zip_zctaMonitorZipCity <- zip_zctaMonitorZip %>% 
  left_join(city_monitor_match, by = 'monitor_id')

# 2e Confirm all unique spatial units are present
# 2e.i Count unique zctas
length(unique(zip_zctaMonitorZipCity$ZCTA5CE10)) # n = 198
# 2e.ii Count unique monitors
#       One monitor (Pinnacle State Park) had no zcta centroids inside its
#       5km buffer, and so is not included
length(unique(zip_zctaMonitorZipCity$monitor_id)) # n = 18
# 2e.iii Count unique cities
#        One city (Corning) is not included because it was the city connected to
#        Pinnacle State Park, which had no zcta centroids inside its 5km buffer,
#        and so is not included
length(unique(zip_zctaMonitorZipCity$city)) # n = 8
# 2e Count unique zip codes
#    This is also the final spatial n for zip code level analysis
length(unique(zip_zctaMonitorZipCity$zip_code)) # n = 322

# 2f Rename and save
zipcode_analysis_spatial_crosswalk <- zip_zctaMonitorZipCity
write_fst(zipcode_analysis_spatial_crosswalk, 
          paste0(data_path, 'zipcode_analysis_spatial_crosswalk'))


####***********************************************************
#### 3: Sanity Check: Make Sure All Zip Codes are Included #### 
####***********************************************************

# Sanity check: compare zip codes for which MI data was obtained to zip codes
# represented in zip level and city level crosswalks to make sure all are 
# represented
a <- read_csv(paste0(data_path, 'zips_for_mi.csv'))
b <- city_cityZipZcta %>% dplyr::select(zip_code)
c <- zip_zctaMonitorZipCity %>% dplyr::select(zip_code)
d <- bind_rows(b,c)
e <- unique(d)
f <- a %>% filter(!zip_code %in% e$zip_code) 
  # There are three fewer observations in f than a
  # one of these is NA
  # 501 is represented by 00501 (its full number), so is accounted for
  # 544 is represented by 00544 (its full number), so is accounted for

# Determine which zip codes are in both analyses
b <- b %>% distinct()
c <- c %>% distinct()
e$zc_level <- ifelse(e$zip_code %in% c$zip_code, 1, 0)
e$city_level <- ifelse(e$zip_code %in% b$zip_code, 1, 0)
e$both <- ifelse(e$zc_level == 1 & e$city_level == 1, 1, 0)
sum(e$both) # n = 171

