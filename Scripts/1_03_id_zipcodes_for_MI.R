# Load and Explore Health Data
# NO2-MI Analysis
# Jenni A. Shearston 
# Updated 04/29/2021

####***********************
#### Table of Contents #### 
####***********************

# N: Notes
# 0: Preparation 
# 1: Add lat/longs to monitors
# 2: Identify zip codes of interest


####*****************
#### Preparation #### 
####*****************

# 0a Declare root directory
project.folder <- paste0(print(here::here()),'/')

# 0b Load Packages
source(paste0(project.folder, 'Scripts/', 'packages.R'))

# 0c Set up filepath(s)
data_path <- '/Users/jennishearston/Dropbox/Columbia/Research/NO2_MI/Data/Intermediate_Data/'


####**********************************
#### 1. Add lat/longs to monitors #### 
####**********************************

# Notes: Need to add lat/longs so that the monitor locations can be read into QGIS

# 1a Load monitor list
monitor_list <- read_csv(paste0(data_path, 'monitor_list.csv'), 
                         col_names = c("monitor_ids"))

# 1b Add lat longs to monitor list
monitor_list_geo <- monitor_list %>% 
  mutate(lat = c(44.980577, 43.145013, 43.146180, 42.993280, 42.921107, 42.876907, 
                 42.091420, 40.867900, 40.865850, 40.836060, 40.816000, 40.784200, 
                 40.759120, 40.739546, 40.736140, 40.739264, 40.755270, 40.743160, 
                 40.827990),
         long = c(-74.695005, -77.557280, -77.548170, -78.771530, -78.765825, -78.809526, 
                  -77.209780, -73.878090, -73.880830, -73.920090, -73.902000, -73.847570,
                  -73.966610, -73.985694, -73.821530, -73.817694, -73.758610, -73.585490,
                  -73.057540))

# 1c Save monitor list with lat longs
write_csv(monitor_list_geo, paste0(data_path, 'monitor_list_geo.csv'))

# Notes: GIS work done in QGIS
# Overall process
#  In QGIS, a nationwide ZCTA shapefile is loaded, and clipped to New York state
#  using a states/county shapefile. Centroids are identified for each ZCTA, of note,
#  "create centroid for each part" is not selected. Then monitors are loaded as a 
#  point file, a ~5km buffer is created around each monitor, and then zcta centroids
#  within each buffer are identified using a spatial join. Of note, centroids often 
#  fall within more than one buffer, especially in densely populated areas, which 
#  resulted in some zcta centroids being present in more than one monitor's buffer.
#  (Or, perhaps more accurately, resulted in some zcta centroids being covered by 
#  more than one monitor's buffer.)
# Degree to km conversion for making buffers 
#  The unit of distance used in QGIS for the buffer function is degrees (per the projections)
#  1 degree longitude = 81 km (mid NY state); 0.01234568 degree longitude = 1 km
#  1 degree of latitude = 111 km (constant across globe)
#  citations: https://www.usna.edu/Users/oceano/pguth/md_help/html/approx_equivalents.htm
#             https://www.nhc.noaa.gov/gccalc.shtml
# CRS 
#  Both WGS 84 (EPSG 4326) and NAD83 (EPSG 4269) were used, however these are
#  basically the same, as per https://gis.stackexchange.com/questions/27493/is-nad-83-the-same-as-epsg4326


####**************************************
#### 2. Identify zipcodes of interest #### 
####**************************************

# 2a Load zcta centroids within monitor buffers (work done in QGIS)
zctas <- read_csv(paste0(data_path, 'centroids_in_buffers.csv'))

# 2b Check for duplicate zctas in a single monitor buffer (this indicates multiple 
#    centroids in a zcta)
check <- zctas %>% group_by(monitor_id) %>% 
  summarize(num_unique = n_distinct(ZCTA5CE10), 
            num_total = n())

# 2c Load zcta to zip code crosswalk, as MI data is at zip code level
# Source: https://udsmapper.org/zip-code-to-zcta-crosswalk/
crosswalk <- readxl::read_xlsx(paste0(data_path, 'zip_to_zcta_crosswalk_2020.xlsx')) %>% 
  janitor::clean_names() %>% 
  filter(state == "NY")

# 2d Join zcta and crosswalk files
crosswalk_zcta_join <- crosswalk %>% 
  full_join(zctas, by = c("zcta" = "ZCTA5CE10"))

# 2e Filter to observations that are inside a buffer
crosswalk_zcta_join <- crosswalk_zcta_join %>% 
  filter(!is.na(monitor_id)) 

# 2f Identify unique zip codes in buffers to obtain MI data
zips_in_buffers <- crosswalk_zcta_join %>% 
  summarise(zip_code = unique(zip_code))

# 2g Add cities listed on AirData, for city-wide analysis and save
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
city_monitor_match %>% write_fst(paste0(data_path, 'city_monitor_match'))

# 2h Add zip codes for each city
# Notes: Includes some zips that are for a specific company, organization, or PO box
#        Some zips (eg 14261) are assigned to more than one city
# Source: https://tools.usps.com/zip-code-lookup.htm
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

# 2i Add zips for all of nyc (all five boroughs)
# Notes: I don't believe this includes zips for specific companies/organizations, 
#        given the smaller n. 
# Source: https://data.beta.nyc/en/dataset/pediacities-nyc-neighborhoods/resource/7caac650-d082-4aea-9f9b-3681d568e8a5
nyc_zips <- read_csv(paste0(data_path, 'nyc_zip_borough_neighborhoods_pop.csv')) %>% 
  dplyr::select(zip) %>% 
  rename(zip_code = zip) %>% 
  mutate(zip_code = as.character(zip_code))

# 2j Create city-level zip to monitor crosswalk
city_monitor_zipcode_match <- tribble(
  ~monitor_id, ~city, ~zip_code,
  "36-033-7003", "Hogansburg", hogansburg_zips$zip_code,
  "36-055-0015", "Rochester", rochester_zips$zip_code,
  "36-055-1007", "Rochester", rochester_zips$zip_code,
  "36-029-0002", "Amherst", amherst_zips$zip_code,
  "36-029-0023", "Cheektowaga", cheektowaga_zips$zip_code,
  "36-029-0005", "Buffalo", buffalo_zips$zip_code,
  "36-101-0003", "Corning", corning_zips$zip_code,
  "36-005-0133", "New York", nyc_zips$zip_code,
  "36-005-0083", "New York", nyc_zips$zip_code,
  "36-005-0080", "New York", nyc_zips$zip_code,
  "36-005-0110", "New York", nyc_zips$zip_code,
  "36-081-0098", "New York", nyc_zips$zip_code,
  "36-061-0056", "New York", nyc_zips$zip_code,
  "36-061-0010", "New York", nyc_zips$zip_code,
  "36-081-0124", "New York", nyc_zips$zip_code,
  "36-081-0125", "New York", nyc_zips$zip_code,
  "36-081-0097", "New York", nyc_zips$zip_code,
  "36-059-0005", "East Meadow", eastmeadow_zips$zip_code,
  "36-103-0009", "Holtsville", holtsville_zips$zip_code
) %>% unnest(cols = zip_code)

# 2k Save city-monitor-zipcode-zcta crosswalk file
write_csv(city_monitor_zipcode_match, 
          paste0(data_path, 'city_monitor_zipcode_match.csv'))

# 2l Bind city zips together (n should be 603)
zips_for_mi <- zips_in_buffers %>% 
  bind_rows(nyc_zips, amherst_zips, buffalo_zips, cheektowaga_zips, corning_zips,
            eastmeadow_zips, hogansburg_zips, holtsville_zips, rochester_zips)
  
# 2m Restrict to unique observations
zips_for_mi <- zips_for_mi %>% 
  summarise(zip_code = unique(zip_code))

# 2n Save out zips to match with MI data
write_csv(zips_for_mi, paste0(data_path, 'zips_for_mi.csv')) 







