# Recreate Sebastian's process of converting weather datetimes in UTC 
# to an hour index

# The goal here is to ensure I convert the hour index back into the correct 
# datetime, in America/New_York timezone, so weather can be merged wtih MI data

# Start with two years of datetimes in UTC, representing 1999 and 2000
fakeW_9900_UTC <- tibble(datetime_UTC = seq(ymd_hms('1999-01-01 00:00:00', tz = "UTC"), 
                                            ymd_hms('2000-12-31 23:00:00', tz = "UTC"), 
                                            by = '1 hour'))

# Create a "weather" variable that is the row number
fakeW_9900_UTC <- fakeW_9900_UTC %>% mutate(weather = seq.int(nrow(.)))

# Convert datetimes to America/New_York just to see what happens during Daylight
# Savings
# The time series now starts 5 hours earlier, at 1998-12-31 19:00:00
# Spring Forward: 1999-04-04 02:00:00 (02:00:00 is skipped)
# Fall Back: 1999-10-31 02:00:00 (there are two 01:00:00)
fakeW_9900_UTC <- fakeW_9900_UTC %>% mutate(datetime_ANY = with_tz(datetime_UTC, 
                                                                   tz = 'America/New_York'))

# Create a datetime variable in America/New_York (not converted) to make sure
# it is the same -- it is
# test_ANY <- tibble(datetime_ANY = seq(ymd_hms('1999-01-01 00:00:00', tz = "America/New_York"), 
#                                             ymd_hms('2000-12-31 23:00:00', tz = "America/New_York"), 
#                                             by = '1 hour'))

# Create duration and hour index using 1999/01/01 as the origin
# This makes it easier to see what interval and as.duration are doing
fakeW_9900_UTC <- fakeW_9900_UTC %>% 
  mutate(Jen_duration = as.duration(interval(parse_date_time('1999/01/01 00:00:00', 
                                                             'ymd HMS', tz = 'America/New_York'), 
                                             datetime_UTC))) %>% 
  mutate(Jen_HourIndex = as.numeric(Jen_duration, 'hours'))

# Create duration and hour index following the same process Sebastian used 
fakeW_9900_UTC <- fakeW_9900_UTC %>% 
  mutate(Seb_duration = as.duration(interval(parse_date_time('1990/01/01 00:00:00', 
                                                             'ymd HMS', tz = 'America/New_York'), 
                                             datetime_UTC))) %>% 
  mutate(Seb_HourIndex = as.numeric(Seb_duration, 'hours'))

# Add five hours to the hour index, because the as_datetime function we will use
# to convert back assumes the origin is in UTC (and we had an origin in America/New_York)
fakeW_9900_UTC <- fakeW_9900_UTC %>% 
  mutate(Seb_HourIndex_plus5 = Seb_HourIndex + 5)

# Convert hour index back to datetime using America/New_York timezone to maintain
# Daylight Savings
fakeW_9900_UTC <- fakeW_9900_UTC %>% 
  mutate(back_to_datetime = as_datetime(Seb_HourIndex_plus5*3600, 
                                        origin = '1990/01/01 00:00:00', 
                                        tz = 'America/New_York'))
  


  
  
  
  
  