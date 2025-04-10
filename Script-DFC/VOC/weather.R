########################################################################################
#----- Importing weather data ------------ 
########################################################################################

#Changing date format in excel file#
weather$date <- parse_date_time(weather$date, orders = c("d/m/y", "d-m-y"))

#Selecting experiment date range#
start_date <- dmy("18/09/2024")
end_date <- dmy("24/09/2024")

# Filter the data within the date range
weather <- weather[weather$date >= start_date & weather$date <= end_date, ]

########################################################################################

########################################################################################
#----- Joining weather data with DFC data ------------
########################################################################################

#Round both columns to the nearest hour#
weather$time <- as.numeric(weather$time)
weather$date.time.weather <- paste(weather$date, weather$time)
dat$date.time.weather <- dat$date.time
dat$date.time.weather <- round_date(dat$date.time.weather, unit = "hour")
dat$date.time <- round_date(dat$date.time, unit = "hour")

#Convert both to POSIXct#
weather$date.time.weather <- sprintf("%s %02d:00", weather$date, weather$time)
weather$date.time.weather <- as.POSIXct(weather$date.time.weather, format = '%Y-%m-%d %H:%M', tz = "EST")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "EST")
head(weather$date.time.weather)

#Merging data#
dat <- left_join(dat, weather, by = c('date.time' = 'date.time.weather'))
########################################################################################
