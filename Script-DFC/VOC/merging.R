########################################################################################
#----- Joining DFC (valve) and VOC data ------------
########################################################################################

#Creating a date.time#
voc$date.time.v <- paste(voc$date, voc$time)

#Creating a date.time#
dat$date.time.v <- dat$date.time

#Round date.time values to the nearest second for both datasets#
dat$date.time.v <- round_date(dat$date.time.v, unit = "second")
dat$date.time <- round_date(dat$date.time, unit = "second")

# Converting to POSIXct#
voc$date.time.v <- as.POSIXct(voc$date.time.v, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")

# Merge valve and VOC data#
dat <- left_join(dat, voc, by = c('date.time' = 'date.time.v'), relationship = 'many-to-many')
dat <- na.omit(dat)
