########################################################################################
#----- Ordering data ------------
########################################################################################
#Renaming the column MPVPosition to valve#
names(dat)[names(dat) == "MPVPosition"]  <- "valve"

#Removing valve changing rows#
dat <- dat %>%
  filter(valve == lag(valve) | valve == lead(valve))

########################################################################################
#----- Calculating time difference ------------
########################################################################################
start.picarro <- as.POSIXct("2024-09-18 11:05:00", tz = "EST")
start.ptrms <- as.POSIXct("2024-09-18 12:36:00", tz = "EST")
time.diff <- start.ptrms - start.picarro


########################################################################################
#----- Joining DFC (valve) and VOC data ------------
########################################################################################
#Creating a date.time#
voc$date.time <- paste(voc$date, voc$time)

# Converting to POSIXct#
voc$date.time <- as.POSIXct(voc$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")

#Subtract time_diff from VOC timestamps#
voc <- voc %>%
  mutate(adj.time = date.time - time.diff)

#Adjusting time difference to match exact picarro time#
voc <- voc %>%
  mutate(
    adj.time = (date.time - time.diff) + hours(1)
  )

# Converting to POSIXct in main dataset#
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")

#Creating a date.time#
dat$date.time.v <- dat$date.time


########################################################################################
#----- Adjust VOC Timestamps ------------
########################################################################################
#Round date.time values to the nearest second for both datasets#
dat$date.time.v <- round_date(dat$date.time.v, unit = "second")
dat$date.time <- round_date(dat$date.time, unit = "second")

# Merge valve and VOC data#
dat <- left_join(dat, voc, by = c('date.time' = 'adj.time'), relationship = 'many-to-many')
dat <- na.omit(dat)
