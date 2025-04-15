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
# Calculate the difference between actual and recorded start time
ori.time <- as.POSIXct("2024-09-18 12:36:02", tz = "UCT")
cor.time <- as.POSIXct("2024-09-18 11:05:02", tz = "UCT")

time_shift <- cor.time - ori.time  # This will be a difftime object

#Applying the shift#
voc$adj.time <- voc$date.time + time_shift

########################################################################################
#----- Joining DFC (valve) and VOC data ------------
########################################################################################
#Rearranging the columns#
voc <- voc %>%
  select(adj.time, everything())

# Converting both data sets to POSIXct#
dat$date.time <- as.POSIXct(format(dat$date.time, "%Y-%m-%d %H:%M:%S"))
voc$adj.time <- as.POSIXct(format(voc$adj.time, "%Y-%m-%d %H:%M:%S"))

dat<- left_join(dat, voc, by = c('date.time' = 'adj.time'))
dat <- na.omit(dat)
