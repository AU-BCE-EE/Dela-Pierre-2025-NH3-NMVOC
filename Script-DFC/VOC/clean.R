########################################################################################
#----- Ordering data ------------
########################################################################################

#Renaming the column MPVPosition to valve#
names(dat)[names(dat) == "MPVPosition"]  <- "valve"

#Cropping data and taking the last point of each measurement from each valve#
dat <- filter(dat, !(dat$valve == lead(dat$valve)))

#Remove valve changing values from 1-19#
dat <- dat[dat$valve == '1' | dat$valve == '2' | dat$valve == '3' | dat$valve == '4' | dat$valve == '5' | dat$valve == '6' | dat$valve == '7' | 
             dat$valve == '8' | dat$valve == '9' | dat$valve == '10' | dat$valve == '11' | dat$valve == '12' | dat$valve == '13' | dat$valve == '14' |
             dat$valve == '15' | dat$valve == '16' | dat$valve == '17' | dat$valve == '18' | dat$valve == '19', ]

#Ordering data according to valve#
split_valve <- split(dat, f = dat$valve)
valve <- paste0("V", unique(dat$valve))
new_da <- NULL

########################################################################################
#----- Calculating elapsed time ------------ 
########################################################################################

for (i in seq_along(split_valve)) {
  subset_data <- split_valve[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
#Merging data#
dat<- new_da

#Removing benzen column#
dat <- dat %>% select(-benzen)

#Rounding elapsed time to days#
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24