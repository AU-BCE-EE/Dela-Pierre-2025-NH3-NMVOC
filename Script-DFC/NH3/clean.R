########################################################################################
#----- Cleaning and ordering data ------------
########################################################################################

#Removing unnecessary data#
da <- da[, -c(1:15, 17:19, 21:27)]

#Renaming the column MPVPosition to valve#
names(da)[names(da) == "MPVPosition"]  <- "valve"

#Cropping data and taking the last point of each measurement from each vavle#
da <- filter(da, !(da$valve == lead(da$valve)))

#Remove valve changing valveues from 1-19#
da <- da[da$valve == '1' | da$valve == '2' | da$valve == '3' | da$valve == '4' | da$valve == '5' | da$valve == '6' | da$valve == '7' | 
           da$valve == '8' | da$valve == '9' | da$valve == '10' | da$valve == '11' | da$valve == '12' | da$valve == '13' | da$valve == '14' |
           da$valve == '15' | da$valve == '16' | da$valve == '17' | da$valve == '18' | da$valve == '19', ]

#Ordering data according to valve#
split_valve <- split(da, f = da$valve)
valve <- paste0("V", unique(da$valve))
new_da <- NULL

########################################################################################
#----- Calculating elapsed time ------------ 
########################################################################################

for (i in seq_along(split_valve)) {
  subset_data <- split_valve[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}

#Binding and filtering data#
dat <- new_da %>%
  filter(elapsed.time >= 0 & elapsed.time <= 120)

#Rounding elapsed time to days#
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24