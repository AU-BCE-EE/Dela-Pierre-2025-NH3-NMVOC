####### Flavia Project #########################
####### NH3 Emissions from DFC Chambers ########
####### Codes by Ali ###########################



#Libraries used to run script#

library(data.table)
library(lubridate)
library(dplyr)


#Prerequisites to run readCRDS function#
devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')
source('Functions/PicarroFunction.R')  #source of readCRDS function coding file#


#Reading in Picarro data#
da <- readCRDS(('DFC_Picarro'), From = '17.09.2024 11:49:23', To = '24.09.2024 08:00:00', mult = F, tz = "ETC/GMT-1", rm = F)


# Making date.time stamp#
da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 


# Removing unnecessary data#
str(da); da
names(da)
da <- da[, -c(1:15, 17:19, 21:27)]
str(da); da


# Renaming the column MPVPosition to valve
names(da)[names(da) == "MPVPosition"]  <- "valve"


# Cropping data and taking the last point of each measurement from each valve 
da <- filter(da, !(da$valve == lead(da$valve)))


# Removal of valve changing values from 1-19
da <- da[da$valve %in% 1:19, ]


# Ordering data according to valves
split_valda <- split(da, f = da$valve)
valid <- paste0("V", unique(da$valve))
new_da <- NULL


# Calculate elapsed time of splited subset 
for (i in seq_along(split_valda)) {
  subset_data <- split_valda[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
dat<- new_da


# Rounding elapsed time to days
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24


#Assign names to valve values

val_data <- dat %>%
  mutate(treatment = recode(valve,
                            `1` = '0-bls',
                            `2` = '0-bp',
                            `3` = '1.5',
                            `4` = '2.9',
                            `5` = 'bkg',
                            `6` = '0-bp',
                            `7` = '5.7',
                            `8` = '0-bls',
                            `9` = 'bkg',
                            `10` = '1.5',
                            `11` = '2.9',
                            `12` = '0-bls',
                            `13` = '5.7',
                            `14` = '0-bp',
                            `15` = '1.5',
                            `16` = 'bkg',
                            `17` = '2.9',
                            `18` = '0-bls',
                            `19` = '5.7'
  ),
group = case_when(
    valve %in% c(2, 6, 14) ~ 'No acid',
    valve %in% c(3, 10, 15) ~ 'Low acid',
    valve %in% c(4, 11, 17) ~ 'Medium acid',
    valve %in% c(7, 13, 19) ~ 'High acid',
    valve %in% c(5, 9, 16) ~ 'Background',
    valve %in% c(1, 6, 8, 18) ~ 'bLS'
  )
)


#Background corrected concentration 
#Background data
DFC.bg <- dat[val_data$group == 'Background', ]

#DFC oulet data
DFC <- dat[val_data$group%in% c('No acid', 'Low acid', 'Medium acid', 'High acid'), ]

#Mean background values
DFC.bg.summ <- aggregate(NH3_30s ~ elapsed.time, data = DFC.bg, FUN = mean)

#Joining average background and outlet data
DFC <- full_join(DFC.bg.summ, DFC, by = 'elapsed.time')
DFC <- na.omit(DFC)

#Subtracting background from outlet
DFC$NH3.corr <- DFC$NH3_30s -  DFC.bg$NH3_30s
DFC[! complete.cases(DFC), ]

#Rebind again in DFC datasheet
dat <- rbind(DFC)


#Emission calculation




