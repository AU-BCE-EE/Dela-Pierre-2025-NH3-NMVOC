########################################################################################
#----- Loading picarro data ------------
########################################################################################

da <- readCRDS(('/Users/AU775281/Documents/PhD/Flavia Experiment/DFC R/DFC_Picarro'), From = '18.09.2024 06:00:40', To = '24.09.2024 08:00:00', mult = F, tz = "EST", rm = F)

#Making date time stamp#

da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
da <- da[, -c(2:15, 17:27)]
dat <- rbind(da)
########################################################################################


########################################################################################
#----- Loading VOC data ------------
########################################################################################
# Importing VOC data
voc <- read_xlsx('/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/DFC/Data/input data/VOC Data/ptrms.xlsx')
voc <- voc %>%
  filter(date.time >= as.POSIXct("2024-09-18 12:36:02", tz = "UCT"))
voc$date <- as.Date(voc$date.time, format = "%m/%d/%y %H:%M")
voc$time <- format(as.POSIXct(voc$date.time, format = "%m/%d/%y %H:%M"), "%H:%M:%S")

########################################################################################
#----- Loading Weather data ------------
########################################################################################
#Import Weather Data and filter data#
header <- c('date', 'time', 'temp')
weather <- read.csv('/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Temp.csv', fill = T, stringsAsFactors = F)
weather <- weather[, -1 ]
colnames(weather) <- header
########################################################################################


########################################################################################
#----- Loading VOC mass data ------------
########################################################################################
#Mass of VOCs [g * mol^-1]#
MW <- read_excel("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/VOC_MW.xlsx")
########################################################################################


########################################################################################
#----- Loading OTV data ------------
########################################################################################
OTV <- read_excel("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/VOC_OTV.xlsx")
########################################################################################