########################################################################################
#----- Loading picarro data ------------
########################################################################################

da <- readCRDS(('/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/DFC/Data/input data/NH3/picarro_data'), From = '18.09.2024 06:00:40', To = '24.09.2024 08:00:00', mult = F, tz = "EST", rm = F)

#Making date time stamp#

da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
########################################################################################


########################################################################################
#----- Loading Weather data ------------
########################################################################################
#Import Weather Data and filter data#
header <- c('date', 'time', 'temp')
weather <- read.csv('/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/DFC/Data/input data/NH3/Temp.csv', fill = T, stringsAsFactors = F)
weather <- weather[, -1 ]
colnames(weather) <- header
########################################################################################


########################################################################################
#----- Loading TAN data ------------
########################################################################################
#Import Tan Data#
header <- c('Id', 'Treatment', 'g Slurry', 'Dilution Factor', 'N-NH4', 'N-NH4 mg/L')
Tan <- read.csv('/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/DFC/Data/input data/NH3/TAN analysis.csv', fill = T, stringsAsFactors = F)
Tan <- Tan [, -c(1, 3:5)]
Tan$treatment <- as.factor(Tan$treatment)

########################################################################################
#----- Loading bLS data ------------
########################################################################################
#Import bLS Data#
bls <- read.csv('/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/DFC/Data/input data/NH3/NH3_fluxes_bLS.csv', fill = T, stringsAsFactors = F)
