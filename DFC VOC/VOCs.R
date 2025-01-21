
#Libraries used to run script#

library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(ggplot2)
library(readxl)
library(tidyr)

################################################################################################################################
#Prerequ#Prerequ#Prerequisites to run readCRDS function#
devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')

#Reading in Picarro data#
da <- readCRDS(('DFC_Picarro'), From = '18.09.2024 12:00:40', To = '24.09.2024 12:00:00', mult = F, tz = "ETC/GMT-1", rm = F)
range(da$date.time)
################################################################################################################################
# Making date.time stamp#
da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
da <- da[, -c(1:15, 17:19, 21:27)]
dat <- rbind(da)

################################################################################################################################

# List of file paths VOCs
files <- c("VOC Data/File 1.xlsx", "VOC Data/File 2.xlsx", "VOC Data/File 3.xlsx")

# Read and combine data from each file
vocs <- lapply(files, function(file) {
  # Read the file and sheet
  data <- read_excel(file, sheet = "Conc_ppb")
  
  return(data)
})

# Combine all data into a single data frame
voc <- bind_rows(vocs)


# Making date.time stamp#
voc$date <- as.Date(voc$`Absolute Time`)                                 
voc$time <- format(voc$`Absolute Time`, format = "%H:%M:%S") 
voc$date.time.v <- paste(voc$date, voc$time)

start_time <- ymd_hms("2024-09-18 09:00:30", tz = "ETC/GMT-1")
end_time <- ymd_hms("2024-09-24 11:00:00", tz = "ETC/GMT-1")
# Filter the data based on the new timezone
voc <- voc %>%
  filter(date.time.v >= start_time & date.time.v <= end_time)


dat$date.time.v <- dat$date.time
dat$date.time.v <- round_date(dat$date.time.v, unit = "hour")
dat$date.time <- round_date(dat$date.time, unit = "hour")
voc$date.time.v <- sprintf("%s %s", voc$date, voc$time)
voc$date.time.v <- as.POSIXct(voc$date.time.v, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")

#Removing unnecessary data#
voc<- voc[, -c(1, 2, 24:47)]


dat <- left_join(dat, voc, by = c('date.time' = 'date.time.v'), relationship = "many-to-many")














