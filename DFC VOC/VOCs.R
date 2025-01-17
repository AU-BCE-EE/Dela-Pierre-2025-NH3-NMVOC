
#Libraries used to run script#

library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(ggplot2)
library(readxl)

################################################################################################################################
#Prerequ#Prerequ#Prerequisites to run readCRDS function#
devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')

#Reading in Picarro data#
da <- readCRDS(('DFC_Picarro'), From = '18.09.2024 06:00:40', To = '24.09.2024 08:00:00', mult = F, tz = "ETC/GMT-1", rm = F)

################################################################################################################################
# Making date.time stamp#
da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
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

#Time stamp
voc$date.time.v <- paste(voc$`Absolute Time`)
voc$date.time.v <- ymd_hms(voc$date.time.v)
voc$date.time.v <- with_tz(voc$date.time.v, tz = "ETC/GMT-1")  

# Define the start and end
start_time <- ymd_hms("2024-09-18 05:00:40")
end_time <- ymd_hms("2024-09-24 07:00:00")
# Filter the data based on the new timezone
voc <- voc %>%
  filter(date.time.v >= start_time & date.time.v <= end_time)

#Removing unnecessary data#
voc<- voc[, -c(1, 2, 24:45)]


#Merging Data
da$date.time <- as.POSIXct(da$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
voc$date.time.v <- as.POSIXct(voc$date.time.v, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
da$date.time <- floor_date(da$date.time, unit = "hour")
voc$date.time.v <- floor_date(voc$date.time.v, unit = "hour")

mdata <- left_join(da, voc, by = c('date.time' = 'date.time.v'))































