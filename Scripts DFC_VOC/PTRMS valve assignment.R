library(tidyr)
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)
library(hms)
library(ggplot2)
library(readxl)


devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')

# Picarro data
a<-readCRDS("", 
              From = '18.09.2024 11:00:00', 
              To = '24.09.2024 08:00:00', 
              mult = T, 
              tz = "UTC", 
              rm = F) #Picarro data
# PTRMS data
dt<-read_xlsx("ptrms.xlsx") #PTRMS data, file ptrms.xlsx
#create a date&time column for Picarro data
a$date.time <- paste(a$DATE, a$TIME)
a$date.time<-ymd_hms(a$date.time)
a$date.time <- a$date.time
#Add 1 hr and 31 minutes to Picarro data to match the real (and PTRMS) time
a$date.time<-a$date.time+(60*60+31*60)
# getting time and valve number from NH3 data 
a<- a[, c(16, 28)]
# rounding times so it match in the two data frames
a$date.time <- ceiling_date(a$date.time, 'seconds')
dt$date.time <- ceiling_date(dt$date.time, 'seconds')
#probalby not necessary
a$MPVPosition<-as.numeric(a$MPVPosition)
str(a$MPVPosition)

# adding the valve no to PTRMS  data
dt <- left_join(dt, a, by = 'date.time')

dt <- dt[! is.na(dt$MPVPosition), ]

dt$elapsed.time <- difftime(dt$date.time, min(dt$date.time), units='hour')
dt$id <- as.character(dt$MPVPosition)

# Selecting points with whole numbers (when the valve change there is a measurement where the valve position
# is in between two valves, these are removed)
dt <- dt[dt$id == '1' | dt$id == '2' | dt$id == '3' | dt$id == '4' | dt$id == '5' | dt$id == '6' | dt$id == '7' | 
           dt$id == '8' | dt$id == '9' | dt$id == '10' | dt$id == '11' | dt$id == '12' | dt$id == '13' | dt$id == '14'| dt$id == '15'| dt$id == '16'| dt$id == '17'| dt$id == '18'| dt$id == '19',]

dt$valve<-dt$id
# Remove duplicate Cycle.number rows
dt <- dt %>%
  distinct(`Cycle number`, .keep_all = TRUE)
View(dt)
#save the file, for some reason if I save as a csv and read it in again R rounds the time to the minutes instead of seconds
write.table(dt, "raw.ptrms.valve.txt", row.names = F, sep=",")

