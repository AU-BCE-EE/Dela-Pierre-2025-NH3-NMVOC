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
a<-readCRDS("../DFC/Data/input data/NH3", 
              From = '18.09.2024 11:00:00', 
              To = '24.09.2024 08:00:00', 
              mult = T, 
              tz = "UTC", 
              rm = F) #Picarro data
#PTRMs data
dt <- read_csv("../Flavia_VOC_DFC_data/VOC_DFC_ppb.txt")
#rename for consistency with other scripts
dt<- dt %>% rename(
  methanol = m_z_33_methanol,
  H2S = `m_z_35_H2S`,
  X4.Methylphenol = `m_z_109_4_Methylphenol`,
  acetic_acid =`m_z_61_43_acetic_acid`,
  butanoic_acid = `m_z_71_89_butanoic_acid`,
  pentanoic_acid =`m_z_85_103_pentanoic_acid`,
  propanoic_acid = `m_z_57_75_propanoic_acid`,
  acetladheyde = m_z_45_00_acetaldheyde,
  formic_acid = m_z_47_00_formic_acid,
  methanthiol = m_z_49_00_methanthiol,
  acetone = m_z_59_00_acetone,
  trimethylamine = m_z_60_00_trimethylamine,
  dimethyl_sulfide = m_z_63_00_dimethyl_sulfide,
  isopren = m_z_69_00_isopren,
  butanone = m_z_73_00_2_butanone,
  benzen = m_z_79_00_benzen,
  butandion = m_z_87_00_2_3_butandion,
  phenol = m_z_95_00_phenol,
  X4_ethyl_phenol = m_z_123_00_4_ethyl_phenol,
  methyl_indole = m_z_132_00_3_methyl_indole
)
#change column time name for consistency with Picarro
names(dt)[names(dt) == "Absolute Time"] <- "date.time"
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
names(dt)
#keep measurements from the start of the experiment
dt <- dt %>%    
  filter(date.time >= ymd_hms("2024-09-18 12:31:48")) %>%
  mutate(valve = as.numeric(valve))
names(dt)
#don't keep benzene column
dt$benzen <- NULL

#save the file, for some reason if I save as a csv and read it in again R rounds the time to the minutes instead of seconds
write.table(dt, "raw.ptrms.valve.txt", row.names = F, sep=",")

