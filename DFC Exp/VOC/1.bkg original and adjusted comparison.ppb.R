# Libraries used to run script
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(ggplot2)
library(readxl)
library(tidyr)
library(gridExtra)

# Prerequisites to run readCRDS function
devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')

# Reading in Picarro data
da <- readCRDS('DFC_Picarro', From = '18.09.2024 12:00:40', To = '24.09.2024 08:00:00', mult = F, tz = "ETC/GMT-1", rm = F)

# Making date.time stamp
da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
da <- da[, -c(2:15, 17:27)]
dat <- rbind(da)


# Importing VOC data
voc <- read.csv('voc data.csv', fill = T, stringsAsFactors = F)
voc$date <- as.Date(voc$date.time, format = "%m/%d/%y %H:%M")
voc$time <- format(as.POSIXct(voc$date.time, format = "%m/%d/%y %H:%M"), "%H:%M:%S")


#Round both columns to the nearest hour#
voc$date.time.v <- paste(voc$date, voc$time)
#weather$date.time.weather <- paste(weather$date, weather$time)
dat$date.time.v <- dat$date.time
#dat$date.time.weather <- dat$date.time
dat$date.time.v <- round_date(dat$date.time.v, unit = "second")
dat$date.time <- round_date(dat$date.time, unit = "second")

#Convert both to POSIXct#
voc$date.time.v <- as.POSIXct(voc$date.time.v, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
#weather$date.time.weather <- as.POSIXct(weather$date.time.weather, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
#dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")


#Merging data#
dat <- left_join(dat, voc, by = c('date.time' = 'date.time.v'), relationship = 'many-to-many')
dat <- na.omit(dat)

################################################################################################################################
####Checking############################################################################################################################
# Check if 'cycle_number' column exists
if("cycle_number" %in% colnames(dat)) {
  # Identify duplicated cycle numbers
  duplicated_cycles <- dat$cycle_number[duplicated(dat$cycle_number)]
  
  # Check if there are any duplicated cycle numbers
  if(length(duplicated_cycles) > 0) {
    print("Duplicated cycle numbers found:")
    print(duplicated_cycles)
  } else {
    print("No duplicated cycle numbers found.")
  }
} else {
  print("The 'cycle_number' column does not exist in the data frame.")
}
################################################################################################################################
################################################################################################################################


#Renaming the column MPVPosition to valve#
names(dat)[names(dat) == "MPVPosition"]  <- "valve"

#Cropping data and taking the last point of each measurement from each vavle#
dat <- filter(dat, !(dat$valve == lead(dat$valve)))

# Selecting points with whole numbers (when the valve change there is a measurement where the valve position
# is in between two valves, these are removed)
dat <- dat[dat$valve == '1' | dat$valve == '2' | dat$valve == '3' | dat$valve == '4' | dat$valve == '5' | dat$valve == '6' | dat$valve == '7' | 
             dat$valve == '8' | dat$valve == '9' | dat$valve == '10' | dat$valve == '11' | dat$valve == '12' | dat$valve == '13' | dat$valve == '14' |
             dat$valve == '15' | dat$valve == '16' | dat$valve == '17' | dat$valve == '18' | dat$valve == '19', ]

#Ordering data according to valve#
split_valve <- split(dat, f = dat$valve)
valve <- paste0("V", unique(dat$valve))
new_da <- NULL


#Calculate elapsed time of splited subset#
for (i in seq_along(split_valve)) {
  subset_data <- split_valve[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
dat<- new_da
dat <- dat %>% select(-benzen)
#Rounding elapsed time to days#
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24
################################################################################################################################

#Assign names to valve valveues#
valve_data <- dat %>%
  mutate(treatment = recode(valve,
                            `1` = 'Mp',
                            `2` = '0-bp',
                            `3` = '1.5',
                            `4` = '2.9',
                            `5` = 'bkg',
                            `6` = '0-bp',
                            `7` = '5.7',
                            `8` = 'Mp',
                            `9` = 'bkg',
                            `10` = '1.5',
                            `11` = '2.9',
                            `12` = 'Mp',
                            `13` = '5.7',
                            `14` = '0-bp',
                            `15` = '1.5',
                            `16` = 'bkg',
                            `17` = '2.9',
                            `18` = 'Mp',
                            `19` = '5.7'
  ),
  group = case_when(
    valve %in% c(2, 6, 14) ~ 'No acid',
    valve %in% c(3, 10, 15) ~ 'Low acid',
    valve %in% c(4, 11, 17) ~ 'Medium acid',
    valve %in% c(7, 13, 19) ~ 'High acid',
    valve %in% c(5, 9, 16) ~ 'Background',
    valve %in% c(1, 8, 12, 18) ~ 'Machine plot'
  )
  )
dat <- rbind(valve_data)

#Rename vocs
dat <- dat %>%
  rename(
    voc1 = methanol,
    voc2 = H2S,
    voc3 = X4.Methylphenol,
    voc4 = acetic_acid,
    voc5 = butanoic_acid,
    voc6 = pentanoic_acid,
    voc7 = propanoic_acid,
    voc8 = acetladheyde,
    voc9 = formic_acid,
    voc10 = methanthiol,
    voc11 = acetone,
    voc12 = trimethylamine,
    voc13 = dimethyl_sulfide,
    voc14 = isopren,
    voc15 = butanone,
    voc16 = butandion,
    voc17 = phenol,
    voc18 = X4_ethyl_phenol,
    voc19 = methyl_indole
  )


#Background corrected concentration# 
#Background data#

DFC.bg <- dat %>%
  filter(group == "Background") %>%
  mutate(group = "Original")  # Rename "Background" to "Original"

DFC.bg <- DFC.bg %>%
  filter(elapsed.time >= 0 & elapsed.time <= 124)

# Filter for data in the last 24 hours
Bg.filtered <- DFC.bg %>%
  filter(elapsed.time >= 101 & elapsed.time <= 124) %>%
  mutate(group = "endhours")

# Convert to data.table 
DFC.bg.end <- DFC.bg
setDT(DFC.bg.end)
voc_columns <- paste0("voc", 1:20)  


# Split data according to valve
Split.bg <- split(DFC.bg.end, f = DFC.bg.end$valve)
Split.filter.bg <- split(Bg.filtered, f = Bg.filtered$valve)

# Repeat the last 10 rows in Split.filter.bg to fill up to 54 rows for each valve
for (i in seq_along(Split.filter.bg)) {
  # Get the valve number
  valve_num <- names(Split.filter.bg)[i]
  
  # Get the filtered data for this valve
  filtered_data <- Split.filter.bg[[i]]
  
  # Repeat the last 10 rows to match the number of rows in Split.bg (54 rows)
  if (nrow(filtered_data) < 50) {
    # Calculate how many times to repeat the data
    repeat_times <- ceiling(50 / nrow(filtered_data))
    
    # Repeat the rows and trim to exactly 54 rows
    repeated_data <- filtered_data[rep(1:nrow(filtered_data), repeat_times), ]
    repeated_data <- repeated_data[1:50, ]
    
    # Replace the corresponding valve data in Split.bg with the repeated data
    Split.bg[[i]] <- repeated_data
  }
}

# Merge all valve data back into one data frame
DFC.bg.end <- do.call(rbind, Split.bg)

#Delete the 'elapsed.time' column from DFC.bg.end
DFC.bg.end$elapsed.time <- NULL

#Copy 'elapsed.time' from DFC.bg to DFC.bg.end
DFC.bg.end$elapsed.time <- DFC.bg$elapsed.time

# Get the names of all VOC columns in Bg.filtered
voc_columns <- c("voc1", "voc2", "voc3", "voc4", "voc5", "voc6", "voc7", "voc8", "voc9", "voc10", 
                 "voc11", "voc12", "voc13", "voc14", "voc15", "voc16", "voc17", "voc18", "voc19")

#write.csv(DFC.bg, file = "bg.end.hours.ppb.csv", row.names = T)
# Subset the data for the DFC outlet data
DFC <- dat[valve_data$group %in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Machine plot'), ]
names(DFC)[7:25] <- paste0("voc.dfc", 1:19)
DFC <- DFC %>%
  filter(elapsed.time >= 0 & elapsed.time <= 124)


#Convert voc columns to numeric
DFC.bg[, (voc_columns) := lapply(.SD, as.numeric), .SDcols = voc_columns]

# Summarize the data by calculating the mean for each elapsed time
DFC.bg.summ <- DFC.bg %>%
  group_by(elapsed.time) %>%
  summarise(across(all_of(voc_columns), ~mean(.x, na.rm = TRUE))) %>%
  ungroup()


# Convert the VOC columns from character to numeric
#DFC.bg.end[DFC.bg.end[, (voc_columns) := lapply(.SD, as.numeric), .SDcols = voc_columns]
DFC.bg.end[, (voc_columns) := lapply(.SD, as.numeric), .SDcols = voc_columns]

# Summarize the data by calculating the mean for each elapsed time
DFC.bg.end.summ <- DFC.bg.end %>%
  group_by(elapsed.time) %>%
  summarise(across(all_of(voc_columns), ~mean(.x, na.rm = TRUE))) %>%
  ungroup()

DFC.bg.summ <- merge(DFC.bg.summ, DFC.bg[, c("elapsed.time", "group", "treatment")], 
                         by = "elapsed.time", all.x = TRUE, allow.cartesian = TRUE)
DFC.bg.end.summ <- merge(DFC.bg.end.summ, DFC.bg.end[, c("elapsed.time", "group", "treatment")], 
                     by = "elapsed.time", all.x = TRUE, allow.cartesian = TRUE)

################################################################################################################################
################################################################################################################################
# Combine both datasets
background <- rbindlist(list(DFC.bg.end.summ, DFC.bg.summ), use.names = TRUE, fill = TRUE)
################################################################################################################################
################################################################################################################################

# Rename the columns and filter the data
bkg.com <- background %>%
  rename(
    methanol = voc1,
    H2S = voc2,
    `4_Methylphenol` = voc3,
    acetic_acid = voc4,
    butanoic_acid = voc5,
    pentanoic_acid = voc6,
    propanoic_acid = voc7,
    acetladheyde = voc8,
    formic_acid = voc9,
    methanethiol = voc10,
    acetone = voc11,
    trimethylamine = voc12,
    dimethyl_sulfide = voc13,
    isopren = voc14,
    butanone = voc15,
    butandion = voc16,
    phenol = voc17,
    `4_ethyl_phenol` = voc18,
    methyl_indole = voc19
  ) %>%
  filter(elapsed.time >= 0 & elapsed.time <= 124) %>%
  pivot_longer(
    cols = methanol:methyl_indole,  # Use proper column range
    names_to = "compound",
    values_to = "concentration"
  )
# Set compound order
desired_order <- c("acetic_acid", "acetladheyde", "acetone", "butandion", "butanoic_acid", 
                   "butanone", "dimethyl_sulfide", "formic_acid", "H2S", "isopren", 
                   "methanol", "methanethiol", "methyl_indole", "pentanoic_acid", 
                   "phenol", "propanoic_acid", "trimethylamine", "4_ethyl_phenol", 
                   "4_Methylphenol")

# Fix incorrect reference to 'all$compound'
bkg.com$compound <- factor(bkg.com$compound, levels = desired_order)

all <- ggplot(bkg.com, aes(x = elapsed.time, y = concentration, colour = group)) + 
  geom_point(size = 0.4) +  # Adjust point size
  geom_line() +  
  facet_wrap(~ compound, scales = "free_y") + 
  labs(x = "Time after slurry application (hours)", y = "ppb")+
  theme_minimal(); all
#ggsave("Figure/ppb/bkg original and adjusted comparison.ppb.png", plot = all)
