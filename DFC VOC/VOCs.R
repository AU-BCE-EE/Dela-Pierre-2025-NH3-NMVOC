library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)

# List of file paths
files <- c("VOC Data/File 1.xlsx", "VOC Data/File 2.xlsx", "VOC Data/File 3.xlsx")

# Read and combine data from each file
da <- lapply(files, function(file) {
  # Read the file and sheet
  data <- read_excel(file, sheet = "Conc_ppb")
  
  return(data)
})

# Combine all data into a single data frame
data <- bind_rows(da)



data$date.time <- paste(data$`Absolute Time`)
data$date.time <- ymd_hms(data$date.time)
data$date.time <- with_tz(data$date.time, tz = "ETC/GMT-1")  # Adjust to Ecuador Time (ECT)
#Removing unnecessary data#
data <- data[, -c(1, 2, 24:45)]

# Define the start and end
start_time <- ymd_hms("2024-09-18 11:30:00")
end_time <- ymd_hms("2024-09-24 6:59:00")
# Filter the data based on the new timezone
fdata <- data %>%
  filter(date.time >= start_time & date.time <= end_time)

fdata$date.time <- floor_date(fdata$date.time, unit = "hour")


################################################################################################################################


# Read the CSV file containing the NH3 data (MPV position)
mpv_data <- read.csv('mpv.csv')

mpv_data$date.time <- paste(mpv_data$st)
mpv_data$date.time <- ymd_hms(mpv_data$date.time)
mpv_data$date.time <- with_tz(mpv_data$date.time, tz = "ETC/GMT-1")  # Adjust to Ecuador Time (ECT)
mpv_data$date.time <- floor_date(mpv_data$date.time, unit = "hour")

################################################################################################################################





