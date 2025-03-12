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
voc <- read.csv('/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/VOC Data/voc data.csv', fill = T, stringsAsFactors = F)
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
voc_columns <- paste0("voc", 1:19)  


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

# Subset the data for the DFC outlet data
DFC <- dat[valve_data$group %in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Machine plot'), ]
names(DFC)[7:25] <- paste0("voc.dfc", 1:19)
DFC <- DFC %>%
  filter(elapsed.time >= 0 & elapsed.time <= 124)


# Mean background values
# Create a data frame with the columns to be aggregated
voc_columns <- c("voc1", "voc2", "voc3", "voc4", "voc5", 
                 "voc6", "voc7", "voc8", "voc9", "voc10", 
                 "voc11", "voc12", "voc13", "voc14", "voc15", 
                 "voc16", "voc17", "voc18", "voc19")

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

# Rename the columns of the summarized data
names(DFC.bg.end.summ)[2:20] <- paste0("voc.bg", 1:19)

DFC <- DFC %>%
  mutate(across(starts_with("voc"), ~ as.numeric(as.character(.)))) 


#Joining average background and outlet data#
DFC <- full_join(DFC.bg.end.summ, DFC, by = 'elapsed.time')


#subtract background
for (i in 1:19) {
  # Define column names
  dfc_col <- paste0("voc.dfc", i)
  bg_col <- paste0("voc.bg", i)
  
  # Check if columns exist in the data frame
  if (dfc_col %in% names(DFC) && bg_col %in% names(DFC)) {
    # Subtract background from outlet and store in a new column
    DFC[[paste0("voc_corr", i)]] <- DFC[[dfc_col]] - DFC[[bg_col]]
  } 
}

#Rebind again in dat datasheet#
dat <- rbind(DFC)
dat <- dat[order(dat$treatment), ]

names(dat)[which(names(dat) %in% paste0("voc_corr", 1:19))] <- paste0("voc", 1:19)
################################################################################################################################
################################################################################################################################
#---- Detection limit----#

# Convert VOC columns to numeric in both DFC.bg and DFC
DFC.bg[, (voc_columns) := lapply(.SD, as.numeric), .SDcols = voc_columns]

# Calculate the standard deviation for each VOC column in DFC.bg
sd_values <- DFC.bg[, lapply(.SD, sd, na.rm = TRUE), .SDcols = voc_columns]

# Calculate the detection limit (three times the standard deviation)
detection_limit <- sd_values * 2

# Apply detection limit to 'voc_corr' columns in 'dat'
for (col in voc_columns) {
  dat[[col]] <- ifelse(dat[[col]] < detection_limit[[col]], detection_limit[[col]], dat[[col]])
}

names(dat)[which(names(dat) %in% paste0("voc", 1:19))] <- paste0("voc_corr", 1:19)
##############################################################################
##############################################################################
#-----------Negative values percent----------------------

dat.neg <- dat %>%
  select(starts_with("voc_corr"))
names(dat)[7:25] <- paste0("voc", 1:19)

# Rename compounds in your dat dataset
dat.neg <- dat.neg %>%
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
    methanthiol = voc10,
    acetone = voc11,
    trimethylamine = voc12,
    dimethyl_sulfide = voc13,
    isopren = voc14,
    butanone = voc15,
    butandion = voc16,
    phenol = voc17,
    `4_ethyl_phenol` = voc18,
    methyl_indole = voc19
  )

#Calculate the percentage of negative values per compound
negative_percentage <- sapply(dat.neg, function(compound_col) {
  # Calculate the percentage of negative values for this compound
  negative_count <- sum(compound_col < 0, na.rm = TRUE)   # count negative values
  total_count <- sum(!is.na(compound_col))                   # total non-NA values
  return(negative_count / total_count * 100)                 # percentage of negative values
})

#Create a data frame for plotting
negative_df <- data.frame(
  Compound = c("methanol", "H2S", "4_Methylphenol", "acetic_acid", "butanoic_acid", 
               "pentanoic_acid", "propanoic_acid", "acetladheyde", "formic_acid", 
               "methanthiol", "acetone", "trimethylamine", "dimethyl_sulfide", 
               "isopren", "butanone", "butandion", "phenol", "4_ethyl_phenol", 
               "methyl_indole"),
  Negative_Percentage = negative_percentage
)
#Plot the results
neg <- ggplot(negative_df, aes(x = Negative_Percentage, y = Compound )) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme_minimal() +
  labs(title = "Percentage of Negative Values per Compound", 
       x = "Compound", 
       y = "Negative Value Percentage (%)") +
  theme(axis.text.x = element_text()) +
  scale_x_continuous(breaks = seq(0, 100, by = 5));neg
#ggsave ("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Figure/Neg values like NH3 treatments.png", plot = neg)
##############################################################################
##############################################################################
##############################################################################
##############################################################################


#Import Weather Data and filter data#
header <- c('date', 'time', 'temp')
weather <- read.csv('Temp.csv', fill = T, stringsAsFactors = F)
weather <- weather[, -1 ]
colnames(weather) <- header

#Changing date format in excel file#
weather$date <- parse_date_time(weather$date, orders = c("d/m/y", "d-m-y"))

#Selecting experiment date range#
start_date <- dmy("18/09/2024")
end_date <- dmy("24/09/2024")

# Filter the data within the date range
weather <- weather[weather$date >= start_date & weather$date <= end_date, ]

################################################################################################################################

#Round both columns to the nearest hour#
weather$time <- as.numeric(weather$time)
weather$date.time.weather <- paste(weather$date, weather$time)
dat$date.time.weather <- dat$date.time
dat$date.time.weather <- round_date(dat$date.time.weather, unit = "hour")
dat$date.time <- round_date(dat$date.time, unit = "hour")
head(dat)
#Convert both to POSIXct#
weather$date.time.weather <- sprintf("%s %02d:00", weather$date, weather$time)
weather$date.time.weather <- as.POSIXct(weather$date.time.weather, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
head(weather$date.time.weather)

#Merging data#
dat <- left_join(dat, weather, by = c('date.time' = 'date.time.weather'))
voc_ppb <- dat
voc_ppb <- voc_ppb [, -c(2:22, 25:47)]

################################################################################################################################

#Convert temperature from C to F#
dat$temp <- as.numeric(dat$temp)
dat$air.temp.K <- dat$temp + 273.15
################################################################################################################################

#Air flow Calculation#
dat$air.flow <- 2.28 * 1000 # L min^-1 

#Chamber Area Calculation#
dat$dfc.area <- (0.7/2)**2 * 3.14 #m^2


#Constants for flux calculation#
#Atmospheric constant#
atm.con <- 1 #atm

#Gas constant [L * atm * K^-1 * mol^-1]#
g.con <- 0.082057338 

#Mass of VOCs [g * mol^-1]#
MW <- read_excel("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/VOC_MW.xlsx")
head(MW)

# Convert the MW tibble to a usable format
MW_long <- MW %>%
  pivot_longer(-compund, names_to = "VOC", values_to = "value") %>%
  pivot_wider(names_from = compund, values_from = value) %>%
  mutate(MW = as.numeric(MW))

# Check if essential columns exist in dat
required_columns <- c("air.temp.K", "air.flow", "dfc.area")
missing_columns <- setdiff(required_columns, names(dat))
if (length(missing_columns) > 0) {
  stop(paste("The following required columns are missing in dat:", paste(missing_columns, collapse = ", ")))
}
# Loop through 1 to 20 to convert voc_corr from ppb to mol and calculate flux
for (i in 1:19) {
  voc_bg_col <- paste0("voc_corr", i)
  voc_mol_col <- paste0("voc_corr_mol", i)
  flux_voc_col <- paste0("voc_flux", i)
  
  # Get the molecular weight for the current VOC
  voc_id <- paste0("voc", i)
  mw_value <- MW_long %>% filter(RN == voc_id) %>% pull(MW)
  
  # Debugging: Print voc_id, voc_corr_col, and mw_value
  print(paste("Processing", voc_id, "with column", voc_bg_col, "and MW", mw_value))
  
  # Check if the voc_corr column exists in the data frame and molecular weight is available
  if (voc_bg_col %in% names(dat) && length(mw_value) > 0 && !is.na(mw_value)) {
    
    # Convert voc_corr from ppb to mol
    dat[[voc_mol_col]] <- (atm.con / (g.con * dat$air.temp.K)) * dat[[voc_bg_col]] * 10^-9
    
    # Calculate flux from mol * L^-1 to mg * min^-1 * m^-2
    if (length(dat[[voc_bg_col]]) > 0 && length(dat$air.flow) > 0 && length(dat$dfc.area) > 0 && length(dat[[voc_mol_col]]) > 0) {
      dat[[flux_voc_col]] <- ((dat[[voc_mol_col]] * mw_value * dat$air.flow) / dat$dfc.area) * 1000
    } else {
      warning(paste("One or more required columns are empty for flux calculation:", flux_voc_col))
    }
  } else {
    warning(paste("Column", voc_bg_col, "does not exist in the data frame, or molecular weight is missing"))
  }
}

# Check the result
head(dat)

# Identify the columns to be deleted
columns_to_delete <- c(
  paste0("voc_corr_mol", 1:19),
  paste0("voc.bg", 1:19),
  paste0("voc_corr", 1:19)
  
)

# Remove the identified columns from the dat data frame
dat <- dat %>% select(-all_of(columns_to_delete))

dat <- dat[, -c(2, 4:29)]

#Rename vocs
dat <- dat %>%
  rename(
    methanol = voc_flux1,
    H2S = voc_flux2,
    `4_Methylphenol` = voc_flux3,
    acetic_acid = voc_flux4,
    butanoic_acid = voc_flux5,
    pentanoic_acid = voc_flux6,
    propanoic_acid = voc_flux7,
    acetladheyde = voc_flux8,
    formic_acid = voc_flux9,
    methanthiol = voc_flux10,
    acetone = voc_flux11,
    trimethylamine = voc_flux12,
    dimethyl_sulfide = voc_flux13,
    isopren = voc_flux14,
    butanone = voc_flux15,
    butandion = voc_flux16,
    phenol = voc_flux17,
    `4_ethyl_phenol` = voc_flux18,
    methyl_indole = voc_flux19
  )


# Define the groups for each VOC
voc_groups <- c(
  acetic_acid = "Carboxylic Acids",
  butanoic_acid = "Carboxylic Acids",
  pentanoic_acid = "Carboxylic Acids",
  propanoic_acid = "Carboxylic Acids",
  formic_acid = "Carboxylic Acids",
  methyl_indole = "Indole",
  `4_Methylphenol` = "Phenols",
  phenol = "Phenols",
  `4_ethyl_phenol` = "Phenols",
  H2S = "Volatile Sulfur Compounds (VSC)",
  methanthiol = "Volatile Sulfur Compounds (VSC)",
  dimethyl_sulfide = "Volatile Sulfur Compounds (VSC)",
  methanol = "Other",
  acetladheyde = "Other",
  acetone = "Other",
  trimethylamine = "Other",
  isopren = "Other",
  butanone = "Other",
  benzen = "Other",
  butandion = "Other"
)

# Convert data to long format
dat_long <- dat %>%
  pivot_longer(
    cols = c(methanol:H2S, `4_Methylphenol`:methyl_indole), 
    names_to = "VOC", 
    values_to = "Flux"
  )
desired_order <- c("acetic_acid", "acetladheyde", "acetone", "butandion", "butanoic_acid", 
                   "butanone", "dimethyl_sulfide", "formic_acid", "H2S", "isopren", 
                   "methanol", "methanthiol", "methyl_indole", "pentanoic_acid", 
                   "phenol", "propanoic_acid", "trimethylamine", "4_ethyl_phenol", 
                   "4_Methylphenol")

dat_long$VOC<- factor(dat_long$VOC, levels = desired_order)
# Create the plot
vocplot <- ggplot(dat_long, aes(x = elapsed.time, y = Flux, colour = group)) + 
  geom_point(size = 0.4) +  # Adjust point size
  geom_line(aes(group = valve), linewidth = 0.5) +  
  facet_wrap(~ VOC, scales = "free_y") + 
  labs(x = "Time after slurry application (hours)", y = ("mg/m2/min")) +
  theme_minimal();vocplot
ggsave("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Figure/VOC flux sd itself.png", plot = vocplot)

####################################################################
# Ensure the treatment variable is a factor with the correct levels
dat_long$treatment <- factor(dat_long$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))
# Add the group information to the long data frame
dat_long <- dat_long %>%
  mutate(Group = factor(voc_groups[VOC], levels = c("Carboxylic Acids", "Indole", "Phenols", "Volatile Sulfur Compounds (VSC)", "Other")))

# Aggregate data for all treatments and groups
sum.voc <- dat_long %>%
  group_by(elapsed.time, treatment, Group) %>%
  summarize(Sum_Flux = sum(Flux, na.rm = TRUE), .groups = 'drop')

custom_colors <- c(
  "Carboxylic Acids" = "#E69F00", 
  "Indole" = "#56B4E9", 
  "Other" = "#009E73", 
  "Phenols" = "#F0E442", 
  "Volatile Sulfur Compounds (VSC)" = "#CC79A7"
)

# Define a named vector for the facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid',
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

# Generate the faceted plot with the corrected custom colors and facet labels
vocflux <- ggplot(sum.voc, aes(x = elapsed.time, y = Sum_Flux, fill = Group)) +
  geom_area(alpha = 0.8, position = "stack", color = "black", linewidth = 0.2) +  # Black outline
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(
    x = "Time after slurry application (hours)",
    y = ("mg/m2/min")
  ) + 
  facet_wrap(~ treatment, scales = "fixed", ncol = 3, labeller = as_labeller(facet_labels)) +  # Flexible grid layout with custom labels
  theme_minimal(base_size = 15) +  # Increase base font size for larger elements
  theme(
    # Titles
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray40"),
    
    # Axes
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "gray50", linewidth = 0.5),
    
    # Legend
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    
    # Facets
    strip.text = element_text(size = 15, face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray70", linewidth = 0.4),
    
    # Background and Grid
    panel.grid = element_line(color = "gray84", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5)
  ) +
  guides(
    fill = guide_legend(nrow = 1)  # Arrange legend in a single row
  ); vocflux
#ggsave ("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Figure/VOC Flux SD itself grouped.png", plot = vocflux)


####################################################################
####################################################################
####################################################################
#---- Cummulative by mintegrate----------------

cum.dat <- dat  
names(cum.dat)[12:30] <- paste0("voc", 1:19)
# Define the VOC column names
voc_cols <- paste0("voc", 1:19)

# Calculatedate.time# Calculate cumulative emissions using mintegrate function 
source("Functions/mintegrate.R")

for (i in seq_along(voc_cols)) {
  new_col <- paste0("cum.treat", i)
  cat("Processing VOC", i, "(", voc_cols[i], ")\n")
  cum.dat[[new_col]] <- mintegrate(cum.dat$elapsed.time * 60, 
                                   cum.dat[[voc_cols[i]]], 
                                   by = cum.dat$valve, method = 'trap')
}

cum.dat <- cum.dat [, -c(5:30)]

#Cumulative emissions by treatment from mintegrate function
cum.dat <- cum.dat %>%
  rename_with(~paste0("cum.emis", seq_along(.)), starts_with("cum.treat")) %>%
  group_by(treatment)


#Filter the data to get the last time point for each valve-treatment group
dat_last <- cum.dat %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

# Create a summary dataset for plotting (one point per treatment group)
indsum <- dat_last %>%
  select(valve, treatment, elapsed.time, starts_with("cum.emis")) %>%
  distinct()

# Summarize/mean cumulative emissions by treatment
cumsum <- aggregate(. ~ treatment, data = indsum, FUN = function(x) mean(x, na.rm = TRUE))

# Rename columns
# Create a summary dataset for plotting (one point per treatment group)
indsum_new <- dat_last %>%
  select(valve, treatment, elapsed.time, group, starts_with("cum.emis")) %>%
  distinct()

indsum_new <- indsum_new %>%
  rename(
    methanol = cum.emis1,
    H2S = cum.emis2,
    `4_Methylphenol` = cum.emis3,
    acetic_acid = cum.emis4,
    butanoic_acid = cum.emis5,
    pentanoic_acid = cum.emis6,
    propanoic_acid = cum.emis7,
    acetladheyde = cum.emis8,
    formic_acid = cum.emis9,
    methanthiol = cum.emis10,
    acetone = cum.emis11,
    trimethylamine = cum.emis12,
    dimethyl_sulfide = cum.emis13,
    isopren = cum.emis14,
    butanone = cum.emis15,
    butandion = cum.emis16,
    phenol = cum.emis17,
    `4_ethyl_phenol` = cum.emis18,
    methyl_indole = cum.emis19
  )

# Convert data to long format
indsum_long <- indsum_new %>%
  pivot_longer(
    cols = -c(valve, treatment, group, elapsed.time),  # Select all columns except valve & treatment
    names_to = "VOC", 
    values_to = "emis"
  )

# Fix whitespace issue in desired_order
desired_order <- c("acetic_acid", "acetladheyde", "acetone", "butandion", "butanoic_acid", 
                   "butanone", "dimethyl_sulfide", "formic_acid", "H2S", "isopren", 
                   "methanol", "methanthiol", "methyl_indole", "pentanoic_acid", 
                   "phenol", "propanoic_acid", "trimethylamine", "4_ethyl_phenol", 
                   "4_Methylphenol")

# Define VOC groups
category<- c(
  acetic_acid = "Carboxylic Acids",
  butanoic_acid = "Carboxylic Acids",
  pentanoic_acid = "Carboxylic Acids",
  propanoic_acid = "Carboxylic Acids",
  formic_acid = "Carboxylic Acids",
  methyl_indole = "Indole",
  `4_Methylphenol` = "Phenols",
  phenol = "Phenols",
  `4_ethyl_phenol` = "Phenols",
  H2S = "Volatile Sulfur Compounds (VSC)",
  methanthiol = "Volatile Sulfur Compounds (VSC)",
  dimethyl_sulfide = "Volatile Sulfur Compounds (VSC)",
  methanol = "Other",
  acetladheyde = "Other",
  acetone = "Other",
  trimethylamine = "Other",
  isopren = "Other",
  butanone = "Other",
  butandion = "Other"
)

# Apply factor ordering
indsum_long <- indsum_long %>%
  mutate(
    VOC = factor(VOC, levels = desired_order),
    category = recode(VOC, !!!category)  # Assign group categories
  )

# Plot cumulative emissions for each treatment across all cum.emis columns
cumsum_plot <- ggplot(indsum_long, aes(x = treatment, y = emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(aes(x = treatment, y = emis, color = treatment), 
               show.legend = FALSE) +  
  facet_wrap(~VOC, scales = "free_y") +  # Create separate plots for each cum.emis column
  theme_bw() +
  labs(
    title = "Cumulative Emissions by Treatment",  
    x = "Treatment",  
    y = "mg/m2"
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 10),  # Adjust facet label size
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); cumsum_plot
#ggsave ("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Figure/cumulative SD itself treatments.png", plot = cumsum_plot)


#################################################
#------Plot category mintegrate--------
# Ensure the treatment variable is a factor with the correct levels
indsum_long$treatment <- factor(indsum_long$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))


indsum_mean <- indsum_long %>%
  group_by(elapsed.time, treatment, VOC, category, group) %>%
  summarise(mean_emis = mean(emis, na.rm = TRUE), .groups = "drop")

indsum_cat <- indsum_mean %>%
  group_by(treatment, category) %>%
  summarise(mean_emis = sum(mean_emis, na.rm = TRUE), .groups = "drop")

# Define custom colors for categories
custom_colors <- c(
  "Carboxylic Acids" = "#E69F00", 
  "Indole" = "#56B4E9", 
  "Other" = "#009E73", 
  "Phenols" = "#F0E442", 
  "Volatile Sulfur Compounds (VSC)" = "#CC79A7"
)

# Define a named vector for facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid', 
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

# Create the bar plot with facet_wrap
grouped <- ggplot(indsum_cat, aes(x = category, y = mean_emis, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bar plot
  facet_wrap(~treatment, labeller = as_labeller(facet_labels)) +  # Facet by treatment
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    y = "mg/m2",  # Label for y-axis
    fill = "Category"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(),  # Remove y-axis title
    strip.text = element_text(size = 12, face = "bold")  # Format facet labels
  );grouped

#ggsave ("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Figure/cumulative SD 2 itself grouped.png", plot = grouped)


#################################################
#------cumulative cumsum--------
#------Flavia--------
cum.f <- dat

names(cum.f)[12:30] <- paste0("voc", 1:19)
# Define the VOC column names
voc_cols <- paste0("voc", 1:19)


cum.f <- cum.f %>%
  group_by(valve, treatment) %>%                         # Group by 'valve' and 'treatment'
  arrange(elapsed.time) %>%                              # Ensure data is sorted by elapsed time
  mutate(across(starts_with("voc"),                       # Apply transformation to VOC columns
                ~ cumsum(replace_na(. * (elapsed.time * 60), 0)))) %>%  # Multiply by 60 to convert hours to minutes
  ungroup()
# Remove grouping structure

cum.f <- cum.f [, -c(5:11)]

#Cumulative emissions by treatment from mintegrate function
cum.f <- cum.f %>%
  rename_with(~paste0("cum.emis", seq_along(.)), starts_with("voc")) %>%
  group_by(treatment)


#Filter the data to get the last time point for each valve-treatment group
dat_f <- cum.f %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

# Create a summary dataset for plotting (one point per treatment group)
indsum.f <- dat_f %>%
  select(valve, treatment, elapsed.time, starts_with("cum.emis")) %>%
  distinct()

# Summarize/mean cumulative emissions by treatment
cumsum.f <- aggregate(. ~ treatment, data = indsum, FUN = function(x) mean(x, na.rm = TRUE))

# Rename columns
# Create a summary dataset for plotting (one point per treatment group)
indsum_new.f <- dat_f %>%
  select(valve, treatment, elapsed.time, group, starts_with("cum.emis")) %>%
  distinct()

indsum_new.f <- indsum_new.f %>%
  rename(
    methanol = cum.emis1,
    H2S = cum.emis2,
    `4_Methylphenol` = cum.emis3,
    acetic_acid = cum.emis4,
    butanoic_acid = cum.emis5,
    pentanoic_acid = cum.emis6,
    propanoic_acid = cum.emis7,
    acetladheyde = cum.emis8,
    formic_acid = cum.emis9,
    methanthiol = cum.emis10,
    acetone = cum.emis11,
    trimethylamine = cum.emis12,
    dimethyl_sulfide = cum.emis13,
    isopren = cum.emis14,
    butanone = cum.emis15,
    butandion = cum.emis16,
    phenol = cum.emis17,
    `4_ethyl_phenol` = cum.emis18,
    methyl_indole = cum.emis19
  )

# Convert data to long format
indsum_long.f <- indsum_new.f %>%
  pivot_longer(
    cols = -c(valve, treatment, group, elapsed.time),  # Select all columns except valve & treatment
    names_to = "VOC", 
    values_to = "emis"
  )

# Apply factor ordering
indsum_long.f <- indsum_long.f %>%
  mutate(
    VOC = factor(VOC, levels = desired_order),
    category = recode(VOC, !!!category)  # Assign group categories
  )

# Plot cumulative emissions for each treatment across all cum.emis columns
cumsum_plot <- ggplot(indsum_long.f, aes(x = treatment, y = emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(aes(x = treatment, y = emis, color = treatment), 
               show.legend = FALSE) +  
  facet_wrap(~VOC, scales = "free_y") +  # Create separate plots for each cum.emis column
  theme_bw() +
  labs(
    title = "Cumulative Emissions by Treatment",  
    x = "Treatment",  
    y = "Cumulative Emission Value"
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 10),  # Adjust facet label size
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); cumsum_plot

#------Plot cumulative cumsum--------
# Ensure the treatment variable is a factor with the correct levels
indsum_long.f$treatment <- factor(indsum_long.f$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))


indsum_mean.f <- indsum_long.f %>%
  group_by(elapsed.time, treatment, VOC, category, group) %>%
  summarise(mean_emis = mean(emis, na.rm = TRUE), .groups = "drop")

indsum_cat.f <- indsum_mean.f %>%
  group_by(treatment, category) %>%
  summarise(mean_emis = sum(mean_emis, na.rm = TRUE), .groups = "drop")

# Define a named vector for facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid', 
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

# Create the bar plot with facet_wrap
ggplot(indsum_cat.f, aes(x = category, y = mean_emis, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bar plot
  facet_wrap(~treatment, labeller = as_labeller(facet_labels)) +  # Facet by treatment
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    y = "Mean Emission",  # Label for y-axis
    fill = "Category"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(),  # Remove y-axis title
    strip.text = element_text(size = 12, face = "bold")  # Format facet labels
  )
###################################
#################################################
#------cumulative Trap--------
#------Flavia--------

cum.t <- dat

names(cum.t)[12:30] <- paste0("voc", 1:19)
# Define the VOC column names
voc_cols <- paste0("voc", 1:19)

cum.t <- cum.t %>%
  group_by(valve, treatment) %>%                # Group by 'valve' and 'treatment'
  arrange(elapsed.time) %>%                     # Ensure data is sorted by elapsed time
  mutate(across(voc_cols, ~ {
    # Calculate the time intervals in minutes
    time_intervals <- c(0, diff(elapsed.time) * 60)  # Convert to minutes
    
    # Handle the first row where lag(.) would be NA
    lagged_values <- lag(.) # lag the VOC values
    lagged_values[1] <- 0   # Set the lag for the first row to 0 (it has no previous value)
    
    # Apply the trapezoidal rule to calculate cumulative emissions
    cum_emissions <- cumsum(time_intervals * (lagged_values + .) / 2) # Trapezoidal integration
    return(cum_emissions)
  })) %>%
  ungroup()
# Remove grouping structure

cum.t <- cum.t [, -c(5:11)]

#Cumulative emissions by treatment from mintegrate function
cum.t <- cum.t %>%
  rename_with(~paste0("cum.emis", seq_along(.)), starts_with("voc")) %>%
  group_by(treatment)


#Filter the data to get the last time point for each valve-treatment group
dat_t <- cum.t %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

# Create a summary dataset for plotting (one point per treatment group)
indsum.t <- dat_t %>%
  select(valve, treatment, elapsed.time, starts_with("cum.emis")) %>%
  distinct()

# Summarize/mean cumulative emissions by treatment
cumsum.t <- aggregate(. ~ treatment, data = indsum, FUN = function(x) mean(x, na.rm = TRUE))

# Rename columns
# Create a summary dataset for plotting (one point per treatment group)
indsum_new.t <- dat_t %>%
  select(valve, treatment, elapsed.time, group, starts_with("cum.emis")) %>%
  distinct()

indsum_new.t <- indsum_new.t %>%
  rename(
    methanol = cum.emis1,
    H2S = cum.emis2,
    `4_Methylphenol` = cum.emis3,
    acetic_acid = cum.emis4,
    butanoic_acid = cum.emis5,
    pentanoic_acid = cum.emis6,
    propanoic_acid = cum.emis7,
    acetladheyde = cum.emis8,
    formic_acid = cum.emis9,
    methanthiol = cum.emis10,
    acetone = cum.emis11,
    trimethylamine = cum.emis12,
    dimethyl_sulfide = cum.emis13,
    isopren = cum.emis14,
    butanone = cum.emis15,
    butandion = cum.emis16,
    phenol = cum.emis17,
    `4_ethyl_phenol` = cum.emis18,
    methyl_indole = cum.emis19
  )

# Convert data to long format
indsum_long.t <- indsum_new.t %>%
  pivot_longer(
    cols = -c(valve, treatment, group, elapsed.time),  # Select all columns except valve & treatment
    names_to = "VOC", 
    values_to = "emis"
  )

# Apply factor ordering
indsum_long.t <- indsum_long.t %>%
  mutate(
    VOC = factor(VOC, levels = desired_order),
    category = recode(VOC, !!!category)  # Assign group categories
  )

# Plot cumulative emissions for each treatment across all cum.emis columns
cumsum_plot <- ggplot(indsum_long.t, aes(x = treatment, y = emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(aes(x = treatment, y = emis, color = treatment), 
               show.legend = FALSE) +  
  facet_wrap(~VOC, scales = "free_y") +  # Create separate plots for each cum.emis column
  theme_bw() +
  labs(
    title = "Cumulative Emissions by Treatment",  
    x = "Treatment",  
    y = "Cumulative Emission Value"
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 10),  # Adjust facet label size
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); cumsum_plot

#------Plot cumulative cumsum--------
# Ensure the treatment variable is a factor with the correct levels
indsum_long.t$treatment <- factor(indsum_long.t$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))


indsum_mean.t <- indsum_long.t %>%
  group_by(elapsed.time, treatment, VOC, category, group) %>%
  summarise(mean_emis = mean(emis, na.rm = TRUE), .groups = "drop")

indsum_cat.t <- indsum_mean.t %>%
  group_by(treatment, category) %>%
  summarise(mean_emis = sum(mean_emis, na.rm = TRUE), .groups = "drop")

# Define a named vector for facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid', 
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

# Create the bar plot with facet_wrap
ggplot(indsum_cat.t, aes(x = category, y = mean_emis, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bar plot
  facet_wrap(~treatment, labeller = as_labeller(facet_labels)) +  # Facet by treatment
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    y = "Mean Emission",  # Label for y-axis
    fill = "Category"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(),  # Remove y-axis title
    strip.text = element_text(size = 12, face = "bold")  # Format facet labels
  )
