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


#Round both columns to the nearest second#
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

#write.csv(DFC.bg, file = "bg.end.hours.ppb.csv", row.names = T)
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
MW <- read_excel("VOC_MW.xlsx")
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

dat <- dat[, -c(2, 5:27)]

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
#ggsave("Adjusted/Figure/voc flux.png", plot = vocplot)

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
#ggsave ("Adjusted/Figure/voc flux group.png", plot = vocflux)

###########################################################################
####################################################################
####################################################################


#Cumulative emissions with plot#
# Calculate cumulative emissions using mintegrate function 
cum <- dat
names(cum)[15:33] <- paste0("voc", 1:19)
source("Functions/mintegrate.R")
# Define the VOC column names
voc_cols <- paste0("voc", 1:19)

# Loop over each VOC column and compute cumulative integration
for(i in seq_along(voc_cols)) {
  new_col <- paste0("cum.treat", i)
  cum[[new_col]] <- mintegrate(cum$elapsed.time * 60, 
                               cum[[voc_cols[i]]], 
                               by = cum$valve, method = 'trap')
}

cum <- cum[, -c(4:5, 8:33)]
################################################################################################################################
####Checking#####################################################################################################################
ggplot(cum, aes(treatment, cum.treat2, color = group)) + geom_point()
################################################################################################################################
#Rename vocs
cum <- cum %>%
  rename(
    methanol = cum.treat1,
    H2S = cum.treat2,
    `4_Methylphenol` = cum.treat3,
    acetic_acid = cum.treat4,
    butanoic_acid = cum.treat5,
    pentanoic_acid = cum.treat6,
    propanoic_acid = cum.treat7,
    acetladheyde = cum.treat8,
    formic_acid = cum.treat9,
    methanthiol = cum.treat10,
    acetone = cum.treat11,
    trimethylamine = cum.treat12,
    dimethyl_sulfide = cum.treat13,
    isopren = cum.treat14,
    butanone = cum.treat15,
    butandion = cum.treat16,
    phenol = cum.treat17,
    `4_ethyl_phenol` = cum.treat18,
    methyl_indole = cum.treat19
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
cum_long <- cum %>%
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


####################################################################
# Ensure the treatment variable is a factor with the correct levels
cum_long$treatment <- factor(cum_long$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))
# Add the group information to the long data frame
cum_long <- cum_long %>%
  mutate(Group = factor(voc_groups[VOC], levels = c("Carboxylic Acids", "Indole", "Phenols", "Volatile Sulfur Compounds (VSC)", "Other")))

# Aggregate to get total (cumulative) flux for each treatment and VOC group
cum.voc.total <- cum_long %>%
  group_by(treatment, Group) %>%
  summarize(Total_Flux = sum(Flux, na.rm = TRUE), .groups = 'drop')

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
vocflux <- ggplot(cum.voc.total, aes(x = Group, y = Total_Flux, fill = Group)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.2) +  
  scale_fill_manual(values = custom_colors) +  
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(
    x = "VOC Group",
    y = "Cumulative Flux (mg/m2)"
  ) + 
  facet_wrap(~ treatment, scales = "free", ncol = 3, labeller = as_labeller(facet_labels)) +  
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray40"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "gray50", linewidth = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(size = 15, face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray70", linewidth = 0.4),
    panel.grid = element_line(color = "gray84", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5)
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  )

vocflux
#ggsave ("Adjusted/Figure/voc flux group.png", plot = vocflux)






library(ggplot2)
library(dplyr)
library(tidyr)

# Extract VOC names from voc_groups, excluding benzene
voc_names <- setdiff(names(voc_groups), "benzen")

# Arrange data by elapsed time to ensure proper cumulative sum calculation
dat <- dat %>% arrange(treatment, group, elapsed.time)

# Compute time differences in minutes
dat <- dat %>%
  group_by(treatment, group) %>%
  mutate(time_diff = c(0, diff(elapsed.time * 60))) %>%  # Convert hours to minutes
  ungroup()

# Calculate cumulative sum for each VOC (flux * time difference)
dat_cum <- dat %>%
  group_by(treatment, group) %>%
  arrange(elapsed.time) %>%
  mutate(across(all_of(voc_names), ~ cumsum(.x * time_diff), .names = "cumu_{.col}")) %>%
  ungroup()

# Convert to long format for ggplot
dat_long <- dat_cum %>%
  select(treatment, group, elapsed.time, starts_with("cumu_")) %>%
  pivot_longer(cols = starts_with("cumu_"), names_to = "VOC", values_to = "Cumulative_Flux")

# Clean VOC names for better readability
dat_long$VOC <- gsub("cumu_", "", dat_long$VOC)

# Add VOC group information
dat_long$VOC_Group <- voc_groups[dat_long$VOC]

# Plot using ggplot with facet_wrap
ggplot(dat_long, aes(x = elapsed.time, y = Cumulative_Flux, color = treatment)) +
  geom_line() +
  facet_wrap(~VOC, scales = "free_y") +
  labs(title = "Cumulative VOC Flux Over Time (Excluding Benzene)",
       x = "Elapsed Time (hours)",
       y = "Cumulative Flux (mg m^-2)",
       color = "Treatment") +
  theme_minimal()



# Ensure the treatment variable is a factor with the correct levels
dat_long$treatment <- factor(dat_long$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))
# Add the group information to the long data frame
dat_long <- dat_long %>%
  mutate(Group = factor(voc_groups[VOC], levels = c("Carboxylic Acids", "Indole", "Phenols", "Volatile Sulfur Compounds (VSC)", "Other")))

# Aggregate to get total (cumulative) flux for each treatment and VOC group
dat.voc.total <- dat_long %>%
  group_by(treatment, Group) %>%
  summarize(Total_Flux = sum(Cumulative_Flux, na.rm = TRUE), .groups = 'drop')

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
vocflux <- ggplot(cum.voc.total, aes(x = Group, y = Total_Flux, fill = Group)) +
  geom_col(alpha = 0.8, color = "black", linewidth = 0.2) +  
  scale_fill_manual(values = custom_colors) +  
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(
    x = "VOC Group",
    y = "Cumulative Flux (mg/m2)"
  ) + 
  facet_wrap(~ treatment, scales = "fixed", ncol = 3, labeller = as_labeller(facet_labels)) +  
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray40"),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "gray50", linewidth = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(size = 15, face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray70", linewidth = 0.4),
    panel.grid = element_line(color = "gray84", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5)
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  )

vocflux

