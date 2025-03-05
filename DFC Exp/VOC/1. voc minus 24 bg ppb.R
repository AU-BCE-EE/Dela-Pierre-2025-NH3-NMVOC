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
datall <- dat
dat <- dat[order(dat$treatment), ]
# Identify the columns to be deleted
columns_to_delete <- c(
  paste0("voc.bg", 1:19),
  paste0("voc.dfc", 1:19)
)

# Remove the identified columns from the dat data frame
dat <- dat %>% select(-all_of(columns_to_delete))
dat <- dat[, -c(5:10)]


###############################################################################################################################
###############################################################################################################################

# Rename the columns and filter the data
vocppb <- dat %>%
  rename(
    methanol = voc_corr1,
    H2S = voc_corr2,
    `4_Methylphenol` = voc_corr3,
    acetic_acid = voc_corr4,
    butanoic_acid = voc_corr5,
    pentanoic_acid = voc_corr6,
    propanoic_acid = voc_corr7,
    acetladheyde = voc_corr8,
    formic_acid = voc_corr9,
    methanethiol = voc_corr10,
    acetone = voc_corr11,
    trimethylamine = voc_corr12,
    dimethyl_sulfide = voc_corr13,
    isopren = voc_corr14,
    butanone = voc_corr15,
    butandion = voc_corr16,
    phenol = voc_corr17,
    `4_ethyl_phenol` = voc_corr18,
    methyl_indole = voc_corr19
  ) %>%
  filter(elapsed.time >= 0 & elapsed.time <= 124) %>%
  pivot_longer(
    cols = methanol:methyl_indole,  # Use proper column range
    names_to = "compound",
    values_to = "concentration"
  )

# Set desired order for compounds
desired_order <- c("acetic_acid", "acetladheyde", "acetone", "butandion", "butanoic_acid", 
                   "butanone", "dimethyl_sulfide", "formic_acid", "H2S", "isopren", 
                   "methanol", "methanethiol", "methyl_indole", "pentanoic_acid", 
                   "phenol", "propanoic_acid", "trimethylamine", "4_ethyl_phenol", 
                   "4_Methylphenol")

# Set the factor levels for the 'compound' column in vocppb
vocppb$compound <- factor(vocppb$compound, levels = desired_order)

# Create the plot
vocplot <- ggplot(vocppb, aes(x = elapsed.time, y = concentration, colour = group)) + 
  geom_point(size = 0.4) +  # Adjust point size
  geom_line(aes(group = valve), linewidth = 0.5) +  
  facet_wrap(~ compound, scales = "free_y") + 
  labs(x = "Time after slurry application (hours)", y = "ppb") +
  theme_minimal();vocplot
#ggsave("Figure/endhours/voc ppb.png", plot = vocplot)


dat_long <- vocppb 
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

# Add the group information to the long data frame
dat_long <- dat_long %>%
  mutate(Group = factor(voc_groups[compound], levels = c("Carboxylic Acids", "Indole", "Phenols", "Volatile Sulfur Compounds (VSC)", "Other")))

# Aggregate data for all treatments and groups
sum.voc <- dat_long %>%
  group_by(elapsed.time, treatment, Group) %>%
  summarize(Sum_Flux = sum(concentration, na.rm = TRUE), .groups = 'drop')

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
unique(sum.voc$group)

# Generate the faceted plot with the corrected custom colors and facet labels
vocflux <- ggplot(sum.voc, aes(x = elapsed.time, y = Sum_Flux, fill = Group)) +
  geom_area(alpha = 0.8, position = "stack", color = "black", linewidth = 0.2) +  # Black outline
  scale_fill_manual(values = custom_colors) +  # Use custom colors
  labs(
    x = "Time after slurry application (hours)",
    y = "ppb"
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

#ggsave("Figure/endhours/voc group ppb.png", plot = vocflux)


