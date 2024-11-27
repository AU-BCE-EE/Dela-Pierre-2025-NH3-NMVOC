####### Flavia Project #########################
####### NH3 Emissions from DFC Chambers ########
####### Experiment 1 ###########################
####### Codes by Ali ###########################



#Libraries used to run script#

library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(ggplot2)
library(plotly)
library(patchwork)

################################################################################################################################
#Prerequ#Prerequ#Prerequisites to run readCRDS function#
devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')

#Reading in Picarro data#
da <- readCRDS(('DFC_Picarro'), From = '18.09.2024 12:30:00', To = '24.09.2024 08:00:00', mult = F, tz = "ETC/GMT-1", rm = F)

################################################################################################################################
# Making date.time stamp#
da$date.time <- paste(da$DATE, da$TIME)
da$date.time <- ymd_hms(da$date.time)
da$st <- da$date.time                                       
da$DATE <- as.Date(da$st)                                 
da$TIME <- format(da$st, format = "%H:%M:%S") 
################################################################################################################################

#Removing unnecessary data#
str(da); da
names(da)
da <- da[, -c(1:15, 17:19, 21:27)]
str(da); da


#Renaming the column MPVPosition to valve#
names(da)[names(da) == "MPVPosition"]  <- "valve"


#Cropping data and taking the last point of each measurement from each valve#
da <- filter(da, !(da$valve == lead(da$valve)))


#Removal of valve changing values from 1-19#
da <- da[da$valve %in% 1:19, ]


#Ordering data according to valves#
split_valda <- split(da, f = da$valve)
valid <- paste0("V", unique(da$valve))
new_da <- NULL
################################################################################################################################

#Calculate elapsed time of splited subset#
for (i in seq_along(split_valda)) {
  subset_data <- split_valda[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
dat<- new_da


#Rounding elapsed time to days#
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24
################################################################################################################################

#Assign names to valve values#

val_data <- dat %>%
  mutate(treatment = recode(valve,
                            `1` = '0-bls',
                            `2` = '0-bp',
                            `3` = '1.5',
                            `4` = '2.9',
                            `5` = 'bkg',
                            `6` = '0-bp',
                            `7` = '5.7',
                            `8` = '0-bls',
                            `9` = 'bkg',
                            `10` = '1.5',
                            `11` = '2.9',
                            `12` = '0-bls',
                            `13` = '5.7',
                            `14` = '0-bp',
                            `15` = '1.5',
                            `16` = 'bkg',
                            `17` = '2.9',
                            `18` = '0-bls',
                            `19` = '5.7'
  ),
  group = case_when(
    valve %in% c(2, 6, 14) ~ 'No acid',
    valve %in% c(3, 10, 15) ~ 'Low acid',
    valve %in% c(4, 11, 17) ~ 'Medium acid',
    valve %in% c(7, 13, 19) ~ 'High acid',
    valve %in% c(5, 9, 16) ~ 'Background',
    valve %in% c(1, 8, 12, 18) ~ 'Open plot'
  )
  )
dat <- rbind(val_data)
################################################################################################################################

#Background corrected concentration# 
#Background data#
DFC.bg <- dat[val_data$group == 'Background', ]


#DFC outlet data#
DFC <- dat[val_data$group%in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Open plot'), ]

#Mean background values#
DFC.bg.mean <- aggregate(NH3_30s ~ elapsed.time, data = DFC.bg, FUN = mean)


#Joining average background and outlet data#
DFC <- full_join(DFC.bg.mean, DFC, by = 'elapsed.time')
DFC <- na.omit(DFC)

#Subtracting background from outlet#
DFC$NH3_corr <- DFC$NH3_30s.y - DFC$NH3_30s.x
DFC[! complete.cases(DFC), ]

#Rebind again in DFC datasheet#
dat <- rbind(DFC)
################################################################################################################################

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
################################################################################################################################

#Round both columns to the nearest hour#
weather$time <- as.numeric(weather$time)
weather$date.time.weather <- paste(weather$date, weather$time)
dat$date.time.weather <- dat$date.time
dat$date.time.weather <- round_date(dat$date.time.weather, unit = "hour")
dat$date.time <- round_date(dat$date.time, unit = "hour")

#Convert both to POSIXct#
weather$date.time.weather <- sprintf("%s %02d:00", weather$date, weather$time)
weather$date.time.weather <- as.POSIXct(weather$date.time.weather, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "ETC/GMT-1")
head(weather$date.time.weather)

#Merging data#
dat <- left_join(dat, weather, by = c('date.time' = 'date.time.weather'))
################################################################################################################################

#Convert temperture from C to F#
dat$temp <- as.numeric(dat$temp)
dat$air.temp.K <- dat$temp + 273.15
################################################################################################################################


#NH3 flux prerequisite components#
#Air flow Calculation#
dat$air.flow <- 1
dat$air.flow <- 2.604 * 1000 # L min^-1 

#Chamber Area Calculation#
dat$dfc.area <- 1
dat$dfc.area <- (0.7/2)**2 * 3.14 #m^2


#Constants for flux calculation#
#Atmospheric constant#
atm.con <- 1 

#Gas constant [L * atm * K^-1 * mol^-1]#
g.con <- 0.082057338 

#Mass of nitrogen [g * mol^-1]#
M.N <- 14.0067 


#Calculation of NH3 flux#
#convert NH3.corr from ppb to mol (mol * L^-1)#
dat$n <- atm.con / (g.con * dat$air.temp.K) * dat$NH3_corr * 10^-9  # mol * L^-1   

#Calculation of flux, from mol * L^-1 to g.NH3 * min^-1 * m^-2#
dat$NH3.flux <- (dat$n * M.N * dat$air.flow) / dat$dfc.area

#Rearranging data by treatment# 
dat <- arrange(dat, by = treatment)
################################################################################################################################


#Calculation of total flux over time#
# Calculate the rolling average of NH3_flux with a window of 2#
dat$flux.treat <- rollapplyr(dat$NH3.flux, 2, mean, fill = NA)
dat$flux.treat[is.na(dat$flux.treat)] <- dat$NH3.flux[1]

#Calculation of total flux over time, time from start to last start (19 x 8 min)#
dat$flux.time <- dat$flux.tr * 152

#Cumulative emissions#
dat <- mutate(group_by(dat, treatment), cum.emis = cumsum(flux.time))
################################################################################################################################


# Plot NH3 Flux Over Time by Treatment#
# Custom order for treatments
dat$group <- factor(dat$group, levels = c("Open plot", "No acid", "Low acid", "Medium acid", "High acid"))

# Plot NH3 Flux Over Time by Treatment
g <- ggplot(dat, aes(x = elapsed.time, y = flux.time, color = group)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  geom_line(aes(group=valve)) + # Assuming 'valve' defines the individual line groupings
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 290, by = 30)) +
  
  # Axis labels and title, with ammonia flux
  labs(
    title = expression(paste("NH"[3], " Flux Over Time")),
    y = expression(paste(NH[3], " Flux (g NH"[3]-N, " * min"^-1, " * m"^-2, ")")),
    x = "Elapsed Time (hours)",
    color = "Treatment"
  ) +
  
  # Theme settings
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom"        
  ) +
  
  # Set up legend and print
  guides(color = guide_legend(nrow = 2)); g

################################################################################################################################

# Create a duplicate of the original dataframe
dat_duplicate <- copy(dat)

# Remove row with elapsed.time == 139
dat_duplicate <- dat_duplicate %>% filter(elapsed.time != 139)

# Ensure cumulative emissions are calculated by treatment
dat_duplicate <- mutate(group_by(dat_duplicate, treatment), cum.emis = cumsum(flux.time))

#Ensure cumulative emissions are calculated by treatment
dat_duplicate <- dat_duplicate %>%
  group_by(treatment) %>% 
  mutate(cum.emis = cumsum(flux.time)) %>%
  ungroup()

#Filter for the last time point by valve and treatment
dat_last <- dat_duplicate %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%
  ungroup()

#Summarize data for plotting (one point per treatment group)
isummMac_last <- dat_last %>%
  select(valve, treatment, cum.emis) %>%
  distinct()

#Average cumulative emissions for plotting (average of last points for each treatment)
esummMac_last <- isummMac_last %>%
  group_by(treatment) %>%
  summarise(cum.emis = mean(cum.emis, na.rm = TRUE), .groups = 'drop')

#Plotting cumulative emissions by treatment (including boxplot)
cumsum <- ggplot(isummMac_last, aes(x = treatment, y = cum.emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  # Scatter plot for the last cumulative emission points
  geom_boxplot(data = esummMac_last, aes(x = treatment, y = cum.emis, color = treatment), 
               show.legend = F) +  # Boxplot showing the distribution of cumulative emissions by treatment
  theme_bw() +  # Clean theme
  labs(
    title = NULL,  
    x = "Treatment",  # X-axis label
    y = expression(paste(NH[3], " Cumulative emissions (g NH"[3]-N, " * min"^-1, " * m"^-2, ")"))  # Y-axis label with units
  ) + 
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_blank(),             
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
  ), cumsum



################################################################################################################################
###############################################################################################################################
###############################################################################################################################


# Create the 'tan_data' data frame
tan_data <- data.frame(
  treatment = c('0-bls', '0-bp', '1.5', '2.9', '5.7'),
  total_tan = c(2478.6055, 2519.7095, 2222.396, 1479.7655, 1131.413)
)

# Define chamber volume in liters
Tan.volume <- 1.33  # Liters per chamber

# Convert TAN from mg/L to grams using the chamber volume
tan_data$total_tan <- tan_data$total_tan * Tan.volume / 1000  

#Join your cumulative emissions data with the 'tan_data' for each treatment
dat_last <- left_join(dat_last, tan_data, by = "treatment")

# Calculate TAN loss over time (using cumulative NH3 emissions and total TAN values)
dat_last <- dat_last %>%
  mutate(
    TAN_loss_percent = ( cum.emis / total_tan) * 100  # Calculate percentage of TAN lost
  )

#Create summary data for the boxplot
#Cumulative emissions (isummMac_last)
isummMac_last <- dat_last %>%
  select(treatment, TAN_loss_percent) %>%
  distinct()

#Average TAN loss percentage for plotting (esummMac_last)
esummMac_last <- isummMac_last %>%
  group_by(treatment) %>%
  summarise(TAN_loss_percent = mean(TAN_loss_percent, na.rm = TRUE), .groups = 'drop')

#Plotting TAN loss percentage using boxplot

# Create the boxplot for TAN loss percentage
tan_loss_plot <- ggplot() + 
  # Boxplot for TAN loss percentage
  geom_boxplot(data = dat_last, aes(x = treatment, y = TAN_loss_percent, color = treatment, fill = treatment), 
               alpha = 0.3, show.legend = FALSE) +
  
  # Scatter points for the TAN loss percentage (individual observations)
  geom_point(data = isummMac_last, aes(x = treatment, y = TAN_loss_percent, color = treatment), 
             size = 3, alpha = 0.7) + 
  
  theme_bw() +  # Clean theme
  labs(
    title = "TAN Loss Percentage by Treatment",
    x = "Treatment",  # X-axis label
    y = "TAN Loss Percentage (%)"  # Y-axis label for TAN loss percentage
  ) +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for clarity
  )
print(tan_loss_plot)






























####Dont run#######

############################################################################################

# Summing up cumulative emissions by treatment over the entire period
total_emissions <- dat %>%
  group_by(treatment) %>%
  summarize(total_emission = sum(cum.emis, na.rm = TRUE))

# Bar graph of cumulative emissions by treatment (assuming 'treatment' is your grouping variable)
ggplot(total_emissions, aes(x = treatment, y = total_emission, fill = treatment)) + 
  geom_bar(stat = "identity", show.legend = FALSE) +   # Create bar chart, remove legend for fill
  scale_fill_viridis_d() +                             # Use a perceptually uniform color scale
  labs(
    title = "Cumulative Emissions by Group",
    y = expression(paste("Total Emissions (g NH"[3], " * min"^-1, " * m"^-2, ")")), # Y-axis label
    x = "Group"
  ) +
  theme_bw() +                                         # Clean theme
  theme(
    axis.title = element_text(size = 14),              # Axis title font size
    axis.text = element_text(size = 12),               # Axis text font size
    plot.title = element_text(size = 16, hjust = 0.5), # Centered plot title
    strip.text = element_text(size = 14),              # Font size for facet strip text
    legend.text = element_text(size = 12),             # Font size for legend text
    legend.title = element_blank(),                    # Remove legend title
    legend.position = "bottom"                          # Place legend at the bottom
  )



# Summing up cumulative emissions by group (instead of treatment)
total_emissions_by_group <- dat %>%
  group_by(group) %>%
  summarize(total_emission = sum(cum.emis, na.rm = TRUE))

# First plot (g1) - NH3 Flux Over Time by Group
g1 <- ggplot(dat, aes(x = elapsed.time, y = flux.time, color = group)) +
  geom_point(size = 1.5, alpha = 0.8) +      # Scatter plot with larger points and transparency
  geom_line() +                              # Connect points to show trends over time
  scale_color_viridis_d() +                  # Color scale with perceptually uniform colors
  scale_x_continuous(breaks = seq(0, 290, by = 30)) +  # X-axis breaks every 30 minutes for readability
  
  # Axis labels and title, with ammonia flux units in the y-axis label
  labs(
    title = "NH3 Flux Over Time by Group",  # Update title to Group
    y = expression(paste(NH[3], " Flux (g NH"[3], " * min"^-1, " * m"^-2, ")")),  # Y-axis label with flux units
    x = "Elapsed Time (hours)",
    color = "Group"  # Change legend label to "Group"
  ) +
  
  # Theme customizations for a publication-quality look
  theme_bw() +  # Use a clean, black-and-white theme
  theme(
    axis.title = element_text(size = 14),       # Font size for axis titles
    axis.text = element_text(size = 12),        # Font size for axis text
    plot.title = element_text(size = 16, hjust = 0.5),  # Centered plot title
    strip.text = element_text(size = 14),       # Font size for facet strip text
    legend.text = element_text(size = 12),      # Font size for legend text
    legend.title = element_blank(),             # Remove legend title
    legend.position = "bottom"                  # Place legend at the bottom
  ) +
  
  # Set up legend to appear in multiple rows for better readability
  guides(color = guide_legend(nrow = 2))

# Second plot (g2) - Total NH3 Emissions by Group
g2 <- ggplot(total_emissions_by_group, aes(x = group, y = total_emission, fill = group)) +
  geom_bar(stat = "identity", width = 0.7) +  # Bar plot with adjusted width
  labs(
    x = NULL,  # Remove x-axis label
    y = expression("Total NH"[3]*" Emissions (g)")  # y-axis label with NH3 and subscript 3
  ) +
  theme_minimal(base_size = 14) +  # Clean minimal theme with larger base font
  theme(
    plot.title = element_blank(),  # Remove plot title
    axis.title.x = element_blank(),  # Remove x-axis title
    axis.title.y = element_text(size = 14),  # Keep y-axis label
    panel.grid.major.x = element_blank(),  # No vertical gridlines
    panel.grid.minor = element_blank(),   # No minor gridlines
    legend.position = "none"  # Remove legend entirely
  ) +
  scale_fill_brewer(palette = "Set2")  # Use the Set2 palette

# Convert g2 to a grob (graphical object)
g2_grob <- ggplotGrob(g2)

# Now combine g1 and g2 using annotation_custom()
g1_with_inset <- g1 + 
  annotation_custom(
    grob = g2_grob,  # Add the g2 plot as a grob (graphical object)
    xmin = 90, xmax = 175,  # X limits for the inset plot (adjust as needed)
    ymin = 0.15, ymax = 0.3   # Y limits for the inset plot (adjust as needed)
  )

# Print the final plot with the inset
print(g1_with_inset)






