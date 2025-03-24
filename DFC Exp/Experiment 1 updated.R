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
library(plyr)
library(tidyr)

################################################################################################################################
#Prerequ#Prerequ#Prerequisites to run readCRDS function#
devtools::source_url('https://raw.githubusercontent.com/AU-BCE-EE/guidance/main/Picarro/PicarroFunction.R')

#Reading in Picarro data#
da <- readCRDS(('DFC_Picarro'), From = '18.09.2024 06:00:40', To = '24.09.2024 08:00:00', mult = F, tz = "EST", rm = F)

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

#Cropping data and taking the last point of each measurement from each vavle#
da <- filter(da, !(da$valve == lead(da$valve)))

#Removalve of valve changing valveues from 1-19#

# Selecting points with whole numbers (when the valve change there is a measurement where the valve position
# is in between two valves, these are removed)
da <- da[da$valve == '1' | da$valve == '2' | da$valve == '3' | da$valve == '4' | da$valve == '5' | da$valve == '6' | da$valve == '7' | 
           da$valve == '8' | da$valve == '9' | da$valve == '10' | da$valve == '11' | da$valve == '12' | da$valve == '13' | da$valve == '14' |
           da$valve == '15' | da$valve == '16' | da$valve == '17' | da$valve == '18' | da$valve == '19', ]

#Ordering data according to valve#
split_valve <- split(da, f = da$valve)
valve <- paste0("V", unique(da$valve))
new_da <- NULL

#Calculate elapsed time of splited subset#
for (i in seq_along(split_valve)) {
  subset_data <- split_valve[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
dat<- new_da

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
################################################################################################################################

#Background corrected concentration# 
#Background data#

DFC.bg <- dat[valve_data$group == 'Background', ]

#DFC outlet data#
DFC <- dat[valve_data$group%in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Machine plot'), ]
names(DFC)[2] <- "NH3.DFC"

#Mean background valveues#
DFC.bg.summ <- aggregate(DFC.bg$NH3_30s, by = list(elapsed.time = DFC.bg$elapsed.time), FUN = mean)
names(DFC.bg.summ)[2] <- "NH3.bg"


#Joining average background and outlet data#
DFC <- full_join(DFC.bg.summ, DFC, by = 'elapsed.time')
DFC <- na.omit(DFC)

#Subtracting background from outlet#
DFC$NH3_corr <- DFC$NH3.DFC - DFC$NH3.bg
DFC[! complete.cases(DFC), ]

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

#Convert both to POSIXct#
weather$date.time.weather <- sprintf("%s %02d:00", weather$date, weather$time)
weather$date.time.weather <- as.POSIXct(weather$date.time.weather, format = '%Y-%m-%d %H:%M', tz = "EST")
dat$date.time <- as.POSIXct(dat$date.time, format = '%Y-%m-%d %H:%M', tz = "EST")
head(weather$date.time.weather)

#Merging data#
dat <- left_join(dat, weather, by = c('date.time' = 'date.time.weather'))
################################################################################################################################

#Convert temperature from C to F#
dat$temp <- as.numeric(dat$temp)
dat$air.temp.K <- dat$temp + 273.15
################################################################################################################################
#NH3 flux calculation#

#NH3 flux prerequisite components#
#Air flow Calculation#
dat$air.flow <- 2.28 * 1000 # L min^-1 

#Chamber Area Calculation#
dat$dfc.area <- (0.7/2)**2 * 3.14 #m^2


#Constants for flux calculation#
#Atmospheric constant#
atm.con <- 1 #atm

#Gas constant [L * atm * K^-1 * mol^-1]#
g.con <- 0.082057338 

#Mass of nitrogen [g * mol^-1]#
M.N <- 14.0067 


#Calculation of NH3 flux#
#convert NH3.corr from ppb to mol (mol * L^-1)#
dat$n <- atm.con / (g.con * dat$air.temp.K) * dat$NH3_corr * 10^-9  # mol * L^-1   


#Calculation of flux, from mol * L^-1 to mg.NH3 * min^-1 * m^-2#
dat$NH3.flux <- ((dat$n * M.N * dat$air.flow) / dat$dfc.area) * 1000

################################################################################################################################
####################################################################################################################

#NH3 flux plotting#
# Plot NH3 Flux Over Time by Treatment

dat$group <- factor(dat$group, levels = c("No acid", "Low acid", "Medium acid", "High acid", "Machine plot"))


# Define category colors
category_colors <- c(
  "No acid" = "#4e79a7",
  "Machine plot" = "#f28e2b", 
  "Medium acid" = "#e15759",
  "High acid" = "#59a14f",
  "Low acid" = "#76b7b2"
)

dat_summary <- aggregate(
  NH3.flux ~ elapsed.time + group, 
  data = dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)


dat_summary <- dat_summary %>%
  filter(elapsed.time >= 0 & elapsed.time <= 120)

# Convert the list columns to separate columns
dat_summary <- do.call(data.frame, dat_summary)
colnames(dat_summary)[3:4] <- c("mean_flux", "sd_flux")  # Rename columns

# Plot with filled ribbon
Fluxes <- ggplot(dat_summary, aes(x = elapsed.time, y = mean_flux, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), alpha = 0.3, color = NA) +  # Shaded area
  geom_line(size = 1) +  # Mean flux line
  scale_color_manual(values = category_colors) +  # Apply custom color scale for color
  scale_fill_manual(values = category_colors) +  # Use the same colors for fill
  scale_x_continuous(breaks = seq(0, 290, by = 25)) +
  
  # Axis labels and title
  labs(
    y = expression(paste(NH[3], " Flux (mg NH"[3]-N, " * m"^-2, " * min"^-1, ")")),
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment"
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
  
  guides(color = guide_legend(nrow = 1)); Fluxes 
#ggsave("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/DFC VOC Figures/NH3 flux.png", 
plot = Fluxes, 
width = 12, 
height = 10, 
dpi = 300, 
bg = "white")

################################################################################################################################
#######Cmulative by mintegrate#############################################################################################################

dat <- dat %>% filter(elapsed.time >= 0 & elapsed.time <= 120)
# Calculate cumulative emissions using mintegrate function 
source("Functions/mintegrate.R")
dat$cum.treat <- mintegrate((dat$elapsed.time*60), dat$NH3.flux, by = dat$valve, method = 'trap') #(NH3-N mg m^-2)

#Cumulative emissions by treatment from mintegrate function
dat_tan <- dat %>%
  mutate(cum.emis = cum.treat) %>% 
  group_by(treatment)

################################################################################################################################
########################################################################################################################

#Loss of TAN with plot#
#Import TAN Data#
header <- c('Id', 'Treatment', 'g Slurry', 'Dilution Factor', 'N-NH4', 'N-NH4 mg/L')
Tan <- read.csv('Tan analysis.csv', fill = T, stringsAsFactors = F)
Tan <- Tan [, -c(1, 3:5)]
Tan$treatment <- as.factor(Tan$treatment)

# Calculate mean
tan.mean <- aggregate(Tan$`N.NH4.mg.L`, by = list(treatment = Tan$treatment), FUN = function(x) mean(x, na.rm = TRUE))  # mg/L
names(tan.mean)[2] <- "mean"

#Calculate TAN applied in mg/m^2#
tan.mean$volume.applied <- 1.33 /((0.7/2)**2 * 3.14) #L/m^2
tan.mean$totaltan <- (tan.mean$mean* tan.mean$volume.applied) #mg/m^2

#Merging Tan data with cumulative data#
dat_tan <- dat_tan %>%
  left_join(tan.mean %>% select(treatment, totaltan), by = "treatment")

# Calculate TAN fractional loss using total TAN valve#
dat_tan <- dat_tan %>%
  mutate(
    tanloss = ( cum.emis/ totaltan) *100
  )

# Summarize data using aggregate()
dat_tan_summary <- aggregate(
  NH3.flux ~ elapsed.time + group, 
  data = dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Convert the list columns to separate columns
dat_tan_summary <- do.call(data.frame, dat_tan_summary)
colnames(dat_tan_summary)[3:4] <- c("mean_flux", "sd_flux")  # Rename columns

# Plot with filled ribbon
Tan <- ggplot(dat_tan_summary, aes(x = elapsed.time, y = mean_flux, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), alpha = 0.3, color = NA) +  # Shaded area for SD
  geom_line(size = 1) +  # Mean flux line
  geom_point(aes(x = elapsed.time, y = mean_flux), size = 2, shape = 16, alpha = 0.7) +  # Dots at elapsed times
  scale_color_manual(values = category_colors) +  # Apply custom color scale for color
  scale_fill_manual(values = category_colors) +  # Apply custom color scale for fill
  scale_x_continuous(breaks = seq(0, 290, by = 25)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  # Axis labels and title
  labs(
    y = expression("Tan h"^-1 * " [%]"),
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment"
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
  
  guides(color = guide_legend(nrow = 1)); Tan



custom_colors <- c(
  "0-bp" = "#4e79a7",  # No acid
  "Mp" = "#f28e2b",    # Machine plot
  "1.5" = "#76b7b2",   # Low acid
  "2.9" = "#e15759",   # Medium acid
  "5.7" = "#59a14f"    # High acid
)

# Filter the data to get the last time point for each valve-treatment group
dat_last <- dat_tan %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

# Prepare summary data for visualization
# Individual TAN fractional loss data
indsum.tan <- dat_last %>%
  select(treatment, tanloss) %>%
  distinct()

# Average TAN loss fraction for plotting
cumsum.tan <- aggregate(indsum.tan$tanloss, by = list(treatment = indsum.tan$treatment), FUN = function(x) mean(x, na.rm = TRUE))
names(cumsum.tan)[2] <- "tanloss"

# Plot the TAN loss fraction for each treatment
tan.loss <- ggplot(indsum.tan, aes(x = treatment, y = tanloss, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(data = cumsum.tan, aes(x = treatment, y = tanloss, color = treatment), 
               show.legend = F) +  
  theme_bw() +
  labs(
    title = NULL,  
    x = "Treatment",  
    y = expression("TAN loss (% of applied)")
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  scale_color_manual(values = custom_colors) +  # Apply custom colors here
  theme(
    axis.title.x = element_text(size = 10),  # Set size for x axis title
    axis.title.y = element_text(size = 10),  # Set size for y axis title
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text.y = element_text(size = 9),  # Set size for y axis text
    strip.text = element_blank(),  # Remove facet strip text
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "none"  # Remove the legend completely
  ); tan.loss

# Combine the plots using patchwork with a cleaner inset
combined_plot <- Tan + inset_element(
  tan.loss, 
  left = 0.6, bottom = 0.6, right = 0.95, top = 0.96
); combined_plot

#ggsave("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/DFC VOC Figures/Tan loss .png", 
plot = combined_plot, 
width = 12, 
height = 10, 
dpi = 300, 
bg = "white")

################################################################################################################################
###############################################################################################################################
####bLs Flux###############################################################################################################
# Read BLS data
bls <- read.csv('/Users/AU775281/Documents/GitHub/Flavia-Project/DFC Exp/NH3_fluxes_bLS.csv')

# Summarize BLS data
bls_summary <- aggregate(
  Flux_mg_min ~ hours, 
  data = bls, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE))
)

# Convert list columns to separate columns
bls_summary <- do.call(data.frame, bls_summary)
colnames(bls_summary)[1:2] <- c("elapsed.time", "mean_flux")  # Rename columns


# Define color for BLS data
bls_color <- "gray20" 

# Plot with filled ribbon
# Create the plot
Fluxes <- ggplot(dat_summary, aes(x = elapsed.time, y = mean_flux, color = group, fill = group)) +
  
  # Add a ribbon for uncertainty in dat_summary
  geom_ribbon(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), alpha = 0.2, color = NA) +
  
  # Add a solid line plot for dat_summary
  geom_line(size = 1) +
  
  # Add a separate dashed line for BLS summary with the correct linetype
  geom_line(data = bls_summary, aes(x = elapsed.time, y = mean_flux, linetype = "bLS plot"), 
            color = bls_color, size = 1, inherit.aes = FALSE) +
  
  # Custom colors
  scale_color_manual(values = category_colors) +
  scale_fill_manual(values = category_colors) +
  
  # Correct the scale for linetype, matching the "BLS Treatment" label
  scale_linetype_manual(values = c("bLS plot" = "dashed")) +  # Define dashed style
  
  # X-axis settings
  scale_x_continuous(
    breaks = seq(0, 290, by = 25)
  ) +
  
  # Single Y-axis for both datasets
  scale_y_continuous(
    name = expression(paste(NH[3], " Flux (mg NH"[3]-N, " * m"^-2, " * min"^-1, ")"))
  ) +
  
  # Axis labels and title
  labs(
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment",
    linetype = "Legend"
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
  
  # Adjust legend settings
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  );Fluxes

ggsave("/Users/AU775281/Documents/GitHub/Flavia-Project/DFC Exp/VOC/DFC VOC Figures/NH3 flux.png", 
plot = Fluxes, 
width = 12, 
height = 10, 
dpi = 300, 
bg = "white")

################################################################################################################################
########################################################################################################################

