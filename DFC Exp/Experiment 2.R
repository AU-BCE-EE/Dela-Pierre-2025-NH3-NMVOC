    
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
da <- readCRDS(('DFC_Picarro'), From = '24.09.2024 3:07:07', To = '30.09.2024 02:00:00', mult = F, tz = "EST", rm = F)

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

###Checking############################################################################################################################
da$elapsed.time <- difftime(da$date.time, min(da$date.time), units = 'hours')
da <- da[da$MPVPosition %in% 1:19, ]
da$MPVPosition <- as.character(da$MPVPosition)
ggplot(da, aes(elapsed.time, NH3_30s, color = MPVPosition)) + geom_point() + xlim (0,5)


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

################################################################################################################################
####Checking############################################################################################################################
da$elapsed.time <- difftime(da$date.time, min(da$date.time), units = 'hours')
da$valve <- as.character(da$valve)
ggplot(da, aes(elapsed.time, NH3_30s, color = valve)) + geom_point() + xlim (0,3)
################################################################################################################################
################################################################################################################################


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

#Assign names to valve values#

val_data <- dat %>%
  mutate(treatment = recode(valve,
                            `1` = 'Mp',
                            `2` = '2.9',
                            `3` = '1.5',
                            `4` = '0-dfc',
                            `5` = 'bkg',
                            `6` = 'Mp',
                            `7` = '5.7',
                            `8` = '0-dfc',
                            `9` = 'bkg',
                            `10` = '5.7',
                            `11` = 'Mp',
                            `12` = '1.5',
                            `13` = '2.9',
                            `14` = '5.7',
                            `15` = 'Mp',
                            `16` = 'bkg',
                            `17` = '2.9',
                            `18` = '0-dfc',
                            `19` = '1.5'
  ),
  group = case_when(
    valve %in% c(4, 8, 18) ~ 'No acid',
    valve %in% c(3, 12, 19) ~ 'Low acid',
    valve %in% c(2, 13, 17) ~ 'Medium acid',
    valve %in% c(7, 10, 14) ~ 'High acid',
    valve %in% c(5, 9, 16) ~ 'Background',
    valve %in% c(1, 6, 11, 15) ~ 'Machine plot'
  )
  )
dat <- rbind(val_data)
################################################################################################################################
################################################################################################################################
####Checking############################################################################################################################
ggplot(dat, aes(elapsed.time, NH3_30s, color = group)) + geom_point()
###############################################################################################################################################################################################################################################################

#Background corrected concentration# 
#Background data#
DFC.bg <- dat[val_data$group == 'Background', ]

################################################################################################################################
####Checking############################################################################################################################
ggplot(DFC.bg, aes(elapsed.time, NH3_30s, color = group)) + geom_point()
################################################################################################################################
################################################################################################################################

#DFC outlet data#
DFC <- dat[val_data$group%in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Machine plot'), ]
names(DFC)[2] <- "NH3.DFC"

################################################################################################################################
####Checking############################################################################################################################
ggplot(DFC, aes(elapsed.time, NH3.DFC, color = group)) + geom_point()
################################################################################################################################
################################################################################################################################

#Mean background valveues#
DFC.bg.summ <- aggregate(DFC.bg$NH3_30s, by = list(elapsed.time = DFC.bg$elapsed.time), FUN = mean)
names(DFC.bg.summ)[2] <- "NH3.bg"


#Joining average background and outlet data#
DFC <- full_join(DFC.bg.summ, DFC, by = 'elapsed.time')
DFC <- na.omit(DFC)

#Subtracting background from outlet#
DFC$NH3_corr <- DFC$NH3.DFC - DFC$NH3.bg
DFC[! complete.cases(DFC), ]

################################################################################################################################
####Checking#####################################################################################################################
ggplot(DFC, aes(elapsed.time, NH3_corr, colour = group)) + geom_point()
################################################################################################################################
################################################################################################################################


#Rebind again in dat datasheet#
dat <- rbind(DFC)
dat <- dat[order(dat$treatment), ]


################################################################################################################################
####Checking#####################################################################################################################
ggplot(dat, aes(treatment, NH3_corr, colour = treatment)) + geom_point()
################################################################################################################################
################################################################################################################################


#Import Weather Data and filter data#
header <- c('date', 'time', 'temp')
weather <- read.csv('Temp.csv', fill = T, stringsAsFactors = F)
weather <- weather[, -1 ]
colnames(weather) <- header

#Changing date format in excel file#
weather$date <- parse_date_time(weather$date, orders = c("d/m/y", "d-m-y"))

#Selecting experiment date range#
start_date <- dmy("24/09/2024")
end_date <- dmy("30/09/2024")
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

#Convert temperture from C to F#
dat$temp <- as.numeric(dat$temp)
dat$air.temp.K <- dat$temp + 273.15
################################################################################################################################

#NH3 flux calculation#

#NH3 flux prerequisite components#
#Air flow Calculation#
dat$air.flow <- 1
dat$air.flow <- 2.28 * 1000 # L min^-1 

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

################################################################################################################################
####Checking#####################################################################################################################
ggplot(dat, aes(treatment, NH3_corr, colour = group)) + geom_point() ##ppb
ggplot(dat, aes(treatment, n, colour = group)) + geom_point() ## mol * L^-1
################################################################################################################################
################################################################################################################################


#Calculation of flux, from mol * L^-1 to mg.NH3 * min^-1 * m^-2#
dat$NH3.flux <- ((dat$n * M.N * dat$air.flow) / dat$dfc.area) * 1000

################################################################################################################################
####Checking#####################################################################################################################
ggplot(dat, aes(treatment, NH3.flux, colour = group)) + geom_point()
################################################################################################################################
################################################################################################################################


###############################################################################################################################
#NH3 flux plotting#

# Plot NH3 Flux Over Time by Treatment


g <- ggplot(dat, aes(x = elapsed.time, y = NH3.flux, color = group)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  geom_line(aes(group=valve)) + 
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0, 290, by = 30)) +
  scale_y_continuous(
    breaks = seq(0, 13, by = 2),  # Adjust breaks to your desired range
  ) +
  
  # Axis labels and title, with ammonia flux
  labs(
    title = expression(paste("NH"[3], " Flux Over Time")),
    y = expression(paste(NH[3], " Flux (mg NH"[3]-N, " * m"^-2, " * min"^-1, ")")),
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
#Cumulative plot#


# Calculate cumulative emissions using mintegrate function 
source("Functions/mintegrate.R")
dat$cum.treat <- mintegrate(dat$elapsed.time, dat$NH3.flux, by = dat$valve, method = 'trap') #(NH3-N mg m^-2)

################################################################################################################################
####Checking#####################################################################################################################
ggplot(dat, aes(treatment, cum.treat, color = group)) + geom_point()
################################################################################################################################

#Cumulative emissions by treatment from mintegrate function
dat <- dat %>%
  mutate(cum.emis = cum.treat) %>% 
  group_by(treatment)

#Filter the data to get the last time point for each valve-treatment group
dat_last <- dat %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  # Select the last observation per treatment group
  ungroup()

#Create a summary dataset for plotting (one point per treatment group)
indsum <- dat_last %>%
  select(valve, treatment, cum.emis) %>%
  distinct()

################################################################################################################################
####Checking#####################################################################################################################
ggplot(indsum, aes(treatment, cum.emis, color = treatment)) + geom_point()
################################################################################################################################

#Summarize cumulative emissions by treatment (average across last time points for each treatment)
cumsum <- aggregate(indsum$cum.emis, by = list(treatment = indsum$treatment), FUN = function(x) mean(x, na.rm = TRUE))
names(cumsum)[2] <- "cum.emis"

################################################################################################################################
####Checking#####################################################################################################################
ggplot(cumsum, aes(treatment, cum.emis, color = treatment)) + geom_point()
################################################################################################################################

# Plot cumugroup# Plot cumulative emissions for each treatment with points and boxplot for averages
cumsum_plot <- ggplot(indsum, aes(x = treatment, y = cum.emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(data = cumsum, aes(x = treatment, y = cum.emis, color = treatment), 
               show.legend = FALSE) +  
  theme_bw() +
  labs(
    title = NULL,  
    x = "Treatment",  
    y = expression(paste(NH[3], "-N (mg * m"^-2, ")"))
  ) + 
  scale_x_discrete(labels = c(
    '0-bls' = 'Open plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_blank(),             
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); cumsum_plot

################################################################################################################################

# Define TAN value (mg/L) and chamber volume (Liters)
Tan.volume <- 1.33  # Liters per chamber
Tan.conc <- 2519    # TAN concentration in mg/L

# Calculate total (mg/L)/chamber
total_tan <- Tan.conc * Tan.volume 


# Calculate TAN fractional loss using total TAN valve
dat_last <- dat_last %>%
  mutate(
    tanloss = ((cum.emis * 60) / total_tan) *100
  )

# Prepare summary data for visualization
# Individual TAN fractional loss data
indsum.tan <- dat_last %>%
  select(treatment, tanloss) %>%
  distinct()

# Average TAN loss fraction for plotting
cumsum.tan <- aggregate(indsum.tan$tanloss, by = list(treatment = indsum$treatment), FUN = function(x) mean(x, na.rm = TRUE))
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
    y= expression("Loss of TAN (% of applied)")
  ) + 
  
  scale_x_discrete(labels = c(
    '0-bls' = 'Open plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_blank(),             
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); tan.loss

ggsave(filename = '/Users/AU775281/Documents/PhD/Flavia Experiment/Figures/tan_loss_analysis.png', 
       plot = tan.loss, 
       width = 15, 
       height = 12, 
       dpi = 400)


## The end of Flavia experiment 2 ####


#####################################################################################################################
#####################################################################################################################
#####################################################################################################################
## N-Grass ####

#NH3 flux plotting#

# Filter the data 
machine_applied_data <- filter(dat, treatment %in% c('0-bls'))  
handheld_applied_data <- filter(dat, treatment %in% c('0-bp', '1.5', '2.9', '5.7'))  

# Combine them for the plot
flux_data <- bind_rows(machine_applied_data, handheld_applied_data)

# Define colors for the two groups: Machine-applied and Handheld-applied slurry
flux_data <- flux_data %>%
  mutate(treatment_group = ifelse(treatment == '0-bls', 'Machine-applied slurry', 'Handheld-applied slurry'))

# Create the flux plot with colored treatments (Machine vs Handheld)
flux_plot <- ggplot(flux_data, aes(x = elapsed.time, y = NH3.flux, color = treatment_group)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  geom_line(aes(group = valve)) + # Assuming 'valve' defines the individual line groupings
  
  # Set custom colors for the two groups
  scale_color_manual(values = c('Machine-applied slurry' = '#a8d08d',  # Green
                                'Handheld-applied slurry' = '#ff7f0e')) +  # Orange
  
  labs(
    y = expression(paste(NH[3], " Flux (µg NH"[3]-N, " * min"^-1, " * m"^-2, ")")),
    x = "Elapsed Time (hours)",
    color = "Slurry Application"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom"        
  ); flux_plot


output_folder <- '/Users/AU775281/Documents/PhD/Meetings/N Grass meeting' 
output_file <- file.path(output_folder, "flux_plot_ex2.png")  

# Save the plot to the designated folder
ggsave(output_file, plot = flux_plot, width = 10, height = 6, dpi = 300)
#####################################################################################################################
#####################################################################################################################


# Add the 'group' column to isummMac_last
isummMac_last <- isummMac_last %>%
  mutate(group = ifelse(treatment == "0-bls", "Machine-applied slurry", "Handheld-applied slurry"))

# Add the 'group' column to esummMac_last
esummMac_last <- esummMac_last %>%
  mutate(group = ifelse(treatment == "0-bls", "Machine-applied slurry", "Handheld-applied slurry"))

# Now plot the cumulative emissions
cumsum_plot <- ggplot(isummMac_last, aes(x = treatment, y = cum.emis, color = group)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(data = esummMac_last, aes(x = treatment, y = cum.emis, fill = group), 
               show.legend = F) +  # Use 'fill' for boxplot's internal color
  theme_bw() +
  labs(
    title = NULL,  
    x = "Treatment",  
    y = expression(paste(NH[3], " Cumulative emissions (µg NH"[3]-N, " * min"^-1, " * m"^-2, ")"))  
  ) + 
  scale_x_discrete(labels = c(
    '0-bls' = 'M',  
    '0-bp' = 'H',  
    '1.5' = 'H',
    '2.9' = 'H',
    '5.7' = 'H'
  )) +  
  scale_color_manual(values = c("Machine-applied slurry" = "dodgerblue4", "Handheld-applied slurry" = "forestgreen")) +  
  scale_fill_manual(values = c("Machine-applied slurry" = "dodgerblue4", "Handheld-applied slurry" = "forestgreen")) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_blank(),             
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  ); cumsum_plot

output_folder <- '/Users/AU775281/Documents/PhD/Meetings/N Grass meeting' 
output_file <- file.path(output_folder, "cumsum_plot_ex2.png")  

# Save the plot to the designated folder
ggsave(output_file, plot = cumsum_plot, width = 10, height = 6, dpi = 300)

#####################################################################################################################
#####################################################################################################################

#NH3 flux with Sd plotting#

# Calculate mean and standard deviation by treatment group and elapsed time
flux_stats <- flux_data %>%
  group_by(treatment_group, elapsed.time) %>%
  summarise(
    mean_flux = mean(NH3.flux, na.rm = TRUE),
    sd_flux = sd(NH3.flux, na.rm = TRUE)
  )

# Plot with error bars (standard deviation) and different colors for error bars
flux_plot_with_errorbars <- ggplot(flux_stats, aes(x = elapsed.time, y = mean_flux, color = treatment_group)) +
  geom_point(size = 1.5, alpha = 0.8) + 
  geom_line(aes(group = treatment_group)) +  # Assuming 'treatment_group' defines the individual line groupings
  
  # Adding error bars with colors based on treatment group
  geom_errorbar(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux, color = treatment_group),
                width = 0.2, size = 1) + # Error bars
  
  scale_color_manual(values = c('Machine-applied slurry' = '#a8d08d',  # Green
                                'Handheld-applied slurry' = '#ff7f0e')) +  # Orange
  
  labs(
    y = expression(paste(NH[3], " Flux (µg NH"[3]-N, " * min"^-1, " * m"^-2, ")")),
    x = "Elapsed Time (hours)",
    color = "Slurry Application"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom"        
  ); (flux_plot_with_errorbars)


output_folder <- '/Users/AU775281/Documents/PhD/Meetings/N Grass meeting' 
output_file <- file.path(output_folder, "flux_plot_with_errorbars_ex2.png")  

# Save the plot to the designated folder
ggsave(output_file, plot = flux_plot_with_errorbars, width = 10, height = 6, dpi = 300)

## The N Grass experiment 2 ####



