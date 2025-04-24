########################################################################################
#----- Calculating flux ------------ 
########################################################################################

#NH3 flux prerequisite components#
#Convert temperature from C to F#
dat$temp <- as.numeric(dat$temp)
dat$air.temp.K <- dat$temp + 273.15

#Air flow Calculation#
dat$air.flow <- 2.28 * 1000 # L min^-1 

#Chamber Area Calculation#
dat$dfc.area <- (0.7/2)**2 * 3.14 #m^2

#Atmospheric constant#
atm.con <- 1 #atm

#Gas constant#
g.con <- 0.082057338 #L * atm * K^-1 * mol^-1

#Mass of nitrogen#
M.N <- 14.0067 #g * mol^-1


#Calculation of NH3 flux#
#Convert NH3.corr from ppb to mol (mol * L^-1)#
dat$n <- atm.con / (g.con * dat$air.temp.K) * dat$NH3_corr * 10^-9  # mol * L^-1   


#Calculation of flux, from mol * L^-1 to mg.NH3 * min^-1 * m^-2#
dat$NH3.flux <- ((dat$n * M.N * dat$air.flow) / dat$dfc.area) * 1000


########################################################################################
#----- Cumulative emissions ------------ 
########################################################################################
#Calculating cumulative emissions#
dat$cum.treat <- mintegrate((dat$elapsed.time*60), dat$NH3.flux, by = dat$valve, method = 'trap') #(NH3-N mg m^-2)


########################################################################################
#----- Setting data for plots ------------
########################################################################################
#Aggregate NH3 flux data by elapsed time and treatment group for plots#
dat_summary <- aggregate(
  NH3.flux ~ elapsed.time + group, 
  data = dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Convert the list columns to separate columns for plots#
dat_summary <- do.call(data.frame, dat_summary)
colnames(dat_summary)[3:4] <- c("mean_flux", "sd_flux")  # Rename columns

########################################################################################


########################################################################################
#----- Calculating TAN loss ------------
########################################################################################

#Creating new dataset for TAN#
dat_tan <- dat %>%
  mutate(cum.emis = cum.treat) %>% 
  group_by(treatment)

#Calculating mean#
tan.mean <- aggregate(Tan$`N.NH4.mg.L`, by = list(treatment = Tan$treatment), FUN = function(x) mean(x, na.rm = TRUE))  # mg/L
names(tan.mean)[2] <- "mean"

#Calculating applied Tan#
tan.mean$volume.applied <- 1.33 /((0.7/2)**2 * 3.14) #L/m^2
tan.mean$totaltan <- (tan.mean$mean* tan.mean$volume.applied) #mg/m^2

#Merging Tan data with cumulative emissions#
dat_tan <- dat_tan %>%
  left_join(tan.mean %>% select(treatment, totaltan), by = "treatment")

# Calculate TAN fractional loss#
dat_tan <- dat_tan %>%
  mutate(
    tanloss = ( cum.emis/ totaltan) *100 # (%)
  )


########################################################################################
#----- Setting data for plots ------------
########################################################################################
# Summarize data for TAN over time#
tan.plot.dat  <- aggregate(
  NH3.flux ~ elapsed.time + group, 
  data = dat, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
)

# Convert the list columns to separate columns and renaming
tan.plot.dat <- do.call(data.frame, tan.plot.dat)
colnames(tan.plot.dat)[3:4] <- c("mean", "sd")


#Plotting TAN loss fraction for each treatment# 
# Filtering the data to get the last time point for each valve-treatment group
dat_last <- dat_tan %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

#Preparing summary data for visualization#
indsum.tan <- dat_last %>%
  select(treatment, tanloss) %>%
  distinct()

#Average TAN loss fraction for plotting#
cumsum.tan <- aggregate(indsum.tan$tanloss, by = list(treatment = indsum.tan$treatment), FUN = function(x) mean(x, na.rm = TRUE))
names(cumsum.tan)[2] <- "tanloss"





