
library(emmeans)
library(multcompView)
library(multcomp)
library(readr)
library(dplyr)
#-------------cumulative NMVOCs at elapsed_time 119 h-----------------#
cum.voc <- read_csv("../Flavia_VOC_DFC_data/cum.voc.emis.csv")

#======================= investigate if dose has some effect on cumulative emissions of NMVOC=============================

# Filter out the treatment you want to exclude 
filtered_data_voc <- cum.voc[! cum.voc$group == 'Machine plot', ]

# Add dose column based on group names
filtered_data_voc$dose <- NA  # Initialize dose column with NA

# Assign dose values based on group names
filtered_data_voc$dose[filtered_data_voc$group == "No acid"] <- 0
filtered_data_voc$dose[filtered_data_voc$group == "Low acid"] <- 2.9
filtered_data_voc$dose[filtered_data_voc$group == "Medium acid"] <- 5.3
filtered_data_voc$dose[filtered_data_voc$group == "High acid"] <- 10.5
#ensure data$dose is numeric
filtered_data_voc$dose <- as.numeric(filtered_data_voc$dose)
#write the model
model<-lm(total_cum~dose, data=filtered_data_voc )
summary(model)
library(olsrr)
ols_regress(model)  #There is an effect: 1 incremental unit of acid gives around 230 mg m^-2 NMVOC emissions more
# Run Levene's test  and Shapiro test
res_abs=abs(residuals(model))
levene<-lm(res_abs~ dose, data = filtered_data_voc)
anova(levene)
shapiro.test(residuals(model))

##########A One-Way ANOVA to compare  machine and manual application ####### 

# Filter to keep only two treatments (Machine plot and No acid)
filtered_data_2 <- cum.voc[cum.voc$group == 'Machine plot' | cum.voc$group == 'No acid', ]
# linear model cum.emis~treatment
model_2 <- lm(total_cum ~ group, data = filtered_data_2)
#one way ANOVA
anova(model_2)   #Machine application gives significantly higher cumulative NMVOCs emissions
shapiro.test(model_2$residuals) #check normality of residuals
filtered_data_2 <- cbind(filtered_data_2, res_abs = abs(model_2$residuals))
model_levene_2 <- lm(res_abs ~ group, data = filtered_data_2)

# check homoschedasticity
anova(model_levene_2)  

# extract the means
means_2 <- emmeans(model_2, ~ group)



#-------------cumulative NH3 at elapsed_time 119 h-----------------#


cum_NH3 <- read_delim("cum.NH3.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)


#======================= investigate if dose has some effect on cumulative emissions of NH3=============================

# Filter out the treatment you want to exclude 
filtered_data_NH3 <- cum_NH3[! cum_NH3$treatment == 'Machine plot DFC', ]

# Add dose column based on group names
filtered_data_NH3$dose <- NA  # Initialize dose column with NA

# Assign dose values based on group names
filtered_data_NH3$dose[filtered_data_NH3$treatment == "No acid DFC"] <- 0
filtered_data_NH3$dose[filtered_data_NH3$treatment == "Low acid DFC"] <- 2.9
filtered_data_NH3$dose[filtered_data_NH3$treatment == "Medium acid DFC"] <- 5.3
filtered_data_NH3$dose[filtered_data_NH3$treatment == "High acid DFC"] <- 10.5
#ensure data$dose is numeric
filtered_data_NH3$dose <- as.numeric(filtered_data_NH3$dose)
#write the model
model<-lm(cum.emis.TAN~dose, data=filtered_data_NH3 )
summary(model)
library(olsrr)
ols_regress(model)   #no significant effect of the acid dosage is detected on NH3 cumulative emissions
# Run Levene's test  and SHapiro test
res_abs=abs(residuals(model))
levene<-lm(res_abs~ dose, data = filtered_data_NH3)
anova(levene)
shapiro.test(residuals(model))

##########A One-Way ANOVA to compare  machine and manual application ####### 

# Filter to keep only two treatments (Machine plot DFC and No acid DFC)
filtered_data_3 <- cum_NH3[cum_NH3$treatment == 'Machine plot DFC' | cum_NH3$treatment == 'No acid DFC', ]
# linear model cum.emis~treatment
model_3 <- lm(cum.emis.TAN ~ treatment, data = filtered_data_3)
#one way ANOVA
anova(model_3) # Machine application gives significantly higher cumulative NH3 emissions
shapiro.test(model_3$residuals) #check normality of residuals
filtered_data_3 <- cbind(filtered_data_3, res_abs = abs(model_3$residuals))
model_levene_3 <- lm(res_abs ~ treatment, data = filtered_data_3)

# check homoschedasticity
anova(model_levene_3)  

# extract the means
means_3 <- emmeans(model_3, ~ treatment)

#-------------OAV over time (elpasesd_time from 0 to 119, irregular intervals)-------------#
OAV<- read_csv("../Flavia_VOC_DFC_data/OAV.txt")
# Filter out the treatment you want to exclude 
filtered_data_OAV <- OAV[! OAV$group == 'Machine plot', ]

# Add dose column based on group names
filtered_data_OAV$dose <- NA  # Initialize dose column with NA

# Assign dose values based on group names
filtered_data_OAV$dose[filtered_data$group == "No acid"] <- 0
filtered_data_OAV$dose[filtered_data$group == "Low acid"] <- 2.9
filtered_data_OAV$dose[filtered_data_OAV$group == "Medium acid"] <- 5.3
filtered_data_OAV$dose[filtered_data_OAV$group == "High acid"] <- 10.5

#find intercept (Total OAV at time 0) and slope (rate of OAV decline over time) (for each valve which corresponds to a specific chamber)
fit <- filtered_data_OAV %>% group_by(valve) %>% summarise(
  intercept = coef(lm(total_OAV ~ poly(elapsed_time, 1, raw = TRUE)))[1],
  slope     = coef(lm(total_OAV ~ poly(elapsed_time, 1, raw = TRUE)))[2]
)
# See if intercept and slope depend on dose
fit.dose <- left_join(fit,filtered_data_OAV %>% dplyr::select(valve, dose) %>% distinct(), by = "valve")
summary(lm(intercept ~ dose, data = fit.dose))
summary(lm(slope ~ dose, data = fit.dose))
#they don't depend on dose

##########A One-Way ANOVA to compare  machine and manual application on initial (elpased_time == 0) OAV ####### 

# Filter to keep only two treatments (Machine plot DFC and No acid DFC)
filtered_data_4 <- OAV[OAV$group == 'Machine plot' | OAV$group == 'No acid', ]
# linear model cum.emis~treatment
filtered_data_4 <- filtered_data_4[filtered_data_4$elapsed_time == 0, ]

model_4 <- lm(total_OAV ~ group, data = filtered_data_4)
#one way ANOVA
anova(model_4) #initial OAV are not affected by application method
shapiro.test(model_4$residuals) #check normality of residuals
filtered_data_4 <- cbind(filtered_data_4, res_abs = abs(model_4$residuals))
model_levene_4 <- lm(res_abs ~ group, data = filtered_data_4)

# check homoschedasticity
anova(model_levene_4)  

# extract the means
means_4 <- emmeans(model_4, ~ group)

