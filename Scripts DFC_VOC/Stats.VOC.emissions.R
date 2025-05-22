
library(emmeans)
library(multcompView)
library(multcomp)
library(readr)

data <- read_csv("../Flavia_VOC_DFC_data/cum.voc.emis.csv")
#======================= investigate if dose has some effect on cumulative emissions of NMVOC=============================

# Filter out the treatment you want to exclude 
filtered_data <- data[! data$group == 'Machine plot', ]

# Add dose column based on group names
filtered_data$dose <- NA  # Initialize dose column with NA

# Assign dose values based on group names
filtered_data$dose[filtered_data$group == "No acid"] <- 0
filtered_data$dose[filtered_data$group == "Low acid"] <- 2.9
filtered_data$dose[filtered_data$group == "Medium acid"] <- 5.7
filtered_data$dose[filtered_data$group == "High acid"] <- 10.5
#enusre data$dose is numeric
filtered_data$dose <- as.numeric(filtered_data$dose)
#write the model
model<-lm(total_cum~dose, data=filtered_data )
summary(model)
# Run Levene's test  and SHapiro test
res_abs=abs(residuals(model))
levene<-lm(res_abs~ dose, data = filtered_data)
anova(levene)
shapiro.test(residuals(model))

##########A t-test to compare  machine and manual application ####### 

# Filter to keep only two treatments (Mp, 0-bp)
filtered_data_2 <- data[data$group == 'Machine plot' | data$group == 'No acid', ]
# linear model cum.emis~treatment
model_2 <- lm(total_cum ~ group, data = filtered_data_2)
shapiro.test(model_2$residuals) #check normality of residuals
filtered_data_2 <- cbind(filtered_data_2, res_abs = abs(model_2$residuals))
model_levene_2 <- lm(res_abs ~ group, data = filtered_data_2)

# check homoschedasticity
anova(model_levene_2)  
# perform  # Welch t-test
t.test(total_cum~ group, data = filtered_data_2)
# extract the means
means_2 <- emmeans(model_2, ~ treatment)




