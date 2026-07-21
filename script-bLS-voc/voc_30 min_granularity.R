
#upload the file with ppb concentrations
odors_ppb<- read_excel("BLANK")

#upload the file with Molar Weights to perform the transformation is ug/m3
MW <- read_excel("../input-bLS/odors_MW.xlsx", 
                           sheet = "MW")


#transform from ppb in ug/m3
odors_ppb$m_z_33_methanol_ug<-odors_ppb$m_z_33_methanol* 0.0409*MW$Methanol
odors_ppb$m_z_35_H2S_ug<-odors_ppb$`m/z_35_H2S`*0.0409*MW$`Hydrogen sulfide`
odors_ppb$`m_z_109_4-Methylphenol_ug`<-odors_ppb$`m_z_109_4-Methylphenol`*0.0409*MW$`4-methylphenol`
odors_ppb$`m_z_61+43_acetic_acid_ug`<-odors_ppb$`m_z_61+43_acetic_acid`*0.0409*MW$`Acetic acid`
odors_ppb$`m_z_71+89_butanoic_acid_ug`<-odors_ppb$`m_z_71+89_butanoic_acid`*0.0409*MW$`Butanoic acid`
odors_ppb$`m_z_85+103_pentanoic_acid_ug`<-odors_ppb$`m_z_85+103_pentanoic_acid`*0.0409*MW$`Pentanoic aicd`
odors_ppb$m_z_45.00_acetladheyde_ug<-odors_ppb$m_z_45.00_acetaldheyde*0.0409*MW$Acetaldehyd
odors_ppb$m_z_47.00_formic_acid_ug<-odors_ppb$m_z_47.00_formic_acid*0.0409*MW$`Formic acid`
odors_ppb$m_z_49.00_methanthiol_ug<-odors_ppb$m_z_49.00_methanthiol*0.0409*MW$Methanthiol
odors_ppb$m_z_59.00_acetone_ug<-odors_ppb$m_z_59.00_acetone*0.0409*MW$Acetone
odors_ppb$m_z_63.00_dimethyl_sulfide_ug<-odors_ppb$m_z_63.00_dimethyl_sulfide*0.0409*MW$`Dimethyl sulfide`
odors_ppb$m_z_73.00_2_butanone_ug<-odors_ppb$m_z_73.00_2_butanone*0.0409*MW$`2-butanone`
odors_ppb$m_z_87.00_2_3_butandion_ug<-odors_ppb$m_z_87.00_2_3_butandion*0.0409*MW$`2,3-butandion`
odors_ppb$m_z_95.00_phenol_ug<-odors_ppb$m_z_95.00_phenol*0.0409*MW$Phenol
odors_ppb$m_z_109.00_4_methyl_phenol_ug<-odors_ppb$`m_z_109_4-Methylphenol`*0.0409*MW$`4-methylphenol`
odors_ppb$m_z_123.00_4_ethyl_phenol_ug<-odors_ppb$m_z_123.00_4_ethyl_phenol*0.0409*MW$`4-ethylphenol`
odors_ppb$m_z_132.00_3_methyl_indole_ug<-odors_ppb$m_z_132.00_3_methyl_indole*0.0409*MW$`3-methylindole`
odors_ppb$`m_z_57+75_propanoic_acid_ug`<-odors_ppb$`m_z_57+75_propanoic_acid`*0.0409*MW$`Propionic acid`
odors_ppb$m_z_60.00_trimethylamine_ug<-odors_ppb$m_z_60.00_trimethylamine*0.0409*MW$Trimethylamine
odors_ppb$m_z_69.00_isoprene_ug<-odors_ppb$m_z_69.00_isopren*0.0409*MW$Isopren
data<-odors_ppb

# adjust the time offset of the instrument: PTRMS plot time - 0hr - 3 min - 0sec
data$adjusted_datetime <- data$`Absolute Time` - hours(0) - minutes(3)- seconds(0)
str(data$`Absolute Time`)
## 30 minutes granularity
# Step 1: Extract minutes and seconds
vec <- (data$adjusted_datetime)
vec1 <- as.POSIXlt(vec)
minutes <- vec1$min
minutes <- vec1$min
seconds <- vec1$sec
# Step 2: Combine minutes and seconds into total minutes
v5 <- minutes + seconds / 60
# Step 3: Round the minutes to the nearest 15-minute interval
rounded_minutes <- round(v5 / 15) * 15
# Step 4: Update the minutes and set seconds to 0
vec1$min <- rounded_minutes
vec1$sec <- 0
# Step 5: Convert the updated time components back to POSIXct
updated_time <- as.POSIXct(vec1)
updated_time<- na.omit(updated_time)
# Step 6: Generate a sequence of times at 30-minute intervals
rt <- seq(from = min(updated_time),
          to = max(updated_time) + minutes(30),
          by = "30 min")

# Step 7: Round `updated_time` to 30-minute intervals for grouping
data <- data %>%
  mutate(RoundedTime = floor_date(updated_time, unit = "30 minutes"))

# Step 8: Group by `RoundedTime` and calculate the mean for each variable
data_aggregated <- data %>%
  group_by(RoundedTime) %>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  ungroup()
# Specify the date/time you want to split on
start_time <- as.POSIXct("2024-09-18 12:00:00", tz= "UTC")
end_time<- as.POSIXct("2024-09-23 12:00:00", tz= "UTC")
data_aggregated$RoundedTime
# keep only data from slurry application
df_plot_2 <- subset(data_aggregated, RoundedTime >= start_time)    # Rows on or after the specified timestamp
df_plot_2$RoundedTime
#delete the Na row
# This deletes rows 1, 242, and 186. We need to do it to keep only the measurement time, and delete line 242 where the anemometer failed
df_plot <- df_plot_2[-c(1, 242, 186), ]

#save the file
#write.csv(df_plot,"plot_voc_bLS.csv", row.names = F)



