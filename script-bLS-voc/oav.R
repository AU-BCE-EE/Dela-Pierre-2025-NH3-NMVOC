
# Load the dataset of the concentrations (ug/m3)
adjusted_conc <- read.csv("../output-bLS/odors_adjusted_conc.csv")
adjusted_conc$Time
adjusted_conc$hours
View(adjusted_conc)
# Load the dataset of the Molar Weights of each compound
MW <- read_excel("../input-bLS/odors_MW.xlsx", sheet = "MW")

# Load the dataset with the Odor Threshold Values
OTV <- read_excel("../input-DFC/OTV.xlsx", sheet = "Sheet2")
#Load dataset for T
bLS<-read.table("../input-bLS/Run1E5_2_PTR_1.txt", row.names=NULL, header = T, sep="")
bLS<- bLS[-1, ]
bLS$Time
# Transform concentrations from ug/m3 back to ppb
k <- 0.0409
adjusted_conc$adjusted_concentration_2_3_butandion_ppb <- adjusted_conc$adjusted_concentration_butandion / k / MW$`2,3-butandion`
adjusted_conc$adjusted_concentration_2_butanone_ppb <- adjusted_conc$adjusted_concentration_butanone / k / MW$`2-butanone`
adjusted_conc$adjusted_concentration_4_methylphenol_ppb <- adjusted_conc$adjusted_concentration_methylphenol / k / MW$`4-methylphenol`
adjusted_conc$adjusted_concentration_acetaldheyde_ppb <- adjusted_conc$adjusted_concentration_acetaldehyde / k / MW$Acetaldehyd
adjusted_conc$adjusted_concentration_acetic_acid_ppb <- adjusted_conc$adjusted_concentration_acetic_acid / k / MW$`Acetic acid`
adjusted_conc$adjusted_concentration_acetone_ppb <- adjusted_conc$adjusted_concentration_acetone / k / MW$Acetone
adjusted_conc$adjusted_concentration_Hydrogen_sulfide_ppb <- adjusted_conc$adjusted_concentration_H2S / k / MW$`Hydrogen sulfide`
adjusted_conc$adjusted_concentration_isoprene_ppb <- adjusted_conc$adjusted_concentration_isoprene / k / MW$Isopren
adjusted_conc$adjusted_concentration_pentanoic_acid_ppb <- adjusted_conc$adjusted_concentration_pentanoic_acid / k / MW$`Pentanoic aicd`
adjusted_conc$adjusted_concentration_trimethylamine_ppb <- adjusted_conc$adjusted_concentration_trimethylamine / k / MW$Trimethylamine
adjusted_conc$adjusted_concentration_4_ethyl_phenol_ppb <- adjusted_conc$adjusted_concentration_ethylphenol / k / MW$`4-ethylphenol`
adjusted_conc$adjusted_concentration_butanoic_acid_ppb <- adjusted_conc$adjusted_concentration_butanoic_acid / k / MW$`Butanoic acid`
adjusted_conc$adjusted_concentration_formic_acid_ppb <- adjusted_conc$adjusted_concentration_formic_acid / k / MW$`Formic acid`
adjusted_conc$adjusted_concentration_methanol_ppb <- adjusted_conc$adjusted_concentration_methanol / k / MW$Methanol
adjusted_conc$adjusted_concentration_phenol_ppb <- adjusted_conc$adjusted_concentration_phenol / k / MW$Phenol
adjusted_conc$adjusted_concentration_propanoic_acid_ppb <- adjusted_conc$adjusted_concentration_propanoic_acid / k / MW$`Propionic acid`
adjusted_conc$adjusted_concentration_methanthiol_ppb <- adjusted_conc$adjusted_concentration_methanthiol / k / MW$Methanthiol
adjusted_conc$adjusted_concentration_dimethyl_sulfide_ppb <- adjusted_conc$adjusted_concentration_dimethyl_sulfide / k / MW$`Dimethyl sulfide`
adjusted_conc$adjusted_concentration_3_methyl_indole_ppb <- adjusted_conc$adjusted_concentration_methyl_indole / k / MW$`3-methylindole`

# Calculate the OAV dividing the concentrations by the OTV
acetic_acid <- adjusted_conc$adjusted_concentration_acetic_acid_ppb / OTV$`Acetic acid`
pentanoic_acid <- adjusted_conc$`adjusted_concentration_pentanoic_acid_ppb` / OTV$`Pentanoic acid`
acetaldheyde <- adjusted_conc$adjusted_concentration_acetaldheyde_ppb / OTV$Acetaldehyde
formic_acid <- adjusted_conc$adjusted_concentration_formic_acid_ppb / OTV$`Formic acid`
methanthiol <- adjusted_conc$adjusted_concentration_methanthiol_ppb / OTV$`Methane thiol`
acetone <- adjusted_conc$adjusted_concentration_acetone_ppb / OTV$Acetone
`2_butanone` <- adjusted_conc$adjusted_concentration_2_butanone_ppb / OTV$`2-Butanone`
`2_3_butandion` <- adjusted_conc$`adjusted_concentration_2_3_butandion_ppb` / OTV$`2,3-Butanedione`
phenol <- adjusted_conc$adjusted_concentration_phenol_ppb / OTV$Phenol
`4_methyl_phenol` <- adjusted_conc$`adjusted_concentration_4_methylphenol_ppb` / OTV$`4-Methylphenol`
`4_ethyl_phenol` <- adjusted_conc$`adjusted_concentration_4_ethyl_phenol_ppb` / OTV$`4-Ethylphenol`
propanoic_acid <- adjusted_conc$`adjusted_concentration_propanoic_acid_ppb` / OTV$`Propionic acid`
trimethylamine <- adjusted_conc$adjusted_concentration_trimethylamine_ppb / OTV$Trimethylamine
isoprene <- adjusted_conc$adjusted_concentration_isoprene_ppb / OTV$Isoprene
hydrogen_sulfide <- adjusted_conc$adjusted_concentration_Hydrogen_sulfide_ppb / OTV$`Hydrogen sulphide`
methanol <- adjusted_conc$adjusted_concentration_methanol_ppb / OTV$Methanol
dimethyl_sulfide <- adjusted_conc$adjusted_concentration_dimethyl_sulfide_ppb / OTV$`Dimethyl sulphide`
`3_methylindole` <- adjusted_conc$adjusted_concentration_3_methyl_indole_ppb / OTV$`3-Methylindole`
butanoic_acid <- adjusted_conc$adjusted_concentration_butanoic_acid_ppb / OTV$`Butanoic acid`

# Create the dataframe for OAV
OAV <- data.frame(
  acetic_acid = acetic_acid,
  pentanoic_acid = pentanoic_acid,
  acetaldheyde = acetaldheyde,
  formic_acid = formic_acid,
  methanthiol = methanthiol,
  acetone = acetone,
  `2_butanone` = `2_butanone`,
  `2_3_butandion` = `2_3_butandion`,
  phenol = phenol,
  `4_methyl_phenol` = `4_methyl_phenol`,
  `4_ethyl_phenol` = `4_ethyl_phenol`,
  propanoic_acid = propanoic_acid,
  trimethylamine = trimethylamine,
  isoprene = isoprene,
  hydrogen_sulfide = hydrogen_sulfide,
  methanol = methanol,
  dimethyl_sulfide = dimethyl_sulfide,
  `3_methylindole` = `3_methylindole`,
  butanoic_acid = butanoic_acid
)

# View the first few rows of the dataframe
View(OAV)
str(OAV)
# Add the 'hours' column

OAV$hours <- seq(0, by = 0.5, length.out = nrow(OAV))

# Step 2: Find the irregular interval and adjust all subsequent values
# Assuming the irregular interval is at index 17 (8am to 9am instead of 8am to 8:30am)
irregular_index <- 185  # Change this to the actual row where the irregularity occurs

# Add 0.5 to all rows after the irregular interval
OAV$hours[irregular_index:nrow(OAV)] <- 
  OAV$hours[irregular_index:nrow(OAV)] + 0.5

# Calculate SOAV excluding hours column
OAV$SOAV <- rowSums(OAV[, !names(OAV) %in% "hours"], na.rm = TRUE)

# Verify results
View(OAV)

# Group compounds into categories
compound_categories <- c(
  "acetic_acid" = "VFA", 
  "pentanoic_acid" = "VFA", 
  "acetaldheyde" = "Other", 
  "formic_acid" = "VFA", 
  "methanthiol" = "VSC", 
  "acetone" = "Other", 
  "X2_butanone" = "Other", 
  "X2_3_butandion" = "Other", 
  "phenol" = "Phenol", 
  "X4_methyl_phenol" = "Phenol", 
  "X4_ethyl_phenol" = "Phenol", 
  "propanoic_acid" = "VFA", 
  "trimethylamine" = "Other", 
  "isoprene" = "Other", 
  "hydrogen_sulfide" = "VSC", 
  "methanol" = "Other", 
  "dimethyl_sulfide" = "VSC", 
  "X3_methylindole" = "Indole", 
  "butanoic_acid" = "VFA"
)

# Reshape and categorize data

data_long <- OAV %>%
  pivot_longer(
    cols = -c(hours, SOAV),
    names_to = "Compound",
    values_to = "Contribution"
  ) %>%
  mutate(Category = compound_categories[Compound])
View(data_long)

# Calculate max complete interval
max_hour <- max(OAV$hours)
max_interval <- floor(max_hour/5)*5

# Print first few rows of original data to verify hours

print(head(OAV$hours))

# Verify hour grouping calculation

print(head(data_long %>% 
             mutate(hour_group = floor(hours/5)*5) %>%
             select(hours, hour_group)))

# Adjust hour grouping in data processing
hourly_data <- data_long %>%
  mutate(
    # Ensure hours are properly grouped starting at 0
    hour_group = floor(hours/5)*5,
    hour_label = sprintf("%d-%d", hour_group, hour_group + 5)
  ) %>%
  group_by(hour_group, hour_label, Category) %>%
  summarise(
    Category_Sum = sum(Contribution, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(hour_group) %>%
  mutate(
    Total = sum(Category_Sum, na.rm = TRUE),
    Percentage = (Category_Sum/Total) * 100
  ) %>%
  filter(!is.na(Category))

View(hourly_data)

# Prepare data for area plot
soav_categories <- data_long %>%
  group_by(hours, Category) %>%
  summarise(
    Contribution = sum(Contribution, na.rm = TRUE),
    .groups = 'drop'
  )

# Calculate total contribution by category to determine order
category_totals <- soav_categories %>%
  group_by(Category) %>%
  summarise(Total_Contribution = sum(Contribution, na.rm = TRUE)) %>%
  arrange(Total_Contribution)

# Print the order for verification
print("Categories ordered by total contribution (highest to lowest):")
print(category_totals)

# Reorder the Category factor in soav_categories
soav_categories$Category <- factor(soav_categories$Category, 
                                   levels = category_totals$Category)

# Also reorder in hourly_data if it exists
if(exists("hourly_data")) {
  hourly_data$Category <- factor(hourly_data$Category, 
                                 levels = category_totals$Category)
}

# Save outputs for plotting
write.csv(soav_categories, "../output-bLS/soav_categories.csv", row.names = FALSE)
write.csv(hourly_data,     "../output-bLS/OAV_by_hour_cat.csv", row.names = FALSE)
write.csv(data_long,       "../output-bLS/OAV_by_hour.csv",     row.names = FALSE)
write.csv(OAV,             "../output-bLS/OAV.csv",             row.names = FALSE)

# ---- Plot code has been moved to plot.R ----

