########################################################################################
#----- Calculating OAV ------------ 
########################################################################################
#Renaming OTV and VOV ppb file data
voc_names <- c(
  "Methanol",
  "Hydrogen sulfide",
  "4-Methylphenol",
  "Acetic acid",
  "Butanoic acid",
  "Pentanoic acid",
  "Propanoic acid",
  "Acetaldehyde",
  "Formic acid",
  "Methanethiol",
  "Acetone",
  "Trimethylamine",
  "Dimethyl sulfide",
  "Isoprene",
  "Butanone",
  "Butanedione",
  "Phenol",
  "4-ethyl phenol",
  "Methyl indole"
)

names(voc_ppb)[8:(8 + length(voc_names) - 1)] <- voc_names
names(OTV)[2:20] <- voc_names


#Ensuring same column names and order
common_columns <- intersect(names(voc_ppb), names(OTV))
dat_common <- voc_ppb[common_columns]
OTV_columns <- OTV[common_columns]

#Calculating OAV for each VOC
OAV <- as.data.frame(mapply(`/`, dat_common, OTV_columns))

#Adding necessary data to OAV deatset
required_columns <- c("treatment", "group", "elapsed.time", "valve")

if (all(required_columns %in% names(voc_ppb))) {
  OAV <- cbind(voc_ppb[, required_columns], OAV)
} else {
  stop("Missing required columns in voc_ppb: 'treatment', 'group', 'elapsed.time', or 'valve'")
}

#Checking
head(OAV)


########################################################################################
#----- Calculating SOAV ------------ 
########################################################################################

#Selecting data for SOAV calculation
SOAV_columns <- colnames(OAV)[5:23]

#For non-scientific number printing
options(scipen = 999)

#Calculating SOAV from VOC columns only
OAV$SOAV <- rowSums(OAV[, SOAV_columns], na.rm = TRUE)

#Separating SOAV from OAV
SOAV <- OAV %>% select(-all_of(SOAV_columns))

########################################################################################
#----- Setting data for plots ------------ 
########################################################################################
# Define the groups for each compound
voc_category <- c(
  `Acetic acid` = "Carboxylic Acids",
  `Butanoic acid` = "Carboxylic Acids",
  `Pentanoic acid` = "Carboxylic Acids",
  `Propanoic acid` = "Carboxylic Acids",
  `Formic acid` = "Carboxylic Acids",
  `Methyl indole` = "Indole",
  `4-Methylphenol` = "Phenols",
  Phenol = "Phenols",
  `4-ethyl phenol` = "Phenols",
  `Hydrogen sulfide` = "Volatile Sulfur Compounds (VSC)",
  Methanethiol = "Volatile Sulfur Compounds (VSC)",
  `Dimethyl sulfide` = "Volatile Sulfur Compounds (VSC)",
  Methanol = "Other",
  Acetaldehyde = "Other",
  Acetone = "Other",
  Trimethylamine = "Other",
  Isoprene = "Other",
  Butanone = "Other",
  Butanedione = "Other"
)

#Creating long format for plotting
oav_long <- OAV %>%
  pivot_longer(
    cols = c(Methanol:`Hydrogen sulfide`, `4-Methylphenol`:`Methyl indole`),
    names_to = "compound",
    values_to = "value"
  )

#Adding the group information to the long data frame
oav_long <- oav_long %>%
  mutate(Group = factor(voc_category[compound], levels = c("Carboxylic Acids", "Indole", "Phenols", "Volatile Sulfur Compounds (VSC)", "Other")))

#Aggregating OAV data for plotting
oav_summary <- oav_long %>%
  group_by(elapsed.time, treatment, Group) %>%
  summarise(
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  )

#Taking sum for all treatments and groups for plotting
sum.oav <- oav_long %>%
  group_by(elapsed.time, treatment, Group) %>%
  summarize(Sum_Flux = sum(value, na.rm = TRUE), .groups = 'drop')

#Making order for plottiong
desired_order <- c("Acetic acid", "Acetaldehyde", "Acetone", "Butanedione", "Butanoic acid", 
                   "Butanone", "Dimethyl sulfide", "Formic acid", "Hydrogen sulfide", "Isoprene", 
                   "Methanol", "Methanethiol", "Methyl indole", "Pentanoic acid", 
                   "Phenol", "Propanoic acid", "Trimethylamine", "4-ethyl phenol", 
                   "4-Methylphenol")
oav_long$compound<- factor(oav_long$compound, levels = desired_order)


