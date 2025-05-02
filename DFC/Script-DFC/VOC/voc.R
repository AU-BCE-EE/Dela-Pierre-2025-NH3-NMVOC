########################################################################################
#----- Flux calculation prerequisite data ------------ 
########################################################################################
#VOC flux prerequisite components#
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

# Convert Mass data to long format
MW_long <- MW %>%
  pivot_longer(-compund, names_to = "VOC", values_to = "value") %>%
  pivot_wider(names_from = compund, values_from = value) %>%
  mutate(MW = as.numeric(MW))

s.MW <- MW_long %>%
  filter(VOC %in% c("Hydrogen sulfide", "Methanthiol", "Dimethyl sulfide")) %>%
  mutate(RN = case_when(
    row_number() == 1 ~ "voc1",
    row_number() == 2 ~ "voc2",
    row_number() == 3 ~ "voc3",
    TRUE ~ as.character(RN)
  ))
########################################################################################
#----- Setting data for sulfur compounds ------------ 
########################################################################################
oth.data <- dat %>%
  filter(elapsed.time == 0) %>%
  select(air.temp.K, valve, air.flow, dfc.area)
s.dat <- left_join(s.dat, oth.data, by = c('valve' = 'valve'))

########################################################################################
#----- Calculating flux for sulfur ------------ 
########################################################################################

# Sulfur flux
for (i in 1:3) {
  s.col <- paste0("voc.corr", i)
  s.mol <- paste0("voc.mol", i)
  s.flux <- paste0("voc_flux", i)
  
  s.voc_id <- paste0("voc", i)
  s.mw_value <- s.MW %>% filter(RN == s.voc_id) %>% pull(MW)
  
  if (s.col %in% names(s.dat) && length(s.mw_value) > 0 && !is.na(s.mw_value)) {
    s.dat[[s.mol]] <- (atm.con / (g.con * s.dat$air.temp.K)) * s.dat[[s.col]] * 1e-9
    s.dat[[s.flux]] <- ((s.dat[[s.mol]] * s.mw_value * s.dat$air.flow) / s.dat$dfc.area) * 1000
  } else {
    warning(paste("Column", s.col, "does not exist, or molecular weight is missing"))
  }
}


########################################################################################
#----- Calculating flux for all voc ------------ 
########################################################################################
#VOC flux
for (i in 1:19) {
  voc_bg_col <- paste0("voc_corr", i)
  voc_mol_col <- paste0("voc_corr_mol", i)
  flux_voc_col <- paste0("voc_flux", i)
  
  #Get the molecular weight for VOC
  voc_id <- paste0("voc", i)
  mw_value <- MW_long %>% filter(RN == voc_id) %>% pull(MW)
  
  #Convert voc_corr from ppb to mol
  if (voc_bg_col %in% names(dat) && length(mw_value) > 0 && !is.na(mw_value)) {
    
    dat[[voc_mol_col]] <- (atm.con / (g.con * dat$air.temp.K)) * dat[[voc_bg_col]] * 10^-9
    
    # Calculate flux from mol * L^-1 to mg * min^-1 * m^-2
    if (length(dat[[voc_bg_col]]) > 0 && length(dat$air.flow) > 0 && length(dat$dfc.area) > 0 && length(dat[[voc_mol_col]]) > 0) {
      dat[[flux_voc_col]] <- ((dat[[voc_mol_col]] * mw_value * dat$air.flow) / dat$dfc.area) * 1000
    } else {
      warning(paste("One or more required columns are empty for flux calculation:", flux_voc_col))
    }
  } else {
    warning(paste("Column", voc_bg_col, "does not exist in the data frame, or molecular weight is missing"))
  }
}

# Identify the columns to be deleted
columns_to_delete <- c(
  paste0("voc_corr_mol", 1:19),
  paste0("voc.bg", 1:19),
  paste0("voc_corr", 1:19)
  
)
col_delete <- c(
  paste0("voc.mol", 1:3),
  paste0("voc.bg", 1:3),
  paste0("voc.corr", 1:3),
  paste0("voc", 1:3)
  
)
# Remove the identified columns from the dat data frame
s.dat <- s.dat %>% select(-all_of(col_delete))
dat <- dat %>% select(-all_of(columns_to_delete))
dat <- dat[, -c(4, 6:24, 27:33)]
options(scipen = 999)
colnames(s.dat)[11:13] <- c("voc_flux2", "voc_flux10", "voc_flux13")


combined_mean <- dat %>%
  left_join(s.dat %>% select(elapsed.time, valve, voc_flux2, voc_flux10, voc_flux13),
            by = c("elapsed.time", "valve"),
            suffix = c("", "_sdat")) %>%
  mutate(
    voc_flux2  = voc_flux2  + ifelse(is.na(voc_flux2_sdat),  0, voc_flux2_sdat),
    voc_flux10 = voc_flux10 + ifelse(is.na(voc_flux10_sdat), 0, voc_flux10_sdat),
    voc_flux13 = voc_flux13 + ifelse(is.na(voc_flux13_sdat), 0, voc_flux13_sdat)
  ) %>%
  select(-voc_flux2_sdat, -voc_flux10_sdat, -voc_flux13_sdat)

#Rename vocs
dat <- combined_mean %>%
  rename(
    methanol = voc_flux1,
    H2S = voc_flux2,
    `4_Methylphenol` = voc_flux3,
    acetic_acid = voc_flux4,
    butanoic_acid = voc_flux5,
    pentanoic_acid = voc_flux6,
    propanoic_acid = voc_flux7,
    acetladheyde = voc_flux8,
    formic_acid = voc_flux9,
    methanthiol = voc_flux10,
    acetone = voc_flux11,
    trimethylamine = voc_flux12,
    dimethyl_sulfide = voc_flux13,
    isopren = voc_flux14,
    butanone = voc_flux15,
    butandion = voc_flux16,
    phenol = voc_flux17,
    `4_ethyl_phenol` = voc_flux18,
    methyl_indole = voc_flux19
  )

########################################################################################
#----- Calculating cumulative emissions ------------ 
########################################################################################

#Cumulative emissions#

#creating new dataset for cumulative
cum.voc <- dat  
names(cum.voc)[7:25] <- paste0("voc", 1:19)

# Define the VOC column names
voc_cols <- paste0("voc", 1:19)

#Calculating cumulative emissions#
for (i in seq_along(voc_cols)) {
  new_col <- paste0("cum.treat", i)
  cat("Processing VOC", i, "(", voc_cols[i], ")\n")
  cum.voc[[new_col]] <- mintegrate(cum.voc$elapsed.time * 60, 
                                   cum.voc[[voc_cols[i]]], 
                                   by = cum.voc$valve, method = 'trap')
}

#Removing unnecessary data
cum.voc <- cum.voc [, -c(7:25)]

#Renaming vocs
cum.voc <- cum.voc %>%
  rename(
    Methanol = cum.treat1,
    `Hydrogen sulfide` = cum.treat2,
    `4-Methylphenol` = cum.treat3,
    `Acetic acid` = cum.treat4,
    `Butanoic acid` = cum.treat5,
    `Pentanoic acid` = cum.treat6,
    `Propanoic acid` = cum.treat7,
    Acetaldehyde = cum.treat8,
    `Formic acid` = cum.treat9,
    Methanethiol = cum.treat10,
    Acetone = cum.treat11,
    Trimethylamine = cum.treat12,
    `Dimethyl sulfide` = cum.treat13,
    Isoprene = cum.treat14,
    Butanone = cum.treat15,
    Butanedione = cum.treat16,
    Phenol = cum.treat17,
    `4-ethyl phenol` = cum.treat18,
    `Methyl indole` = cum.treat19
  )

########################################################################################
#----- Setting data for plots ------------ 
########################################################################################

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
names(dat)[7:(7 + length(voc_names) - 1)] <- voc_names

# Convert data to long format
dat_long <- dat %>%
  pivot_longer(
    cols = c(Methanol:`Hydrogen sulfide`, `4-Methylphenol`:`Methyl indole`),
    names_to = "compound",
    values_to = "value"
  )

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

#Adding the group information to the long data frame
dat_long <- dat_long %>%
  mutate(Group = factor(voc_category[compound], levels = c("Carboxylic Acids", "Indole", "Phenols", "Volatile Sulfur Compounds (VSC)", "Other")))

#Taking sum of voc data for plotting
dat_sum <- dat_long %>%
  group_by(elapsed.time, treatment, valve, Group) %>%
  summarise(Sumvoc = sum(value), .groups = "drop")

#Aggregating voc data for plotting
dat_summary <- dat_sum %>%
  group_by(elapsed.time, treatment, Group) %>%
  summarise(Avgvoc = mean(Sumvoc), .groups = "drop")

dat_summary$Group <- factor(dat_summary$Group, levels = c(
  "Volatile Sulfur Compounds (VSC)", "Phenols", "Carboxylic Acids", "Other", "Indole"
))

