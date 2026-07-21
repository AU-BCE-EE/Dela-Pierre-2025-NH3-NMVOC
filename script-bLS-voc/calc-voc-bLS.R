# ==============================================================================
# NMVOC Flux Calculations - bLS Model
# ==============================================================================


# ==============================================================================
# 1. LOAD AND PROCESS CONCENTRATION DATA
# ==============================================================================

observed_concentration <- read_csv("../input-bLS/plot_voc_bLS.csv")
observed_concentration$RoundedTime <- as.POSIXct(observed_concentration$RoundedTime, format = "%d/%m/%Y %H.%M", tz = "UTC")
# Function to process compounds with background correction using last 24 hours
process_compound <- function(data, observed_values, time_column = "RoundedTime") {
  max_time <- max(data[[time_column]], na.rm = TRUE)
  bg_start_time <- max_time - hours(24)
  
  background_indices <- which(data[[time_column]] >= bg_start_time & 
                                data[[time_column]] <= max_time)
  
  cat("Background period:", as.character(bg_start_time), "to", 
      as.character(max_time), "\n")
  cat("Background indices:", length(background_indices), "time points\n")
  
  background_range <- observed_values[background_indices]
  adjusted_concentration <- observed_values
  repeated_background <- numeric(length(observed_values))
  
  repeated_background[background_indices] <- background_range
  
  pre_bg_indices <- seq_len(min(background_indices) - 1)
  
  if(length(pre_bg_indices) > 0) {
    for (i in pre_bg_indices) {
      bg_index <- ((i - 1) %% length(background_range)) + 1
      repeated_background[i] <- background_range[bg_index]
    }
    adjusted_concentration[pre_bg_indices] <- 
      observed_values[pre_bg_indices] - repeated_background[pre_bg_indices]
  }
  
  adjusted_concentration[background_indices] <- 0
  adjusted_concentration[adjusted_concentration < 0] <- 0
  
  return(list(
    adjusted = adjusted_concentration,
    background = repeated_background,
    background_indices = background_indices,
    background_start_time = bg_start_time,
    background_end_time = max_time
  ))
}

# Define column mapping
column_mapping <- list(
  pentanoic_acid = "m_z_85+103_pentanoic_acid_ug",
  isoprene = "m_z_69.00_isoprene_ug",
  butandione = "m_z_87.00_2_3_butandion_ug",
  acetaldehyde = "m_z_45.00_acetladheyde_ug",
  butanone = "m_z_73.00_2_butanone_ug",
  acetic_acid = "m_z_61+43_acetic_acid_ug",
  H2S = "m_z_35_H2S_ug",
  acetone = "m_z_59.00_acetone_ug",
  methylphenol = "m_z_109_4-Methylphenol_ug",
  trimethylamine = "m_z_60.00_trimethylamine_ug",
  ethylphenol = "m_z_123.00_4_ethyl_phenol_ug",
  butanoic_acid = "m_z_71+89_butanoic_acid_ug",
  formic_acid = "m_z_47.00_formic_acid_ug",
  methanol = "m_z_33_methanol_ug",
  phenol = "m_z_95.00_phenol_ug",
  propanoic_acid = "m_z_57+75_propanoic_acid_ug",
  methanthiol = "m_z_49.00_methanthiol_ug",
  dimethyl_sulfide = "m_z_63.00_dimethyl_sulfide_ug",
  methyl_indole = "m_z_132.00_3_methyl_indole_ug"
)

# Build compounds list
compounds <- list()
available_columns <- names(observed_concentration)

for (comp_name in names(column_mapping)) {
  col_name <- column_mapping[[comp_name]]
  matching_cols <- grep(gsub("\\+", "\\\\+", gsub("\\.", "\\\\.", col_name)), 
                        available_columns, value = TRUE)
  
  if (length(matching_cols) > 0) {
    compounds[[comp_name]] <- observed_concentration[[matching_cols[1]]]
    cat("Added", comp_name, "from column", matching_cols[1], "\n")
  }
}

# Process all compounds
results <- list()
for(comp_name in names(compounds)) {
  tryCatch({
    results[[comp_name]] <- process_compound(observed_concentration, 
                                             compounds[[comp_name]])
    cat("Successfully processed:", comp_name, "\n")
  }, error = function(e) {
    cat("Error processing", comp_name, ":", e$message, "\n")
  })
}

# Create adjusted concentration dataframe
adjusted_conc <- data.frame(Time = observed_concentration$RoundedTime)

repeated_bg <- data.frame(Time = observed_concentration$RoundedTime)

for (name in names(results)) {
  adjusted_conc[[paste0("adjusted_concentration_", name)]] <- results[[name]]$adjusted
  repeated_bg[[paste0("background_", name)]] <- results[[name]]$background
}

# ==============================================================================
# 2. TIME PROCESSING AND FILTERING
# ==============================================================================

# Define problem time range (compounds exhibited unlikely peaks during istrument malfunction, these are remove
start_time <- ymd_hms("2024-09-19 16:30:00")
end_time <- ymd_hms("2024-09-20 16:30:00")

# Set specific values to NA for problem time range
specific_columns <- c(
  "adjusted_concentration_butandione", "adjusted_concentration_butanone",
  "adjusted_concentration_methyl_indole", "adjusted_concentration_ethylphenol",
  "adjusted_concentration_acetone", "adjusted_concentration_dimethyl_sulfide",
  "adjusted_concentration_H2S", "adjusted_concentration_methanthiol",
  "adjusted_concentration_trimethylamine"
)

existing_columns <- intersect(names(adjusted_conc), specific_columns)

if (length(existing_columns) > 0) {
  adjusted_conc <- adjusted_conc %>%
    mutate(across(
      all_of(existing_columns),
      ~if_else(Time >= start_time & Time <= end_time, NA_real_, .)
    ))
}

# Add hours column
adjusted_conc$hours <- seq(0, by = 0.5, length.out = nrow(adjusted_conc))
irregular_index <- 185
if (irregular_index <= nrow(adjusted_conc)) {
  adjusted_conc$hours[irregular_index:nrow(adjusted_conc)] <- 
    adjusted_conc$hours[irregular_index:nrow(adjusted_conc)] + 0.5
}

# ==============================================================================
# 3. LOAD bLS DATA AND CALCULATE FLUXES
# ==============================================================================

bLS <- read_delim("../input-bLS/Run1E5_2_PTR_1.txt", 
                  delim = "\t", escape_double = FALSE, trim_ws = TRUE)

bLS$Time <- dmy_hms(bLS$Time)
start_date_bLS <- dmy_hms("18-Sep-2024 12:30:00")
bLS <- bLS[bLS$Time >= start_date_bLS, ]

# Calculate flux values
for (col in names(adjusted_conc)) {
  if (startsWith(col, "adjusted_concentration_")) {
    compound_name <- sub("adjusted_concentration_", "", col)
    flux_name <- paste0("Flux_", compound_name)
    
    bLS[[flux_name]] <- NA
    
    for (i in 1:nrow(bLS)) {
      bls_time <- bLS$Time[i]
      
      if (!is.na(bls_time) && (bls_time >= start_time && bls_time <= end_time)) {
        if (compound_name %in% c("butandione", "butanone", "methyl_indole", 
                                 "ethylphenol", "acetone", "dimethyl_sulfide", 
                                 "H2S", "methanthiol", "trimethylamine")) {
          bLS[i, flux_name] <- NA
          next
        }
      }
      
      if (!is.na(bls_time)) {
        time_diff <- abs(difftime(adjusted_conc$Time, bls_time, units = "mins"))
        closest_idx <- which.min(time_diff)
        
        if (min(time_diff) <= 15) {
          bLS[i, flux_name] <- adjusted_conc[closest_idx, col] / bLS$CE[i]
        }
      }
    }
  }
}

# ==============================================================================
# 4. APPLY FILTERING CRITERIA
# ==============================================================================

filtered_Flux_odors <- bLS %>%
  dplyr::mutate(
    is_first_row = row_number() == 1,
    UST = ifelse(is_first_row, UST, ifelse(UST < 0.05, NA, UST)),
    L = ifelse(is_first_row, L, ifelse(abs(L) < 2, NA, L)),
    z0 = ifelse(is_first_row, z0, ifelse(z0 > 0.1, NA, z0)),
    sigU = ifelse(is_first_row, sigU, ifelse(sigU > 4.5, NA, sigU)),
    sigV = ifelse(is_first_row, sigV, ifelse(sigV > 4.5, NA, sigV)),
    C0 = ifelse(is_first_row, C0, ifelse(C0 > 10, NA, C0)),
    across(starts_with("Flux_"), 
           ~ifelse(is_first_row, .,
                   ifelse(is.na(UST) | is.na(L) | is.na(z0) | 
                            is.na(sigU) | is.na(sigV) | is.na(C0), NA, .)))
  ) %>%
  dplyr::select(-is_first_row) %>%
  dplyr::arrange(Time) %>%
  dplyr::mutate(hours = as.numeric(difftime(Time, min(Time, na.rm = TRUE), units = "hours")))

# ==============================================================================
# 5. HANDLE TIME GAPS- there is a 30 min interval for which the sonic elaboration
   
# ==============================================================================

flux_cols <- grep("^Flux_", names(filtered_Flux_odors), value = TRUE)
filtered_Flux_odors$time_diff <- c(0, as.numeric(diff(filtered_Flux_odors$Time), 
                                                 units = "secs"))

# Add missing 08:30 row if needed
idx_08_00 <- which(format(filtered_Flux_odors$Time, "%d/%m/%Y %H:%M") == "22/09/2024 08:00")
idx_09_00 <- which(format(filtered_Flux_odors$Time, "%d/%m/%Y %H:%M") == "22/09/2024 09:00")

if (length(idx_08_00) > 0 && length(idx_09_00) > 0) {
  row_08_00 <- filtered_Flux_odors[idx_08_00, ]
  new_row <- row_08_00
  new_time <- ymd_hms("2024-09-22 08:30:00", 
                      tz = attr(filtered_Flux_odors$Time, "tzone"))
  new_row$Time <- new_time
  
  flux_columns <- grep("Flux", names(filtered_Flux_odors), value = TRUE)
  for (col in flux_columns) {
    new_row[[col]] <- NA
  }
  
  filtered_Flux_odors <- rbind(
    filtered_Flux_odors[1:idx_08_00, ],
    new_row,
    filtered_Flux_odors[(idx_08_00+1):nrow(filtered_Flux_odors), ]
  )
  
  filtered_Flux_odors$hours <- seq(0, by = 0.5, length.out = nrow(filtered_Flux_odors))
}

filtered_Flux_odors$time_diff <- NULL

# ==============================================================================
# 6. INTERPOLATE FLUX VALUES
# ==============================================================================

problematic_compounds <- c(
  "Flux_butandione", "Flux_butanone", "Flux_methyl_indole", 
  "Flux_ethylphenol", "Flux_acetone", "Flux_dimethyl_sulfide", 
  "Flux_H2S", "Flux_methanthiol", "Flux_trimethylamine"
)

filtered_Flux_odors <- filtered_Flux_odors %>%
  mutate(across(
    all_of(intersect(names(filtered_Flux_odors), problematic_compounds)),
    ~ifelse(Time >= start_time & Time <= end_time, NA_real_, .)
  ))

problem_indices <- which(filtered_Flux_odors$Time >= start_time & 
                           filtered_Flux_odors$Time <= end_time)

for (col in flux_cols) {
  if (col %in% problematic_compounds) {
    if (length(problem_indices) > 0 && min(problem_indices) > 1) {
      before_indices <- 1:(min(problem_indices) - 1)
      filtered_Flux_odors[[col]][before_indices] <- na.approx(
        filtered_Flux_odors[[col]][before_indices], 
        x = filtered_Flux_odors$Time[before_indices], 
        na.rm = FALSE
      )
    }
    
    if (length(problem_indices) > 0 && max(problem_indices) < nrow(filtered_Flux_odors)) {
      after_indices <- (max(problem_indices) + 1):nrow(filtered_Flux_odors)
      filtered_Flux_odors[[col]][after_indices] <- na.approx(
        filtered_Flux_odors[[col]][after_indices], 
        x = filtered_Flux_odors$Time[after_indices], 
        na.rm = FALSE
      )
    }
  } else {
    filtered_Flux_odors[[col]] <- na.approx(
      filtered_Flux_odors[[col]], 
      x = filtered_Flux_odors$Time, 
      na.rm = FALSE
    )
  }
}

filtered_Flux_odors <- filtered_Flux_odors %>%
  dplyr::arrange(Time) %>%
  dplyr::mutate(hours = seq(0, by = 0.5, length.out = n()))

# ==============================================================================
# 7. CALCULATE MASS EMISSIONS
# ==============================================================================

# Function to standardize compound names
standardize_compound_name <- function(name) {
  name <- gsub("^adjusted_concentration_|^Flux_", "", name)
  mapping <- c(
    "methanthiol" = "Methanethiol",
    "acetone" = "Acetone",
    "butanone" = "2-Butanone",
    "butandione" = "2,3-Butanedione",
    "phenol" = "Phenol",
    "methylphenol" = "4-Methylphenol",
    "ethylphenol" = "4-Ethylphenol",
    "propanoic_acid" = "Propionic Acid",
    "trimethylamine" = "Trimethylamine",
    "isoprene" = "Isoprene",
    "H2S" = "Hydrogen Sulfide",
    "methanol" = "Methanol",
    "dimethyl_sulfide" = "Dimethyl Sulfide",
    "methyl_indole" = "3-Methylindole",
    "butanoic_acid" = "Butanoic Acid",
    "acetic_acid" = "Acetic Acid",
    "formic_acid" = "Formic Acid",
    "pentanoic_acid" = "Pentanoic Acid",
    "acetaldehyde" = "Acetaldehyde"
  )
  
  if (name %in% names(mapping)) {
    return(mapping[name])
  } else {
    return(name)
  }
}

# Create long format data
adjusted_concentrations_long <- adjusted_conc %>%
  select(-hours) %>%
  pivot_longer(
    cols = -Time,
    names_to = "Compound",
    values_to = "Adjusted_Concentration"
  ) %>%
  mutate(Compound = sapply(Compound, standardize_compound_name))

filtered_Flux_odors_long <- filtered_Flux_odors %>%
  pivot_longer(
    cols = starts_with("Flux_"),
    names_to = "Compound",
    values_to = "Flux"
  ) %>%
  mutate(Compound = sapply(Compound, standardize_compound_name))

# Calculate mass emissions
mass_emissions_long <- filtered_Flux_odors_long %>%
  mutate(
    Mass_Emission = (Flux * 60) / 1000,
    Category = case_when(
      Compound %in% c("Acetic Acid", "Pentanoic Acid", "Formic Acid",
                      "Propionic Acid", "Butanoic Acid") ~ "VFA",
      Compound %in% c("Methanethiol", "Hydrogen Sulfide", "Dimethyl Sulfide") ~ "VSC",
      Compound %in% c("Phenol", "4-Methylphenol", "4-Ethylphenol") ~ "Phenol",
      Compound == "3-Methylindole" ~ "Indole",
      TRUE ~ "Other"
    )
  )
mass_emissions_long$Compound
# Calculate mass emissions by category
mass_emissions_by_category <- mass_emissions_long %>%
  dplyr::group_by(hours, Category) %>%
  dplyr::summarise(
    Category_Mass = sum(Mass_Emission, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::group_by(hours) %>%
  dplyr::mutate(Total_Mass = sum(Category_Mass, na.rm = TRUE))
View(mass_emissions_long)
View(mass_emissions_by_category)
# Calculate category totals for ordering
category_totals_mass <- mass_emissions_by_category %>%
  dplyr::group_by(Category) %>%
  dplyr::summarise(Total_Mass_Contribution = sum(Category_Mass, na.rm = TRUE)) %>%
  dplyr::arrange(Total_Mass_Contribution)
View(category_totals_mass)
# Apply factor ordering
mass_emissions_by_category$Category <- factor(
  mass_emissions_by_category$Category, 
  levels = category_totals_mass$Category
)

# Calculate mass emissions by compound
mass_emissions_by_compound <- mass_emissions_long %>%
  dplyr::group_by(hours, Compound, Category) %>%
  dplyr::summarise(
    Compound_Mass = sum(Mass_Emission, na.rm = TRUE),
    .groups = 'drop'
  )

mass_emissions_by_compound$Category <- factor(
  mass_emissions_by_compound$Category, 
  levels = category_totals_mass$Category
)

# Calculate cumulative emissions
cumulative_emissions_by_category <- mass_emissions_long %>%
  dplyr::group_by(hours, Category) %>%
  dplyr::summarise(Mass_Emission = sum(Mass_Emission * 30, na.rm = TRUE), .groups = 'drop') %>%
  dplyr::group_by(Category) %>%
  dplyr::arrange(hours) %>%
  dplyr::mutate(Cumulative_Mass = cumsum(replace_na(Mass_Emission, 0))) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Category = factor(Category, levels = category_totals_mass$Category))

cumulative_emissions_by_compound <- mass_emissions_long %>%
  dplyr::group_by(hours, Compound) %>%
  dplyr::summarise(Mass_Emission = sum(Mass_Emission * 30, na.rm = TRUE), .groups = 'drop') %>%
  dplyr::group_by(Compound) %>%
  dplyr::arrange(hours) %>%
  dplyr::mutate(Cumulative_Mass = cumsum(replace_na(Mass_Emission, 0))) %>%
  dplyr::ungroup()

# Calculate contribution by compound
contribution_by_compound <- mass_emissions_by_compound %>%
  dplyr::group_by(hours, Category) %>%
  dplyr::mutate(Total_Category_Mass = sum(Compound_Mass, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Contribution_Percentage = (Compound_Mass / Total_Category_Mass) * 100)

cumulative_emissions <- mass_emissions_long %>%
  dplyr::group_by(Compound) %>%
  dplyr::arrange(hours) %>%
  dplyr::mutate(Cumulative_Mass = cumsum(replace_na(Mass_Emission, 0))) %>%
  dplyr::ungroup()

# ==============================================================================
# 8. SAVE OUTPUTS
# ==============================================================================

write.csv(adjusted_conc, "../output-bLS/odors_adjusted_conc.csv", row.names = FALSE)
write.csv(filtered_Flux_odors, "../output-bLS/fluxes_vocs_1stweek.csv", row.names = FALSE)
write.csv(mass_emissions_long, "../output-bLS/mass_emissions.csv", row.names = FALSE)
write.csv(cumulative_emissions_by_category, 
          "../output-bLS/cumulative_emissions_by_category.csv", row.names = FALSE)
write.csv(cumulative_emissions_by_compound, 
          "../output-bLS/contribution_by_compound_mass_cum.csv", row.names = FALSE)
write.csv(mass_emissions_by_category, 
          "../output-bLS/mass_emissions_cat.csv", row.names = FALSE)

cat("\n=== CALCULATIONS COMPLETE ===\n")
cat("All output files saved to ../output-bLS/\n")

