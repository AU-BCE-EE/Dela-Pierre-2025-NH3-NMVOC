
# Read data files
CRDS_bLS_2_ <- read_csv("../input-bLS/CRDS_bLS.csv")
CRDS_bg_2_ <- read_csv("../input-bLS/CRDS_bg.csv")

# Convert time columns to proper datetime format
CRDS_bg_2_$RoundedTime <- as.POSIXct(CRDS_bg_2_$RoundedTime, format="%d/%m/%Y %H:%M", tz="UTC")
CRDS_bLS_2_$RoundedTime <- as.POSIXct(CRDS_bLS_2_$RoundedTime, format="%d/%m/%Y %H:%M", tz="UTC")

# Define correct date range
start_date <- as.POSIXct("2024-09-18 12:30:00", tz = "UTC")
end_date <- as.POSIXct("2024-09-23 12:00:00", tz = "UTC")

# Filter data based on date range
CRDS_bLS_2_ <- CRDS_bLS_2_[CRDS_bLS_2_$RoundedTime >= start_date & CRDS_bLS_2_$RoundedTime <= end_date, ]
CRDS_bg_2_ <- CRDS_bg_2_[CRDS_bg_2_$RoundedTime >= start_date & CRDS_bg_2_$RoundedTime <= end_date, ]

# Rename background column
names(CRDS_bg_2_)[names(CRDS_bg_2_) == "NH3_ug"] <- "NH3_bg"

# Read bLS data
bLS <- read.table("../input-bLS/Run1E5_2_CRDS_1.txt", row.names=NULL, header = T, sep="")
bLS$Time <- dmy_hms(bLS$Time)
str(bLS$Time)
# Filter bLS data
start_date_bLS <- as.POSIXct("2024-09-18 12:30:00", format="%Y-%m-%d %H:%M:%S", tz="UTC")
bLS <- bLS[bLS$Time >= start_date_bLS, ]
# Combine data
concentrations <- cbind(CRDS_bLS_2_, CRDS_bg_2_$NH3_bg)
concentrations <- concentrations[-186, ]

# Remove row 186
names(concentrations)[names(concentrations) == "CRDS_bg_2_$NH3_bg"] <- "NH3_bg"

# Add concentration data to bLS
bLS <- cbind(bLS, concentrations$NH3_ug, concentrations$NH3_bg)

# Calculate flux
bLS$Flux <- (bLS$`concentrations$NH3_ug` - bLS$`concentrations$NH3_bg`) / bLS$CE

# Filter flux data
filtered_Flux <- bLS %>%
  dplyr::mutate(
    # Flag first row
    is_first_flux = row_number() == 1,
    
    # Apply conditions except to first row
    UST = ifelse(!is_first_flux & UST < 0.05, NA, UST),
    L = ifelse(!is_first_flux & abs(L) < 2, NA, L),
    z0 = ifelse(!is_first_flux & z0 > 0.1, NA, z0),
    sigU = ifelse(!is_first_flux & sigU > 4.5, NA, sigU),
    sigV = ifelse(!is_first_flux & sigV > 4.5, NA, sigV),
    C0 = ifelse(!is_first_flux & C0 > 10, NA, C0),
    
    # Update Flux preserving first row
    Flux = ifelse(
      !is_first_flux & (is.na(UST) | is.na(L) | is.na(z0) | 
                          is.na(sigU) | is.na(sigV) | is.na(C0)),
      NA, 
      Flux
    )
  ) %>%
  dplyr::select(-is_first_flux)

# Save original flux values
filtered_Flux$Flux_original <- filtered_Flux$Flux

# Calculate time differences to identify gaps
filtered_Flux$time_diff <- c(1800, diff(as.numeric(filtered_Flux$Time)))
cat("Time differences summary:\n")
print(summary(filtered_Flux$time_diff))

# Check for gaps > 1800 seconds
gaps <- which(filtered_Flux$time_diff > 1800)
cat("Found", length(gaps), "gaps > 1800 seconds\n")
if (length(gaps) > 0) {
  cat("Gap indices:", gaps, "\n")
  cat("Gap at index", gaps, "has time difference of", filtered_Flux$time_diff[gaps], "seconds\n")
}

# Find the indices of the rows with 08:00 and 09:00
idx_08_00 <- which(format(filtered_Flux$Time, "%d/%m/%Y %H:%M") == "22/09/2024 08:00")
idx_09_00 <- which(format(filtered_Flux$Time, "%d/%m/%Y %H:%M") == "22/09/2024 09:00")

# Check if both rows exist
if (length(idx_08_00) > 0 && length(idx_09_00) > 0) {
  # Get the rows for 08:00 and 09:00
  row_08_00 <- filtered_Flux[idx_08_00, ]
  
  # Create a new row with values from 08:00 row
  new_row <- row_08_00
  
  # Create the time for 08:30 explicitly using POSIXct
  new_time <- as.POSIXct("2024-09-22 08:30:00", format="%Y-%m-%d %H:%M:%S", 
                         tz = attr(filtered_Flux$Time, "tzone"))
  
  # Double-check the time format
  cat("New row time:", format(new_time, "%d/%m/%Y %H:%M"), "\n")
  
  new_row$Time <- new_time
  
  # Set NA for Flux values instead of interpolating
  new_row$Flux <- NA
  new_row$Flux_original <- NA
  
  # Insert the new row between the two existing rows
  filtered_Flux <- rbind(
    filtered_Flux[1:idx_08_00, ],
    new_row,
    filtered_Flux[(idx_08_00+1):nrow(filtered_Flux), ]
  )
  
  # Recalculate hours column to ensure it has exact 0.5 hour intervals
  filtered_Flux$hours <- seq(0, by = 0.5, length.out = nrow(filtered_Flux))
  
  cat("Inserted row with NA flux at 22/09/2024 08:30\n")
  
  # Display the three rows to verify
  subset_rows <- which(format(filtered_Flux$Time, "%d/%m/%Y %H") %in% c("22/09/2024 08", "22/09/2024 09"))
  print(filtered_Flux[subset_rows, c("Time", "Flux", "Flux_original")])
} else {
  cat("Could not find rows for both 22/09/2024 08:00 and 22/09/2024 09:00\n")
}
# Remove the time_diff column as it's no longer needed
filtered_Flux$time_diff <- NULL
filtered_Flux$Time
# Recalculate time differences to verify
filtered_Flux$time_diff <- c(1800, diff(as.numeric(filtered_Flux$Time)))
cat("Time differences after interpolation:\n")
print(summary(filtered_Flux$time_diff))

# Interpolate NA values - keeping separate from original
filtered_Flux$Flux <- na.approx(filtered_Flux$Flux, x = filtered_Flux$Time, na.rm = FALSE)

# Add hours column with exact 0.5 hour intervals
filtered_Flux$hours <- seq(0, by = 0.5, length.out = nrow(filtered_Flux))

# Convert Flux from ?g/m?/s to kg/s
filtered_Flux$Flux_kg_s <- filtered_Flux$Flux * 1e-9 * filtered_Flux$SourceArea
filtered_Flux$Flux_kg_original <- filtered_Flux$Flux_original * 1e-9 * filtered_Flux$SourceArea

# Calculate emission for each interval in kg
filtered_Flux$Emission_kg <- filtered_Flux$Flux_kg_s * 1800  # Each interval is 1800 seconds (30 min)
filtered_Flux$Emission_kg_original <- filtered_Flux$Flux_kg_original * 1800

# Handle NA values for cumulative calculation
filtered_Flux$Emission_kg_non_na_original <- ifelse(
  is.na(filtered_Flux$Emission_kg_original), 
  0, 
  filtered_Flux$Emission_kg_original
)

# Calculate cumulative emissions
filtered_Flux$Cumulative_Emission <- cumsum(filtered_Flux$Emission_kg)
filtered_Flux$Cumulative_Emission_original <- cumsum(filtered_Flux$Emission_kg_non_na_original)

# Calculate slurry application and TAN
filtered_Flux$total_slurry_kg <- ((35*1000) * filtered_Flux$SourceArea) / 10000
filtered_Flux$total_TAN <- (filtered_Flux$total_slurry_kg * 2498.84 * 1E-6)

# Calculate percentage of TAN emitted
filtered_Flux$Emission_applied_N <- (filtered_Flux$Emission_kg / filtered_Flux$total_TAN) * 100
filtered_Flux$Emission_applied_N_no_gap <- (filtered_Flux$Emission_kg_non_na_original / filtered_Flux$total_TAN) * 100

# Calculate cumulative percentage
filtered_Flux$Cumulative_Emission_applied_N <- cumsum(filtered_Flux$Emission_applied_N)* 14.0067/17.031
filtered_Flux$Cumulative_Emission_applied_N_no_gap <- cumsum(filtered_Flux$Emission_applied_N_no_gap)

# Calculate flux in mg/min/m2
filtered_Flux$Flux_mg_min <- filtered_Flux$Flux * 10^-3 * 60

# Save processed data
write.csv(filtered_Flux, "../output-bLS/NH3_bLS.csv", row.names = FALSE)







