
library(tidyverse)     
library(lubridate)     
library(patchwork)     
source("mintegrate.R")
# Set constants for calculations
p.con <- 1             # Atmospheric pressure (atm)
R.con <- 0.082057338   # Gas constant (L·atm/(mol·K))
air.flow <- 1927       # Air flow rate (L/min)
A.frame <- (0.7/2)^2 * 3.14  # Chamber area (m^2)

# ========== Import and prepare data ==========
# Import ambient temperature data
weather <- read_csv(""../Flavia_VOC_DFC_data/Temp.csv") #find this in Flavia_VOC_DFC_data folder

# Import VOC data, the 30s background corrected averages created in the VOC concentrations and OAV.R
dat <- read_delim(""../Flavia_VOC_DFC_data/voc.30s.corrected.csv", 
                  delim = ",", escape_double = FALSE, trim_ws = TRUE)
vsc_ini <- read_csv("vsc.first.minutes.txt") #file created in the VOC concentrations and OAV script, these are the initial concentrations for VSC (first 8 minutes)
#remove any duplicate rows in dat, otherwise mintegrate does not work
dat <- dat %>%
  # Group by valve
  group_by(valve) %>%
  # If there are duplicates in elapsed_time, keep the first occurrence
  distinct(elapsed_time, .keep_all = TRUE) %>%
  ungroup()
# ========== Process temperature data ==========
# Parse dates in weather data
weather$date <- parse_date_time(weather$date, orders = c("d/m/y", "d-m/y"))

# Filter for experiment date range
start_date <- ymd("2024-09-18")
end_date <- ymd("2024-09-24")
weather <- weather[weather$date >= start_date & weather$date <= end_date, ]

# Format datetime for joining, otherwise I do not have the same format across the 2 datasets
weather$date.time.weather <- sapply(paste(weather$date, weather$time), function(x) {
  parts <- strsplit(x, " ")[[1]]
  date_part <- parts[1]
  hour_part <- parts[2]
  formatted_hour <- sprintf("%02d:00:00", as.numeric(hour_part))
  paste(date_part, formatted_hour)
})

# ========== Join weather data with VOC data ==========
# Round VOC datetime to match weather data

# Parse the date using dmy_hm (day, month, year, hour, minute format)
dat$date.time.weather <- ymd_hms(dat$end_time)

# Round to nearest hour
dat$date.time.weather<- round_date(dat$date.time.weather, unit = "hour")

# Format the result
dat$date.time.weather <- format(dat$date.time.weather, "%Y-%m-%d %H:%M:%S")

# Join datasets
dat <- left_join(dat, weather, by = 'date.time.weather')

# Convert temperature to Kelvin
dat$temp.K <- dat$metp + 273.15

# ========== Define molecular weights for flux calculation of each compound ==========
mw_data <- data.frame(
  compound = c(
    "methanol", "H2S", "acetladheyde", "formic_acid", "methanthiol",
    "acetone", "trimethylamine", "acetic_acid", "propanoic_acid",
    "butanoic_acid", "pentanoic_acid", "dimethyl_sulfide", "isopren",
    "butanone", "phenol", "X4.Methylphenol", "X4_ethyl_phenol",
    "methyl_indole", "butandion"
  ),
  mw = c(
    32.0262, 33.9877, 44.2620, 46.0055, 48.0034, 58.0419, 59.0735, 
    60.0211, 74.0368, 89.0597, 102.0681, 62.0190, 68.0626, 73.0648, 
    94.0419, 108.0575, 122.0732, 131.0735, 86.0368
  )
)

# ========== Calculate flux for all compounds ==========
# Get all the bg-corrected-concentrations compound column names
corrected_cols <- grep("_corrected$", names(dat), value = TRUE)

# Process each compound
for(col in corrected_cols) {
  # Extract base compound name
  compound_name <- gsub("_corrected$", "", col)
  
  # Find corresopnding molecular weight
  mw <- mw_data$mw[mw_data$compound == compound_name]
  if(length(mw) == 0) {
    warning(paste("No molecular weight found for", compound_name))
    mw <- NA
  }
  
  # Calculate concentration in mol/L
  mol_col <- paste0(compound_name, "_mol_L")
  dat[[mol_col]] <- p.con / (R.con * dat$temp.K) * dat[[col]] * 10^-9
  
  # Calculate flux in mg/min/m^2
  flux_col <- paste0(compound_name, "_flux_mg")
  dat[[flux_col]] <- ((dat[[mol_col]] * air.flow * mw) / A.frame)  * 1000
  
  # Set negative values to zero (althought it is not necessary, already done in the ppb concentrations)
  dat[[flux_col]][dat[[flux_col]] < 0] <- 0
}


final_dat <- dat %>%
  select(
    # Time information
    contains("time"), contains("date"),
    
    # Temperature
    contains("temp"),
    
    # Original corrected concentrations (ppb)
    ends_with("_corrected"),
    
    # Calculated flux values
    ends_with("_flux_mg"),
    
    # Any grouping variables you need
    group, treatment, valve
  )

# ========== Calculate initial flux for vsc compounds ==========
# Get all the bg-corrected-concentrations compound column names
# Extract temp.K from cycle 1 of each valve
cycle1_temp <- dat %>%
  filter(cycle_number == 1) %>%
  group_by(valve) %>%
  summarise(temp.K = first(temp.K), .groups = "drop")

# Join with vsc_ini
vsc_ini <- vsc_ini %>%
  left_join(cycle1_temp, by = "valve")


corrected_cols <- grep("_corrected$", names(vsc_ini), value = TRUE)

# Process each compound
for(col in corrected_cols) {
  # Extract base compound name
  compound_name <- gsub("_corrected$", "", col)
  
  # Find corresopnding molecular weight
  mw <- mw_data$mw[mw_data$compound == compound_name]
  if(length(mw) == 0) {
    warning(paste("No molecular weight found for", compound_name))
    mw <- NA
  }
  
  # Calculate concentration in mol/L
  mol_col <- paste0(compound_name, "_mol_L")
  vsc_ini[[mol_col]] <- p.con / (R.con * vsc_ini$temp.K) * vsc_ini[[col]] * 10^-9
  
  # Calculate flux in mg/min/m^2
  flux_col <- paste0(compound_name, "_flux_mg")
  vsc_ini[[flux_col]] <- ((vsc_ini[[mol_col]] * air.flow * mw) / A.frame)  * 1000
  
  # Set negative values to zero (althought it is not necessary, already done in the ppb concentrations)
  vsc_ini[[flux_col]][vsc_ini[[flux_col]] < 0] <- 0
}


final_vsc_ini <- vsc_ini %>%
  select(
    # Time information
    contains("time"), contains("date"),
    
    # Temperature
    contains("temp"),
    
    # Original corrected concentrations (ppb)
    ends_with("_corrected"),
    
    # Calculated flux values
    ends_with("_flux_mg"),
    
    # Any grouping variables you need
    group, treatment, valve
  )
# ========== Define compound categories and palette for the plots ===============================================================
compound_categories <- tibble(
  compound = c(
    "acetic_acid", "butanoic_acid", "pentanoic_acid", "propanoic_acid", "formic_acid",
    "methyl_indole", "X4.Methylphenol", "phenol", "X4_ethyl_phenol",
    "H2S", "methanthiol", "dimethyl_sulfide",
    "methanol", "acetladheyde", "acetone", "trimethylamine", "isopren", "butanone", "butandion"
  ),
  category = c(
    "Carboxylic Acids", "Carboxylic Acids", "Carboxylic Acids", "Carboxylic Acids", "Carboxylic Acids",
    "Indole", "Phenols", "Phenols", "Phenols",
    "Volatile Sulfur Compounds (VSC)", "Volatile Sulfur Compounds (VSC)", "Volatile Sulfur Compounds (VSC)",
    "Other", "Other", "Other", "Other", "Other", "Other", "Other"
  )
)

# Define color palette 
voc_colors <- c(
  "Carboxylic Acids" = "#4e79a7",
  "Volatile Sulfur Compounds (VSC)" = "#f28e2b",
  "Phenols" = "#e15759",
  "Other" = "#76b7b2",
  "Indole" = "#59a14f"
)
# ===========================================================================================================================



# ========== Prepare data for visualization ==========
# Gather flux data by category, accounting for valve replicates
flux_time_series <- dat %>%
  # Select relevant columns
  select(elapsed_time, group, valve, ends_with("_flux_mg")) %>%
  
  # Convert to long format
  pivot_longer(
    cols = ends_with("_flux_mg"),
    names_to = "compound",
    values_to = "flux_mg_min_m2"
  ) %>%
  
  # Clean up compound names
  mutate(compound = str_remove(compound, "_flux_mg")) %>%
  
  # Join with category information
  left_join(compound_categories, by = "compound") %>%
  
  # Sum compounds within each category for each valve
  group_by(elapsed_time, group, valve, category) %>%
  summarize(
    category_flux = sum(flux_mg_min_m2, na.rm = TRUE),
    .groups = "keep"
  ) %>%
  
  # Average across valves within each group to have a group visualization
  group_by(elapsed_time, group, category) %>%
  summarize(
    mean_flux = mean(category_flux, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate total flux for each group at each elapsed time point
group_totals <- flux_time_series %>%
  group_by(elapsed_time, group) %>%
  summarize(
    total_flux = sum(mean_flux, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate percentage contribution of each category
flux_time_series <- flux_time_series %>%
  left_join(group_totals, by = c("elapsed_time", "group")) %>%
  mutate(
    percent_contribution = mean_flux / total_flux * 100
  )
# Identify all flux columns in the dataset
flux_cols <- grep("_flux_mg$", names(dat), value = TRUE)

# Calculate cumulative emissions for each flux column with m-integrate
for (flux_col in flux_cols) {
  # Create name for the cumulative column
  compound_name <- gsub("_flux_mg$", "", flux_col)
  cum_col <- paste0("cum.", compound_name)
  
  # Calculate cumulative emissions using mintegrate
  dat[[cum_col]] <- mintegrate(
    (dat$elapsed_time * 60),  # Convert elapsed time to minutes
    dat[[flux_col]],          # The flux values for this compound
    by = dat$valve,           # Group by valve
    method = 'trap'           # Use trapezoidal method
  )
 
  
}
names(flux_time_series)


### calculate cum emissions for vsc data initial minutes

# Identify all flux columns in the dataset
flux_cols <- grep("_flux_mg$", names(vsc_ini), value = TRUE)

# Calculate cumulative emissions for each flux column with m-integrate
for (flux_col in flux_cols) {
  # Create name for the cumulative column
  compound_name <- gsub("_flux_mg$", "", flux_col)
  cum_col <- paste0("cum.", compound_name)
  
  # Calculate cumulative emissions using mintegrate
  vsc_ini[[cum_col]] <- mintegrate(
    (vsc_ini$elapsed_time_sec/60),  # Convert elapsed time to minutes
    vsc_ini[[flux_col]],          # The flux values for this compound
    by = vsc_ini$valve,           # Group by valve
    method = 'trap'           # Use trapezoidal method
  )
  
  
}

# Get all cumulative emission columns
cum_cols <- grep("^cum\\.", names(dat), value = TRUE)

# Extract data at elapsed_time  119 (total cumulative emissions) for each valve
final_emissions <- dat %>%
  # Group by valve and get the row with elapsed_time 119
  group_by(valve) %>%
  filter(elapsed_time == 119) %>%
  select(group, valve, elapsed_time, all_of(cum_cols))%>%
         mutate(total_cum = rowSums(across(all_of(cum_cols)), na.rm = TRUE)) %>%
  ungroup()

# ========== ADD CUMULATIVE EMISSIONS OF THE VSC INITIAL MEASUREMENTS TO THE CUMULATIVE EMISSIONS AT TIME 119 ==========
#Select relevant cumulative VSCs from vsc_ini
vsc_cum_add <- vsc_ini %>%
  group_by(valve) %>%
  filter(elapsed_time_sec == max(elapsed_time_sec, na.rm = TRUE)) %>%
  ungroup() %>%
  select(valve, cum.H2S, cum.dimethyl_sulfide, cum.methanthiol)


# Add these to final_emissions by valve
final_emissions <- final_emissions %>%
  left_join(vsc_cum_add, by = "valve", suffix = c("", ".vsc_ini")) %>%
  mutate(
    cum.H2S = cum.H2S + cum.H2S.vsc_ini,
    cum.dimethyl_sulfide = cum.dimethyl_sulfide + cum.dimethyl_sulfide.vsc_ini,
    cum.methanthiol = cum.methanthiol + cum.methanthiol.vsc_ini
  ) %>%
  select(-cum.H2S.vsc_ini, -cum.dimethyl_sulfide.vsc_ini, -cum.methanthiol.vsc_ini)

# Calculate group averages
group_emissions <- final_emissions %>%
  # Group by treatment group
  group_by(group) %>%
  # Calculate average cumulative emissions across valves in each group
  summarize(
    across(all_of(cum_cols), ~mean(.x, na.rm = TRUE)),
    n_valves = n(),
    .groups = "drop"
  ) %>%
  # Calculate total emission for each group
  mutate(total_emission = rowSums(across(all_of(cum_cols)), na.rm = TRUE))

# check the contribution of each compound to the group's total cumulative emissions
group_contributions <- group_emissions %>%
  # Keep only group, compound columns, and total
  select(group, all_of(cum_cols), total_emission) %>%
  # Convert to long format
  pivot_longer(
    cols = all_of(cum_cols),
    names_to = "compound_cum",
    values_to = "cum_emission"
  ) %>%
  # Clean up compound names
  mutate(compound = str_remove(compound_cum, "^cum\\.")) %>%
  # Join with category information 
  left_join(compound_categories, by = "compound") %>%
  # Calculate percentage contribution
  mutate(percent = cum_emission / total_emission * 100)

# Create summary table with absolute and percentage values
emission_summary <- group_contributions %>%
  select(group, compound, category, cum_emission, percent) %>%
  mutate(
    formatted = sprintf("%.2f mg/m² (%.1f%%)", cum_emission, percent)
  ) %>%
  arrange(group, desc(percent))


# Summary by category
category_summary <- group_contributions %>%
  group_by(group, category) %>%
  summarize(
    category_total = sum(cum_emission, na.rm = TRUE),
    category_percent = sum(percent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(group, desc(category_percent))

# Display overall group totals
group_totals <- group_emissions %>%
  select(group, total_emission) %>%
  mutate(formatted = sprintf("%.2f mg/m²", total_emission))



# Create visualization of category contributions
ggplot(category_summary, 
       aes(x = group, y = category_total, fill = category)) +
  geom_col() +
  geom_text(aes(label = sprintf("%.1f%%", category_percent)),
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold") +
  scale_fill_manual(values = voc_colors) +
  labs(
    title = "Cumulative Emissions at 119 Hours by Group and Category",
    x = "Treatment Group",
    y = expression(Cumulative~Emission~(mg/m^2)),
    fill = "VOC Category"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# ========== Create visualization ==========
# Create stacked area plot
p_publication <- ggplot(flux_time_series, aes(x = elapsed_time, y = mean_flux, fill = category)) +
  geom_area(position = "stack", alpha = 0.6) +
  # Add total flux line
  geom_line(data = flux_time_series, aes(x = elapsed_time, y = total_flux, group = group), 
            color = "black", size = 0.5, inherit.aes = FALSE) +
  scale_fill_manual(values = voc_colors) +
  facet_wrap(~group, scales = "free_y") +
  # Replace xlim() with scale_x_continuous to set explicit breaks
  scale_x_continuous(limits = c(0, 119), 
                     breaks = c(0, 24, 48, 72, 96, 119),
                     expand = c(0, 0)) +  # Remove padding for exact limits
  labs(
    title = "",
    x = "Time from slurry application (hours)",
    y = expression(Flux~(mg~m^{-2}~min^{-1})),
    fill = "VOC Category"
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12, margin = margin(b = 5, t = 5)),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )
# Display plot
print(p_publication)

# Save plots and data
ggsave("voc_flux_by_category.png", p_publication, width = 12, height = 8, dpi = 300)
#save flux
write_csv(final_dat, "flux_voc_dfc.csv")
#save cumulative emissions
write_csv(final_emissions, "cum.voc.emis.csv")

















