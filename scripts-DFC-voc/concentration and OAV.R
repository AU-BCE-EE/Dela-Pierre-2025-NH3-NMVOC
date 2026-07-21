
# ======== DATA IMPORT & PREPROCESSING ========

# Import OTV values and corrected concentrations
OTV <- read_excel("../input-DFC/OTV.xlsx", sheet = "Sheet2") 

dt <- read_csv("../output-DFC/raw.ptrms.valve.txt") 

# Groups and treatment assignment
dat <- dt %>%
  mutate(
    treatment = recode(valve,
                       `1` = 'Mp', `2` = '0-DFC', `3` = '2.9-DFC', `4` = '5.3-DFC', `5` = 'bkg',
                       `6` = '0-DFC', `7` = '10.5-DFC', `8` = 'Mp', `9` = 'bkg', `10` = '2.9-DFC',
                       `11` = '5.3-DFC', `12` = 'Mp', `13` = '10.5-DFC', `14` = '0-DFC', `15` = '2.9-DFC',
                       `16` = 'bkg', `17` = '5.3-DFC', `18` = 'Mp', `19` = '10.5-DFC'),
    group = case_when(
      valve %in% c(2, 6, 14) ~ 'No acid',
      valve %in% c(3, 10, 15) ~ 'Low acid',
      valve %in% c(4, 11, 17) ~ 'Medium acid',
      valve %in% c(7, 13, 19) ~ 'High acid',
      valve %in% c(5, 9, 16) ~ 'Background',
      valve %in% c(1, 8, 12, 18) ~ 'Machine plot'
    )
  )

# Define compounds to process
compounds <- c("methanol", "H2S", "X4.Methylphenol", "acetic_acid", 
               "butanoic_acid", "pentanoic_acid", "propanoic_acid", 
               "acetladheyde", "formic_acid", "methanthiol", 
               "acetone", "trimethylamine", "dimethyl_sulfide",
               "isopren", "butanone", "phenol", "X4_ethyl_phenol",
               "methyl_indole", "butandion")

# Define VSC compounds for peak calculation
vsc_compounds <- c("H2S", "dimethyl_sulfide", "methanthiol")

# ======== CYCLE ASSIGNMENT ========
# Assign cycle numbers (1 cycle goes from valve 1 to valve 19)
data_with_cycles <- dat %>%
  arrange(date.time) %>%
  mutate(
    valve_id = as.numeric(valve),
    next_valve_id = lead(valve_id, default = valve_id[1]),
    valve_transition = (valve_id > next_valve_id & next_valve_id == 1),
    cycle_number = cumsum(valve_transition) + 1
  ) %>%
  dplyr::select(-valve_id, -next_valve_id, -valve_transition) %>%
  # Calculate elapsed time in same step
  group_by(valve) %>%
  mutate(elapsed_time = round(as.numeric(difftime(date.time, min(date.time), units = 'hour')))) %>%
  ungroup()

# ======== 30-SECOND AVERAGES ========
# Extract last 30 seconds of data for each valve/cycle. we have 2 or 3 measurements to get 30 s, so we can't use a fixed number of values
last_30s_data <- data_with_cycles %>%
  group_by(valve, cycle_number) %>%
  mutate(max_time = max(date.time)) %>%
  filter(date.time >= (max_time - seconds(30))) %>%
  dplyr::select(-max_time) %>%
  dplyr::select(valve, cycle_number, date.time, group, treatment, elapsed_time, all_of(compounds)) %>%
  ungroup()

# Calculate 30s averages (the average of the last 30 s measurements for each compound)
valve_30s_averages <- last_30s_data %>%
  group_by(valve, cycle_number, group, treatment) %>%
  summarise(
    across(all_of(compounds), ~mean(., na.rm = TRUE), .names = "{.col}_avg_30s"),
    start_time = min(date.time),
    end_time = max(date.time),
    elapsed_time = mean(elapsed_time),
    .groups = "drop"
  )

# ======== BACKGROUND CORRECTION ========
# Calculate background averages by cycle (for each cycle we want the average value of the bg)
background_averages <- valve_30s_averages %>%
  filter(group == "Background") %>%
  group_by(cycle_number) %>%
  summarise(
    across(ends_with("_avg_30s"), ~mean(., na.rm = TRUE), .names = "{.col}_bg"),
    n_bg_valves = n(),
    .groups = "drop"
  )

# Apply background correction (for each cycle, we subtract to each cmpound the corresponding bg value)
corrected_averages <- valve_30s_averages %>%
  filter(group != "Background") %>%
  left_join(background_averages, by = "cycle_number") %>%
  mutate(across(
    ends_with("_avg_30s"),
    ~{
      bg_col <- paste0(cur_column(), "_bg")
      pmax(0, . - get(bg_col))
    },
    .names = "{sub('_avg_30s$', '_corrected', .col)}"
  )) %>%
  dplyr::select(
    valve, cycle_number, group, treatment, elapsed_time,
    start_time, end_time,
    ends_with("_avg_30s"), ends_with("_corrected")
  )

# ======== CREATE THE DATASET OF THE FIRST 8 MINUTES MEASUREMENTS OF VSC FOR FURTHER CALCULATIONS IN CUM EMISSIONS ========

#  Filter data for cycle 1 data
cycle1_data <- data_with_cycles %>% 
  filter(cycle_number == 1)

#  Extract background values for cycle 1
cycle1_bg_values <- background_averages %>% 
  filter(cycle_number == 1) %>%
  dplyr::select(H2S_avg_30s_bg, dimethyl_sulfide_avg_30s_bg, methanthiol_avg_30s_bg)

# Store background values in named vector
vsc_bg_values <- c(
  H2S = cycle1_bg_values$H2S_avg_30s_bg,
  dimethyl_sulfide = cycle1_bg_values$dimethyl_sulfide_avg_30s_bg,
  methanthiol = cycle1_bg_values$methanthiol_avg_30s_bg
)

# Subtract background from all VSC measurements
cycle1_corrected <- cycle1_data %>%
  mutate(
    H2S_corrected = pmax(0, H2S - vsc_bg_values["H2S"]),
    dimethyl_sulfide_corrected = pmax(0, dimethyl_sulfide - vsc_bg_values["dimethyl_sulfide"]),
    methanthiol_corrected = pmax(0, methanthiol - vsc_bg_values["methanthiol"])
  )

#  Calculate elapsed time (seconds) from first timepoint in each valve
cycle1_corrected <- cycle1_corrected %>%
  group_by(valve) %>%
  mutate(
    elapsed_time_sec = as.numeric(difftime(date.time, min(date.time), units = "min"))
  ) %>%
  ungroup()

# create the dataset
vsc_only <- cycle1_corrected %>%
  dplyr::select(
    valve, treatment, group, cycle_number, date.time,
    H2S, dimethyl_sulfide, methanthiol,
    H2S_corrected, dimethyl_sulfide_corrected, methanthiol_corrected,
    elapsed_time_sec
  )
vsc_only<-filter(vsc_only, group!="Background")

# ======== OAV CALCULATIONS ========
# Define compound mapping for OTV values (we want to match the names with the OTV dataset)
compound_mapping <- c(
  "methanol_avg_30s" = "Methanol",
  "H2S_avg_30s" = "Hydrogen sulphide",
  "X4.Methylphenol_avg_30s" = "4-Methylphenol",
  "acetic_acid_avg_30s" = "Acetic acid",
  "butanoic_acid_avg_30s" = "Butanoic acid", 
  "pentanoic_acid_avg_30s" = "Pentanoic acid",
  "propanoic_acid_avg_30s" = "Propionic acid",
  "acetladheyde_avg_30s" = "Acetaldehyde",
  "formic_acid_avg_30s" = "Formic acid",
  "methanthiol_avg_30s" = "Methane thiol",
  "acetone_avg_30s" = "Acetone",
  "trimethylamine_avg_30s" = "Trimethylamine",
  "dimethyl_sulfide_avg_30s" = "Dimethyl sulphide",
  "isopren_avg_30s" = "Isoprene",
  "butanone_avg_30s" = "2-Butanone",
  "phenol_avg_30s" = "Phenol",
  "X4_ethyl_phenol_avg_30s" = "4-Ethylphenol",
  "methyl_indole_avg_30s" = "3-Methylindole",
  "butandion_avg_30s" = "2,3-Butanedione"
)

# Calculate OAV values (we divide each compound concnetration for its OTV)
OAV <- corrected_averages %>%
  mutate(
    across(
      ends_with("_corrected"),
      ~{
        base_compound <- sub("_corrected$", "", cur_column())
        otv_name <- compound_mapping[paste0(base_compound, "_avg_30s")]
        ./OTV[[otv_name]]
      },
      .names = "OAV_{compound_mapping[sub('_corrected$', '_avg_30s', .col)]}"
    )
  ) %>%
  dplyr::select(
    valve, cycle_number, treatment, group, elapsed_time, end_time,
    starts_with("OAV_")
  ) %>%
  mutate(
    valve = as.numeric(valve),
    elapsed_time = as.numeric(elapsed_time),
    across(starts_with("OAV_"), as.numeric),
    # Calculate total OAV in same step
    total_OAV = rowSums(across(starts_with("OAV_")), na.rm = TRUE)
  )

# ======== VSC PEAK CALCULATIONS ========
# Get cycle 1 data and background values
cycle1_data <- data_with_cycles %>% filter(cycle_number == 1)
cycle1_bg_values <- background_averages %>% 
  filter(cycle_number == 1) %>%
  dplyr::select(paste0(vsc_compounds, "_avg_30s_bg"))

# Extract VSC background values for cycle 1
vsc_bg_values <- c(
  cycle1_bg_values$H2S_avg_30s_bg,
  cycle1_bg_values$dimethyl_sulfide_avg_30s_bg,
  cycle1_bg_values$methanthiol_avg_30s_bg
)

# Calculate VSC peaks
vsc_peaks <- cycle1_data %>%
  filter(group != "Background") %>%
  group_by(valve) %>%
  summarise(
    # Calculate peaks with background subtraction
    H2S_peak = max(pmax(0, H2S - vsc_bg_values[1]), na.rm = TRUE),
    dimethyl_sulfide_peak = max(pmax(0, dimethyl_sulfide - vsc_bg_values[2]), na.rm = TRUE),
    methanthiol_peak = max(pmax(0, methanthiol - vsc_bg_values[3]), na.rm = TRUE),
    # Calculate total VSC peak
    VSC_peak = H2S_peak + dimethyl_sulfide_peak + methanthiol_peak,
    treatment = first(treatment),
    group = first(group),
    .groups = "drop"
  )

# Calculate OAV for VSC peaks (dividing peak of each VSC by its OTV, then summing again)
vsc_peaks_oav <- vsc_peaks %>%
  mutate(
    H2S_OAV = H2S_peak / OTV$`Hydrogen sulphide`,
    dimethyl_sulfide_OAV = dimethyl_sulfide_peak / OTV$`Dimethyl sulphide`,
    methanthiol_OAV = methanthiol_peak / OTV$`Methane thiol`,
    VSC_OAV = H2S_OAV + dimethyl_sulfide_OAV + methanthiol_OAV
  )

# Calculate average VSC OAV by group
avg_vsc_peaks <- vsc_peaks_oav %>%
  group_by(group) %>%
  summarise(mean_VSC_OAV = mean(VSC_OAV, na.rm = TRUE))

# Create dataset for VSC peaks at time 0
vsc_peak_data <- avg_vsc_peaks %>%
  mutate(
    elapsed_time = 0,
    category = "VSC",
    value = mean_VSC_OAV
  ) %>%
  dplyr::select(group, elapsed_time, category, value)

# Save datasets
write.table(OAV, "../output-DFC/OAV.txt", row.names = F, sep=",")
write.table(corrected_averages, "../output-DFC/voc.30s.corrected.txt", row.names = F, sep=",")
write.table(vsc_only, "../output-DFC/vsc.first.minutes.txt", row.names= F, sep= ",")

write.table(vsc_peak_data, "../output-DFC/vsc.for.plot.txt", row.names= F, sep= ",")


