
library(readr)       
library(dplyr)       
library(tidyr)      
library(lubridate)
library(zoo)
library(readxl)
library(patchwork)
library(ggplot2)
library(scales)# For combining plots

# ======== DATA IMPORT & PREPROCESSING ========

# Import OTV values and corrected concentrations
OTV <- read_excel("OTV.xlsx", sheet = "Sheet2") # OTV file OTV.xlsx find this in the Flavia_VOC_DFC_data folder

dt <- read_csv("raw.ptrms.valves.txt") #dt is the dataset created in the PTRMS valve assignment script
# filter for exact starting time
dat <- dt %>%    
  filter(date.time >= ymd_hms("2024-09-18 12:32:02")) %>%
  mutate(valve = as.numeric(valve))

# Groups and treatment assignment
dat <- dat %>%
  mutate(
    treatment = recode(valve,
                       `1` = 'Mp', `2` = '0-bp', `3` = '1.5', `4` = '2.9', `5` = 'bkg',
                       `6` = '0-bp', `7` = '5.7', `8` = 'Mp', `9` = 'bkg', `10` = '1.5',
                       `11` = '2.9', `12` = 'Mp', `13` = '5.7', `14` = '0-bp', `15` = '1.5',
                       `16` = 'bkg', `17` = '2.9', `18` = 'Mp', `19` = '5.7'),
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
  select(-valve_id, -next_valve_id, -valve_transition) %>%
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
  select(-max_time) %>%
  select(valve, cycle_number, date.time, group, treatment, elapsed_time, all_of(compounds)) %>%
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
View(background_averages)
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
  select(
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
  select(H2S_avg_30s_bg, dimethyl_sulfide_avg_30s_bg, methanthiol_avg_30s_bg)

# Store background values in named vector
vsc_bg_values <- c(
  H2S = cycle1_bg_values$H2S_avg_30s_bg,
  dimethyl_sulfide = cycle1_bg_values$dimethyl_sulfide_avg_30s_bg,
  methanthiol = cycle1_bg_values$methanthiol_avg_30s_bg
)

# Subtract background from all VSC measurements
cycle1_corrected <- cycle1_data %>%
  mutate(
    H2S_corr = pmax(0, H2S - vsc_bg_values["H2S"]),
    dimethyl_sulfide_corr = pmax(0, dimethyl_sulfide - vsc_bg_values["dimethyl_sulfide"]),
    methanthiol_corr = pmax(0, methanthiol - vsc_bg_values["methanthiol"])
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
  select(
    valve, treatment, group, cycle_number, date.time,
    H2S, dimethyl_sulfide, methanthiol,
    H2S_corr, dimethyl_sulfide_corr, methanthiol_corr,
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
  select(
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
  select(paste0(vsc_compounds, "_avg_30s_bg"))

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
    category = "Volatile Sulfur Compounds (VSC)",
    value = mean_VSC_OAV
  ) %>%
  select(group, elapsed_time, category, value)

# ======== DATA FOR VISUALIZATION ========
# Define VOC categories mapping
Category <- c(
  "OAV_Acetic acid" = "Carboxylic Acids",
  "OAV_Butanoic acid" = "Carboxylic Acids",
  "OAV_Pentanoic acid" = "Carboxylic Acids",
  "OAV_Propionic acid" = "Carboxylic Acids",
  "OAV_Formic acid" = "Carboxylic Acids",
  "OAV_3-Methylindole" = "Indole",
  "OAV_4-Methylphenol" = "Phenols",
  "OAV_Phenol" = "Phenols",
  "OAV_4-Ethylphenol" = "Phenols",
  "OAV_Hydrogen sulphide" = "Volatile Sulfur Compounds (VSC)",
  "OAV_Methane thiol" = "Volatile Sulfur Compounds (VSC)",
  "OAV_Dimethyl sulphide" = "Volatile Sulfur Compounds (VSC)",
  "OAV_Methanol" = "Other",
  "OAV_Acetaldehyde" = "Other",
  "OAV_Acetone" = "Other",
  "OAV_Trimethylamine" = "Other",
  "OAV_Isoprene" = "Other",
  "OAV_2-Butanone" = "Other",
  "OAV_2,3-Butanedione" = "Other"
)
# Define color palette for the categories
voc_colors <- c(
  "Carboxylic Acids" = "#4e79a7",
  "Volatile Sulfur Compounds (VSC)" = "#f28e2b",
  "Phenols" = "#e15759",
  "Other" = "#76b7b2",
  "Indole" = "#59a14f"
)
# Calculate average OAV by group to check the values
avg_OAV_by_group <- OAV %>%
  group_by(elapsed_time, group) %>%
  summarise(mean_total_OAV = mean(total_OAV, na.rm = TRUE))

# Create long format data for area plots
OAV_long <- OAV %>%
  group_by(group, elapsed_time) %>%
  summarise(across(starts_with("OAV_"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("OAV_"), names_to = "compound", values_to = "value") %>%
  mutate(category = Category[compound]) %>%
  group_by(group, elapsed_time, category) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  left_join(avg_OAV_by_group, by = c("group", "elapsed_time")) %>%
  group_by(group, elapsed_time) %>%
  mutate(
    prop = value / sum(value, na.rm = TRUE),
    value = prop * mean_total_OAV
  ) %>%
  ungroup()



# Filter out "Machine plot" group
OAV_long_filtered <- OAV_long %>% filter(group != "Machine plot")
vsc_peak_data_filtered <- vsc_peak_data %>% filter(group != "Machine plot")

# Set maximum x-axis value to 119 hours
max_x <- 119

# Create summarized data for total OAV line
total_OAV <- OAV_long_filtered %>%
  group_by(group, elapsed_time) %>%
  summarize(total_value = sum(value, na.rm = TRUE), .groups = "drop")

# Function to create a broken axis plot with no gap
create_broken_axis_plot <- function(group_name, show_x_title = FALSE, show_y_title = FALSE) {
  # Filter data for this group
  group_data <- OAV_long_filtered %>% filter(group == group_name)
  group_peaks <- vsc_peak_data_filtered %>% filter(group == group_name)
  group_totals <- total_OAV %>% filter(group == group_name)
  
  # Get max peak value for this group
  max_peak <- max(group_peaks$value, na.rm = TRUE)
  rounded_max <- ceiling(max_peak/1000)*1000
  
  # Get x-axis range for consistent alignment
  x_min <- 0  # Start from 0
  x_max <- max_x  # Use 119 as maximum
  
  # Create a single plot with manipulated coordinates
  p <- ggplot() +
    # Add stacked areas for lower part (up to 1250 now)
    geom_area(data = group_data, 
              aes(x = elapsed_time, y = pmin(value, 1250), fill = category),
              alpha = 0.8) +
    # Add VSC peaks for lower part (up to 1250)
    geom_linerange(data = group_peaks,
                   aes(x = elapsed_time, ymin = 0, ymax = pmin(value, 1250)),
                   color = "#f28e2b", size = 0.5) +
    # Add VSC peaks for upper part (above 1250)
    geom_linerange(data = group_peaks %>% filter(value > 1250),
                   aes(x = elapsed_time, 
                       ymin = 1250,
                       ymax = 1250 + (value - 1250) / (rounded_max - 1250) * 125),
                   color = "#f28e2b", size = 0.5) +
    # Add total OAV line on top of stacked areas
    geom_line(data = group_totals,
              aes(x = elapsed_time, y = pmin(total_value, 1250)),
              color = "black", size = 0.7) +
    # Add break lines at 1250
    geom_hline(yintercept = 1250, linetype = "dotted", color = "black", size = 0.7) +
    geom_hline(yintercept = 1256, linetype = "dotted", color = "black", size = 0.7) +
    # Custom scale specifications
    scale_fill_manual(values = voc_colors, name = "Category") +
    # Explicit x-axis scale truncated at 119
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = c(0, 25, 50, 75, 100, 119),
      labels = c("0", "25", "50", "75", "100", "119")
    ) +
    scale_y_continuous(
      breaks = c(0, 250, 500, 750, 1000, 1250, 1375), 
      labels = c("0", "250", "500", "750", "1000", "1250", as.character(rounded_max)),
      limits = c(0, 1380)
    ) +
    labs(title = group_name) +
    theme_bw() +
    theme(
      # Increase plot title size to 16
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      # Increase axis text size
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      # Increase axis title size
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 16),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  # Add x-axis title only for bottom plots
  if (show_x_title) {
    p <- p + labs(x = "Elapsed Time (hours)")
  } else {
    p <- p + theme(axis.title.x = element_blank())
  }
  
  # Add y-axis title only for left column plots (High acid and Medium acid)
  if (show_y_title) {
    p <- p + labs(y = "OAV")
  } else {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  return(p)
}

# Create plots for each group
group_names <- unique(OAV_long_filtered$group)

# Create plots with appropriate axis titles
plot_list <- list()
for (i in seq_along(group_names)) {
  # Show x-axis title only for plots in bottom row
  show_x_title <- (i >= 3)
  # Show y-axis title only for plots in left column (High acid and Medium acid)
  show_y_title <- (i %% 2 == 1)  # Odd indices (1, 3) are left column
  plot_list[[i]] <- create_broken_axis_plot(group_names[i], show_x_title, show_y_title)
}

# Combine all plots
final_plot <- wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    # Increase legend text and title size
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

print(final_plot)
#save OAV, 30s background corrected averages, and VSC initial dataset
write_csv(OAV, "OAV_fin.csv")
write_csv(corrected_averages, "voc.30s.corrected.csv")
write.table(vsc_only, "vsc.first.minutes.txt") #need to use write.table otherwise it changes the time format
