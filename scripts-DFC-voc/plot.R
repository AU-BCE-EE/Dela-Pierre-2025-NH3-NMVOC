# ======== DATA FOR VISUALIZATION ========
OAV <- read_csv("../output-DFC/OAV.txt")
library(readr)
vsc_peak_data <- read_csv("../output-DFC/vsc.for.plot.txt")

# Define VOC categories mapping
Category <- c(
  "OAV_Acetic acid" = "VFA",
  "OAV_Butanoic acid" = "VFA",
  "OAV_Pentanoic acid" = "VFA",
  "OAV_Propionic acid" = "VFA",
  "OAV_Formic acid" = "VFA",
  "OAV_3-Methylindole" = "Indole",
  "OAV_4-Methylphenol" = "Phenol",
  "OAV_Phenol" = "Phenol",
  "OAV_4-Ethylphenol" = "Phenol",
  "OAV_Hydrogen sulphide" = "VSC",
  "OAV_Methane thiol" = "VSC",
  "OAV_Dimethyl sulphide" = "VSC",
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
  "VFA" = "#4e79a7",
  "VSC" = "#d62728",
  "Phenol" = "#f28e2b",
  "Other" = "#76b7b2",
  "Indole" = "#59a14f"
)
# Calculate average OAV by group to check the values

avg_OAV_by_group <- OAV %>%
  group_by(elapsed_time, group) %>%
  summarise(mean_total_OAV = mean(total_OAV, na.rm = TRUE))

# Create long format data for area plots
OAV_long <- OAV %>%
  dplyr::group_by(group, elapsed_time) %>%
  dplyr::summarise(across(starts_with("OAV_"), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = starts_with("OAV_"), names_to = "compound", values_to = "value") %>%
  dplyr::mutate(category = Category[compound]) %>%
  dplyr::group_by(group, elapsed_time, category) %>%
  dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
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

# Function to create a broken axis plot with customizable breaking point
create_broken_axis_plot <- function(group_name, show_x_title = FALSE, show_y_title = FALSE, break_at = 500) {
  # Filter data for this group
  group_data <- OAV_long_filtered %>% filter(group == group_name)
  group_peaks <- vsc_peak_data_filtered %>% filter(group == group_name)
  group_totals <- total_OAV %>% filter(group == group_name)
  
  # Get max peak value for this group
  max_peak <- max(group_peaks$value, na.rm = TRUE)
  rounded_max <- ceiling(max_peak/1000)*1000
  
  # Override break point for High acid group
  if(group_name == "High acid") {
    break_at <- 900
  }
  
  # Define y-axis breaks and labels based on breaking point
  if(break_at == 500) {
    y_breaks <- c(0, 250, 500, 625)
    y_labels <- c("0", "250", "500", as.character(rounded_max))
  } else { # For High acid with break_at = 900
    y_breaks <- c(0, 300, 600, 900, 1025)
    y_labels <- c("0", "300", "600", "900", as.character(rounded_max))
  }
  
  # Get x-axis range for consistent alignment
  x_min <- 0  # Start from 0
  x_max <- max_x  # Use 119 as maximum
  
  # Rename group for display
  display_name <- case_when(
    group_name == "No acid" ~ "0-DFC",
    group_name == "Low acid" ~ "2.9-DFC",
    group_name == "Medium acid" ~ "5.3-DFC",
    group_name == "High acid" ~ "10.5-DFC",
    TRUE ~ group_name
  )
  
  # Create a single plot with manipulated coordinates
  p <- ggplot() +
    # Add stacked areas for lower part (up to the breaking point)
    geom_area(data = group_data %>% 
                mutate(category = factor(category, levels = c("VSC", "Indole","Other", "VFA", "Phenol" ))), 
              aes(x = elapsed_time, y = pmin(value, break_at), fill = category),
              alpha = 1) +
    # Add VSC peaks for lower part
    geom_linerange(data = group_peaks,
                   aes(x = elapsed_time, ymin = 0, ymax = pmin(value, break_at)),
                   color = "#d62728", size = 0.5) +
    # Add VSC peaks for upper part (above breaking point)
    geom_linerange(data = group_peaks %>% filter(value > break_at),
                   aes(x = elapsed_time, 
                       ymin = break_at,
                       ymax = break_at + (value - break_at) / (rounded_max - break_at) * 125),
                   color = "#d62728", size = 0.5) +
    # Add total OAV line on top of stacked areas
    geom_line(data = group_totals,
              aes(x = elapsed_time, y = pmin(total_value, break_at)),
              color = "black", size = 0.7) +
    # Add break lines at the breaking point
    geom_hline(yintercept = break_at, linetype = "dotted", color = "black", size = 0.7) +
    geom_hline(yintercept = break_at + 6, linetype = "dotted", color = "black", size = 0.7) +
    # Custom scale specifications with ordered categories
    scale_fill_manual(values = voc_colors, 
                      name = "Category",
                      breaks = c("VSC", "Indole","Other", "VFA", "Phenol" )) +
    # Explicit x-axis scale with time formatting from citation
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = c(0, 24, 48, 72, 96, 119),
      labels = c("0", "24", "48", "72", "96", "119")
    ) +
    # Use custom y-axis breaks and labels
    scale_y_continuous(
      breaks = y_breaks, 
      labels = y_labels,
      limits = c(0, break_at + 130)
    ) +
    labs(title = display_name) +  # Use renamed group
    theme_bw() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      axis.title.x = element_text(size = 20),
      axis.title.y = element_text(size = 20),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  # Add x-axis title only for bottom plots
  if (show_x_title) {
    p <- p + labs(x = "Time from slurry application (hours)")
  } else {
    p <- p + theme(axis.title.x = element_blank())
  }
  
  # Add y-axis title only for left column plots
  if (show_y_title) {
    p <- p + labs(y = "OAV")
  } else {
    p <- p + theme(axis.title.y = element_blank())
  }
  
  return(p)
}

# Filter out Machine plot and set order
group_names <- c("No acid", "Low acid", "Medium acid", "High acid")  # Exclude Machine plot and set specific order

# Create plots with appropriate axis titles
plot_list <- list()
for (i in seq_along(group_names)) {
  # Show x-axis title only for plots in bottom row
  show_x_title <- (i >= 3)
  # Show y-axis title only for plots in left column
  show_y_title <- (i %% 2 == 1)
  # Default breaking point is 500, but High acid will override to 900 in the function
  plot_list[[i]] <- create_broken_axis_plot(group_names[i], show_x_title, show_y_title)
}

# Make sure voc_colors is ordered correctly
voc_colors <- c(
  "Other" = voc_colors["Other"],
  "VFA" = voc_colors["VFA"], 
  "VSC" = voc_colors["VSC"],
  "Phenol" = voc_colors["Phenol"],
  "Indole" = voc_colors["Indole"]
)

# Combine all plots
final_plot <- wrap_plots(plot_list, ncol = 2) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

print(final_plot)
# ggsave("../plots/OAV.broken.axis.png", final_plot, width = 14, height = 12, dpi=300, bg="white")



# ==============================================================================
# FLUX AND OAV COMPARISON: bLS vs DFC
# ==============================================================================

library(readxl)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(grid)
library(gridExtra)
library(patchwork)
library(cowplot)
# Load data fluxes
data <- read_csv("../output-DFC/flux_voc_dfc.csv")
bLS_voc <- read_csv("../output-bLS/mass_emissions.csv")
getwd()
# Create compound mapping
compound_mapping <- c(
  "methanol" = "Methanol",
  "H2S" = "Hydrogen Sulfide", 
  "X4.Methylphenol" = "4-Methylphenol",
  "acetic_acid" = "Acetic Acid",
  "butanoic_acid" = "Butanoic Acid",
  "pentanoic_acid" = "Pentanoic Acid",
  "propanoic_acid" = "Propionic Acid",
  "acetladheyde" = "Acetaldehyde",
  "formic_acid" = "Formic Acid",
  "methanthiol" = "Methanethiol",
  "acetone" = "Acetone",
  "trimethylamine" = "Trimethylamine",
  "dimethyl_sulfide" = "Dimethyl Sulfide",
  "isopren" = "Isoprene",
  "butanone" = "2-Butanone",
  "phenol" = "Phenol",
  "X4_ethyl_phenol" = "4-Ethylphenol",
  "methyl_indole" = "3-Methylindole",
  "butandion" = "2,3-Butanedione"
)

# Identify and process flux columns
flux_columns <- grep("_flux_mg$", names(data), value = TRUE)

long_data <- data %>%
  dplyr::select(elapsed_time, treatment, valve, group, all_of(flux_columns)) %>%
  pivot_longer(
    cols = all_of(flux_columns),
    names_to = "flux_type",
    values_to = "Mass_Emission"
  ) %>%
  dplyr::mutate(
    compound_raw = sub("(.*)_flux_mg$", "\\1", flux_type),
    compound = ifelse(compound_raw %in% names(compound_mapping), 
                      compound_mapping[compound_raw], 
                      compound_raw),
    hours = elapsed_time
  ) %>%
  dplyr::filter(hours <= 119) %>%
  dplyr::group_by(hours, group, compound) %>%
  dplyr::summarise(
    Mass_Emission = mean(Mass_Emission, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  dplyr::mutate(application = case_when(
    group == "Machine plot" ~ "Machine Application",
    TRUE ~ "Manual Application"
  ))

# Process bLS data
bLS_voc <- bLS_voc[bLS_voc$hours <= 119, ]
names(bLS_voc)[names(bLS_voc) == "Compound"] <- "compound"
bLS_voc$group <- "bLS"
bLS_voc$application <- "Machine Application"

# Combine datasets
plot_data <- bind_rows(long_data, bLS_voc)

# Add category assignment
plot_data <- plot_data %>%
  mutate(
    Category = case_when(
      compound %in% c("Acetic Acid", "Pentanoic Acid", "Formic Acid",
                      "Propionic Acid", "Butanoic Acid") ~ "VFA",
      compound %in% c("Methanethiol", "Hydrogen Sulfide", "Dimethyl Sulfide") ~ "VSC",
      compound %in% c("Phenol", "4-Methylphenol", "4-Ethylphenol") ~ "Phenol",
      compound == "3-Methylindole" ~ "Indole",
      TRUE ~ "Other"
    )
  )

# Define color palette 
voc_colors <- c(
  "VFA" = "#4e79a7",
  "VSC" = "#d62728",
  "Phenol" = "#f28e2b",
  "Other" = "#76b7b2",
  "Indole" = "#59a14f"
)

# Calculate category flux data
category_flux_time_series <- plot_data %>%
  group_by(hours, group, application, Category) %>%
  summarize(
    mean_flux = sum(Mass_Emission, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate total flux
total_flux_time_series <- plot_data %>%
  group_by(hours, group, application) %>%
  summarize(
    total_flux = sum(Mass_Emission, na.rm = TRUE),
    .groups = "drop"
  )

# Create separate datasets for machine and manual applications
machine_data_total <- total_flux_time_series %>% filter(application == "Machine Application")
machine_data_category <- category_flux_time_series %>% filter(application == "Machine Application")
manual_data_total <- total_flux_time_series %>% filter(application == "Manual Application")
manual_data_category <- category_flux_time_series %>% filter(application == "Manual Application")

# Define plotmath-compatible facet labels
group_label_mapping <- c(
  "Machine plot" = "0-DFC",
  "bLS" = "0-bLS",
  "Low acid" = "2.9-DFC",
  "Medium acid" = "5.3-DFC",
  "High acid" = "10.5-DFC",
  "No acid" = "0-DFC"
)

# Add parsed group labels to each dataset
machine_data_category <- machine_data_category %>%
  mutate(group_label = group_label_mapping[group])

machine_data_total <- machine_data_total %>%
  mutate(group_label = group_label_mapping[group])

# For manual data, reorder the facets
manual_data_category <- manual_data_category %>%
  mutate(
    group_label = group_label_mapping[group],
    # Create a custom order for facets
    facet_order = case_when(
      group == "No acid" ~ 1,      # First position (top-left)
      group == "Low acid" ~ 2,     # Second position (top-right)
      group == "Medium acid" ~ 3,  # Third position (bottom-left)
      group == "High acid" ~ 4,    # Fourth position (bottom-right)
      TRUE ~ 5
    )
  ) %>%
  arrange(facet_order)

manual_data_total <- manual_data_total %>%
  mutate(
    group_label = group_label_mapping[group],
    # Create a custom order for facets
    facet_order = case_when(
      group == "No acid" ~ 1,      # First position (top-left)
      group == "Low acid" ~ 2,     # Second position (top-right)
      group == "Medium acid" ~ 3,  # Third position (bottom-left)
      group == "High acid" ~ 4,    # Fourth position (bottom-right)
      TRUE ~ 5
    )
  ) %>%
  arrange(facet_order)

# Machine application plot
p_machine <- ggplot() +
  geom_area(data = machine_data_category, 
            aes(x = hours, y = mean_flux, fill = Category),
            position = "stack", alpha = 1) +
  geom_line(data = machine_data_total,
            aes(x = hours, y = total_flux),
            color = "black", size = 0.5) +
  facet_wrap(~ group_label, labeller = label_parsed, nrow = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20),  expand = expansion(mult = c(0.02, 0.02))) +
  theme(axis.text.x = element_blank())+
  scale_y_continuous(limits=c(0,3),
                     breaks = seq(0,3, by=1),
                     expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = voc_colors) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none",
    plot.margin = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16) , axis.text.x = element_blank(),          # <--- move this here
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = expression("Flux (mg " * m^{-2} * " " * min^{-1} * ")")
  ) +
  coord_cartesian(clip = "off")
print(p_machine)
# Manual application plot with custom order but keeping original scales
p_manual <- ggplot() +
  geom_area(data = manual_data_category, 
            aes(x = hours, y = mean_flux, fill = Category),
            position = "stack", alpha = 1) +
  geom_line(data = manual_data_total,
            aes(x = hours, y = total_flux),
            color = "black", size = 0.5) +
  # Use a custom facet function to control ordering
  facet_wrap(~ factor(group_label, 
                      levels = group_label_mapping[c("No acid", "Low acid", "Medium acid", "High acid")]), 
             labeller = label_parsed, 
             nrow = 2, 
             ncol = 2, 
             scales = "free_y") +  # Keep free_y scales as before
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20),  expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits=c(0,9),
                     breaks = seq(0,9, by=3),
                     expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = voc_colors) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16),
    plot.margin = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  ) +
  labs(
    x = "Time after slurry application (hours)",
    y = expression("Flux (mg " * m^{-2} * " " * min^{-1} * ")"),
    fill = "Category"
  ) +
  coord_cartesian(clip = "off")
print(p_manual)
# Create final plot with precise rectangle alignment
jpeg("../plots/Fig4.jpg", width = 12, height = 14, units = "in", res = 300)
getwd()
# Create layout for the combined plot
grid.newpage()
layout <- grid.layout(nrow = 2, ncol = 2, 
                      heights = c(0.8, 2.2), 
                      widths = c(0.92, 0.08))
pushViewport(viewport(layout = layout))

# Draw machine plot in top cell
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(ggplotGrob(p_machine))
popViewport()

# Draw manual plot in bottom cell
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(ggplotGrob(p_manual))
popViewport()

# For Machine Application (top rectangle)
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.rect(x = 0.5, 
          y = 0.474,  
          width = 0.8, 
          height = 0.704,
          just = "centre", 
          gp = gpar(fill = "lightgray", col = "gray"))

grid.text("Machine\nApplication", 
          x = 0.5, 
          y = 0.474,
          rot = 270,
          gp = gpar(fontsize = 16, col = "black", fontface = "bold"),
          just = "centre")
popViewport()

# For Manual Application (bottom rectangle)
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
grid.rect(x = 0.5, 
          y = 0.55,  
          width = 0.8, 
          height = 0.74,
          just = "centre", 
          gp = gpar(fill = "lightgray", col = "gray"))

grid.text("Manual Application", 
          x = 0.5, 
          y = 0.55,
          rot = 270,
          gp = gpar(fontsize = 16, col = "black", fontface = "bold"),
          just = "centre")
popViewport()

dev.off()











# Load data 
data <- read_csv("../output-DFC/OAV.txt")
bLS_oav <- read_csv("../output-bLS/OAV_by_hour.csv")


# Identify OAV columns
OAV_columns <- grep("OAV_", names(data), value = TRUE)
data$hours <- data$elapsed_time

# Process main OAV data
OAV_long <- data %>%
  dplyr::select(elapsed_time, treatment, valve, group, all_of(OAV_columns)) %>%
  pivot_longer(
    cols = all_of(OAV_columns),
    names_to = "OAV_type",
    values_to = "value"
  ) %>%
  mutate(
    compound = sub("OAV_(.+)", "\\1", OAV_type),
    hours = elapsed_time
  ) %>%
  filter(hours <= 119) %>%
  group_by(hours, group, compound) %>%
  summarise(
    value = mean(value, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    application = case_when(
      group == "Machine plot" ~ "Machine Application",
      TRUE ~ "Manual Application"
    )
  )

# Process bLS data with compound mapping
bls_compound_mapping <- c(
  "acetic_acid" = "Acetic acid",
  "pentanoic_acid" = "Pentanoic acid",
  "acetaldheyde" = "Acetaldehyde",
  "formic_acid" = "Formic acid",
  "methanthiol" = "Methane thiol",
  "acetone" = "Acetone",
  "X2_butanone" = "2-Butanone",
  "X2_3_butandion" = "2,3-Butanedione",
  "phenol" = "Phenol",
  "X4_methyl_phenol" = "4-Methylphenol",
  "X4_ethyl_phenol" = "4-Ethylphenol",
  "propanoic_acid" = "Propionic acid",
  "trimethylamine" = "Trimethylamine",
  "isoprene" = "Isoprene",
  "hydrogen_sulfide" = "Hydrogen sulphide",
  "methanol" = "Methanol",
  "dimethyl_sulfide" = "Dimethyl sulphide",
  "X3_methylindole" = "3-Methylindole",
  "butanoic_acid" = "Butanoic acid"
)

bLS_OAV <- bLS_oav %>%
  rename(value = Contribution) %>%
  mutate(
    compound = bls_compound_mapping[Compound],
    compound = ifelse(is.na(compound), Compound, compound),
    group = "bLS",
    application = "Machine Application"
  ) %>%
  dplyr::select(hours, value, compound, group, application)

# Add category assignments
add_categories <- function(data) {
  data %>%
    mutate(category = case_when(
      compound %in% c("Acetic acid", "Pentanoic acid", "Formic acid",
                      "Propionic acid", "Butanoic acid") ~ "VFA",
      compound %in% c("Methane thiol", "Hydrogen sulphide", "Dimethyl sulphide") ~ "VSC",
      compound %in% c("Phenol", "4-Methylphenol", "4-Ethylphenol") ~ "Phenol",
      compound == "3-Methylindole" ~ "Indole",
      TRUE ~ "Other"
    ))
}

OAV_long <- add_categories(OAV_long)
bLS_OAV <- add_categories(bLS_OAV)

# Check for duplicates in bLS data
if(any(duplicated(bLS_OAV[, c("hours", "compound")]))) {
  message("Found duplicates in bLS data - removing them")
  bLS_OAV <- bLS_OAV %>% 
    distinct(hours, compound, .keep_all = TRUE)
}

# Combine datasets
OAV_long_combined <- bind_rows(OAV_long, bLS_OAV)

# Calculate category OAV data - EXCLUDE VSC peaks at hour 0 for manual application
category_OAV_time_series <- OAV_long_combined %>%
  filter(!(application == "Manual Application" & hours == 0 & category == "VSC")) %>%
  group_by(hours, group, application, category) %>%
  summarize(
    mean_OAV = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate total OAV - EXCLUDE VSC peaks at hour 0 for manual application
total_OAV <- OAV_long_combined %>%
  filter(!(application == "Manual Application" & hours == 0 & category == "VSC")) %>%
  group_by(hours, group, application) %>%
  summarize(total_OAV = sum(value, na.rm = TRUE), .groups = "drop")
# Define the desired category order (from top to bottom in stacked areas)
desired_category_order <- c("VSC", "Indole", "Other", "VFA", "Phenol")

# Apply the custom ordering to category data
category_OAV_time_series$category <- factor(category_OAV_time_series$category, 
                                            levels = desired_category_order)


# ORDER CATEGORIES BY TOTAL CONTRIBUTION (MOST TO LEAST)
#category_totals_oav <- category_OAV_time_series %>%
#group_by(category) %>%
# summarise(Total_Contribution = sum(mean_OAV, na.rm = TRUE))

#print("OAV categories ordered by total contribution (highest to lowest):")
#print(category_totals_oav)

# Apply ordering to category data
#category_OAV_time_series$category <- factor(category_OAV_time_series$category, 
#levels = category_totals_oav$category)

# Define color palette
voc_colors <- c(
  "VFA" = "#4e79a7",
  "VSC" = "#d62728",
  "Phenol" = "#f28e2b",
  "Other" = "#76b7b2",
  "Indole" = "#59a14f"
)

# Define plotmath-compatible facet labels
group_label_mapping <- c(
  "Machine plot" = "0-DFC",
  "bLS" = "0-bLS",
  "Low acid" = "2.9-DFC",
  "Medium acid" = "5.3-DFC",
  "High acid" = "10.5-DFC",
  "No acid" = "0-DFC"
)
View(category_OAV_time_series)
# Create separate datasets for machine and manual applications
machine_data_category <- category_OAV_time_series %>% 
  filter(application == "Machine Application") %>%
  mutate(group_label = group_label_mapping[group])

machine_data_total <- total_OAV %>% 
  filter(application == "Machine Application") %>%
  mutate(group_label = group_label_mapping[group])

manual_data_category <- category_OAV_time_series %>% 
  filter(application == "Manual Application") %>%
  mutate(group_label = group_label_mapping[group])

manual_data_total <- total_OAV %>% 
  filter(application == "Manual Application") %>%
  mutate(group_label = group_label_mapping[group])

p_machine <- ggplot() +
  geom_area(data = machine_data_category, 
            aes(x = hours, y = mean_OAV, fill = category),
            position = "stack", alpha = 1) +
  geom_line(data = machine_data_total,
            aes(x = hours, y = total_OAV),
            color = "black", size = 0.5) +
  facet_wrap(~ group_label, labeller = label_parsed, nrow = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits=c(0,500),
                     breaks = seq(0,500, by=150),
                     expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = voc_colors) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "none",
    plot.margin = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank() 
  ) +
  labs(
    x = NULL,
    y = "OAV"
  )

p_manual <- ggplot() +
  geom_area(data = manual_data_category, 
            aes(x = hours, y = mean_OAV, fill = category),
            position = "stack", alpha = 1) +
  geom_line(data = manual_data_total,
            aes(x = hours, y = total_OAV),
            color = "black", size = 0.5) +
  facet_wrap(~ factor(group_label, 
                      levels = group_label_mapping[c("No acid", "Low acid", "Medium acid", "High acid")]), 
             labeller = label_parsed, 
             nrow = 2, 
             ncol = 2, 
             scales = "free_y") +
  scale_x_continuous(
    limits = c(0, 119), 
    breaks = seq(0, 120, by = 20), 
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(limits=c(0,900),
                     breaks = seq(0,900, by=300),
                     expand = expansion(mult = c(0, 0.1)) 
  ) +
  scale_fill_manual(values = voc_colors) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 16),
    plot.margin = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 16)
  ) +
  labs(
    x = "Time after slurry application (hours)",
    y = "OAV",
    fill = "Category"
  )

# Create final plot with precise rectangle alignment
jpeg("../plots/Fig5.jpg", width = 12, height = 14, units = "in", res = 300)

# Create layout for the combined plot
grid.newpage()
layout <- grid.layout(nrow = 2, ncol = 2, 
                      heights = c(0.8, 2.2), 
                      widths = c(0.92, 0.08))
pushViewport(viewport(layout = layout))

# Draw machine plot in top cell
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(ggplotGrob(p_machine))
popViewport()

# Draw manual plot in bottom cell
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(ggplotGrob(p_manual))
popViewport()

# For Machine Application (top rectangle)
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.rect(x = 0.5, 
          y = 0.474,  
          width = 0.9, 
          height = 0.704,
          just = "centre", 
          gp = gpar(fill = "lightgray", col = "gray"))

grid.text("Machine\nApplication", 
          x = 0.5, 
          y = 0.474,
          rot = 270,
          gp = gpar(fontsize = 16, col = "black", fontface = "bold"),
          just = "centre")
popViewport()

# For Manual Application (bottom rectangle)
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
grid.rect(x = 0.5, 
          y = 0.55,  
          width = 0.9, 
          height = 0.74,
          just = "centre", 
          gp = gpar(fill = "lightgray", col = "gray"))

grid.text("Manual Application", 
          x = 0.5, 
          y = 0.55,
          rot = 270,
          gp = gpar(fontsize = 16, col = "black", fontface = "bold"),
          just = "centre")
popViewport()

dev.off()
getwd()




cum_voc_emis <- read_csv("../output-DFC/cum.voc.emis.csv")

# Create compound mapping for cleaner labels
compound_mapping <- c(
  "cum.methanol" = "Methanol",
  "cum.H2S" = "Hydrogen Sulfide", 
  "cum.X4.Methylphenol" = "4-Methylphenol",
  "cum.acetic_acid" = "Acetic Acid",
  "cum.butanoic_acid" = "Butanoic Acid",
  "cum.pentanoic_acid" = "Pentanoic Acid",
  "cum.propanoic_acid" = "Propionic Acid",
  "cum.acetladheyde" = "Acetaldehyde",
  "cum.formic_acid" = "Formic Acid",
  "cum.methanthiol" = "Methanethiol",
  "cum.acetone" = "Acetone",
  "cum.trimethylamine" = "Trimethylamine",
  "cum.dimethyl_sulfide" = "Dimethyl Sulfide",
  "cum.isopren" = "Isoprene",
  "cum.butanone" = "2-Butanone",
  "cum.phenol" = "Phenol",
  "cum.X4_ethyl_phenol" = "4-Ethylphenol",
  "cum.methyl_indole" = "3-Methylindole",
  "cum.butandion" = "2,3-Butanedione"
)

# Define group name mapping
group_label_mapping <- c(
  "No acid" = "0-DFC",
  "Low acid" = "2.9-DFC", 
  "Medium acid" = "5.3-DFC",
  "High acid" = "10.5-DFC",
  "bLS" = "0-bLS"
)

# Define the desired order for groups
group_order <- c("0-DFC", "2.9-DFC", "5.3-DFC", "10.5-DFC", "0-bLS")

# Define consistent colors for each group (using renamed labels)
group_colors <- c(
  "0-DFC" = "#E31A1C",      # Red
  "2.9-DFC" = "#1F78B4",    # Blue  
  "5.3-DFC" = "#33A02C",    # Green
  "10.5-DFC" = "#FF7F00",   # Orange
  "0-bLS" = "#6A3D9A"       # Purple
)

# Filter out "Machine plot" and get compound columns
cum_columns <- grep("^cum\\.", names(cum_voc_emis), value = TRUE)
cum_columns <- cum_columns[cum_columns != "total_cum"]  # Exclude total_cum

plot_data <- cum_voc_emis %>%
  filter(group != "Machine plot") %>%
  select(group, all_of(cum_columns)) %>%
  mutate(group_label = group_label_mapping[group]) %>%  # Add renamed group labels
  pivot_longer(cols = all_of(cum_columns),
               names_to = "compound",
               values_to = "cumulative_emission") %>%
  mutate(compound_clean = ifelse(compound %in% names(compound_mapping),
                                 compound_mapping[compound],
                                 compound))

# Calculate mean and standard deviation for each group-compound combination
summary_data <- plot_data %>%
  group_by(group_label, compound_clean) %>%
  summarise(
    mean_emission = mean(cumulative_emission, na.rm = TRUE),
    sd_emission = sd(cumulative_emission, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  # Apply factor ordering to group_label
  mutate(group_label = factor(group_label, levels = group_order))

# Create the plot with facets by compound
a<-ggplot(summary_data, aes(x = group_label, y = mean_emission, fill = group_label)) +
  geom_col(alpha = 1, width = 0.7) +
  geom_errorbar(aes(ymin = mean_emission - sd_emission, 
                    ymax = mean_emission + sd_emission),
                width = 0.25, color = "black") +
  facet_wrap(~ compound_clean, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = group_colors) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    axis.title.x = element_text(size=20),
    axis.title.y = element_text(size=20),
    # Bigger y-axis text
    strip.text = element_text(size = 20, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),  # Classic grey facet strips
    legend.position = "none",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10),
    panel.border = element_rect(color = "gray", fill = NA)
  ) +
  labs(
    x = "",
    y = expression("Cumulative Emissions (mg " * m^{-2} * ")"),
    title = "",
    fill = ""
  )
print(a)
# Save the plot
# ggsave("../plots/FigS11.jpg",a, width = 16, height = 12, dpi = 300, bg="white")








