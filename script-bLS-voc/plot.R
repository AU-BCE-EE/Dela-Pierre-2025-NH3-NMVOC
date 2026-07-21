# ==============================================================================
# NMVOC Flux Visualizations
# ==============================================================================


# ==============================================================================
# 1. LOAD CALCULATED DATA
# ==============================================================================

mass_emissions_by_category <- read_csv("../output-bLS/mass_emissions_cat.csv")
View(mass_emissions_by_category)
filtered_Flux_odors <- read_csv("../output-bLS/fluxes_vocs_1stweek.csv")


# Load temperature data
hour_T <- read_excel("../input-bLS/hour_T.xlsx")
hour_T$T <- na.approx(hour_T$T, rule = 2, na.rm = FALSE)

# ==============================================================================
# 2. DEFINE PLOT THEME AND COLORS
# ==============================================================================

category_colors <- c(
  "VFA" = "#4e79a7",
  "VSC" = "#d62728", 
  "Phenol" = "#f28e2b",
  "Indole" = "#59a14f",
  "Other" = "#76b7b2"
)

# ==============================================================================
# 3. STACKED AREA PLOT
# ==============================================================================
mass_emissions_by_category$Category <- factor(
  mass_emissions_by_category$Category,
  levels = category_totals_mass$Category
)
mass_emissions_by_category$hours
mass_emissions_by_category<-filter(mass_emissions_by_category, hours<=95)
p1 <- ggplot() +
  geom_area(data = mass_emissions_by_category,
            aes(x = hours, y = Category_Mass, fill = Category),
            position = "stack", alpha = 1, na.rm = TRUE) +
  geom_line(data = mass_emissions_by_category,
            aes(x = hours, y = Total_Mass),
            color = "black", size = 0.5, na.rm = TRUE) +
  scale_fill_manual(values = category_colors) +
  scale_x_continuous(limits = c(-3, 100), expand = c(0, 0)) +
  labs(y = expression(paste("Mass Emission (mg m"^-2, " min"^-1, ")"))) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        legend.position = "none",
        plot.margin = margin(b = 0))
print(p1)
# Create p2 without a legend (with ordered categories)
p2 <- mass_emissions_by_category %>%
  mutate(hour_group = floor(hours/5) * 5 + 2.5) %>%
  group_by(hour_group, Category) %>%
  summarise(Category_Mass = sum(Category_Mass, na.rm = TRUE), .groups = 'drop') %>%
  group_by(hour_group) %>%
  mutate(Percentage = (Category_Mass/sum(Category_Mass, na.rm = TRUE)) * 100) %>%
  # Apply same factor ordering to the grouped data
  mutate(Category = factor(Category, levels = category_totals_mass$Category)) %>%
  ggplot(aes(x = hour_group, y = Percentage, fill = Category)) +
  geom_col(position = "stack", width = 4, alpha = 1, na.rm = TRUE) +
  scale_x_continuous(
    limits = c(-3, 100),
    breaks = seq(0, 95, by = 5),
    labels = sprintf("%d-%d", seq(0, 95, by = 5), seq(5, 100, by = 5)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = category_colors) +
  theme_minimal(base_size = 16) +
  theme(axis.text.x = element_text(angle = 90, vjust = 2.7, hjust = 1),
        legend.position = "none") +
  labs(x = "Time from slurry application (hours)", y = "Mass emission (%)")

print(p2)

# ==============================================================================
# 5. COMBINE PLOTS
# ==============================================================================

legend <- get_legend(
  p1 + 
    theme_minimal(base_size = 16) +
    theme(legend.position = "right") + 
    guides(fill = guide_legend(title = "Category"))
)

combined_plots <- plot_grid(
  p1, p2, 
  ncol = 1, 
  align = "v", 
  axis = "lr", 
  rel_heights = c(0.7, 0.3)
)

combined_mass <- plot_grid(
  combined_plots, legend, 
  ncol = 2, 
  rel_widths = c(0.85, 0.15)
)

# Save combined plot
# ggsave("../plots/FigS4.png", 
# combined_mass, 
# width = 12, 
# height = 10, 
# dpi = 600,
#  bg = "white")

# ==============================================================================
# 6. INDIVIDUAL COMPOUND FLUX PLOTS WITH ENVIRONMENTAL VARIABLES
# ==============================================================================

# Define Group 1 compounds
group1_compounds <- c( "2,3-Butanedione","4-Methylphenol", "Acetic Acid", "Butanoic Acid", "Dimethyl Sulfide"
,"Formic Acid" , "Hydrogen Sulfide","Methanethiol","Phenol")

# Standardize compound names
standardize_compound_name <- function(name) {
  name <- gsub("^Flux_", "", name)
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
filtered_Flux_odors_long <- filtered_Flux_odors %>%
  pivot_longer(
    cols = starts_with("Flux_"),
    names_to = "Compound",
    values_to = "Flux"
  ) %>%
  mutate(
    Compound = sapply(Compound, standardize_compound_name),
    Category = case_when(
      Compound %in% c("Acetic Acid", "Pentanoic Acid", "Formic Acid",
                      "Propionic Acid", "Butanoic Acid") ~ "VFA",
      Compound %in% c("Methanethiol", "Hydrogen Sulfide", "Dimethyl Sulfide") ~ "VSC",
      Compound %in% c("Phenol", "4-Methylphenol", "4-Ethylphenol") ~ "Phenol",
      Compound == "3-Methylindole" ~ "Indole",
      TRUE ~ "Other"
    )
  )

# Filter for Group 1 compounds
group1_data <- filtered_Flux_odors_long %>%
  filter(Compound %in% group1_compounds)

# Flux plot
flux_plot <- ggplot(group1_data, aes(x = hours, y = Flux/1000*60, color = Category)) + 
  geom_line(size = 0.5) + geom_point(size=1.5)+
  facet_wrap(~Compound, ncol = 3, scales = "free_y", strip.position = "top") +
  scale_color_manual(values = category_colors) +
  scale_x_continuous(
    breaks = seq(0, 120, by = 24),
    limits = c(0, 120),
    name = NULL
  ) +
  scale_y_continuous(
    name = expression("Flux (mg m"^-2 ~ " min"^-1*")"),
    expand = expansion(mult = c(0, 0.15))
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray85"),
    strip.text = element_text(size = 20, face = "bold"),
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, margin = margin(r = 15)),
    plot.margin = margin(t = 20, r = 10, b = 20, l = 10),
    legend.position = "none"
  )

# Temperature plot
temperature_plot <- ggplot(hour_T, aes(x = hour, y = T)) +
  geom_line(color = "black", size = 0.8) +
  scale_x_continuous(
    breaks = seq(0, 120, by = 24),
    limits = c(0, 120),
    name = NULL
  ) +
  scale_y_continuous(
    name = "T (B0C)",
    limits = c(0, 25),
    breaks = seq(0, 25, by = 5),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, margin = margin(r = 15)),
    plot.margin = margin(t = 20, r = 10, b = 20, l = 10)
  )

# Wind speed plot
ws_plot <- ggplot(filtered_Flux_odors, aes(x = hours, y = WS)) +
  geom_line(color = "black", size = 0.8) +
  scale_x_continuous(
    breaks = seq(0, 120, by = 24),
    limits = c(0, 120),
    name = NULL
  ) +
  scale_y_continuous(
    name = expression("WS (m" ~ s^-1 ~ ")"),
    limits = c(0, max(filtered_Flux_odors$WS, na.rm = TRUE) * 1.1),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, margin = margin(r = 15)),
    plot.margin = margin(t = 20, r = 10, b = 20, l = 10)
  )

# Wind direction plot
wd_plot <- ggplot(filtered_Flux_odors, aes(x = hours, y = WD)) +
  geom_line(color = "black", size = 0.8) +
  scale_x_continuous(
    breaks = seq(0, 120, by = 24),
    limits = c(0, 120),
    name = NULL
  ) +
  scale_y_continuous(
    name = "WD (B0)",
    limits = c(0, 360),
    breaks = seq(0, 360, by = 90),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 20),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 20, margin = margin(r = 15)),
    plot.margin = margin(t = 20, r = 10, b = 20, l = 10)
  )

# Align plot widths
xlim_common <- c(0, 120)
strip_theme <- theme(plot.margin = margin(5, 5, 5, 5))

flux_plot <- flux_plot + scale_x_continuous(limits = xlim_common) + strip_theme
temperature_plot <- temperature_plot + scale_x_continuous(limits = xlim_common) + strip_theme
ws_plot <- ws_plot + scale_x_continuous(limits = xlim_common) + strip_theme
wd_plot <- wd_plot + scale_x_continuous(limits = xlim_common) + strip_theme

# Combine with patchwork
design <- "
aaa
bcd
"

combined_flux_env <- flux_plot + temperature_plot + ws_plot + wd_plot +
  plot_layout(
    design = design,
    heights = c(2, 1),
    widths  = c(1, 1, 1)
  ) +
  plot_annotation(
    caption = "Time from slurry application (hours)",
    theme = theme(
      plot.caption = element_text(size = 20, hjust = 0.5, margin = margin(t = 15))
    )
  )
print(combined_flux_env)
# Save combined flux and environmental plot
ggsave("../plots/Fig3.png", 
       combined_flux_env, 
       width = 16, 
       height = 12, 
       dpi = 600,
       bg = "white")

cat("\n=== PLOTTING COMPLETE ===\n")
cat("Plots saved to ../plots/\n")


# ==============================================================================
# OAV TIME SERIES AND CATEGORY HISTOGRAM (moved from oav.R)
# ==============================================================================

soav_categories <- read_csv("../output-bLS/soav_categories.csv")
hourly_data     <- read_csv("../output-bLS/OAV_by_hour_cat.csv")
OAV             <- read_csv("../output-bLS/OAV.csv")

# Restore factor ordering (highest to lowest total contribution)
category_totals <- soav_categories %>%
  group_by(Category) %>%
  summarise(Total_Contribution = sum(Contribution, na.rm = TRUE)) %>%
  arrange(Total_Contribution)

soav_categories$Category <- factor(soav_categories$Category,
                                   levels = category_totals$Category)
hourly_data$Category     <- factor(hourly_data$Category,
                                   levels = category_totals$Category)

# Define consistent colors for categories
category_colors <- c(
  "VFA"    = "#4e79a7",
  "VSC"    = "#d62728",
  "Phenol" = "#f28e2b",
  "Indole" = "#59a14f",
  "Other"  = "#76b7b2"
)

alpha_value <- 1

p1 <- ggplot() +
  geom_area(data = soav_categories,
            aes(x = hours, y = Contribution, fill = Category),
            position = "stack",
            alpha = alpha_value) +
  geom_line(data = data.frame(hours = OAV$hours, SOAV = OAV$SOAV),
            aes(x = hours, y = SOAV),
            color = "black",
            size = 0.5) +
  scale_fill_manual(values = category_colors) +
  scale_x_continuous(limits = c(-3, 100), expand = c(0, 0)) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x  = element_blank(),
    axis.title.x = element_blank(),
    plot.margin  = margin(b = 0),
    legend.position = "none"
  ) +
  labs(y = "OAV")

# Shift bar centers to midpoint of each 5-hour interval
hourly_data <- hourly_data %>%
  mutate(hour_group = hour_group + 2.5)

p2 <- ggplot(hourly_data, aes(x = hour_group, y = Percentage, fill = Category)) +
  geom_bar(position = "stack", stat = "identity", width = 4, alpha = alpha_value) +
  scale_x_continuous(
    limits = c(-3, 100),
    breaks = seq(0, 95, by = 5),
    labels = sprintf("%d-%d", seq(0, 95, by = 5), seq(5, 100, by = 5)),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = category_colors) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x     = element_text(angle = 90, vjust = 2),
    legend.position = "none"
  ) +
  labs(
    x = "Time from slurry application (hours)",
    y = "OAV (%)"
  )

legend <- get_legend(
  p1 + theme_minimal(base_size = 16) +
    theme(legend.position = "right") +
    guides(fill = guide_legend(title = "Category"))
)

combined_plots <- plot_grid(
  p1, p2,
  ncol        = 1,
  align       = "v",
  axis        = "lr",
  rel_heights = c(0.7, 0.3)
)

combined_oav <- plot_grid(
  combined_plots, legend,
  ncol       = 2,
  rel_widths = c(0.85, 0.15)
)

# ggsave("../plots/FigS5.jpg", combined_oav, width = 12, height = 10, dpi = 300, bg = "white")

