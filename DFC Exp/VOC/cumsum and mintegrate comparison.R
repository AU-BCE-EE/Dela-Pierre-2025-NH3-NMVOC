####################################################################
####################################################################


#------cumulative cumsum--------
#------Flavia--------
cum.f <- dat

names(cum.f)[12:30] <- paste0("voc", 1:19)
# Define the VOC column names
voc_cols <- paste0("voc", 1:19)


cum.f <- cum.f %>%
  group_by(valve, treatment) %>%                         # Group by 'valve' and 'treatment'
  arrange(elapsed.time) %>%                              # Ensure data is sorted by elapsed time
  mutate(across(starts_with("voc"),                       # Apply transformation to VOC columns
                ~ cumsum(replace_na(. * (elapsed.time * 8))))) %>% 
  ungroup()
# Remove grouping structure

cum.f <- cum.f [, -c(5:11)]

#Cumulative emissions by treatment from mintegrate function
cum.f <- cum.f %>%
  rename_with(~paste0("cum.emis", seq_along(.)), starts_with("voc")) %>%
  group_by(treatment)


#Filter the data to get the last time point for each valve-treatment group
dat_f <- cum.f %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

# Create a summary dataset for plotting (one point per treatment group)
indsum.f <- dat_f %>%
  select(valve, treatment, elapsed.time, starts_with("cum.emis")) %>%
  distinct()

# Summarize/mean cumulative emissions by treatment
cumsum.f <- aggregate(. ~ treatment, data = indsum, FUN = function(x) mean(x, na.rm = TRUE))

# Rename columns
# Create a summary dataset for plotting (one point per treatment group)
indsum_new.f <- dat_f %>%
  select(valve, treatment, elapsed.time, group, starts_with("cum.emis")) %>%
  distinct()

indsum_new.f <- indsum_new.f %>%
  rename(
    methanol = cum.emis1,
    H2S = cum.emis2,
    `4_Methylphenol` = cum.emis3,
    acetic_acid = cum.emis4,
    butanoic_acid = cum.emis5,
    pentanoic_acid = cum.emis6,
    propanoic_acid = cum.emis7,
    acetladheyde = cum.emis8,
    formic_acid = cum.emis9,
    methanthiol = cum.emis10,
    acetone = cum.emis11,
    trimethylamine = cum.emis12,
    dimethyl_sulfide = cum.emis13,
    isopren = cum.emis14,
    butanone = cum.emis15,
    butandion = cum.emis16,
    phenol = cum.emis17,
    `4_ethyl_phenol` = cum.emis18,
    methyl_indole = cum.emis19
  )

# Convert data to long format
indsum_long.f <- indsum_new.f %>%
  pivot_longer(
    cols = -c(valve, treatment, group, elapsed.time),  # Select all columns except valve & treatment
    names_to = "VOC", 
    values_to = "emis"
  )

# Apply factor ordering
indsum_long.f <- indsum_long.f %>%
  mutate(
    VOC = factor(VOC, levels = desired_order),
    category = recode(VOC, !!!category)  # Assign group categories
  )

# Plot cumulative emissions for each treatment across all cum.emis columns
cumsum_plot <- ggplot(indsum_long.f, aes(x = treatment, y = emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(aes(x = treatment, y = emis, color = treatment), 
               show.legend = FALSE) +  
  facet_wrap(~VOC, scales = "free_y") +  # Create separate plots for each cum.emis column
  theme_bw() +
  labs(
    title = "Cumulative Emissions by Treatment",  
    x = "Treatment",  
    y = "Cumulative Emission Value"
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 10),  # Adjust facet label size
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); cumsum_plot

#------Plot cumulative cumsum--------
# Ensure the treatment variable is a factor with the correct levels
indsum_long.f$treatment <- factor(indsum_long.f$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))


indsum_mean.f <- indsum_long.f %>%
  group_by(elapsed.time, treatment, VOC, category, group) %>%
  summarise(mean_emis = mean(emis, na.rm = TRUE), .groups = "drop")

indsum_cat.f <- indsum_mean.f %>%
  group_by(treatment, category) %>%
  summarise(mean_emis = sum(mean_emis, na.rm = TRUE), .groups = "drop")

# Define a named vector for facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid', 
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

# Create the bar plot with facet_wrap
f <- ggplot(indsum_cat.f, aes(x = category, y = mean_emis, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bar plot
  facet_wrap(~treatment, labeller = as_labeller(facet_labels)) +  # Facet by treatment
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    y = "Mean Emission",  # Label for y-axis
    fill = "Category"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(),  # Remove y-axis title
    strip.text = element_text(size = 12, face = "bold")  # Format facet labels
  );f

#ggsave ("/Users/AU775281/Documents/PhD/Flavia Experiment/DFC VOC/Adjusted/Figure/cumulative neg set 0 flavia cum sum.png", plot = f)

###################################
#################################################
#------cumulative Trap--------
#------Flavia--------

cum.t <- dat

names(cum.t)[12:30] <- paste0("voc", 1:19)
# Define the VOC column names
voc_cols <- paste0("voc", 1:19)

cum.t <- cum.t %>%
  group_by(valve, treatment) %>%                # Group by 'valve' and 'treatment'
  arrange(elapsed.time) %>%                     # Ensure data is sorted by elapsed time
  mutate(across(voc_cols, ~ {
    # Calculate the time intervals in minutes
    time_intervals <- c(0, diff(elapsed.time * 60))  # Convert to minutes
    
    # Handle the first row where lag(.) would be NA
    lagged_values <- lag(.) # lag the VOC values
    lagged_values[1] <- 0   # Set the lag for the first row to 0 (it has no previous value)
    
    # Apply the trapezoidal rule to calculate cumulative emissions
    cum_emissions <- cumsum(time_intervals * (lagged_values + .) / 2) # Trapezoidal integration
    return(cum_emissions)
  })) %>%
  ungroup()
# Remove grouping structure

cum.t <- cum.t [, -c(5:11)]

#Cumulative emissions by treatment from mintegrate function
cum.t <- cum.t %>%
  rename_with(~paste0("cum.emis", seq_along(.)), starts_with("voc")) %>%
  group_by(treatment)


#Filter the data to get the last time point for each valve-treatment group
dat_t <- cum.t %>%
  group_by(valve, treatment) %>%
  filter(row_number() == n()) %>%  
  ungroup()

# Create a summary dataset for plotting (one point per treatment group)
indsum.t <- dat_t %>%
  select(valve, treatment, elapsed.time, starts_with("cum.emis")) %>%
  distinct()

# Summarize/mean cumulative emissions by treatment
cumsum.t <- aggregate(. ~ treatment, data = indsum, FUN = function(x) mean(x, na.rm = TRUE))

# Rename columns
# Create a summary dataset for plotting (one point per treatment group)
indsum_new.t <- dat_t %>%
  select(valve, treatment, elapsed.time, group, starts_with("cum.emis")) %>%
  distinct()

indsum_new.t <- indsum_new.t %>%
  rename(
    methanol = cum.emis1,
    H2S = cum.emis2,
    `4_Methylphenol` = cum.emis3,
    acetic_acid = cum.emis4,
    butanoic_acid = cum.emis5,
    pentanoic_acid = cum.emis6,
    propanoic_acid = cum.emis7,
    acetladheyde = cum.emis8,
    formic_acid = cum.emis9,
    methanthiol = cum.emis10,
    acetone = cum.emis11,
    trimethylamine = cum.emis12,
    dimethyl_sulfide = cum.emis13,
    isopren = cum.emis14,
    butanone = cum.emis15,
    butandion = cum.emis16,
    phenol = cum.emis17,
    `4_ethyl_phenol` = cum.emis18,
    methyl_indole = cum.emis19
  )

# Convert data to long format
indsum_long.t <- indsum_new.t %>%
  pivot_longer(
    cols = -c(valve, treatment, group, elapsed.time),  # Select all columns except valve & treatment
    names_to = "VOC", 
    values_to = "emis"
  )

# Apply factor ordering
indsum_long.t <- indsum_long.t %>%
  mutate(
    VOC = factor(VOC, levels = desired_order),
    category = recode(VOC, !!!category)  # Assign group categories
  )

# Plot cumulative emissions for each treatment across all cum.emis columns
cumsum_plot <- ggplot(indsum_long.t, aes(x = treatment, y = emis, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(aes(x = treatment, y = emis, color = treatment), 
               show.legend = FALSE) +  
  facet_wrap(~VOC, scales = "free_y") +  # Create separate plots for each cum.emis column
  theme_bw() +
  labs(
    title = "Cumulative Emissions by Treatment",  
    x = "Treatment",  
    y = "Cumulative Emission Value"
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 10),  # Adjust facet label size
    legend.title = element_blank(),                     
    legend.position = "right",                          
    axis.text.x = element_text(angle = 45, hjust = 1)
  ); cumsum_plot

#------Plot cumulative cumsum--------
# Ensure the treatment variable is a factor with the correct levels
indsum_long.t$treatment <- factor(indsum_long.t$treatment, levels = c('Mp', '0-bp', '1.5', '2.9', 'bkg', '5.7'))


indsum_mean.t <- indsum_long.t %>%
  group_by(elapsed.time, treatment, VOC, category, group) %>%
  summarise(mean_emis = mean(emis, na.rm = TRUE), .groups = "drop")

indsum_cat.t <- indsum_mean.t %>%
  group_by(treatment, category) %>%
  summarise(mean_emis = sum(mean_emis, na.rm = TRUE), .groups = "drop")

# Define a named vector for facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid', 
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

# Create the bar plot with facet_wrap
ggplot(indsum_cat.t, aes(x = category, y = mean_emis, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bar plot
  facet_wrap(~treatment, labeller = as_labeller(facet_labels)) +  # Facet by treatment
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(
    y = "Mean Emission",  # Label for y-axis
    fill = "Category"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_blank(),  # Remove y-axis labels
    axis.title.x = element_blank(),  # Remove y-axis title
    strip.text = element_text(size = 12, face = "bold")  # Format facet labels
  )
