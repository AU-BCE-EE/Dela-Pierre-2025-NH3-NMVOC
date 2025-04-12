########################################################################################
#-----Plot prerequisite ------------
########################################################################################

#Assinging colors to treatments and groups
all_colors <- c(
  # Treatment levels
  "No acid" = "#4e79a7",
  "Low acid" = "#f28e2b",
  "Medium acid" = "#e15759",
  "High acid" = "#76b7b2",
  "Machine plot" = "#59a14f",
  
  # Group levels
  "Carboxylic Acids" = "#4e79a7",
  "Volatile Sulfur Compounds (VSC)" = "#f28e2b",
  "Phenols" = "#e15759",
  "Other" = "#76b7b2",
  "Indole" = "#59a14f"
)


# Defining a named vector for the facet labels
facet_labels <- c(
  'Mp' = 'Machine plot',
  '0-bp' = 'No acid',
  '1.5' = 'Low acid',
  '2.9' = 'Medium acid',
  'bkg' = 'Background',
  '5.7' = 'High acid'
)

########################################################################################
#-----Plot OAV Over Time for each compound------------
########################################################################################

#Assigning color to each treatment
OAVplot <- ggplot(oav_long, aes(x = elapsed.time, y = value, colour = group)) + 
  geom_point(size = 0.4) +  # Adjust point size
  geom_line(aes(group = valve), linewidth = 0.5) +  
  facet_wrap(~ compound, scales = "free_y") + 
  labs(
    x = "Time after slurry application (hours)", 
    y = "OAV"
  ) +
  scale_color_manual(values = all_colors) +  
  theme_minimal(base_size = 10) + 
  theme(
    strip.text = element_text(size = 10, face = "bold"),  
    legend.title = element_blank(), 
    legend.text = element_text(size = 10, face = "bold"),
    legend.position = "bottom",  
    plot.title = element_text()
  );
print(OAVplot)


########################################################################################
#-----Plot OAV Over Time groupwise------------
########################################################################################

#Generating the faceted plot
oav.plot <- ggplot(oav_summary, aes(x = elapsed.time, y = mean, fill = Group)) +
  geom_area(alpha = 0.8, position = "stack", color = "black", linewidth = 0.2) +
  scale_fill_manual(values = all_colors) +
  labs(
    x = "Time after slurry application (hours)",
    y = "OAV"
  ) + 
  facet_wrap(~ treatment, scales = "fixed", ncol = 3, labeller = as_labeller(facet_labels)) +
  theme_minimal(base_size = 10) +
  theme(
    # Titles
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5, color = "gray40"),
    
    # Axes
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.line = element_line(color = "gray50", linewidth = 0.5),
    
    # Legend
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = "bottom",
    legend.key.size = unit(0.5, "cm"),
    
    # Facets
    strip.text = element_text(size = 12, face = "bold", color = "white"),
    strip.background = element_rect(fill = "gray70", linewidth = 0.4),
    
    # Background and Grid
    panel.grid = element_line(color = "gray84", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray60", fill = NA, linewidth = 0.5)
  ) +
  scale_x_continuous(
    limits = c(0, 119),
    breaks = seq(0, 120, by = 20)
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  )
print(oav.plot)
########################################################################################
#-----Plot OAV hourly loss in %------------
########################################################################################
hourly_data <- sum.oav %>%
  mutate(
    # Ensure elapsed time is rounded to the nearest multiple of 5
    elapsed.time = floor(elapsed.time / 10) * 10,
    hour_label = sprintf("%d-%d", elapsed.time, elapsed.time + 10)
  ) %>%
  group_by(elapsed.time, hour_label, treatment, Group) %>%  
  summarise(
    Category_Sum = sum(Sum_Flux, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  group_by(elapsed.time, treatment) %>% 
  mutate(
    Total = sum(Category_Sum, na.rm = TRUE),  
    Percentage = (Category_Sum / Total) * 100
  ) %>%
  filter(!is.na(Group))  # Ensure no NA Groups

# Faceted plot for each treatment
Hourly <- ggplot(hourly_data, 
                 aes(x = elapsed.time, y = Percentage, fill = Group)) +
  geom_bar(position = "stack",
           stat = "identity",
           width = 8,  
           alpha = 0.6) +
  scale_x_continuous(
    limits = c(-7, 120),  
    breaks = seq(0, 110, by = 10),  
    labels = sprintf("%d-%d", 
                     seq(0, 110, by = 10),
                     seq(10, 120, by = 10)),  
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = all_colors) +  
  labs(
    x = "Time after slurry application (hours)",
    y = "OAV (%)"
  ) +
  facet_wrap(~ treatment, scales = "fixed", ncol = 3, labeller = as_labeller(facet_labels)) +
  theme_minimal(base_size = 5) +
  theme(
    
    # Axes
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  
    axis.text.y = element_text(size = 10),
    
    # Legend
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    # Facet labels
    strip.text = element_blank(),
    
    # Background and Grid
    panel.border = element_rect(color = "gray70", fill = NA, linewidth = 0.5)
  ) +
  guides(
    fill = guide_legend(nrow = 1)
  ) 
print(Hourly)

########################################################################################
#-----Combing both plot over time and hourly plots------------
########################################################################################
oav.plot <- oav.plot +
  theme(
    legend.position = "none",     
    axis.title.x = element_blank(),  
    axis.text.x = element_blank(),   
    axis.ticks.x = element_blank()
  )
# Combine plots
combined_plot <- plot_grid(
  oav.plot, Hourly,
  ncol = 1,
  align = "v",
  axis = "lr",
  rel_heights = c(0.6, 0.4)
) 
print(combined_plot)
