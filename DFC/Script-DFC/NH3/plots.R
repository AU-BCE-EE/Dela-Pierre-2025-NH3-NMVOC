########################################################################################
#-----Prerequisite for plots ------------
########################################################################################

#Defining the order of treatment groups#
dat$group <- factor(dat$group, levels = c("No acid", "Low acid", "Medium acid", "High acid", "Machine plot"))

# Defining colors for each group#
all_colors <- c(
  # Treatment levels
  "No acid" = "#4e79a7",
  "Low acid" = "#f28e2b",
  "Medium acid" = "#e15759",
  "High acid" = "#76b7b2",
  "Machine plot" = "#59a14f"
)

# Optionally, keep separate mappings for clarity
treatment_colors <- c(
  "0-bp" = "#4e79a7",
  "1.5" = "#f28e2b",
  "2.9" = "#e15759",
  ".5.7" = "#76b7b2",
  "Mp" = "#59a14f"
)

# Define color for BLS data
bls_color <- "gray20" 

########################################################################################
#-----Plot DFC and bLSNH3 Flux Over Time ------------
########################################################################################

# Create the plot
Fluxes <- ggplot(dat_summary, aes(x = elapsed.time, y = mean_flux, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mean_flux - sd_flux, ymax = mean_flux + sd_flux), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1) +
  
  # Add a separate dashed line for BLS
  geom_line(data = bls_summary, aes(x = elapsed.time, y = mean_flux, linetype = "bLS plot"), 
            color = bls_color, linewidth = 1, inherit.aes = FALSE) +
  
  scale_color_manual(values = all_colors) +
  scale_fill_manual(values = all_colors) +
  
  scale_linetype_manual(values = c("bLS plot" = "dashed")) +
  scale_x_continuous(
    breaks = seq(0, 290, by = 25)
  ) +
  scale_y_continuous(
    name = expression(paste(NH[3], " Flux (mg-N * ", m^-2, " * min"^-1, ")"))
  ) +
  
  # Axis labels and title
  labs(
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment",
    linetype = "Legend"
  ) +
  
  # Theme settings
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom"
  ) +
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )
print(Fluxes)

########################################################################################


########################################################################################
#-----Plot Tan Over Time ------------
########################################################################################

#Ploting Tan
Tan <- ggplot(tan.plot.dat, aes(x = elapsed.time, y = mean, color = group, fill = group)) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.3, color = NA) +  
  geom_line(linewidth = 1) +  # Mean flux line
  geom_point(aes(x = elapsed.time, y = mean), size = 2, shape = 16, alpha = 0.7) +  
  scale_color_manual(values = all_colors) +  
  scale_fill_manual(values = all_colors) + 
  scale_x_continuous(breaks = seq(0, 290, by = 25)) +
  scale_y_continuous(breaks = seq(0, 6, by = 1)) +
  # Axis labels and title
  labs(
    y = expression("Tan h"^-1 * " [%]"),
    x = "Time after slurry application (hours)",
    color = "Treatment",
    fill = "Treatment"
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14),       
    axis.text = element_text(size = 12),      
    plot.title = element_text(size = 16, hjust = 0.5),
    strip.text = element_text(size = 14),       
    legend.text = element_text(size = 12),      
    legend.title = element_blank(),             
    legend.position = "bottom"        
  ) +
  
  guides(color = guide_legend(nrow = 1))

print(Tan)
########################################################################################

########################################################################################
#-----Box plot Tan------------
########################################################################################

# Plot the TAN loss fraction for each treatment
tan.loss <- ggplot(indsum.tan, aes(x = treatment, y = tanloss, color = treatment)) +  
  geom_point(size = 2, alpha = 0.7) +  
  geom_boxplot(data = cumsum.tan, aes(x = treatment, y = tanloss, color = treatment), 
               show.legend = F) +  
  theme_bw() +
  labs(
    title = NULL,  
    x = "Treatment",  
    y = expression("TAN loss (% of applied)")
  ) + 
  scale_x_discrete(labels = c(
    'Mp' = 'Machine plot',
    '0-bp' = 'No acid',
    '1.5' = 'Low acid',
    '2.9' = 'Medium acid',
    'bkg' = 'Background',
    '5.7' = 'High acid'
  )) +  
  scale_color_manual(values = treatment_colors) +  # Apply custom colors here
  theme(
    axis.title.x = element_text(size = 10),  # Set size for x axis title
    axis.title.y = element_text(size = 10),  # Set size for y axis title
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    axis.text.y = element_text(size = 9),  # Set size for y axis text
    strip.text = element_blank(),  # Remove facet strip text
    legend.title = element_blank(),  # Remove the legend title
    legend.position = "none"  # Remove the legend completely
  )

print(tan.loss)

#Combining the plots#
combined_plot <- Tan + inset_element(
  tan.loss, 
  left = 0.6, bottom = 0.6, right = 0.95, top = 0.96
)
print(combined_plot)
########################################################################################




