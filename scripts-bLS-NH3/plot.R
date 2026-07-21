filtered_Flux <- read_csv("../output-bLS/NH3_bLS.csv")
# Create the plot using ggplot2
flux_plot <- ggplot(filtered_Flux, aes(x = Time, y = Flux_original)) +
  geom_line(color = "blue", size = 0.5) +
  scale_x_continuous(breaks = seq(0, 120, by = 20)) +
  labs(
    x = "Time from slurry application (hours)",
    y = expression(paste("Flux (mg N ", m^-2, " ", min^-1, ")")),
    title = ""
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    panel.grid.minor = element_blank()
  )

# Save the plot with specified dimensions
#ggsave(
#filename = "NH3_flux_min.png",
#plot = flux_plot,
#width = 8,      # Width in inches
# height = 6,     # Height in inches
# dpi = 300       # Resolution
#)


flux_plot_hod <- ggplot(filtered_Flux, aes(x = Time, y = Flux_original*60/1000*0.8224414)) +
  geom_line(color="red")+
  geom_point(color = "red", size = 1, shape=19) +
  scale_x_datetime(
    date_breaks = "6 hours",     # Adjust breaks as needed
    date_labels = "%H"        # Format for time display on x-axis
  ) +
  labs(
    x = "Time of the day (hour)",
    y = expression(paste("Flux (mg N ", m^-2, " ", min^-1, ")")),
    title = ""
  ) +
  theme_classic() +
  theme(
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 20),
    panel.grid.minor = element_blank()
  )

# ggsave(
#  filename = "../plots/FigS1.jpg",
#  plot = flux_plot_hod,
#  width = 15,      # Width in inches
#  height = 8,     # Height in inches
#  dpi = 300       # Resolution
# )

