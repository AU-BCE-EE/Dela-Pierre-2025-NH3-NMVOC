# Read prepared data
dat_unique   <- read_csv("../output-DFC/dfc.NH3.csv")
machine_data <- read_csv("../output-DFC/NH3_machine_data.csv")
manual_data  <- read_csv("../output-DFC/NH3_manual_data.csv")

# --- Machine plot DFC: flux over time ---
machine_df <- dat_unique %>%
  filter(treatment == "Machine plot DFC")
machine_df$facet_label <- "0-DFC Machine"

p_0dfcramiran <- ggplot(machine_df, aes(x = elapsed.time, y = NH3.value, color = factor(plot.number))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = NULL,
    x = "Time from slurry application (hours)",
    y = expression("Flux (kg N " * ha^{-1} * " " * hour^{-1} * ")"),
    color = "Chamber number"
  ) +
  facet_wrap(~ facet_label) +
  theme_minimal(base_size = 24) +
  theme(
    plot.title       = element_text(hjust = 0.5, face = "bold"),
    legend.position  = "right",
    strip.background = element_rect(fill = "grey90", color = "grey60"),
    strip.text       = element_text(size = 24, face = "bold")
  )

# ggsave("../plots/0dfcmachine.png", p_0dfcramiran, width = 12, height = 8, dpi = 300, bg = "white")

# --- NH3 flux comparison: DFC vs bLS ---
group_label_mapping <- c(
  "Machine plot DFC" = "0-DFC",
  "0-bLS"            = "0-bLS",
  "Low acid DFC"     = "2.9-DFC",
  "Medium acid DFC"  = "5.3-DFC",
  "High acid DFC"    = "10.5-DFC",
  "No acid DFC"      = "0-DFC"
)

treatment_colors <- c(
  "Machine plot DFC" = "#1f77b4",
  "0-bLS"            = "#2ca02c",
  "Low acid DFC"     = "#ff7f0e",
  "Medium acid DFC"  = "#d62728",
  "High acid DFC"    = "#9467bd",
  "No acid DFC"      = "#8c564b"
)

machine_data <- machine_data %>%
  mutate(group_label = group_label_mapping[treatment],
         group_label = factor(group_label, levels = unique(group_label)))

manual_data <- manual_data %>%
  mutate(
    group_label  = group_label_mapping[treatment],
    facet_order  = case_when(
      treatment == "No acid DFC"     ~ 1,
      treatment == "Low acid DFC"    ~ 2,
      treatment == "Medium acid DFC" ~ 3,
      treatment == "High acid DFC"   ~ 4,
      TRUE                           ~ 5
    )
  ) %>%
  mutate(group_label = factor(group_label, levels = group_label_mapping[
    c("No acid DFC", "Low acid DFC", "Medium acid DFC", "High acid DFC")])) %>%
  arrange(facet_order)

# Version 1 (lines + ribbon, no points)
p_machine <- ggplot(machine_data) +
  geom_line(aes(x = elapsed.time, y = NH3.value_avg, color = treatment), size = 1) +
  geom_ribbon(aes(x = elapsed.time, ymin = NH3.value_avg - sd_flux, ymax = NH3.value_avg + sd_flux, fill = treatment), alpha = 0.2, color = NA) +
  facet_wrap(~ group_label, labeller = label_parsed, nrow = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(-0.25, 2), breaks = seq(0, 2, by = 0.25), expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    strip.text       = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border     = element_rect(color = "gray", fill = NA),
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 13),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  ) +
  labs(x = NULL, y = expression("Flux (kg N " * ha^{-1} * " " * hour^{-1} * ")")) +
  coord_cartesian(clip = "off")

p_manual <- ggplot(manual_data) +
  geom_line(aes(x = elapsed.time, y = NH3.value_avg, color = treatment), size = 1) +
  geom_ribbon(aes(x = elapsed.time, ymin = NH3.value_avg - sd_flux, ymax = NH3.value_avg + sd_flux, fill = treatment), alpha = 0.2, color = NA) +
  facet_wrap(~ group_label, labeller = label_parsed, nrow = 2, ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.25), expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    strip.text       = element_text(size = 14, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border     = element_rect(color = "gray", fill = NA),
    panel.grid       = element_blank(),
    legend.position  = "none",
    legend.text      = element_text(size = 12),
    legend.title     = element_text(size = 13),
    plot.margin      = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text        = element_text(size = 12),
    axis.title       = element_text(size = 13)
  ) +
  labs(
    x     = "Time after slurry application (hours)",
    y     = expression("Flux (kg N " * ha^{-1} * " " * hour^{-1} * ")"),
    fill  = "Treatment",
    color = "Treatment"
  ) +
  coord_cartesian(clip = "off")

# png("../plots/Figure2.png", width = 12, height = 10, units = "in", res = 300)
# grid.newpage()
#  <- grid.layout(nrow = 2, ncol = 2, heights = c(0.8, 2.2), widths = c(0.92, 0.08))
# pushViewport(viewport(layout = layout))
# pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
# grid.draw(ggplotGrob(p_machine))
# popViewport()
# pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
# grid.draw(ggplotGrob(p_manual))
# popViewport()
# pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
# grid.rect(x = 0.5, y = 0.474, width = 0.8, height = 0.85, just = "centre", gp = gpar(fill = "lightgray", col = "gray"))
# grid.text("Machine\nApplication", x = 0.5, y = 0.474, rot = 270, gp = gpar(fontsize = 16, fontface = "bold"))
# popViewport()
# pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
# grid.rect(x = 0.5, y = 0.51, width = 0.8, height = 0.83, just = "centre", gp = gpar(fill = "lightgray", col = "gray"))
# grid.text("Manual Application", x = 0.5, y = 0.45, rot = 270, gp = gpar(fontsize = 16, fontface = "bold"))
# popViewport()
# dev.off()

# Version 2 (with points)
p_machine <- ggplot(machine_data) +
  geom_ribbon(aes(x = elapsed.time, ymin = NH3.value_avg - sd_flux, ymax = NH3.value_avg + sd_flux, fill = treatment), alpha = 0.2, color = NA) +
  geom_line(aes(x = elapsed.time, y = NH3.value_avg, color = treatment), size = 1) +
  geom_point(aes(x = elapsed.time, y = NH3.value_avg, color = treatment), size = 1.5) +
  facet_wrap(~ group_label, labeller = label_parsed, nrow = 1, scales = "free_y") +
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(-0.5, 2), breaks = seq(-0.5, 2, by = 0.5), expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    strip.text       = element_text(size = 16, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border     = element_rect(color = "gray", fill = NA),
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text        = element_text(size = 16),
    axis.title       = element_text(size = 16),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  ) +
  labs(x = NULL, y = expression("Flux (kg N " * ha^{-1} * " " * hour^{-1} * ")")) +
  coord_cartesian(clip = "off")

p_manual <- ggplot(manual_data) +
  geom_ribbon(aes(x = elapsed.time, ymin = NH3.value_avg - sd_flux, ymax = NH3.value_avg + sd_flux, fill = treatment), alpha = 0.2, color = NA) +
  geom_line(aes(x = elapsed.time, y = NH3.value_avg, color = treatment), size = 1) +
  geom_point(aes(x = elapsed.time, y = NH3.value_avg, color = treatment), size = 1.5) +
  facet_wrap(~ group_label, labeller = label_parsed, nrow = 2, ncol = 2, scales = "free_y") +
  scale_x_continuous(limits = c(0, 119), breaks = seq(0, 120, by = 20), expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, by = 0.5), expand = expansion(mult = c(0, 0.1))) +
  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    strip.text       = element_text(size = 16, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "gray"),
    panel.border     = element_rect(color = "gray", fill = NA),
    panel.grid       = element_blank(),
    legend.position  = "none",
    plot.margin      = margin(5.5, 5, 5.5, 5.5, "pt"),
    axis.text        = element_text(size = 16),
    axis.title       = element_text(size = 16)
  ) +
  labs(
    x = "Time after slurry application (hours)",
    y = expression("Flux (kg N " * ha^{-1} * " " * hour^{-1} * ")")
  ) +
  coord_cartesian(clip = "off")

png("../plots/Figure2_pub.png", width = 14, height = 14, units = "in", res = 300)
grid.newpage()
layout <- grid.layout(nrow = 2, ncol = 2, heights = c(0.8, 2.2), widths = c(0.92, 0.08))
pushViewport(viewport(layout = layout))
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1))
grid.draw(ggplotGrob(p_machine))
popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 1))
grid.draw(ggplotGrob(p_manual))
popViewport()
pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 2))
grid.rect(x = 0.5, y = 0.474, width = 0.8, height = 0.85, just = "centre", gp = gpar(fill = "lightgray", col = "gray"))
grid.text("Machine\nApplication", x = 0.5, y = 0.474, rot = 270, gp = gpar(fontsize = 16, fontface = "bold"))
popViewport()
pushViewport(viewport(layout.pos.row = 2, layout.pos.col = 2))
grid.rect(x = 0.5, y = 0.51, width = 0.8, height = 0.83, just = "centre", gp = gpar(fill = "lightgray", col = "gray"))
grid.text("Manual Application", x = 0.5, y = 0.45, rot = 270, gp = gpar(fontsize = 16, fontface = "bold"))
popViewport()
dev.off()

