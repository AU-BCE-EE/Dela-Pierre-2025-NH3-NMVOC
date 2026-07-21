source("../Functions/mintegrate.R")

dat <- read_excel("../input-DFC/NH3_ALFAM2.xlsx")

dat$start.date <- as.POSIXct(dat$start.date, format="%d-%m-%Y %H:%M", tz="UTC")
dat$end.date   <- as.POSIXct(dat$end.date,   format="%d-%m-%Y %H:%M", tz="UTC")
dat <- dat %>%
  group_by(plot.number) %>%
  mutate(
    elapsed.time = round(as.numeric(difftime(start.date, min(start.date), units = "hours")))
  ) %>%
  ungroup()

# Calculate cumulative emissions using mintegrate
dat_unique <- dat %>%
  group_by(plot.number, elapsed.time, treatment)

dat_unique$cum.emis <- mintegrate(
  x  = dat_unique$elapsed.time,
  y  = dat_unique$NH3.value,
  by = dat_unique$plot.number,
  method = 'trap'
)
dat_unique$shift.length <- difftime(dat_unique$end.date, dat_unique$start.date)
dat_unique$flux.time    <- as.numeric(dat$NH3.value * dat_unique$shift.length)
dat_unique <- dat_unique %>%
  arrange(plot.number, elapsed.time) %>%
  group_by(plot.number) %>%
  mutate(cum.cumsum = cumsum(flux.time)) %>%
  ungroup()

dat_unique <- dat_unique %>%
  filter(elapsed.time <= 119)

cum_emis_at_119 <- dat_unique %>%
  filter(elapsed.time == 119)
cum_emis_at_119$cum.emis.g.m2 <- cum_emis_at_119$cum.cumsum * 10^3 * 10^-4

tan_data <- data.frame(
  treatment = c("High acid DFC", "Low acid DFC", "Machine plot DFC", "Medium acid DFC", "No acid DFC"),
  TAN = c(8.63, 8.13, 8.75, 8.84, 8.28)
)
cum_emis_at_119 <- left_join(cum_emis_at_119, tan_data, by = "treatment")
cum_emis_at_119$cum.emis.TAN <- cum_emis_at_119$cum.emis.g.m2 / cum_emis_at_119$TAN * 100

avg_cum_emis <- cum_emis_at_119 %>%
  group_by(treatment) %>%
  summarise(mean_cum_emis = mean(cum.emis.g.m2, na.rm = TRUE))

write.csv(cum_emis_at_119, "../output-DFC/cum.NH3.csv")
write.csv(dat_unique,      "../output-DFC/dfc.NH3.csv")

# --- bLS NH3 data ---
bLS_NH3 <- read_xlsx("../input-bLS/bLS_NH3.xlsx")


bLS_NH3$elapsed.time <- seq(0, by = 0.5, length.out = nrow(bLS_NH3))
NH3_combined <- bind_rows(dat_unique, bLS_NH3)

NH3_combined <- NH3_combined %>%
  mutate(application = case_when(
    treatment == "Machine plot DFC" ~ "Machine Application",
    treatment == "0-bLS"            ~ "Machine Application",
    TRUE                            ~ "Manual Application"
  ))

bls_data <- NH3_combined %>%
  filter(treatment == "0-bLS")

summary_non_bls <- NH3_combined %>%
  filter(treatment != "0-bLS") %>%
  group_by(treatment, elapsed.time, application) %>%
  summarise(
    NH3.value_avg = mean(NH3.value, na.rm = TRUE),
    sd_flux       = sd(NH3.value,   na.rm = TRUE),
    .groups = "drop"
  )

summary_combined <- bind_rows(
  summary_non_bls,
  bls_data %>%
    dplyr::mutate(
      NH3.value_avg = NH3.value_avg * 14/17,
      sd_flux       = 0
    ) %>%
    select(treatment, elapsed.time, application, NH3.value_avg, sd_flux)
)

machine_data <- summary_combined %>% filter(application == "Machine Application")
manual_data  <- summary_combined %>% filter(application == "Manual Application")



write.csv(machine_data, "../output-DFC/NH3_machine_data.csv", row.names = FALSE)
write.csv(manual_data,  "../output-DFC/NH3_manual_data.csv",  row.names = FALSE)

