########################################################################################
#----- Calculating 30s average ------------
########################################################################################
#Converting time for valve blocks#
dat <- dat %>%
  mutate(
    time = as_hms(time),
    valve_prev = lag(valve, default = first(valve)),
    group_id = cumsum(valve != valve_prev)
  )

#Calculating end times for each block#
end_times <- dat %>%
  group_by(group_id) %>%
  summarise(
    end_time = max(time),
    .groups = "drop"
  )

#Joining end times and differentiating first cycle for sulfur compounds#
dat <- dat %>%
  left_join(end_times, by = "group_id") %>%  # Single join
  group_by(valve) %>%
  mutate(
    is_first_cycle = (group_id == first(group_id))
  ) %>%
  ungroup()

#Calculate last 30s averages for all compounds#
mean_all <- dat %>%
  group_by(group_id, valve) %>%
  filter(as.numeric(time) >= (as.numeric(end_time) - 30)) %>% 
  summarise(
    time = max(time),
    valve = first(valve),
    across(
      c("methanol", "H2S", "X4.Methylphenol", "acetic_acid", "butanoic_acid", "pentanoic_acid",
        "propanoic_acid", "acetladheyde", "formic_acid", "methanthiol", "acetone",
        "trimethylamine", "dimethyl_sulfide", "isopren", "butanone", "benzen", "butandion",
        "phenol", "X4_ethyl_phenol", "methyl_indole"),
      ~mean(.x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

#Calculate last 8 min averages for sulfur compounds#
mean_sulfur <- dat %>%
  group_by(group_id, valve) %>%
  filter(is_first_cycle) %>%
  summarise(
    H2S_full = mean(H2S, na.rm = TRUE),
    methanthiol_full = mean(methanthiol, na.rm = TRUE),
    dimethyl_sulfide_full = mean(dimethyl_sulfide, na.rm = TRUE),
    .groups = "drop"
  )

#Overwriting sulfur compound mean for 1st cycle#
combined_mean <- mean_all %>%
  left_join(mean_sulfur, by = c("group_id", "valve")) %>%
  mutate(
    H2S = ifelse(is.na(H2S_full), H2S, H2S_full),
    methanthiol = ifelse(is.na(methanthiol_full), methanthiol, methanthiol_full),
    dimethyl_sulfide = ifelse(is.na(dimethyl_sulfide_full), dimethyl_sulfide, dimethyl_sulfide_full)
  ) %>%
  select(-H2S_full, -methanthiol_full, -dimethyl_sulfide_full)

#Taking other columns for dat#
oth.col <- dat %>%
  group_by(group_id) %>%
  slice_max(time) %>%  # Get the last row per group
  select(group_id, st, date.time.y, date.time)  # Adjust columns as needed

#Joining all data#
dat <- left_join(combined_mean, oth.col, by = "group_id")

#Rearranging data#
dat <- dat %>%
  select(24:26, everything(), -1, -3)

#Ordering data according to valve#
split_valve <- split(dat, f = dat$valve)
valve <- paste0("V", unique(dat$valve))
new_da <- NULL

########################################################################################
#----- Calculating elapsed time ------------ 
########################################################################################

for (i in seq_along(split_valve)) {
  subset_data <- split_valve[[i]]
  subset_data$elapsed.time <- difftime(subset_data$date.time, min(subset_data$date.time), units = 'hours')
  new_da <- rbind(new_da, subset_data)
}
#Merging data#
dat<- new_da

#Rounding elapsed time to days#
dat$elapsed.time <- round(as.numeric(dat$elapsed.time))
dat$days <- dat$elapsed.time / 24

#Removing benzen column#
dat <- dat %>% select(-benzen, -days)
