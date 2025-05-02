########################################################################################
#----- Calculating elapsed time ------------
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

#Ordering data according to valve#
split_valve <- split(dat, f = dat$valve)
valve <- paste0("V", unique(dat$valve))
new_da <- NULL

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

