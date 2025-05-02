########################################################################################
#----- Calculating 30s average ------------
########################################################################################

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

#Taking other columns for dat#
oth.col <- dat %>%
  group_by(group_id) %>%
  slice_max(time) %>%  # Get the last row per group
  select(group_id, st, date.time.y, date.time)  # Adjust columns as needed

########################################################################################
#----- Joining means 30s data ------------ 
########################################################################################
dat <- left_join(mean_all, oth.col, by = "group_id")

#Rearranging data#
dat <- dat %>%
  select(24:26, everything(), -1, -3)

########################################################################################
#----- Calculating elapsed time ------------
########################################################################################
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

