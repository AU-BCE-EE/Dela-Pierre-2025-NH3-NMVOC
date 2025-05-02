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

#Filter sulfur compounds of first cycle#
s_comp <- dat %>%
  filter(is_first_cycle) %>%
  select(1:5, 7, 15, 18, 26:31)

#Split data into list by valve#
valve_list <- s_comp %>% group_by(valve) %>% group_split()

compounds <- c("H2S", "methanthiol", "dimethyl_sulfide")

#Calculating AUC#
peak_function <- function(df, compounds) {
  if (nrow(df) == 0 || !"valve" %in% names(df)) return(NULL)
  
  # Convert date.time to minutes since the start of the measurement period
  df$date.time <- as.numeric(difftime(df$date.time, min(df$date.time), units = "min"))
  
  # Prepare result container for AUC and peak calculations
  auc_results <- lapply(compounds, function(compound) {
    if (!compound %in% names(df)) return(NULL)
    
    # Convert the compound values to numeric (handling any non-numeric values)
    df[[compound]] <- as.numeric(as.character(df[[compound]]))
    
    # Calculate AUC for the compound
    auc <- trapz(df$date.time, df[[compound]])
    
    # Find the peak concentration and the time it occurs
    peak_conc <- max(df[[compound]], na.rm = TRUE)
    peak_time <- df$date.time[which.max(df[[compound]])]  # Find the time at peak concentration
    
    # Return the results for this compound
    return(data.frame(
      valve = unique(df$valve),
      compound = compound,
      auc = auc,
      peak = peak_conc
    ))
  })
  
  # Combine all results into one data frame
  result_df <- do.call(rbind, auc_results)
  return(result_df)
}


#Apply the function to all valve groups#
s.dat_long <- lapply(valve_list, function(df) peak_function(df, compounds)) %>%
  bind_rows()

#Long to wide format#
s.dat <- s.dat_long %>%
  pivot_wider(
    names_from = compound,
    values_from = c(auc, peak)
  )%>%
  mutate(elapsed.time = 0)

