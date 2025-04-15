########################################################################################
#----- Adjusting last 24 hours background for subtraction ------------ 
########################################################################################

#Background data#
DFC.bg <- dat %>%
  filter(group == "Background") %>%
  mutate(group = "Original") %>% # Rename "Background" to "Original"
  filter(elapsed.time >= 0 & elapsed.time <= 120) 

#Filtering for data in the last 24 hours#
Bg.filtered <- DFC.bg %>%
  filter(elapsed.time >= 96 & elapsed.time <= 120) %>%
  mutate(group = "endhours") #Adjusted concentration# 

#Convert to data.table# 
DFC.bg.end <- DFC.bg
setDT(DFC.bg.end)

#Specifying VOC columns#
voc_columns <- paste0("voc", 1:19)  

#Splitting background data by valve#
Split.bg <- split(DFC.bg.end, f = DFC.bg.end$valve)
Split.filter.bg <- split(Bg.filtered, f = Bg.filtered$valve)

#Ensuring all rows per group get repeating values#
for (i in seq_along(Split.filter.bg)) {
  valve_num <- names(Split.filter.bg)[i]
  filtered_data <- Split.filter.bg[[i]]
  
  if (nrow(filtered_data) < 48) {
    repeat_times <- ceiling(48 / nrow(filtered_data))
    repeated_data <- filtered_data[rep(1:nrow(filtered_data), repeat_times), ]
    repeated_data <- repeated_data[1:48, ]  
    Split.bg[[i]] <- repeated_data          
  }
}

#Merging all valve data back into one data frame#
DFC.bg.end <- do.call(rbind, Split.bg)

#Deleting the 'elapsed.time' column#
DFC.bg.end$elapsed.time <- NULL

#Copying elapsed.time#
DFC.bg.end$elapsed.time <- DFC.bg$elapsed.time

#Converting the VOC columns to numeric#
setDT(DFC.bg.end)
DFC.bg.end[, (voc_columns) := lapply(.SD, as.numeric), .SDcols = voc_columns]

#Mean for each elapsed time
DFC.bg.end.summ <- DFC.bg.end %>%
  group_by(elapsed.time) %>%
  summarise(across(all_of(voc_columns), ~mean(.x, na.rm = TRUE))) %>%
  ungroup()

# Rename the columns of the summarized data
names(DFC.bg.end.summ)[2:20] <- paste0("voc.bg", 1:19)

########################################################################################
#----- Setting DFC data for subtraction ------------ 
########################################################################################


# Subset the data for the DFC outlet 
DFC <- dat %>%
  filter(group %in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Machine plot'),
         elapsed.time >= 0, elapsed.time <= 120)

# Rename columns 7 to 25
names(DFC)[5:23] <- paste0("voc.dfc", 1:19)

#Convert to numeric
DFC <- DFC %>%
  mutate(across(starts_with("voc"), ~ as.numeric(as.character(.)))) 

#Joining average background and outlet data#
DFC <- full_join(DFC.bg.end.summ, DFC, by = 'elapsed.time')

#subtract background
for (i in 1:19) {
  #Define column names
  dfc_col <- paste0("voc.dfc", i)
  bg_col <- paste0("voc.bg", i)
  corr_col <- paste0("voc_corr", i)  #Corrected (ppb)
  
  #Check if columns exist in the data frame
  if (dfc_col %in% names(DFC) && bg_col %in% names(DFC)) {
    # Subtract background from outlet and store in a new column
    DFC[[corr_col]] <- DFC[[dfc_col]] - DFC[[bg_col]]
    
    #Set negative values to zero
    DFC[[corr_col]][DFC[[corr_col]] < 0] <- 0
  } else {
    warning(paste("Columns", dfc_col, "or", bg_col, "do not exist in the data frame"))
  }
}


#Rebind again in dat datasheet#
dat <- rbind(DFC)

#Checking and ordering
str(dat)

#Concentration in ppb
voc_ppb <- dat %>%
  filter(elapsed.time >= 0 & elapsed.time <= 120) #For OTV calculation
voc_ppb <- voc_ppb [, -c(2:20, 25:43)]



