########################################################################################
#----- Setting bLSdata for plots ------------
########################################################################################

#Summarizing BLS data
bls_summary <- aggregate(
  Flux_mg_min ~ hours, 
  data = bls, 
  FUN = function(x) c(mean = mean(x, na.rm = TRUE))
)

# Convert list columns to separate columns
bls_summary <- do.call(data.frame, bls_summary)
colnames(bls_summary)[1:2] <- c("elapsed.time", "mean_flux")  # Rename columns
