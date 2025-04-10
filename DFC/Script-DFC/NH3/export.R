#Plots

#Saving plot 1#
ggsave("/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Figures/NH3 flux.png", 
       plot = Fluxes, 
       width = 12, 
       height = 10, 
       dpi = 300, 
       bg = "white")

#Saving plot 2#
ggsave("/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Figures/Tan loss .png", 
       plot = combined_plot, 
       width = 12, 
       height = 10, 
       dpi = 300, 
       bg = "white")


#Data file
write.csv(weather, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/NH3/weather data.csv', row.names = F)
write.csv(DFC.bg, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/NH3/background data.csv', row.names = F)
write.csv(DFC, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/NH3/DFC data.csv', row.names = F)
write.csv(dat, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/NH3/NH3 flux.csv', row.names = F)
write.csv(dat_tan, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/NH3/TAN.csv', row.names = F)
