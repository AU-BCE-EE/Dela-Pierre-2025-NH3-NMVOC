#Plots

#Saving plot 1#
ggsave("/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Figures/OAV_plot.png", 
       plot = OAVplot, 
       width = 12, 
       height = 10, 
       dpi = 300, 
       bg = "white")

#Saving plot 2#
ggsave("/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Figures/combined_plot.png", 
       plot = combined_plot, 
       width = 12, 
       height = 10, 
       dpi = 300, 
       bg = "white")


#Data file
write.csv(voc_ppb, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/VOC/VOC_ppb.csv', row.names = F)
write.csv(dat, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/VOC/VOC_flux.csv', row.names = F)
write.csv(cum.voc, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/VOC/VOC_Cumulative.csv', row.names = F)
write.csv(OAV, '/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/Data/output data/VOC/OAV_and_SOAV.csv', row.names = F)

