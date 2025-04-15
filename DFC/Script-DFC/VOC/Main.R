rm(list = ls())

source('packages.R')
source('functions.R')
source('load.R')
source('merging.R')
source('clean.R')
source('settings.R')
source('background.R')
source('weather.R')
source('voc.R')
source('oav.R')
source('plots.R')
source('export.R')

# Save record of package versions
sink('/Users/AU775281/Documents/GitHub/Dela-Pierre-2025-NH3-NMVOC/DFC/logs/logs_VOC.txt')
print(sessionInfo())
sink()
