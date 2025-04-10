library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(plyr)
library(tidyr)
library(patchwork)

# Save record of package versions
sink('../NH3/logs.txt')
print(sessionInfo())
sink()