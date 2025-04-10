# Libraries
library(data.table)
library(lubridate)
library(dplyr)
library(stringr)
library(zoo)
library(ggplot2)
library(readxl)
library(tidyr)
library(gridExtra)
library(cowplot)

# Save record of package versions
sink('../VOC/logs.txt')
print(sessionInfo())
sink()