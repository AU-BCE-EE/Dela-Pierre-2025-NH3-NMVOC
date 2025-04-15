########################################################################################
#----- Subtracting background from treatment ------------ 
########################################################################################

#Background#
DFC.bg <- dat[valve_data$group == 'Background', ]

#Outlet data#
DFC <- dat[valve_data$group%in% c('No acid', 'Low acid', 'Medium acid', 'High acid', 'Machine plot'), ]
names(DFC)[2] <- "NH3.DFC"

#Mean background values#
DFC.bg.summ <- aggregate(DFC.bg$NH3_30s, by = list(elapsed.time = DFC.bg$elapsed.time), FUN = mean)
names(DFC.bg.summ)[2] <- "NH3.bg"

#Joining average background and outlet data#
DFC <- full_join(DFC.bg.summ, DFC, by = 'elapsed.time')
DFC <- na.omit(DFC)

#Subtracting background from outlet#
DFC$NH3_corr <- DFC$NH3.DFC - DFC$NH3.bg
DFC[! complete.cases(DFC), ]

#Rebind again in dat datasheet#
dat <- rbind(DFC)
dat <- dat[order(dat$treatment), ]

#Making duplicate dat.time column#
dat <- dat %>%
  mutate(date.time.ori = date.time)
########################################################################################

