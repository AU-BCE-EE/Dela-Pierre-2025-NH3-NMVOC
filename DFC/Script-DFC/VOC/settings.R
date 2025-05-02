########################################################################################
#----- Assigning treatment names ------------ 
########################################################################################

dat <- dat %>%
  mutate(treatment = recode(valve,
                            `1` = 'Mp',
                            `2` = '0-bp',
                            `3` = '1.5',
                            `4` = '2.9',
                            `5` = 'bkg',
                            `6` = '0-bp',
                            `7` = '5.7',
                            `8` = 'Mp',
                            `9` = 'bkg',
                            `10` = '1.5',
                            `11` = '2.9',
                            `12` = 'Mp',
                            `13` = '5.7',
                            `14` = '0-bp',
                            `15` = '1.5',
                            `16` = 'bkg',
                            `17` = '2.9',
                            `18` = 'Mp',
                            `19` = '5.7'
  ),
  group = case_when(
    valve %in% c(2, 6, 14) ~ 'No acid',
    valve %in% c(3, 10, 15) ~ 'Low acid',
    valve %in% c(4, 11, 17) ~ 'Medium acid',
    valve %in% c(7, 13, 19) ~ 'High acid',
    valve %in% c(5, 9, 16) ~ 'Background',
    valve %in% c(1, 8, 12, 18) ~ 'Machine plot'
  )
  )

#Rename vocs
dat <- dat %>%
  rename(
    voc1 = methanol,
    voc2 = H2S,
    voc3 = X4.Methylphenol,
    voc4 = acetic_acid,
    voc5 = butanoic_acid,
    voc6 = pentanoic_acid,
    voc7 = propanoic_acid,
    voc8 = acetladheyde,
    voc9 = formic_acid,
    voc10 = methanthiol,
    voc11 = acetone,
    voc12 = trimethylamine,
    voc13 = dimethyl_sulfide,
    voc14 = isopren,
    voc15 = butanone,
    voc16 = butandion,
    voc17 = phenol,
    voc18 = X4_ethyl_phenol,
    voc19 = methyl_indole
  )

dat <- dat %>%
  group_by(group, valve, elapsed.time) %>%
  slice(1) %>%
  ungroup()
########################################################################################

########################################################################################
#----- Assigning treatment to sulfur compounds dataset ------------ 
########################################################################################

s.dat <- s.dat %>%
  mutate(treatment = recode(valve,
                            `1` = 'Mp',
                            `2` = '0-bp',
                            `3` = '1.5',
                            `4` = '2.9',
                            `5` = 'bkg',
                            `6` = '0-bp',
                            `7` = '5.7',
                            `8` = 'Mp',
                            `9` = 'bkg',
                            `10` = '1.5',
                            `11` = '2.9',
                            `12` = 'Mp',
                            `13` = '5.7',
                            `14` = '0-bp',
                            `15` = '1.5',
                            `16` = 'bkg',
                            `17` = '2.9',
                            `18` = 'Mp',
                            `19` = '5.7'
  ),
  group = case_when(
    valve %in% c(2, 6, 14) ~ 'No acid',
    valve %in% c(3, 10, 15) ~ 'Low acid',
    valve %in% c(4, 11, 17) ~ 'Medium acid',
    valve %in% c(7, 13, 19) ~ 'High acid',
    valve %in% c(5, 9, 16) ~ 'Background',
    valve %in% c(1, 8, 12, 18) ~ 'Machine plot'
  )
  )

#Rename vocs
s.dat <- s.dat %>%
  rename(
    voc1 = auc_H2S,
    voc2 = auc_methanthiol,
    voc3 = auc_dimethyl_sulfide
  )
##########################################################################

