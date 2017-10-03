require(dplyr)

# Creates performance bias scores (error|incongruent - errors|congruent)

# Study 1 -----------------------------------------------------------------


dat.cond = read.delim("Study1_errCountLong.txt")


# Look just at subjects that have data for both tasks, otherwise throws error ("Error() model is singular")
# Probably because some subjects don't have data across both levels of task
bsWIT = read.delim("Study1_badsubsWIT.txt")
bsAP = read.delim("Study1_badsubsAP.txt")
dat.cond.nobs = dat.cond[!(dat.cond$Subject %in% bsWIT$Subject) & !(dat.cond$Subject %in% bsAP$Subject),]


perfBias = data.frame("Subject" = unique(dat.cond.nobs$Subject))

# add errors from all congruent trials together, add errors form all incongruent trials together
for (i in unique(perfBias$Subject)) {
  perfBias$WITconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                        dat.cond.nobs$GenType == "black_con" &
                                                                        dat.cond.nobs$Task == "WIT"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_con" &
                           dat.cond.nobs$Task == "WIT"]
  perfBias$WITinconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                          dat.cond.nobs$GenType == "black_incon" &
                                                                          dat.cond.nobs$Task == "WIT"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_incon" &
                           dat.cond.nobs$Task == "WIT"]
  
  
  perfBias$APconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                       dat.cond.nobs$GenType == "black_con" &
                                                                       dat.cond.nobs$Task == "AP"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_con" &
                           dat.cond.nobs$Task == "AP"]
  perfBias$APinconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                         dat.cond.nobs$GenType == "black_incon" &
                                                                         dat.cond.nobs$Task == "AP"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_incon" &
                           dat.cond.nobs$Task == "AP"]
}

# create difference score for performance bias estimate
# larger perf bias estimate means more bias (more errors on incongruent trials than congruent trials)
perfBias = mutate(perfBias, WITperfBias = WITinconErrors/96 - WITconErrors/96) %>%
  mutate(APperfBias = APinconErrors/96 - APconErrors/96)

# create standardized scores
perfBias$WITStand = scale(perfBias$WITperfBias)
perfBias$APStand = scale(perfBias$APperfBias)

# readjust subject factor levels, change standardized scores to numeric
perfBias$Subject = factor(perfBias$Subject)
perfBias$WITStand = as.numeric(perfBias$WITStand)
perfBias$APStand = as.numeric(perfBias$APStand)

write.table(perfBias, "Study1_perfBias.txt", sep="\t", row.names=F)


# Study 2 -----------------------------------------------------------------


dat.cond = read.delim("Study2_errCountLong.txt")


# Look just at subjects that have data for both tasks, otherwise throws error ("Error() model is singular")
# Probably because some subjects don't have data across both levels of task
bsWIT = read.delim("Study2_badsubsWIT.txt")
bsAP = read.delim("Study2_badsubsAP.txt")
dat.cond.nobs = dat.cond[!(dat.cond$Subject %in% bsWIT$Subject) & !(dat.cond$Subject %in% bsAP$Subject),]


perfBias = data.frame("Subject" = unique(dat.cond.nobs$Subject))

# add errors from all congruent trials together, add errors form all incongruent trials together
for (i in unique(perfBias$Subject)) {
  perfBias$WITconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                        dat.cond.nobs$GenType == "black_con" &
                                                                        dat.cond.nobs$Task == "WIT"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_con" &
                           dat.cond.nobs$Task == "WIT"]
  perfBias$WITinconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                          dat.cond.nobs$GenType == "black_incon" &
                                                                          dat.cond.nobs$Task == "WIT"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_incon" &
                           dat.cond.nobs$Task == "WIT"]
  
  
  perfBias$APconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                       dat.cond.nobs$GenType == "black_con" &
                                                                       dat.cond.nobs$Task == "AP"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_con" &
                           dat.cond.nobs$Task == "AP"]
  perfBias$APinconErrors[perfBias$Subject == i] = dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                                                                         dat.cond.nobs$GenType == "black_incon" &
                                                                         dat.cond.nobs$Task == "AP"] + 
    dat.cond.nobs$numErr[dat.cond.nobs$Subject == i &
                           dat.cond.nobs$GenType == "white_incon" &
                           dat.cond.nobs$Task == "AP"]
}

# create difference score for performance bias estimate
# larger perf bias estimate means more bias (more errors on incongruent trials than congruent trials)
perfBias = mutate(perfBias, WITperfBias = WITinconErrors/96 - WITconErrors/96) %>%
  mutate(APperfBias = APinconErrors/96 - APconErrors/96)

# create standardized scores
perfBias$WITStand = scale(perfBias$WITperfBias)
perfBias$APStand = scale(perfBias$APperfBias)

# readjust subject factor levels, change standardized scores to numeric
perfBias$Subject = factor(perfBias$Subject)
perfBias$WITStand = as.numeric(perfBias$WITStand)
perfBias$APStand = as.numeric(perfBias$APStand)

write.table(perfBias, "Study2_perfBias.txt", sep="\t", row.names=F)