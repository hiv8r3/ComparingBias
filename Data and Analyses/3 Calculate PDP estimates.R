require(dplyr)
require(tidyr)

# includes all subjects we have data for

# Study 1 -----------------------------------------------------------------

dat = read.delim("Study1_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)

dat$TrialType = paste(dat$PrimeType, dat$TargetType, sep="_")

# remove trials with no response
dat.nomiss = filter(dat, responseAccData != 3)

# calculate accuracy for each trial type for each participant


# Create WIT PDP estimates ------------------------------------------------

accRateWIT = select(filter(dat.nomiss, blockName == "WIT"), Subject, TrialType, TargetWIT.ACC) %>% 
  group_by(Subject, TrialType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

# create dataframe where PDP calculations are going to go for WIT
pdpWIT = NULL

# For both Black and White trials
# Cb = P(correct|gun trials) - P(incorrect|tool trials)
# Ab = P(incorrect|tool trials)/(1-Cb) --> measures association between prime and guns
# Ab = P(incorrect|gun trials)/(1-Cb) --> measures association between prime and tools (originally presented in paper)

# In each case, A estimate now measures activation of gun association with prime face

for (i in unique(accRateWIT$Subject)) {
  temp = accRateWIT[accRateWIT$Subject == i,]
  # Black trials
  C_black = temp$TargetWIT.ACC[temp$TrialType == "black_weapon"] - 
    (1-temp$TargetWIT.ACC[temp$TrialType == "black_tool"])
  A_gun_black = (1-temp$TargetWIT.ACC[temp$TrialType == "black_tool"])/(1-C_black) # measures association between black primes and guns
  # White trials
  C_white = temp$TargetWIT.ACC[temp$TrialType == "white_weapon"] - 
    (1-temp$TargetWIT.ACC[temp$TrialType == "white_tool"])
  A_gun_white = (1-temp$TargetWIT.ACC[temp$TrialType == "white_tool"])/(1-C_white) # measures association between White primes and guns
  A_tool_white = (1-temp$TargetWIT.ACC[temp$TrialType == "white_weapon"])/(1-C_white) # measures association between White primes and tools
  # Add to dataframe
  pdpWIT = rbind(pdpWIT,
                 data.frame(Subject = i,
                            Task = "WIT",
                            C_black = C_black,
                            A_black = A_gun_black,
                            C_white = C_white,
                            A_gun_white = A_gun_white,
                            A_tool_white = A_tool_white))
}

# # display Ps with negative control estimates
# PDPestimates = pdpWIT[pdpWIT$C_black < 0,] #8 subs have c estimates < 0
# 
# accuracy = accRateWIT[accRateWIT$Subject %in% PDPestimates$Subject,] %>% spread(TrialType, TargetWIT.ACC)
# accuracy = cbind(accuracy[1], round(accuracy[2:5], digits = 2))


# Create A resid scores for WIT -------------------------------------------

model1 = lm(A_black ~ A_tool_white, data = pdpWIT)
pdpWIT$AResid = model1$residuals # how it was done in original submission (Black-gun association, accounting for White-tool association)

# since A_gun_white and A_tool_white are inverses of each other, residual is equivalent whether White-gun or White-tool association is used

# Replace negative C estimates with 0 -------------------------------------

pdpWIT$C_black[pdpWIT$C_black < 0] = 0 #3 subs have C_mean estimates < 0
pdpWIT$C_white[pdpWIT$C_white < 0] = 0 #3 subs have C_mean estimates < 0


# Create C mean scores for WIT --------------------------------------------

pdpWIT = mutate(pdpWIT, C_mean = (C_black + C_white)/2)

pdpWIT[pdpWIT$C_mean < 0,] #3 subs have C_mean estimates < 0






# Create APT PDP estimates ------------------------------------------------

accRateAPT = select(filter(dat.nomiss, blockName == "AP"), Subject, TrialType, TargetAP.ACC) %>% 
  group_by(Subject, TrialType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

# create dataframe where PDP calculations are going to go for APT
pdpAPT = NULL

# For both Black and White trials
# Cb = P(correct|negative trials) - P(incorrect|positive trials)
# Ab = P(incorrect|positive trials)/(1-Cb) --> measures association between prime and negative
# Ab = P(incorrect|negative trials)/(1-Cb) --> measures association between prime and positive

for (i in unique(accRateAPT$Subject)) {
  temp = accRateAPT[accRateAPT$Subject == i,]
  # Black trials
  C_black = temp$TargetAP.ACC[temp$TrialType == "black_negative"] - 
    (1-temp$TargetAP.ACC[temp$TrialType == "black_positive"])
  A_neg_black = (1-temp$TargetAP.ACC[temp$TrialType == "black_positive"])/(1-C_black)
  A_pos_black = (1-temp$TargetAP.ACC[temp$TrialType == "black_negative"])/(1-C_black)
  # White trials
  C_white = temp$TargetAP.ACC[temp$TrialType == "white_negative"] - 
    (1-temp$TargetAP.ACC[temp$TrialType == "white_positive"])
  A_neg_white = (1-temp$TargetAP.ACC[temp$TrialType == "white_positive"])/(1-C_white)
  A_pos_white = (1-temp$TargetAP.ACC[temp$TrialType == "white_negative"])/(1-C_white)
  # Add to dataframe
  pdpAPT = rbind(pdpAPT,
                 data.frame(Subject = i,
                            Task = "APT",
                            C_black = C_black,
                            A_neg_black = A_neg_black,
                            A_pos_black = A_pos_black,
                            C_white = C_white,
                            A_neg_white = A_neg_white,
                            A_pos_white = A_pos_white))
}

pdpAPT[pdpAPT$C_black < 0,] #10 subs have c estimates < 0
pdpAPT[pdpAPT$C_white < 0,] #4 subs have c estimates < 0


# AP
accRateAPT = select(filter(dat, blockName == "AP"), Subject, TrialType, TargetAP.ACC) %>% 
  group_by(Subject, TrialType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()


# Create A resid scores for APT -------------------------------------------

model3 = lm(A_neg_black ~ A_pos_white, data = pdpAPT)
pdpAPT$AResid = model3$residuals # how it was done in original submission (Black-neg association, accounting for White-pos association)

# Residuals are equivalent, regardless of which White association is used.
# Residuals are inverted, depending on which Black association is used.

# Replace negative C estimates with 0 -------------------------------------

pdpAPT$C_black[pdpAPT$C_black < 0] = 0 #3 subs have C_mean estimates < 0
pdpAPT$C_white[pdpAPT$C_white < 0] = 0 #3 subs have C_mean estimates < 0

# Create C mean scores for APT --------------------------------------------

pdpAPT = mutate(pdpAPT, C_mean = (C_black + C_white)/2)

pdpAPT[pdpAPT$C_mean < 0,] #4 subs have C_mean estimates < 0

# Add IMS, EMS, Observer --------------------------------------------------

for (i in unique(pdpWIT$Subject)) {
  pdpWIT$IMS[pdpWIT$Subject == i] = dat$IMS[dat$Subject == i][1]
  pdpWIT$EMS[pdpWIT$Subject == i] = dat$EMS[dat$Subject == i][1]
  pdpWIT$Observer[pdpWIT$Subject == i] = dat$Observer[dat$Subject == i][1]
  
  pdpAPT$IMS[pdpAPT$Subject == i] = dat$IMS[dat$Subject == i][1]
  pdpAPT$EMS[pdpAPT$Subject == i] = dat$EMS[dat$Subject == i][1]
  pdpAPT$Observer[pdpAPT$Subject == i] = dat$Observer[dat$Subject == i][1]
}

write.table(pdpWIT, "Study1_pdpEstimates_WIT.txt", sep = "\t", row.names=F)
write.table(pdpAPT, "Study1_pdpEstimates_APT.txt", sep = "\t", row.names=F)


# Study 2 -----------------------------------------------------------------

dat = read.delim("Study2_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)

dat$TrialType = paste(dat$PrimeType, dat$TargetType, sep="_")

# remove trials with no response
dat.nomiss = filter(dat, responseAccData != 3)

# calculate accuracy for each trial type for each participant


# Create WIT PDP estimates ------------------------------------------------

accRateWIT = select(filter(dat.nomiss, blockName == "WIT_1"), Subject, TrialType, TargetWIT.ACC) %>% 
  rbind(select(filter(dat.nomiss, blockName == "WIT_2"), Subject, TrialType, TargetWIT.ACC)) %>% 
  group_by(Subject, TrialType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

# create dataframe where PDP calculations are going to go for WIT
pdpWIT = NULL

# For both Black and White trials
# Cb = P(correct|gun trials) - P(incorrect|tool trials)
# Ab = P(incorrect|tool trials)/(1-Cb) --> measures association between prime and guns
# Ab = P(incorrect|gun trials)/(1-Cb) --> measures association between prime and tools (originally presented in paper)

# In each case, A estimate now measures activation of gun association with prime face

for (i in unique(accRateWIT$Subject)) {
  temp = accRateWIT[accRateWIT$Subject == i,]
  # Black trials
  C_black = temp$TargetWIT.ACC[temp$TrialType == "black_weapon"] - 
    (1-temp$TargetWIT.ACC[temp$TrialType == "black_tool"])
  A_gun_black = (1-temp$TargetWIT.ACC[temp$TrialType == "black_tool"])/(1-C_black) # measures association between black primes and guns
  # White trials
  C_white = temp$TargetWIT.ACC[temp$TrialType == "white_weapon"] - 
    (1-temp$TargetWIT.ACC[temp$TrialType == "white_tool"])
  A_gun_white = (1-temp$TargetWIT.ACC[temp$TrialType == "white_tool"])/(1-C_white) # measures association between White primes and guns
  A_tool_white = (1-temp$TargetWIT.ACC[temp$TrialType == "white_weapon"])/(1-C_white) # measures association between White primes and tools
  # Add to dataframe
  pdpWIT = rbind(pdpWIT,
                 data.frame(Subject = i,
                            Task = "WIT",
                            C_black = C_black,
                            A_black = A_gun_black,
                            C_white = C_white,
                            A_gun_white = A_gun_white,
                            A_tool_white = A_tool_white))
}

# # display Ps with negative control estimates
# PDPestimates = pdpWIT[pdpWIT$C_black < 0,] #12 subs have c estimates < 0
# 
# accuracy = accRateWIT[accRateWIT$Subject %in% PDPestimates$Subject,] %>% spread(TrialType, TargetWIT.ACC)
# accuracy = cbind(accuracy[1], round(accuracy[2:5], digits = 2))


# Create A resid scores for WIT -------------------------------------------

model1 = lm(A_black ~ A_tool_white, data = pdpWIT)
pdpWIT$AResid = model1$residuals # how it was done in original submission (Black-gun association, accounting for White-tool association)

# since A_gun_white and A_tool_white are inverses of each other, residual is equivalent whether White-gun or White-tool association is used


# Replace negative C estimates with 0 -------------------------------------

pdpWIT$C_black[pdpWIT$C_black < 0] = 0 
pdpWIT$C_white[pdpWIT$C_white < 0] = 0 


# Create C mean scores for WIT --------------------------------------------

pdpWIT = mutate(pdpWIT, C_mean = (C_black + C_white)/2)

pdpWIT[pdpWIT$C_mean < 0,]



# Create APT PDP estimates ------------------------------------------------

accRateAPT = select(filter(dat.nomiss, blockName == "APT_1"), Subject, TrialType, TargetAP.ACC) %>% 
  rbind(select(filter(dat.nomiss, blockName == "APT_2"), Subject, TrialType, TargetAP.ACC)) %>% 
  group_by(Subject, TrialType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

# create dataframe where PDP calculations are going to go for APT
pdpAPT = NULL

# For both Black and White trials
# Cb = P(correct|negative trials) - P(incorrect|positive trials)
# Ab = P(incorrect|positive trials)/(1-Cb) --> measures association between prime and negative
# Ab = P(incorrect|negative trials)/(1-Cb) --> measures association between prime and positive

for (i in unique(accRateAPT$Subject)) {
  temp = accRateAPT[accRateAPT$Subject == i,]
  # Black trials
  C_black = temp$TargetAP.ACC[temp$TrialType == "black_negative"] - 
    (1-temp$TargetAP.ACC[temp$TrialType == "black_positive"])
  A_neg_black = (1-temp$TargetAP.ACC[temp$TrialType == "black_positive"])/(1-C_black)
  A_pos_black = (1-temp$TargetAP.ACC[temp$TrialType == "black_negative"])/(1-C_black)
  # White trials
  C_white = temp$TargetAP.ACC[temp$TrialType == "white_negative"] - 
    (1-temp$TargetAP.ACC[temp$TrialType == "white_positive"])
  A_neg_white = (1-temp$TargetAP.ACC[temp$TrialType == "white_positive"])/(1-C_white)
  A_pos_white = (1-temp$TargetAP.ACC[temp$TrialType == "white_negative"])/(1-C_white)
  # Add to dataframe
  pdpAPT = rbind(pdpAPT,
                 data.frame(Subject = i,
                            Task = "APT",
                            C_black = C_black,
                            A_neg_black = A_neg_black,
                            A_pos_black = A_pos_black,
                            C_white = C_white,
                            A_neg_white = A_neg_white,
                            A_pos_white = A_pos_white))
}
# Sub 59 has perfect accuracy on black trials, so C estimates = 1
# manually enter A estimates as 0
pdpAPT$A_neg_black[pdpAPT$Subject == 59] = 0
pdpAPT$A_pos_black[pdpAPT$Subject == 59] = 0

# pdpAPT[pdpAPT$C_black < 0,] #15 subs have c estimates < 0
# pdpAPT[pdpAPT$C_white < 0,] #15 subs have c estimates < 0
# 
# 
# # AP
# accRateAPT = select(filter(dat, blockName == "AP"), Subject, TrialType, TargetAP.ACC) %>% 
#   group_by(Subject, TrialType) %>% 
#   summarise_all(funs(mean)) %>% 
#   as.data.frame()


# Create A resid scores for APT -------------------------------------------

model3 = lm(A_neg_black ~ A_pos_white, data = pdpAPT)
pdpAPT$AResid = model3$residuals # how it was done in original submission (Black-neg association, accounting for White-pos association)

# Residuals are equivalent, regardless of which White association is used.
# Residuals are inverted, depending on which Black association is used.

# Replace negative C estimates with 0 -------------------------------------

pdpAPT$C_black[pdpAPT$C_black < 0] = 0 
pdpAPT$C_white[pdpAPT$C_white < 0] = 0 

# Create C mean scores for APT --------------------------------------------

pdpAPT = mutate(pdpAPT, C_mean = (C_black + C_white)/2)

pdpAPT[pdpAPT$C_mean < 0,]

# Add IMS, EMS, Observer --------------------------------------------------

for (i in unique(pdpWIT$Subject)) {
  pdpWIT$IMS[pdpWIT$Subject == i] = dat$IMS[dat$Subject == i][1]
  pdpWIT$EMS[pdpWIT$Subject == i] = dat$EMS[dat$Subject == i][1]
  pdpWIT$Observer[pdpWIT$Subject == i] = dat$Observer[dat$Subject == i][1]
  
  pdpAPT$IMS[pdpAPT$Subject == i] = dat$IMS[dat$Subject == i][1]
  pdpAPT$EMS[pdpAPT$Subject == i] = dat$EMS[dat$Subject == i][1]
  pdpAPT$Observer[pdpAPT$Subject == i] = dat$Observer[dat$Subject == i][1]
}

write.table(pdpWIT, "Study2_pdpEstimates_WIT.txt", sep = "\t", row.names=F)
write.table(pdpAPT, "Study2_pdpEstimates_APT.txt", sep = "\t", row.names=F)
