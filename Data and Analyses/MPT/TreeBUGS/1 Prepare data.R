library(dplyr)

# Prepare data for MPT

# Format has each sub on a separate line, separate column for each condition in same order as model

# WIT
# t01 = BT_tool
# t02 = BT_gun
# t03 = BG_tool
# t04 = BG_gun
# t05 = WT_tool
# t06 = WT_gun
# t07 = WG_tool
# t08 = WG_gun

# APT
# t09 = BP_pos
# t10 = BP_neg
# t11 = BN_pos
# t12 = BN_neg
# t13 = WP_pos
# t14 = WP_neg
# t15 = WN_pos
# t16 = WN_neg


# 1. Study 1 --------------------------------------------------------------

dat = read.delim("Study1_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
dat$TrialType = paste(dat$PrimeType, dat$TargetType, sep="_")


# create data frame with correct number of trials in each trial type/response condition
numTrialsWIT = NULL
numTrialsAPT = NULL
for (i in unique(dat$Subject)) {
  WITtemp = dat[dat$Subject == i & dat$blockName == "WIT",]
  for (typenum in 1:4) {
    correct = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 2,])
    incorrect = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 1,])
    miss = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 3,])
    numTrialsWIT = rbind(numTrialsWIT, data.frame(Subject = i,
                                                  TrialType = unique(WITtemp$TrialType)[typenum],
                                                  Correct = correct,
                                                  Incorrect = incorrect,
                                                  Miss = miss))
  }
  
  APTtemp = dat[dat$Subject == i & dat$blockName == "AP",]
  for (typenum in 1:4) {
    correct = nrow(APTtemp[APTtemp$TrialType == unique(APTtemp$TrialType)[typenum] & APTtemp$responseAccData == 2,])
    incorrect = nrow(APTtemp[APTtemp$TrialType == unique(APTtemp$TrialType)[typenum] & APTtemp$responseAccData == 1,])
    miss = nrow(APTtemp[APTtemp$TrialType == unique(APTtemp$TrialType)[typenum] & APTtemp$responseAccData == 3,])
    numTrialsAPT = rbind(numTrialsAPT, data.frame(Subject = i,
                                                  TrialType = unique(APTtemp$TrialType)[typenum],
                                                  Correct = correct,
                                                  Incorrect = incorrect,
                                                  Miss = miss))
  }
}

# rearrange to desired format (wide)
wideWIT = NULL
for (i in unique(numTrialsWIT$Subject)) {
  temp = numTrialsWIT[numTrialsWIT$Subject == i,]
  wideWIT = rbind(wideWIT, 
                  data.frame(Subject = i,
                             t01 = temp$Correct[temp$TrialType == "black_tool"],       # BT_toolResp
                             t02 = temp$Incorrect[temp$TrialType == "black_tool"],     # BT_gunResp
                             t03 = temp$Incorrect[temp$TrialType == "black_weapon"],   # BG_toolResp
                             t04 = temp$Correct[temp$TrialType == "black_weapon"],     # BG_gunResp
                             t05 = temp$Correct[temp$TrialType == "white_tool"],       # WT_toolResp
                             t06 = temp$Incorrect[temp$TrialType == "white_tool"],     # WT_gunResp
                             t07 = temp$Incorrect[temp$TrialType == "white_weapon"],   # WG_toolResp
                             t08 = temp$Correct[temp$TrialType == "white_weapon"]))    # WG_gunResp
}

# order so that sub numbers are in order
wideWIT = wideWIT[order(wideWIT$Subject),]
# missing sub 53

# rearrange to desired format (wide)
wideAPT = NULL
for (i in unique(numTrialsAPT$Subject)) {
  temp = numTrialsAPT[numTrialsAPT$Subject == i,]
  wideAPT = rbind(wideAPT, 
                  data.frame(Subject = i,
                             t09 = temp$Correct[temp$TrialType == "black_positive"],       # BP_posResp
                             t10 = temp$Incorrect[temp$TrialType == "black_positive"],     # BP_negResp
                             t11 = temp$Incorrect[temp$TrialType == "black_negative"],   # BN_posResp
                             t12 = temp$Correct[temp$TrialType == "black_negative"],     # BN_negResp
                             t13 = temp$Correct[temp$TrialType == "white_positive"],       # WP_posResp
                             t14 = temp$Incorrect[temp$TrialType == "white_positive"],     # WP_negResp
                             t15 = temp$Incorrect[temp$TrialType == "white_negative"],   # WN_posResp
                             t16 = temp$Correct[temp$TrialType == "white_negative"]))    # WN_negResp
}

# order so that sub numbers are in order
wideAPT = wideAPT[order(wideAPT$Subject),]
# missing sub 53

write.table(wideWIT, file = "./MPT/TreeBUGS/Study1_WIT.csv", sep = ",", row.names = F)
write.table(wideAPT, file = "./MPT/TreeBUGS/Study1_APT.csv", sep = ",", row.names = F)


# Put both together -------------------------------------------------------

both = merge(wideWIT, wideAPT, by="Subject")

write.table(both, file = "./MPT/TreeBUGS/Study1_bothTasks.csv", sep=",", row.names = F)

#take out subs 4, 28, 53, 64
select.dat = filter(both, !(Subject %in% c(4, 28, 53, 64)))
write.table(select.dat, file = "./MPT/TreeBUGS/Study1_bothTasks_selectSubs.csv", sep=",", row.names = F)


# 1. Study 2 --------------------------------------------------------------

dat = read.delim("Study2_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
dat$TrialType = paste(dat$PrimeType, dat$TargetType, sep="_")


# create data frame with correct number of trials in each trial type/response condition
numTrialsWIT = NULL
numTrialsAPT = NULL
for (i in unique(dat$Subject)) {
  WITtemp = dat[dat$Subject == i & (dat$blockName == "WIT_1"|dat$blockName == "WIT_2"),]
  for (typenum in 1:4) {
    correct = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 2,])
    incorrect = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 1,])
    miss = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 3,])
    numTrialsWIT = rbind(numTrialsWIT, data.frame(Subject = i,
                                                  TrialType = unique(WITtemp$TrialType)[typenum],
                                                  Correct = correct,
                                                  Incorrect = incorrect,
                                                  Miss = miss))
  }
  
  APTtemp = dat[dat$Subject == i & (dat$blockName == "APT_1"|dat$blockName == "APT_2"),]
  for (typenum in 1:4) {
    correct = nrow(APTtemp[APTtemp$TrialType == unique(APTtemp$TrialType)[typenum] & APTtemp$responseAccData == 2,])
    incorrect = nrow(APTtemp[APTtemp$TrialType == unique(APTtemp$TrialType)[typenum] & APTtemp$responseAccData == 1,])
    miss = nrow(APTtemp[APTtemp$TrialType == unique(APTtemp$TrialType)[typenum] & APTtemp$responseAccData == 3,])
    numTrialsAPT = rbind(numTrialsAPT, data.frame(Subject = i,
                                                  TrialType = unique(APTtemp$TrialType)[typenum],
                                                  Correct = correct,
                                                  Incorrect = incorrect,
                                                  Miss = miss))
  }
}

# rearrange to desired format (wide)
wideWIT = NULL
for (i in unique(numTrialsWIT$Subject)) {
  temp = numTrialsWIT[numTrialsWIT$Subject == i,]
  wideWIT = rbind(wideWIT, 
                  data.frame(Subject = i,
                             t01 = temp$Correct[temp$TrialType == "black_tool"],       # BT_toolResp
                             t02 = temp$Incorrect[temp$TrialType == "black_tool"],     # BT_gunResp
                             t03 = temp$Incorrect[temp$TrialType == "black_weapon"],   # BG_toolResp
                             t04 = temp$Correct[temp$TrialType == "black_weapon"],     # BG_gunResp
                             t05 = temp$Correct[temp$TrialType == "white_tool"],       # WT_toolResp
                             t06 = temp$Incorrect[temp$TrialType == "white_tool"],     # WT_gunResp
                             t07 = temp$Incorrect[temp$TrialType == "white_weapon"],   # WG_toolResp
                             t08 = temp$Correct[temp$TrialType == "white_weapon"]))    # WG_gunResp
}

# order so that sub numbers are in order
wideWIT = wideWIT[order(wideWIT$Subject),]
#missing sub 1

# rearrange to desired format (wide)
wideAPT = NULL
for (i in unique(numTrialsAPT$Subject)) {
  temp = numTrialsAPT[numTrialsAPT$Subject == i,]
  wideAPT = rbind(wideAPT, 
                  data.frame(Subject = i,
                             t09 = temp$Correct[temp$TrialType == "black_positive"],       # BP_posResp
                             t10 = temp$Incorrect[temp$TrialType == "black_positive"],     # BP_negResp
                             t11 = temp$Incorrect[temp$TrialType == "black_negative"],   # BN_posResp
                             t12 = temp$Correct[temp$TrialType == "black_negative"],     # BN_negResp
                             t13 = temp$Correct[temp$TrialType == "white_positive"],       # WP_posResp
                             t14 = temp$Incorrect[temp$TrialType == "white_positive"],     # WP_negResp
                             t15 = temp$Incorrect[temp$TrialType == "white_negative"],   # WN_posResp
                             t16 = temp$Correct[temp$TrialType == "white_negative"]))    # WN_negResp
}

# order so that sub numbers are in order
wideAPT = wideAPT[order(wideAPT$Subject),]
# missing sub 1

write.table(wideWIT, file = "./MPT/TreeBUGS/Study2_WIT.csv", sep = ",", row.names = F)
write.table(wideAPT, file = "./MPT/TreeBUGS/Study2_APT.csv", sep = ",", row.names = F)


# Put both together -------------------------------------------------------

both = merge(wideWIT, wideAPT, by="Subject")

write.table(both, file = "./MPT/TreeBUGS/Study2_bothTasks.csv", sep=",", row.names = F)


#take out subs 1, 9, 37, 87, 102
select.dat = filter(both, !(Subject %in% c(1, 9, 37, 87, 102)))
write.table(select.dat, file = "./MPT/TreeBUGS/Study2_bothTasks_selectSubs.csv", sep=",", row.names = F)

