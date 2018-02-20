require(dplyr)
require(tidyr)


# Study 1 -----------------------------------------------------------------

dat = read.delim("Study1_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
# take out bad subs
badSubsWIT = read.delim("Study1_badSubsWIT.txt")
badSubsAP = read.delim("Study1_badSubsAP.txt")

dat = dat[!(dat$Subject %in% badSubsWIT$Subject & dat$blockName == "WIT"),]    
dat = dat[!(dat$Subject %in% badSubsAP$Subject & dat$blockName == "AP"),]


# Separate tasks and add identifiers  -------------------------------------

# WIT
WITdat = dat[dat$blockName == "WIT",]
# create column separating trials into black_tool, black_gun, etc.
WITdat$TrialType = paste(WITdat$PrimeType,WITdat$TargetType, sep = "_")
# separate into halves
WITdat$Half = 1
WITdat$Half[WITdat$SubTrial > 96] = 2

# APT
APTdat = dat[dat$blockName == "AP",]
# create column separating trials into black_tool, black_gun, etc.
APTdat$TrialType = paste(APTdat$PrimeType,APTdat$TargetType, sep = "_")
# separate into halves
APTdat$Half = 1
APTdat$Half[APTdat$SubTrial > 96] = 2


# Calculate error rates for each condition in each half -------------------

accDatWIT = NULL
accDatAPT = NULL
for (h in 1:2) {
  # WIT
  WITtemp = WITdat[WITdat$Half == h,]
  for (i in unique(WITtemp$Subject)) {
    subtemp = WITtemp[WITtemp$Subject == i,]
    for (c in c("black_weapon", "black_tool", "white_weapon", "white_tool")) {
      total = nrow(subtemp[subtemp$TrialType == c,])
      correct = nrow(subtemp[subtemp$TrialType == c & subtemp$TargetWIT.ACC == 1,])
      accDatWIT = rbind(accDatWIT, data.frame(Subject = i,
                                              Half = h,
                                              Condition = c,
                                              Acc = correct/total))
    }
  }
  # AP
  APTtemp = APTdat[APTdat$Half == h,]
  for (i in unique(APTtemp$Subject)) {
    subtemp = APTtemp[APTtemp$Subject == i,]
    for (c in c("black_positive", "black_negative", "white_positive", "white_negative")) {
      total = nrow(subtemp[subtemp$TrialType == c,])
      correct = nrow(subtemp[subtemp$TrialType == c & subtemp$TargetAP.ACC == 1,])
      accDatAPT = rbind(accDatAPT, data.frame(Subject = i,
                                              Half = h,
                                              Condition = c,
                                              Acc = correct/total))
    }
  }
}


# Make race-specific PDP estimates ----------------------------------------

# C = p(correct|congruent) - p(error|incongruent)
# A = p(error|incongruent)/(1-C)

# WIT- Black and White primes
pdpDat = NULL
for (i in unique(accDatWIT$Subject)) {
  for (h in 1:2) {
    temp = accDatWIT[accDatWIT$Subject == i & accDatWIT$Half == h,]
    BlackC = temp$Acc[temp$Condition == "black_weapon"] - (1-temp$Acc[temp$Condition == "black_tool"])
    WhiteC = temp$Acc[temp$Condition == "white_tool"] - (1-temp$Acc[temp$Condition == "white_weapon"])
    BlackA = (1-temp$Acc[temp$Condition == "black_tool"])/(1-BlackC)
    WhiteA = (1-temp$Acc[temp$Condition == "white_tool"])/(1-WhiteC)
    pdpDat = rbind(pdpDat, data.frame(Subject = i,
                                      Task = "WIT",
                                      Half = h,
                                      BlackC = BlackC,
                                      BlackA = BlackA,
                                      WhiteC = WhiteC,
                                      WhiteA = WhiteA))
  }
}

# APT- Black and White primes
for (i in unique(accDatAPT$Subject)) {
  for (h in 1:2) {
    temp = accDatAPT[accDatAPT$Subject == i & accDatAPT$Half == h,]
    BlackC = temp$Acc[temp$Condition == "black_negative"] - (1-temp$Acc[temp$Condition == "black_positive"])
    WhiteC = temp$Acc[temp$Condition == "white_positive"] - (1-temp$Acc[temp$Condition == "white_negative"])
    BlackA = (1-temp$Acc[temp$Condition == "black_positive"])/(1-BlackC)
    WhiteA = (1-temp$Acc[temp$Condition == "white_negative"])/(1-WhiteC)
    pdpDat = rbind(pdpDat, data.frame(Subject = i,
                                      Task = "APT",
                                      Half = h,
                                      BlackC = BlackC,
                                      BlackA = BlackA,
                                      WhiteC = WhiteC,
                                      WhiteA = WhiteA))
  }
}



# Make task-wide pdp estimates (MeanC, AResid) ----------------------------

# MeanC
pdpDat = mutate(pdpDat, MeanC = (BlackC + WhiteC)/2)

# AResid (separate models for each half and each task)
# WIT
ADat.WIT = data.frame(Subject = unique(accDatWIT$Subject))

model1 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 1 & Task == "WIT"))
ADat.WIT$Half1 = model1$residuals

model2 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 2 & Task == "WIT"))
ADat.WIT$Half2 = model2$residuals

# APT
ADat.APT = data.frame(Subject = unique(accDatAPT$Subject))

model3 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 1 & Task == "APT"))
ADat.APT$Half1 = model3$residuals

model4 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 2 & Task == "APT"))
ADat.APT$Half2 = model4$residuals



# Calculate split half reliabilities by correlating across half -----------

# PDP-C estimates in WIT: r = .69; 95%: [.56 - .78]
cor.test(pdpDat$BlackC[pdpDat$Half == 1 & pdpDat$Task == "WIT"], pdpDat$BlackC[pdpDat$Half == 2 & pdpDat$Task == "WIT"])

# PDP-C estimates in APT: r = .71; 95%: [.59 - .80]
cor.test(pdpDat$BlackC[pdpDat$Half == 1 & pdpDat$Task == "APT"], pdpDat$BlackC[pdpDat$Half == 2 & pdpDat$Task == "APT"])

# PDP-A estimates in WIT: r = .58; ; 95%: [.43 - .70]
cor.test(ADat.WIT$Half1, ADat.WIT$Half2)

# PDP-A estimates in APT: r = .54; 95%: [.38 - .67]
cor.test(ADat.APT$Half1, ADat.APT$Half2)






# Study 2 -----------------------------------------------------------------

dat = read.delim("Study2_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
# take out bad subs
badSubsWIT = read.delim("Study2_badSubsWIT.txt")
badSubsAP = read.delim("Study2_badSubsAP.txt")

dat$Task = "WIT"
dat$Task[dat$blockName == "APT_1"] = "APT"
dat$Task[dat$blockName == "APT_2"] = "APT"

dat = dat[!(dat$Subject %in% badSubsWIT$Subject & dat$Task == "WIT"),]    
dat = dat[!(dat$Subject %in% badSubsAP$Subject & dat$Task == "APT"),]

dat$TrialType = paste(dat$PrimeType, dat$TargetType, sep="_")

# Separate tasks and add identifiers  -------------------------------------

# WIT
WITdat = dat[dat$Task == "WIT",]
# separate into halves
WITdat$Half = 1
WITdat$Half[WITdat$blockName == "WIT_2"] = 2

# APT
APTdat = dat[dat$Task == "APT",]
# separate into halves
APTdat$Half = 1
APTdat$Half[APTdat$blockName == "APT_2"] = 2


# Calculate error rates for each condition in each half -------------------

accDatWIT = NULL
accDatAPT = NULL
for (h in 1:2) {
  # WIT
  WITtemp = WITdat[WITdat$Half == h,]
  for (i in unique(WITtemp$Subject)) {
    subtemp = WITtemp[WITtemp$Subject == i,]
    for (c in c("black_weapon", "black_tool", "white_weapon", "white_tool")) {
      total = nrow(subtemp[subtemp$TrialType == c,])
      correct = nrow(subtemp[subtemp$TrialType == c & subtemp$TargetWIT.ACC == 1,])
      accDatWIT = rbind(accDatWIT, data.frame(Subject = i,
                                              Half = h,
                                              Condition = c,
                                              Acc = correct/total))
    }
  }
  # AP
  APTtemp = APTdat[APTdat$Half == h,]
  for (i in unique(APTtemp$Subject)) {
    subtemp = APTtemp[APTtemp$Subject == i,]
    for (c in c("black_positive", "black_negative", "white_positive", "white_negative")) {
      total = nrow(subtemp[subtemp$TrialType == c,])
      correct = nrow(subtemp[subtemp$TrialType == c & subtemp$TargetAP.ACC == 1,])
      accDatAPT = rbind(accDatAPT, data.frame(Subject = i,
                                              Half = h,
                                              Condition = c,
                                              Acc = correct/total))
    }
  }
}


# Make race-specific PDP estimates ----------------------------------------

# C = p(correct|congruent) - p(error|incongruent)
# A = p(error|incongruent)/(1-C)

# WIT- Black and White primes
pdpDat = NULL
for (i in unique(accDatWIT$Subject)) {
  for (h in 1:2) {
    temp = accDatWIT[accDatWIT$Subject == i & accDatWIT$Half == h,]
    BlackC = temp$Acc[temp$Condition == "black_weapon"] - (1-temp$Acc[temp$Condition == "black_tool"])
    WhiteC = temp$Acc[temp$Condition == "white_tool"] - (1-temp$Acc[temp$Condition == "white_weapon"])
    BlackA = (1-temp$Acc[temp$Condition == "black_tool"])/(1-BlackC)
    WhiteA = (1-temp$Acc[temp$Condition == "white_tool"])/(1-WhiteC)
    pdpDat = rbind(pdpDat, data.frame(Subject = i,
                                      Task = "WIT",
                                      Half = h,
                                      BlackC = BlackC,
                                      BlackA = BlackA,
                                      WhiteC = WhiteC,
                                      WhiteA = WhiteA))
  }
}
# problem: Sub 40 got all answers correct for White primes in second half, can't compute WhiteA estimate
pdpDat = filter(pdpDat, Subject != 40)

# APT- Black and White primes
for (i in unique(accDatAPT$Subject)) {
  for (h in 1:2) {
    temp = accDatAPT[accDatAPT$Subject == i & accDatAPT$Half == h,]
    BlackC = temp$Acc[temp$Condition == "black_negative"] - (1-temp$Acc[temp$Condition == "black_positive"])
    WhiteC = temp$Acc[temp$Condition == "white_positive"] - (1-temp$Acc[temp$Condition == "white_negative"])
    BlackA = (1-temp$Acc[temp$Condition == "black_positive"])/(1-BlackC)
    WhiteA = (1-temp$Acc[temp$Condition == "white_negative"])/(1-WhiteC)
    pdpDat = rbind(pdpDat, data.frame(Subject = i,
                                      Task = "APT",
                                      Half = h,
                                      BlackC = BlackC,
                                      BlackA = BlackA,
                                      WhiteC = WhiteC,
                                      WhiteA = WhiteA))
  }
}



# Make task-wide pdp estimates (MeanC, AResid) ----------------------------

# MeanC
pdpDat = mutate(pdpDat, MeanC = (BlackC + WhiteC)/2)

# AResid (separate models for each half and each task)
# WIT
ADat.WIT = data.frame(Subject = unique(pdpDat$Subject[pdpDat$Task == "WIT"]))

model1 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 1 & Task == "WIT"))
ADat.WIT$Half1 = model1$residuals

model2 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 2 & Task == "WIT"))
ADat.WIT$Half2 = model2$residuals

# APT
ADat.APT = data.frame(Subject = unique(pdpDat$Subject[pdpDat$Task == "APT"]))

model3 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 1 & Task == "APT"))
ADat.APT$Half1 = model3$residuals

model4 = lm(BlackA ~ WhiteA, data = filter(pdpDat, Half == 2 & Task == "APT"))
ADat.APT$Half2 = model4$residuals



# Calculate split half reliabilities by correlating across half -----------

# PDP-C estimates in WIT: r = .73; 95%: [.65 - .79]
cor.test(pdpDat$BlackC[pdpDat$Half == 1 & pdpDat$Task == "WIT"], pdpDat$BlackC[pdpDat$Half == 2 & pdpDat$Task == "WIT"])

# PDP-C estimates in APT: r = .52; 95%: [.41 - .61]
cor.test(pdpDat$BlackC[pdpDat$Half == 1 & pdpDat$Task == "APT"], pdpDat$BlackC[pdpDat$Half == 2 & pdpDat$Task == "APT"])

# PDP-A estimates in WIT: r = .50; 95%: [.39 - .60]
cor.test(ADat.WIT$Half1, ADat.WIT$Half2)

# PDP-A estimates in APT: r = .50; 95%: [.39 - .60]
cor.test(ADat.APT$Half1, ADat.APT$Half2)


