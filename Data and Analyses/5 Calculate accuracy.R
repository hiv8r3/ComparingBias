# Prepares data for ANOVA analysis 

library(dplyr)
library(tidyr)


# Study 1 -----------------------------------------------------------------

dat = read.delim("Study1_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)

# Calculate errors for each task

# WIT
WITdat = filter(dat, blockName == "WIT")

WITacc = select(WITdat, Subject, PrimeType, TargetType, TargetWIT.ACC) %>% 
  group_by(Subject, PrimeType, TargetType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

WITacc$Task = "WIT"
WITacc = rename(WITacc, Accuracy = TargetWIT.ACC)

# Do the same for AP trials
APdat = filter(dat, blockName == "AP")

APacc = select(APdat, Subject, PrimeType, TargetType, TargetAP.ACC) %>% 
  group_by(Subject, PrimeType, TargetType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

APacc$Task = "APT"
APacc = rename(APacc, Accuracy = TargetAP.ACC)

# Combine, add descriptive columns/variables

long = rbind(WITacc, APacc)
long$TrialType = paste(long$PrimeType, long$TargetType, sep = "_")

con = c("black_weapon", "white_tool", "black_negative", "white_positive")
incon = c("black_tool", "white_weapon", "black_positive", "white_negative")

long$ConType[long$TrialType %in% con] = "congruent"
long$ConType[long$TrialType %in% incon] = "incongruent"


# Add observer condition
obs = read.delim("./Data/Study I/ConditionList.txt", stringsAsFactors=F)

long$Observer = NULL
for (i in unique(long$Subject)) {
  long$Observer[long$Subject == i] = obs$ObsCond[i]
}

write.table(long, file = "Study1_accuracy.txt", sep = "\t", row.names = F)


# Study 2 -----------------------------------------------------------------

dat = read.delim("Study2_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)

# Calculate errors for each task

# WIT
WITdat = filter(dat, blockName == "WIT_1"|blockName == "WIT_2")

WITacc = select(WITdat, Subject, PrimeType, TargetType, TargetWIT.ACC) %>% 
  group_by(Subject, PrimeType, TargetType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

WITacc$Task = "WIT"
WITacc = rename(WITacc, Accuracy = TargetWIT.ACC)

# Do the same for AP trials
APdat = filter(dat, blockName == "APT_1"|blockName == "APT_2")

APacc = select(APdat, Subject, PrimeType, TargetType, TargetAP.ACC) %>% 
  group_by(Subject, PrimeType, TargetType) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

APacc$Task = "APT"
APacc = rename(APacc, Accuracy = TargetAP.ACC)

# Combine, add descriptive columns/variables

long = rbind(WITacc, APacc)
long$TrialType = paste(long$PrimeType, long$TargetType, sep = "_")

con = c("black_weapon", "white_tool", "black_negative", "white_positive")
incon = c("black_tool", "white_weapon", "black_positive", "white_negative")

long$ConType[long$TrialType %in% con] = "congruent"
long$ConType[long$TrialType %in% incon] = "incongruent"


# Add observer condition

long$Observer = NULL
for (i in unique(long$Subject)) {
  long$Observer[long$Subject == i] = dat$Observer[dat$Subject == i][1]
}

write.table(long, file = "Study2_accuracy.txt", sep = "\t", row.names = F)



# # Get RT data for reporting in Table 1 ------------------------------------
# 
# # WIT
# # select only correct trials
# WITcorr = filter(WITdat, TargetWIT.ACC == 1)
# WITcorr$Subject = factor(WITcorr$Subject)
# 
# # means
# select(WITcorr, TargetWIT.RT, TrialType) %>% 
#   group_by(TrialType) %>% 
#   summarise_each(funs(mean)) %>% 
#   as.data.frame
# 
# # standard deviations 
# select(WITcorr, TargetWIT.RT, TrialType) %>% 
#   group_by(TrialType) %>% 
#   summarise_each(funs(sd)) %>% 
#   as.data.frame
# 
# # APT
# # select only correct trials
# APTcorr = filter(APdat, TargetAP.ACC == 1)
# APTcorr$Subject = factor(APTcorr$Subject)
# 
# # means
# select(APTcorr, TargetAP.RT, TrialType) %>% 
#   group_by(TrialType) %>% 
#   summarise_each(funs(mean)) %>% 
#   as.data.frame
# 
# # standard deviations 
# select(APTcorr, TargetAP.RT, TrialType) %>% 
#   group_by(TrialType) %>% 
#   summarise_each(funs(sd)) %>% 
#   as.data.frame
