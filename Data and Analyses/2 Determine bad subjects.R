# look at mean RT, num of errors for each subject
library(dplyr)

# Study I -----------------------------------------------------------------

dat = read.delim("Study1_experimentalTrials.txt", stringsAsFactors=F)

# 1. First look at WIT trials
WITdat = dat[dat$Procedure.Block. == "WITproc",]

# Look at errors on WIT trials
WITdat$TargetWIT.ACC = as.numeric(WITdat$TargetWIT.ACC)
# separate by subject
numCor.WIT = tapply(WITdat$TargetWIT.ACC, INDEX=WITdat$Subject, FUN=sum)
barplot(numCor.WIT)
hist(numCor.WIT, breaks = 20)

meanCor.WIT = mean(numCor.WIT)
sdCor.WIT = sd(numCor.WIT)

numCor.WIT < (meanCor.WIT - 2*sdCor.WIT)
# 3 subjects with more than 2 sds below the mean: 4, 28, 59
# Apparently low accuracy for 28 is because they mixed up buttons
# 19%, 29%, and 30% accuracy respectively 


# 2. Next look at AP trials
APdat = dat[dat$Procedure.Block. == "APproc",]
# Look at errors on AP trials
APdat$TargetAP.ACC = as.numeric(APdat$TargetAP.ACC)

numCor.AP = tapply(APdat$TargetAP.ACC, INDEX=APdat$Subject, FUN=sum)
barplot(numCor.AP)
hist(numCor.AP, breaks = 20)

meanCor.AP = mean(numCor.AP)
sdCor.AP = sd(numCor.AP)

numCor.AP < (meanCor.AP - 2*sdCor.AP)
sum(numCor.AP < (meanCor.AP - 2*sdCor.AP))
# 2 subjects with more than 2 sds below the mean: 34, 64
# Low accuracy for 64 is fell asleep
# 14%, 11% accuracy, respectively


# 3. Write badsubs files

badSubsWIT = data.frame( "Subject" = integer(), "Reason" = character(), stringsAsFactors=FALSE) %>%
  rbind(data.frame("Subject" = 7,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 9,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 15,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 36,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 39,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 4,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "Only used gun response")) %>%
  rbind(data.frame("Subject" = 28,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "Mixed up buttons")) %>%
  rbind(data.frame("Subject" = 59,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 53,
                   "Reason" = "No data",
                   "Reason2" = ""))

badSubsAP = data.frame( "Subject" = integer(), "Reason" = character(), stringsAsFactors=FALSE) %>%
  rbind(data.frame("Subject" = 7,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 9,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 15,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 36,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 39,
                   "Reason" = "Not a native english speaker",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 34,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "")) %>%
  rbind(data.frame("Subject" = 64,
                   "Reason" = ">2 sds below mean",
                   "Reason2" = "Fell asleep")) %>%
  rbind(data.frame("Subject" = 53,
                   "Reason" = "No data",
                   "Reason2" = "")) 

write.table(badSubsWIT, file = "Study1_badSubsWIT.txt", sep = "\t", row.names = F)
write.table(badSubsAP, file = "Study1_badSubsAP.txt", sep = "\t", row.names = F)


# Study II ----------------------------------------------------------------

dat = read.delim("Study2_experimentalTrials.txt", stringsAsFactors=F)

# 1. First look at WIT trials
WITdat = dat[dat$Procedure.Block. == "WITproc",]
WITdat$TargetWIT.ACC = as.numeric(WITdat$TargetWIT.ACC)

# separate by subject
numCor.WIT = tapply(WITdat$TargetWIT.ACC, INDEX=WITdat$Subject, FUN=sum)
# number of correct trials
numCor.WIT
# accuracy rate for each subject
numCor.WIT/192

# visualize accuracy rates for each subject
barplot(numCor.WIT/192)
hist(numCor.WIT/192, breaks = 20)

# mean and sd across subjects
meanCor.WIT = mean(numCor.WIT)
meanCor.WIT/192

sdCor.WIT = sd(numCor.WIT)

# threshold for elimination: 34.4%
threshold = meanCor.WIT - 2*sdCor.WIT
threshold/192

sort(numCor.WIT/192)

# 5 subjects with more than 2 sds below the mean: 31, 51, 158, 7, 19
# 10.9%, 14%, 22.3%, 29.7%, and 33.3% accuracy respectively 


# 2. Next look at AP trials
APdat = dat[dat$Procedure.Block. == "APproc",]
APdat$TargetAP.ACC = as.numeric(APdat$TargetAP.ACC)

# separate by subject
numCor.AP = tapply(APdat$TargetAP.ACC, INDEX=APdat$Subject, FUN=sum)

# visualize accuracy rates
barplot(numCor.AP/192)
hist(numCor.AP/192, breaks = 20)

# mean and sd across subjects
meanCor.AP = mean(numCor.AP)
sdCor.AP = sd(numCor.AP)

# threshold accuracy is 33.2%
threshold = meanCor.AP - 2*sdCor.AP
threshold/192

sort(numCor.AP/192)
# 3 subjects with more than 2 sds below the mean: 181, 88, 203
# 7.2%, 21.9% and 27.1% accuracy, respectively

# 3. Next look at how often one response button is used

# WIT
buttons = data.frame(Subject = NULL, respA = NULL, respB = NULL)
for (i in unique(WITdat$Subject)) {
  temp = WITdat[WITdat$Subject == i & !(WITdat$responseAccData == 3),]
  respA = sum(temp$TargetWIT.RESP == 6)
  respB = sum(temp$TargetWIT.RESP == 5)
  buttons = rbind(buttons, data.frame(Subject = i, respA = respA, respB = respB))
}

buttons$hits = buttons$respA + buttons$respB

buttons$Subject[buttons$respA/buttons$hits > .85]
buttons$Subject[buttons$respB/buttons$hits > .85]

# AP
buttons = data.frame(Subject = NULL, respA = NULL, respB = NULL)
for (i in unique(APdat$Subject)) {
  temp = APdat[APdat$Subject == i & !(APdat$responseAccData == 3),]
  respA = sum(temp$TargetAP.RESP == 6)
  respB = sum(temp$TargetAP.RESP == 5)
  buttons = rbind(buttons, data.frame(Subject = i, respA = respA, respB = respB))
}

buttons$hits = buttons$respA + buttons$respB

buttons$Subject[buttons$respA/buttons$hits > .85]
buttons$Subject[buttons$respB/buttons$hits > .85]


# 4. Write badsubs files

badSubsWIT = data.frame( "Subject" = integer(), "Reason" = character(), stringsAsFactors=FALSE) %>%
  rbind(data.frame("Subject" = 7,
                   "Reason" = "< 2 SDs below mean")) %>%
  rbind(data.frame("Subject" = 19,
                   "Reason" = "< 2 SDs below mean")) %>%
  rbind(data.frame("Subject" = 31,
                   "Reason" = "< 2 SDs below mean")) %>%
  rbind(data.frame("Subject" = 51,
                   "Reason" = "< 2 SDs below mean")) %>%
  rbind(data.frame("Subject" = 158,
                   "Reason" = "< 2 SDs below mean")) %>% 
  rbind(data.frame("Subject" = 9,
                   "Reason" = "Used one response button more than 85% of the time")) %>% 
  rbind(data.frame("Subject" = 1,
                   "Reason" = "Computer malfunction"))

badSubsAP = data.frame( "Subject" = integer(), "Reason" = character(), stringsAsFactors=FALSE) %>%
  rbind(data.frame("Subject" = 88,
                   "Reason" = "< 2 SDs below mean")) %>%  
  rbind(data.frame("Subject" = 203,
                   "Reason" = "< 2 SDs below mean")) %>%  
  rbind(data.frame("Subject" = 181,
                   "Reason" = "< 2 SDs below mean")) %>% 
  rbind(data.frame("Subject" = 87,
                   "Reason" = "No data, computer malfunction")) %>% 
  rbind(data.frame("Subject" = 9,
                   "Reason" = "Used one response button more than 85% of the time")) %>% 
  rbind(data.frame("Subject" = 37,
                   "Reason" = "Used one response button more than 85% of the time")) %>% 
  rbind(data.frame("Subject" = 102,
                   "Reason" = "Used one response button more than 85% of the time")) %>% 
  rbind(data.frame("Subject" = 1,
                   "Reason" = "Computer malfunction"))





write.table(badSubsWIT, file = "Study2_badSubsWIT.txt", sep = "\t", row.names = F)
write.table(badSubsAP, file = "Study2_badSubsAP.txt", sep = "\t", row.names = F)



# Determine gender and race breakdown of sample ---------------
temp = dat[dat$SubTrial == 1 & dat$blockName == "WIT_1",]

unique(temp$Subject[temp$DemoGender.RESP == 1]) # includes NA
length(unique(temp$Subject[temp$DemoGender.RESP == 1])) # 109 women

unique(temp$Subject[temp$DemoGender.RESP == 2]) # includes NA
length(unique(temp$Subject[temp$DemoGender.RESP == 2])) # 94 men

unique(temp$Subject[temp$DemoGender.RESP == 3]) # includes NA
length(unique(temp$Subject[temp$DemoGender.RESP == 3])) # 1 other


unique(temp$Subject[temp$DemoRace.RESP == 1]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 1])) # 0 American Indian

unique(temp$Subject[temp$DemoRace.RESP == 2]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 2])) # 11 Asian

unique(temp$Subject[temp$DemoRace.RESP == 3]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 3])) # 0 Native Hawaiian

unique(temp$Subject[temp$DemoRace.RESP == 4]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 4])) # 15 Black

unique(temp$Subject[temp$DemoRace.RESP == 5]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 5])) # 166 White

unique(temp$Subject[temp$DemoRace.RESP == 6]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 6])) # 10 multiracial

unique(temp$Subject[temp$DemoRace.RESP == 7]) # includes NA
length(unique(temp$Subject[temp$DemoRace.RESP == 7])) # 2 unknown/choose not to answer


unique(temp$Subject[temp$DemoEthnicity.RESP == 1]) # includes NA
length(unique(temp$Subject[temp$DemoEthnicity.RESP == 1])) # 9 hispanic

