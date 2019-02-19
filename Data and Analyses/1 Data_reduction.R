# Reduces data file to important columns
# Isolates experimental trials, writes to .txt file
# Adds IMS/EMS data; calculates alphas for both scales

require(dplyr)
require(tidyr)


# Study I -----------------------------------------------------------------


# read in data
dat = read.delim("./Data/Study I/ForRMerge101.txt")

dat.select = select(dat, 
                    Subject, 
                    Session,
                    DemoAge.RESP,
                    DemoEthnicity.RESP,
                    DemoGender.RESP,
                    DemoRace.RESP,
                    responseMappingTool,
                    responseMappingVal,
                    Block, 
                    negResponse, 
                    posResponse,
                    Procedure.Block.,
                    toolResponse, 
                    weaponResponse,
                    Trial,
                    blockName,
                    Practice.Trial.,
                    SubTrial,
                    CorrectResponse,
                    facePictureFile,
                    ITIDuration,
                    PrimeType,
                    responseAccData,
                    rtFeedbackText,
                    TargetAP.ACC,
                    TargetAP.CRESP,
                    TargetAP.RESP,
                    TargetAP.RT,
                    targetPictureFile,
                    TargetType,
                    TargetWIT.ACC,
                    TargetWIT.CRESP,
                    TargetWIT.RESP,
                    TargetWIT.RT,
                    targetWord,
                    errorQSlide.RESP)

# select experimental trials (minus practice and questions)
expTrials = dat.select[dat.select$blockName == "WIT"|dat.select$blockName == "AP",]

# check to make sure all subjects have the right number of trials
frame = data.frame(Subject = NULL, nTrials = NULL)
for (i in unique(expTrials$Subject)) {
  a = nrow(expTrials[expTrials$Subject == i,])
  temp = data.frame(Subject = i, nTrials = a)
  frame = rbind(frame, temp)
}

# adds IMS/EMS scores
MTCP = read.delim("./Data/Study I/MTCPscores.txt")
# includes 5 subjects added after semester was over: 37, 45, 70, 73, 85

# reverse score IMS_1
MTCP$IMS_1.rev = 10 - MTCP$IMS_1

# create composite scores for IMS and EMS
MTCP = mutate(MTCP, IMS = (MTCP$IMS_1.rev + MTCP$IMS_2 + MTCP$IMS_3 + MTCP$IMS_4 + MTCP$IMS_5)/5)
MTCP = mutate(MTCP, EMS = (MTCP$EMS_1 + MTCP$EMS_2 + MTCP$EMS_3 + MTCP$EMS_4 + MTCP$EMS_5)/5)
# manually add IMS/EMS scores for subject 15 and 59 because of missing data on one item
MTCP$EMS[MTCP$Subject == 15] = 1.8
MTCP$IMS[MTCP$Subject == 59] = 2.3

# add IMS/EMS scores to trial data
for (i in unique(expTrials$Subject)) {
  expTrials$IMS[expTrials$Subject == i] = MTCP$IMS[MTCP$Subject == i]
  expTrials$EMS[expTrials$Subject == i] = MTCP$EMS[MTCP$Subject == i]
}

# add observer status
obs = read.delim("./Data/Study I/ConditionList.txt", stringsAsFactors=F)

expTrials$Observer = NULL
for (i in unique(expTrials$Subject)) {
  expTrials$Observer[expTrials$Subject == i] = obs$ObsCond[i]
}

# Add column for congruence

expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "weapon"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "tool"] = "incongruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "weapon"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "tool"] = "incongruent"

expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "negative"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "positive"] = "incongruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "negative"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "positive"] = "incongruent"


write.table(expTrials, file = "Study1_experimentalTrials.txt", sep = "\t", row.names = F)

# missing sub 53

# Separate task questions (Study 1) -------------------------------------------------

# Select question data for each task 

# Each item is under "SubTrial
# 1.	How frustrated were you by your errors?
# 2.	How anxious did your errors make you?
# 3.	How unpleasant were your errors?
# 4.	In general, how attentive were you during the task?
# 5.	In general, how hard did you try during the task?

questDat = dat.select[dat.select$blockName == "PostWITquestions"|dat.select$blockName == "PostAPquestions",] %>%
  select(c(Subject, SubTrial, errorQSlide.RESP, blockName))

# make wide
wide.questDat = spread(questDat, SubTrial, errorQSlide.RESP)

# rename variables
# rename variables
names(wide.questDat) = c("Subject", "blockName", 
                     "Frust", 
                     "Anx", 
                     "Unpleas", 
                     "Attent", 
                     "Effort")

# create composite
# since questions are on different scales, standardize first
wide.questDat$Composite = (scale(wide.questDat$Frust) + 
                             scale(wide.questDat$Anx) + 
                             scale(wide.questDat$Unpleas))/3

write.table(wide.questDat, file = "Study1_questDat.txt", sep = "\t", row.names = F)


# Cronbach alphas for anx composite ----------------------------

APTcomp = wide.questDat[wide.questDat$blockName == "PostAPquestions",] %>% 
  select(Frust, Anx, Unpleas)

APTcomp = wide.questDat[wide.questDat$blockName == "PostWITquestions",] %>% 
  select(Frust, Anx, Unpleas)

require(psych)
alpha(APTcomp)
alpha(WITcomp)

write.table(questDat, file = "Study1_questDat.txt", sep = "\t", row.names = F)

# Study II ----------------------------------------------------------------

# read in data (in two batches)
dat = read.delim("./Data/Study II/Merge_11-22.txt", skip = 1) %>% 
  select(-InstructionSet)

dat1 = read.delim("./Data/Study II/Merge_12-7_lastbatch.txt", skip = 1) %>% 
  select(-InstructionSet)

# read in data for individual subjects with data collection issues
dat2 = read.delim("./Data/Study II/72.txt")
dat3 = read.delim("./Data/Study II/79-1_corrected.txt", skip = 1) 
dat4 = read.delim("./Data/Study II/79-2_corrected.txt", skip = 1)
dat5 = read.delim("./Data/Study II/87_corrected.txt", skip = 1)
dat6 = read.delim("./Data/Study II/180.txt", skip = 2) %>% 
  select(-InstructionSet)
dat7 = read.delim("./Data/Study II/186.txt") %>% 
  select(-InstructionSet)
dat8 = read.delim("./Data/Study II/193.txt", skip = 2)

# put data sets together
dat.all = rbind(dat, dat1) %>% 
  rbind(dat2) %>% 
  rbind(dat3) %>% 
  rbind(dat4) %>% 
  rbind(dat5) %>% 
  rbind(dat6) %>% 
  rbind(dat7) %>% 
  rbind(dat8)

# reorder by subject number; still missing 1 so 205 subjects total
dat.all = dat.all[order(dat.all$Subject),]

dat.select = select(dat.all, 
                    Subject, 
                    Session,
                    DemoAge.RESP,
                    DemoEthnicity.RESP,
                    DemoGender.RESP,
                    DemoRace.RESP,
                    responseMappingTool,
                    responseMappingVal,
                    Block, 
                    negResponse, 
                    posResponse,
                    Procedure.Block.,
                    toolResponse, 
                    weaponResponse,
                    Trial,
                    blockName,
                    Practice.Trial.,
                    SubTrial,
                    CorrectResponse,
                    facePictureFile,
                    ITIDuration,
                    PrimeType,
                    responseAccData,
                    rtFeedbackText,
                    TargetAP.ACC,
                    TargetAP.CRESP,
                    TargetAP.RESP,
                    TargetAP.RT,
                    targetPictureFile,
                    TargetType,
                    TargetWIT.ACC,
                    TargetWIT.CRESP,
                    TargetWIT.RESP,
                    TargetWIT.RT,
                    targetWord,
                    errorQSlide.RESP,
                    anxQSlide.RESP)


# Separate experimental trials 

expBlocks = c("APT_1", "APT_2", "WIT_1", "WIT_2")
# select experimental trials (minus practice and questions)
expTrials = dat.select[dat.select$blockName %in% expBlocks,]

# check to make sure all subjects have the right number of trials (except sub 87)
frame = data.frame(Subject = NULL, nTrials = NULL)
for (i in unique(expTrials$Subject)) {
  a = nrow(expTrials[expTrials$Subject == i,])
  temp = data.frame(Subject = i, nTrials = a)
  frame = rbind(frame, temp)
}

# 205 subjects
length(unique(expTrials$Subject))


# add IMS/EMS data
# read in Qualtrics data 

qualDat = read.delim("./Data/Study II/QualtricsData.txt", stringsAsFactors = F)
qualDat = qualDat[order(qualDat$Subject),]

# Calc IMS/EMS scores 
# reverse score IMS_1
qualDat$IMS_1.rev = 10 - qualDat$IMS_1

# create composite scores for IMS and EMS
qualDat = mutate(qualDat, IMS = (qualDat$IMS_1.rev + qualDat$IMS_2 + qualDat$IMS_3 + qualDat$IMS_4 + qualDat$IMS_5)/5)
qualDat = mutate(qualDat, EMS = (qualDat$EMS_1 + qualDat$EMS_2 + qualDat$EMS_3 + qualDat$EMS_4 + qualDat$EMS_5)/5)


# Calc social anxiety scores 

# reverse score SA_3
qualDat$SA_3.rev = 7 - qualDat$SA_3

# create composite scores for SA
qualDat = mutate(qualDat, SA = (qualDat$SA_1 + qualDat$SA_2 + qualDat$SA_3.rev + qualDat$SA_4 + qualDat$SA_5)/5)

# put IMS/EMS and SA scores together
qualDat2 = select(qualDat, Subject, 
                  IMS_1.rev, IMS_2, IMS_3, IMS_4, IMS_5,
                  EMS_1, EMS_2, EMS_3, EMS_4, EMS_5,
                  SA_1, SA_2, SA_3.rev, SA_4, SA_5,
                  IMS,
                  EMS,
                  SA)
qualDat2 = qualDat2[order(qualDat2$Subject),]


# manually calculate scores for subs with missing items
# 35, 63, 79, 124, 195

qualDat2$IMS[qualDat2$Subject == 35] = mean(as.numeric(qualDat2[qualDat$Subject == 35, c(2, 4:6)]))
qualDat2$IMS[qualDat2$Subject == 63] = mean(as.numeric(qualDat2[qualDat$Subject == 63, c(2, 4:6)]))
qualDat2$SA[qualDat2$Subject == 79] = mean(as.numeric(qualDat2[qualDat$Subject == 79, c(12, 14:16)]))
qualDat2$IMS[qualDat2$Subject == 124] = mean(as.numeric(qualDat2[qualDat$Subject == 124, c(2:4,6)]))
qualDat2$SA[qualDat2$Subject == 195] = mean(as.numeric(qualDat2[qualDat$Subject == 195, c(12:14,16)]))

# 111, 189, 201 don't have any data

# add to experimental trials data
for (i in unique(expTrials$Subject)) {
  expTrials$IMS[expTrials$Subject == i] = qualDat2$IMS[qualDat2$Subject == i]
  expTrials$EMS[expTrials$Subject == i] = qualDat2$EMS[qualDat2$Subject == i]
  expTrials$SA[expTrials$Subject == i] = qualDat2$SA[qualDat2$Subject == i]
}

# Add observer status, experimenter info 

paperDat = read.delim("./Data/Study II/PaperData.txt", stringsAsFactors = F)

# anonymize experimenter names by assigning number
names = unique(paperDat$Exp_name)
for (i in 1:23) {
  paperDat$Exp_ID[paperDat$Exp_name == names[i]] = i
}

# add observer condition, experimenter's name, gender, and race, intro status, room data was collected in

for (i in unique(expTrials$Subject)) {
  # add observer
  expTrials$Observer[expTrials$Subject == i] = paperDat$Observer[paperDat$Subject == i]
  # add room
  expTrials$Room[expTrials$Subject == i] = paperDat$Room[paperDat$Subject == i]
  # add experimenter's ID
  expTrials$Exp_ID[expTrials$Subject == i] = paperDat$Exp_ID[paperDat$Subject == i]
  # add experimenter's gender
  expTrials$Exp_gender[expTrials$Subject == i] = paperDat$Exp_gender[paperDat$Subject == i]
  # add experimenter's race
  expTrials$Exp_race[expTrials$Subject == i] = paperDat$Exp_Race[paperDat$Subject == i]
  # add intro status
  expTrials$Intro[expTrials$Subject == i] = paperDat$Intro[paperDat$Subject == i]
}


# Add column for congruence

expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "weapon"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "tool"] = "incongruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "weapon"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "tool"] = "incongruent"

expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "negative"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "black" & expTrials$TargetType == "positive"] = "incongruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "negative"] = "congruent"
expTrials$Congruence[expTrials$PrimeType == "white" & expTrials$TargetType == "positive"] = "incongruent"


write.table(expTrials, file = "Study2_experimentalTrials.txt", sep = "\t", row.names = F)



# Separate task questions (Study 2) -------------------------------------------------

# Rate your agreement with each statement from 1 (Strongly agree) to 6 (Strongly disagree):
# •	I feel a little self-conscious in this task.
# •	I am worried about some of the responses I have given during this task.
# •	I am feeling a bit uncomfortable with this task.
# •	I feel bothered that this task may reveal bias I wasn’t aware of.
# •	I feel good about the way I responded in this task. (R)
# •	I was frustrated by the difficulty of this task.
# •	I feel confident that I performed in an unbiased way.
# •	I am sure that my performance in the task is consistent with my beliefs about race.
# •	I was fully attentive throughout the entire task.
# •	I understood the instructions before I began the task.
# •	I tried my hardest to follow the instructions in the task. 

# While doing the task, to what extent did you experience these emotions? (scale is 1-not at all to 7-an extreme amount)
# -	Dread
# -	Anxiety
# -	Nervous
# -	Worry

# Select question data for each task
questList = c("APT_Qs_1", "APT_Qs_2", "WIT_Qs_1", "WIT_Qs_2")
questDat1 = dat.select[dat.select$blockName %in% questList,] %>%
  select(c(Subject, SubTrial, errorQSlide.RESP, blockName))
questDat2 = dat.select[dat.select$blockName %in% questList,] %>%
  select(c(Subject, SubTrial, anxQSlide.RESP, blockName))

# put into wide format (separately for two sets of questions)
wide1 = spread(questDat1, SubTrial, errorQSlide.RESP)
wide2 = spread(questDat2, SubTrial, anxQSlide.RESP)

# put them together
questWide = cbind(wide1[,1:13], wide2[,14:17])

# rename variables
names(questWide) = c("Subject", "blockName", 
                     "SelfConscious", 
                     "Worried", 
                     "Uncomf", 
                     "Bothered", 
                     "Good", 
                     "Frust", 
                     "Conf", 
                     "Consist", 
                     "Attent", 
                     "Understood", 
                     "Tried", 
                     "Dread", 
                     "Anx", 
                     "Nerv", 
                     "Worry")

# look at correlation table- need to take out sub 87 because missing data
cor(questWide[!(questWide$Subject == 87),3:17])
# first four items correlate highly within themselves

# better way to look at it
library(reshape2)
library(ggplot2)
qplot(x=Var1, y=Var2, data=melt(cor(questWide[!(questWide$Subject == 87),c(3:17)])), fill=value, geom="tile")

# reverse score "good"
questWide$Good.rev = 8 - questWide$Good

# exploratory factor analysis

efaDat = questWide[!(questWide$Subject == 87),c(3:6, 8:18)]

# entering raw data and extracting 3 factors, 
# with varimax rotation 
fit = factanal(efaDat, 4, rotation="varimax")
print(fit, digits=2, cutoff=.3, sort=TRUE)

# plot factor 1 by factor 2 
load <- as.data.frame(fit$loadings[,1:2]) 
load$Item = row.names(load)

ggplot(load, aes(Factor1, Factor2, label = Item)) +
  geom_point() +
  geom_text(hjust = -.1) 

# create anxiety composite

# since questions are on different scales, standardize first
questWide$Anx_composite = (scale(questWide$SelfConscious) + 
                              scale(questWide$Worried) + 
                              scale(questWide$Uncomf) + 
                              scale(questWide$Bothered) +
                              scale(questWide$Dread) +
                              scale(questWide$Anx) +
                              scale(questWide$Nerv) +
                              scale(questWide$Worry)
                            )/8



write.table(questWide, file = "Study2_questDat.txt", sep = "\t", row.names = F)


# Cronbach alphas for anx composite ----------------------------

questWide = read.delim("Study2_questDat.txt")

questWide$blockName = as.character(questWide$blockName)
APTcomp = select(questWide, -Anx_composite) %>% 
  filter(blockName == "APT_Qs_1"|blockName == "APT_Qs_2") %>% 
  select(SelfConscious, Worried, Uncomf, Bothered, Dread, Anx, Nerv, Worry)

WITcomp = select(questWide, -Anx_composite) %>% 
  filter(blockName == "WIT_Qs_1"|blockName == "WIT_Qs_2") %>% 
  select(SelfConscious, Worried, Uncomf, Bothered, Dread, Anx, Nerv, Worry)

require(psych)
alpha(APTcomp)
alpha(WITcomp)










