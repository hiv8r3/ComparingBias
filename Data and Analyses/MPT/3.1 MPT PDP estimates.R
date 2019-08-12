library(MPTinR)
library(dplyr)
library(ggplot2)


# 1. Specify PDP model (separate for black and white trials) -------------------------------------------------------

# Lines 1 & 2: Black-tool trials; first tool response then gun response
# Lines 3 & 4: Black-gun trials; first tool response then gun response

modelb = "
Cb + (1-Cb)*(1-Ab)
(1-Cb)*Ab

(1-Cb)*(1-Ab)
Cb + (1-Cb)*Ab"

check.mpt(textConnection(modelb), restrictions.filename = NULL)

# Lines 1 & 2: White-tool trials; first tool response then gun response
# Lines 3 & 4: White-gun trials; first tool response then gun response

modelw = "
Cw + (1-Cw)*Aw
(1-Cw)*(1-Aw)

(1-Cw)*Aw
Cw + (1-Cw)*(1-Aw)"

check.mpt(textConnection(modelw), restrictions.filename = NULL)


# Data organization:
# 1 participant per row
# 4 columns correspond with lines in model specified above
### first column is number of tool trials with tool response
### second column is number of tool trials with gun response
### third column is number of gun trials with tool response
### fourth column is number of gun trials with gun response

# first four columns for black trials
# second four columns for white trials

# Create data frame to feed to model --------------------------------------


dat = read.delim("Study1_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
dat$TrialType = paste(dat$PrimeType, dat$TargetType, sep="_")

# create data frame with correct number of trials in each trial type/response condition
numTrials = NULL
for (i in unique(dat$Subject)) {
  WITtemp = dat[dat$Subject == i & dat$blockName == "WIT",]
  for (typenum in 1:4) {
    correct = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 2,])
    incorrect = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 1,])
    miss = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 3,])
    numTrials = rbind(numTrials, data.frame(Subject = i,
                                               TrialType = unique(WITtemp$TrialType)[typenum],
                                               Correct = correct,
                                               Incorrect = incorrect,
                                               Miss = miss))
  }
}

# rearrange to desired format (wide)
wide = NULL
for (i in unique(numTrials$Subject)) {
  temp = numTrials[numTrials$Subject == i,]
  wide = rbind(wide, 
               data.frame(Subject = i,
                          BlackTool_ToolResp = temp$Correct[temp$TrialType == "black_tool"],
                          BlackTool_GunResp = temp$Incorrect[temp$TrialType == "black_tool"],
                          BlackGun_ToolResp = temp$Incorrect[temp$TrialType == "black_weapon"],
                          BlackGun_GunResp = temp$Correct[temp$TrialType == "black_weapon"],
                          WhiteTool_ToolResp = temp$Correct[temp$TrialType == "white_tool"],
                          WhiteTool_GunResp = temp$Incorrect[temp$TrialType == "white_tool"],
                          WhiteGun_ToolResp = temp$Incorrect[temp$TrialType == "white_weapon"],
                          WhiteGun_GunResp = temp$Correct[temp$TrialType == "white_weapon"]))
}

# order so that sub numbers are in order
wide = wide[order(wide$Subject),]
# missing sub 53

# Fit the model -----------------------------------------------------------
# data is called wide
# each row is a different individual
# each column is number of trials of that type (following black prime)
# doesn't include missed trials
# if no misses, first and second column should add up to 48 (there's 48 tool trials)
# same for third and fourth columns (there's 48 gun trials)


# Just Black trials -------------------------------------------------------

fia.samples = 200000
pdpMPT = fit.mpt(wide[,2:5], textConnection(modelb), fia = fia.samples) #select columns that have data for Black trials
# error with rows 4, 73, 75 because they have one or more columns with 0
# corresponds with subs 4, 74, and 76
# 74 and 76 had very high accuracy rates (good subs)
# 4 only gave gun response (bad sub)

# fit again without bad sub (4)
filter.wide = filter(wide, Subject != 4) 
pdpMPT = fit.mpt(filter.wide[,2:5], textConnection(modelb), fia = fia.samples)

# examine the fit
pdpMPT[["goodness.of.fit"]][2:3] #for sum and aggregate fits
pdpMPT[["goodness.of.fit"]][1] #for individual fits
# fits well in aggregate
# doesn't fit well for some rows
badfit.rows.b = c(27, 30, 39, 41, 43, 58, 59, 77, 80)
# match rows up with subs
badfit.subs.b = filter.wide$Subject[row.names(filter.wide) %in% badfit.rows.b]


# put individual estimates from both models into data frame form
ind.estimates.MPT = rbind(data.frame(Dataset = 1:99,
                                     Cb.MPT = round(pdpMPT[["parameters"]][["individual"]][1:99*6-4], digits=2),
                                     Ab.MPT = round(pdpMPT[["parameters"]][["individual"]][1:99*6-5], digits=2)))

# add sub numbers
ind.estimates.MPT$Subject = filter.wide$Subject

# merge with algebraic PDP estimates

PDP = read.delim("Study1_pdpEstimates_WIT.txt")

PDP = merge(PDP, select(ind.estimates.MPT, -Dataset), by = "Subject")


# Just White trials -------------------------------------------------------

fia.samples = 200000
pdpMPT = fit.mpt(filter.wide[,6:9], textConnection(modelw), fia = fia.samples)
# error with rows 6, 72, 85 because they have one or more columns with 0
# corresponds with subs 7, 74, 87
# all did not make any errors in White-tool trials

# examine the fit
pdpMPT[["goodness.of.fit"]][2:3] #for sum and aggregate fits
pdpMPT[["goodness.of.fit"]][1] #for individual fits
# fits well in aggregate
# doesn't fit well for some rows
badfit.rows.w = c(12, 27, 41, 59, 80)
# match rows up with subs
badfit.subs.w = filter.wide$Subject[row.names(filter.wide) %in% badfit.rows.w]


# put individual estimates from both models into data frame form
ind.estimates.MPT = rbind(data.frame(Dataset = 1:99,
                                     Cw.MPT = round(pdpMPT[["parameters"]][["individual"]][1:99*6-4], digits=2),
                                     Aw.MPT = round(pdpMPT[["parameters"]][["individual"]][1:99*6-5], digits=2)))

# add sub numbers
ind.estimates.MPT$Subject = filter.wide$Subject

# merge with algebraic PDP estimates

PDP = merge(PDP, select(ind.estimates.MPT, -Dataset), by="Subject")

# compare PDP and MPT estimates
cor(PDP$C_black, PDP$Cb.MPT)
cor(PDP$A_black, PDP$Ab.MPT)

cor(PDP$C_white, PDP$Cw.MPT)
cor(PDP$A_tool_white, PDP$Aw.MPT)


# 2. Do all estimates together (Black and White) -----------------------------


model2 = "
Cb + (1-Cb)*(1-Ab)
(1-Cb)*Ab

(1-Cb)*(1-Ab)
Cb + (1-Cb)*Ab

Cw + (1-Cw)*Aw
(1-Cw)*(1-Aw)

(1-Cw)*Aw
Cw + (1-Cw)*(1-Aw)"

check.mpt(textConnection(model2), restrictions.filename = NULL)

# fit model (excluding sub 4)
fia.samples = 200000
pdpMPT.both = fit.mpt(filter.wide[,2:9], textConnection(model2), fia = fia.samples)

# put individual estimates from both models into data frame form
ind.estimates.MPT.pdp = rbind(data.frame(Database = 1:99,
                                         Ab.MPT.m2 = round(pdpMPT.both[["parameters"]][["individual"]][1:99*12-11], digits=2),
                                         Aw.MPT.m2 = round(pdpMPT.both[["parameters"]][["individual"]][1:99*12-10], digits=2),
                                         Cb.MPT.m2 = round(pdpMPT.both[["parameters"]][["individual"]][1:99*12-9], digits=2),
                                         Cw.MPT.m2 = round(pdpMPT.both[["parameters"]][["individual"]][1:99*12-8], digits=2)))

ind.estimates.MPT.pdp$Subject = filter.wide$Subject

# merge with algebraic PDP estimates

PDP = merge(PDP, select(ind.estimates.MPT, -Database), by="Subject")

cor(PDP$Cb.MPT.m2, PDP$Cb.MPT, use="complete.obs") # parameters are identical, regardless of if black/white trials are fit separately or together
cor(PDP$Ab.MPT.m2, PDP$Ab.MPT, use="complete.obs")

cor(PDP$Cw.MPT.m2, PDP$Cw.MPT, use="complete.obs")
cor(PDP$Aw.MPT.m2, PDP$Aw.MPT, use="complete.obs")

forPlot = rbind(data.frame(Subject = PDP$Subject,
                           Black_C = PDP$C_black,
                           Black_A = PDP$A_black,
                           White_C = PDP$C_white,
                           White_A = PDP$A_gun_white,
                           Method = "pdp"),
                data.frame(Subject = PDP$Subject,
                           Black_C = PDP$Cb.MPT,
                           Black_A = PDP$Ab.MPT,
                           White_C = PDP$Cw.MPT,
                           White_A = PDP$Aw.MPT,
                           Method = "MPT"))

ggplot(forPlot, aes(Subject, Black_C, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=1:50, limits=c(1,50))
ggsave("./MPT/MPT plots/BlackC_Subs1-50.jpg", height = 5, width = 12)

ggplot(forPlot, aes(Subject, Black_C, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=50:101, limits=c(51, 101))
ggsave("./MPT/MPT plots/BlackC_Subs51-101.jpg", height = 5, width = 12)

ggplot(forPlot, aes(Subject, Black_A, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=1:50, limits=c(1,50))
ggsave("./MPT/MPT plots/BlackA_Subs1-50.jpg", height = 5, width = 12)

ggplot(forPlot, aes(Subject, Black_A, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=50:101, limits=c(51, 101))
ggsave("./MPT/MPT plots/BlackA_Subs51-101.jpg", height = 5, width = 12)




# 3. ABC model ---------------------------------------------------------------

# Specify  model -------------------------------------------------------

# Lines 1 & 2: Black-tool trials; first tool response then gun response
# Lines 3 & 4: Black-gun trials; first tool response then gun response
# Lines 5 & 6: White-tool trials; first tool response then gun response
# Lines 7 & 8: White-gun trials; first tool response then gun response

modelABC = "
Cb + (1-Cb)*(1-Ab)*G
(1-Cb)*Ab + (1-Cb)*(1-Ab)*(1-G)

(1-Cb)*(1-Ab)*(G)
Cb + (1-Cb)*Ab + (1-Cb)*(1-Ab)*(1-G)

Cw + (1-Cw)*Aw + (1-Cw)*(1-Aw)*G
(1-Cw)*(1-Aw)*(1-G)

(1-Cw)*Aw + (1-Cw)*(1-Aw)*G
Cw + (1-Cw)*(1-Aw)*(1-G)"

check.mpt(textConnection(modelABC), restrictions.filename = NULL)


# Data organization:
# 1 participant per row
# 4 columns correspond with lines in model specified above
### first column is number of tool trials with tool response
### second column is number of tool trials with gun response
### third column is number of gun trials with tool response
### fourth column is number of gun trials with gun response

# first four columns for black trials
# second four columns for white trials


# Fit the model -----------------------------------------------------------
# data is called filter.wide, excludes sub 4
# each row is a different individual
# each column is number of trials of that type (following black prime)
# doesn't include missed trials
# if no misses, first and second column should add up to 48 (there's 48 tool trials)
# same for third and fourth columns (there's 48 gun trials)


fia.samples = 200000
pdpMPT = fit.mpt(filter.wide[,2:9], textConnection(modelABC), 
                 #fia = fia.samples,
                 n.optim = 1000,
                 starting.values = NULL, 
                 output = 'full')

# not working. Try with both samples together

dat2 = read.delim("Study2_experimentalTrials.txt", stringsAsFactors=F) %>%
  select(-errorQSlide.RESP)
dat2$TrialType = paste(dat2$PrimeType, dat2$TargetType, sep="_")

# create data frame with correct number of trials in each trial type/response condition
numTrials = NULL
for (i in unique(dat2$Subject)) {
  WITtemp = rbind(dat2[dat2$Subject == i & dat2$blockName == "WIT_1",],
                  dat2[dat2$Subject == i & dat2$blockName == "WIT_2",])
  for (typenum in 1:4) {
    correct = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 2,])
    incorrect = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 1,])
    miss = nrow(WITtemp[WITtemp$TrialType == unique(WITtemp$TrialType)[typenum] & WITtemp$responseAccData == 3,])
    numTrials = rbind(numTrials, data.frame(Subject = i,
                                            TrialType = unique(WITtemp$TrialType)[typenum],
                                            Correct = correct,
                                            Incorrect = incorrect,
                                            Miss = miss))
  }
}

# rearrange to desired format (wide)
wide2 = NULL
for (i in unique(numTrials$Subject)) {
  temp = numTrials[numTrials$Subject == i,]
  wide2 = rbind(wide2, 
                data.frame(Subject = i,
                           BlackTool_ToolResp = temp$Correct[temp$TrialType == "black_tool"],
                           BlackTool_GunResp = temp$Incorrect[temp$TrialType == "black_tool"],
                           BlackGun_ToolResp = temp$Incorrect[temp$TrialType == "black_weapon"],
                           BlackGun_GunResp = temp$Correct[temp$TrialType == "black_weapon"],
                           WhiteTool_ToolResp = temp$Correct[temp$TrialType == "white_tool"],
                           WhiteTool_GunResp = temp$Incorrect[temp$TrialType == "white_tool"],
                           WhiteGun_ToolResp = temp$Incorrect[temp$TrialType == "white_weapon"],
                           WhiteGun_GunResp = temp$Correct[temp$TrialType == "white_weapon"]))
}

# order so that sub numbers are in order
wide2 = wide2[order(wide2$Subject),]
wide2$Sample = 2

# add sample identifier to wide data frame for sample 1
wide$Sample = 1

# bind samples together

wide.both = rbind(wide, wide2)

# take out sub 4 from sample 1

filter.wide.both = wide.both[!(wide.both$Subject == 4 & wide.both$Sample == 1),]


# try fitting again with bigger sample

fia.samples = 200000
pdpMPT = fit.mpt(filter.wide.both[,2:9], textConnection(modelABC), 
                 #fia = fia.samples,
                 n.optim = 1000,
                 starting.values = NULL, 
                 output = 'full')

# these participants all have 0s (not problematic)
filter.wide.both[c(6, 72, 74, 85, 107, 133, 141, 201, 207, 263, 303),]

# these participants aren't resolved:
filter.wide.both[c(4, 9, 36, 46, 51, 56, 63, 64, 68, 76, 128, 152, 154, 157, 168, 189, 192, 215, 222, 241, 242, 244, 257, 264),]

ind.estimates.MPT = rbind(data.frame(Database = 1:304,
                                      Ab.MPT.r1 = round(pdpMPT[["parameters"]][["individual"]][1:304*15-14], digits=2),
                                      Aw.MPT.r1 = round(pdpMPT[["parameters"]][["individual"]][1:304*15-13], digits=2),
                                      Cb.MPT.r1 = round(pdpMPT[["parameters"]][["individual"]][1:304*15-12], digits=2),
                                      Cw.MPT.r1 = round(pdpMPT[["parameters"]][["individual"]][1:304*15-11], digits=2),
                                      G.MPT.r1 = round(pdpMPT[["parameters"]][["individual"]][1:304*15-12], digits=2)))

ind.estimates.MPT$Subject = filter.wide.both$Subject
ind.estimates.MPT$Sample = filter.wide.both$Sample

# try again, same model
pdpMPT2 = fit.mpt(filter.wide.both[,2:9], textConnection(modelABC), 
                 #fia = fia.samples,
                 n.optim = 1000,
                 starting.values = NULL, 
                 output = 'full')

# these participants aren't resolved:
filter.wide.both[c(2, 5, 9, 51, 62, 68, 76, 77, 83, 100, 115, 120, 121, 130, 132, 154, 168, 169, 213, 237, 241, 243, 247, 257, 266, 279, 292),]


ind.estimates.MPT2 = rbind(data.frame(Database = 1:304,
                                     Ab.MPT.r2 = round(pdpMPT2[["parameters"]][["individual"]][1:304*15-14], digits=2),
                                     Aw.MPT.r2 = round(pdpMPT2[["parameters"]][["individual"]][1:304*15-13], digits=2),
                                     Cb.MPT.r2 = round(pdpMPT2[["parameters"]][["individual"]][1:304*15-12], digits=2),
                                     Cw.MPT.r2 = round(pdpMPT2[["parameters"]][["individual"]][1:304*15-11], digits=2),
                                     G.MPT.r2 = round(pdpMPT2[["parameters"]][["individual"]][1:304*15-12], digits=2)))

ind.estimates.MPT2$Subject = filter.wide.both$Subject
ind.estimates.MPT2$Sample = filter.wide.both$Sample


cor(ind.estimates.MPT$Ab.MPT.r1, ind.estimates.MPT2$Ab.MPT.r2)
cor(ind.estimates.MPT$Aw.MPT.r1, ind.estimates.MPT2$Aw.MPT.r2)
cor(ind.estimates.MPT$Cb.MPT.r1, ind.estimates.MPT2$Cb.MPT.r2) # C estimates are identical between runs
cor(ind.estimates.MPT$Cw.MPT.r1, ind.estimates.MPT2$Cw.MPT.r2)
cor(ind.estimates.MPT$G.MPT.r2, ind.estimates.MPT2$G.MPT.r2) # G estimates are identical between runs


cor(ind.estimates.MPT$Cb.MPT.r1[1:99], ind.estimates.MPT.pdp$Cb.MPT.m2) # C estimates are almost identical between ABC and pdp models
cor(ind.estimates.MPT$Cw.MPT.r1[1:99], ind.estimates.MPT.pdp$Cw.MPT.m2)
cor(ind.estimates.MPT$Ab.MPT.r1[1:99], ind.estimates.MPT.pdp$Ab.MPT.m2) # A estimates are correlated only r = .52-.55
cor(ind.estimates.MPT$Aw.MPT.r1[1:99], ind.estimates.MPT.pdp$Aw.MPT.m2)


# check if it's the participants that aren't converge that don't correspond

both = cbind(ind.estimates.MPT, ind.estimates.MPT2)

cor(both[c(2, 5, 9, 51, 62, 68, 76, 77, 83, 100, 115, 120, 121, 130, 132, 154, 168, 169, 213, 237, 241, 243, 247, 257, 266, 279, 292),2],
    both[c(2, 5, 9, 51, 62, 68, 76, 77, 83, 100, 115, 120, 121, 130, 132, 154, 168, 169, 213, 237, 241, 243, 247, 257, 266, 279, 292),10]) # Ab
cor(both[c(2, 5, 9, 51, 62, 68, 76, 77, 83, 100, 115, 120, 121, 130, 132, 154, 168, 169, 213, 237, 241, 243, 247, 257, 266, 279, 292),3],
    both[c(2, 5, 9, 51, 62, 68, 76, 77, 83, 100, 115, 120, 121, 130, 132, 154, 168, 169, 213, 237, 241, 243, 247, 257, 266, 279, 292),11]) # Aw

both


# 4. Modified ABC model ---------------------------------------------------------------

# Specify  model -------------------------------------------------------

# Lines 1 & 2: Black-tool trials; first tool response then gun response
# Lines 3 & 4: Black-gun trials; first tool response then gun response
# Lines 5 & 6: White-tool trials; first tool response then gun response
# Lines 7 & 8: White-gun trials; first tool response then gun response

# 4 parameters: A (white), A (black), C, G
modelABC.mod = "
C + (1-C)*(1-Ab)*G
(1-C)*Ab + (1-C)*(1-Ab)*(1-G)

(1-C)*(1-Ab)*(G)
C + (1-C)*Ab + (1-C)*(1-Ab)*(1-G)

C + (1-C)*Aw + (1-C)*(1-Aw)*G
(1-C)*(1-Aw)*(1-G)

(1-C)*Aw + (1-C)*(1-Aw)*G
C + (1-C)*(1-Aw)*(1-G)"

check.mpt(textConnection(modelABC.mod), restrictions.filename = NULL)


# Data organization:
# 1 participant per row
# 4 columns correspond with lines in model specified above
### first column is number of tool trials with tool response
### second column is number of tool trials with gun response
### third column is number of gun trials with tool response
### fourth column is number of gun trials with gun response

# first four columns for black trials
# second four columns for white trials


# Fit the model -----------------------------------------------------------
# data is called filter.wide.both (both study data), excludes sub 4 from Study 1
# each row is a different individual
# each column is number of trials of that type (following black prime)
# doesn't include missed trials
# if no misses, first and second column should add up to 48 (there's 48 tool trials)
# same for third and fourth columns (there's 48 gun trials)


fia.samples = 200000
pdpMPT.mod = fit.mpt(filter.wide.both[,2:9], textConnection(modelABC.mod), 
                     #fia = fia.samples,
                     n.optim = 1000,
                     starting.values = NULL, 
                     output = 'full')

ind.estimates.mod = rbind(data.frame(Database = 1:304,
                                     Ab.MPT.m2 = round(pdpMPT.mod[["parameters"]][["individual"]][1:304*12-11], digits=2),
                                     Aw.MPT.m2 = round(pdpMPT.mod[["parameters"]][["individual"]][1:304*12-10], digits=2),
                                     C.MPT.m2 = round(pdpMPT.mod[["parameters"]][["individual"]][1:304*12-9], digits=2),
                                     G.MPT.m2 = round(pdpMPT.mod[["parameters"]][["individual"]][1:304*12-8], digits=2)))

ind.estimates.mod$Subject = filter.wide.both$Subject

# how does this compare to 5 parameter ABC model?

cor(ind.estimates.mod$C.MPT.m2,                # Ccorrelates pretty highly with mean of C estimates from other model
    (both$Cb.MPT.r1 + both$Cw.MPT.r1)/2)
cor(ind.estimates.mod$C.MPT.m2,
    both$Cb.MPT.r1)
cor(ind.estimates.mod$C.MPT.m2,
    both$Cw.MPT.r1)

cor(ind.estimates.mod$G.MPT.m2,                # G doesn't correlate at all
    both$G.MPT.r1)

cor(ind.estimates.mod$Ab.MPT.m2,                # A correlates the same as between runs (r ~.5-.6)
    both$Aw.MPT.r1)
cor(ind.estimates.mod$Aw.MPT.m2,               
    both$Aw.MPT.r1)
cor(ind.estimates.mod$Ab.MPT.m2,               
    both$Aw.MPT.r2)
cor(ind.estimates.mod$Aw.MPT.m2,               
    both$Aw.MPT.)
