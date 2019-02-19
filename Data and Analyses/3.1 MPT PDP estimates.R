require(MPTinR)
require(dplyr)
require(ggplot2)


# Specify PDP model -------------------------------------------------------

# Lines 1 & 2: Black-tool trials; first tool response then gun response
# Lines 3 & 4: Black-gun trials; first tool response then gun response

modelb = "
Cb + (1-Cb)*(1-Ab)
(1-Cb)*Ab

(1-Cb)*(1-Ab)
Cb + (1-Cb)*Ab"

check.mpt(textConnection(modelb), restrictions.filename = NULL)

modelw = "
Cw + (1-Cw)*(1-Aw)
(1-Cw)*Aw

(1-Cw)*(1-Aw)
Cw + (1-Cw)*Aw"

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




# Fit the model -----------------------------------------------------------
# data is called wide
# each row is a different individual
# each column is number of trials of that type (following black prime)
# doesn't include missed trials
# if no misses, first and second column should add up to 48 (there's 48 tool trials)
# same for third and fourth columns (there's 48 gun trials)


# Just Black trials -------------------------------------------------------

fia.samples = 200000
pdpMPT = fit.mpt(wide[,2:5], textConnection(modelb), fia = fia.samples)
# error with rows 36, 73, 75

wide$Subject[row.names(wide) %in% c(36, 73, 75)]

# fit again without subs that don't converge
filter.wide = filter(wide, !(Subject %in% c(4, 74, 76))) # subs that correspond to rows that won't converge
pdpMPT = fit.mpt(filter.wide[,2:5], textConnection(modelb), fia = fia.samples)

# examine the fit
pdpMPT[["goodness.of.fit"]][2:3] #for sum and aggregate fits
pdpMPT[["goodness.of.fit"]][1] #for individual fits
# fits well in aggregate
# doesn't fit well for some rows
badfit.rows.b = c(23, 27, 36, 38, 57, 58, 75, 79)
# match rows up with subs
badfit.subs.b = filter.wide$Subject[row.names(filter.wide) %in% badfit.rows]


# put individual estimates from both models into data frame form
ind.estimates.MPT = rbind(data.frame(Row = 1:97,
                                      Subject = filter.wide$Subject,
                                      Cb.MPT = round(pdpMPT[["parameters"]][["individual"]][1:97*6-4], digits=2),
                                      Ab.MPT = round(pdpMPT[["parameters"]][["individual"]][1:97*6-5], digits=2)))

# merge with algebraic PDP estimates

PDP = read.delim("Study1_pdpEstimates_WIT.txt")

PDP$Cb.MPT = NA
PDP$Ab.MPT = NA

for (i in unique(ind.estimates.MPT$Subject)) {
  PDP$Cb.MPT[PDP$Subject == i] = ind.estimates.MPT$Cb.MPT[ind.estimates.MPT$Subject == i]
  PDP$Ab.MPT[PDP$Subject == i] = ind.estimates.MPT$Ab.MPT[ind.estimates.MPT$Subject == i]
}


# Just White trials -------------------------------------------------------

fia.samples = 200000
pdpMPT = fit.mpt(filter.wide[,6:9], textConnection(modelw), fia = fia.samples)

# doesn't converge for rows 67 and 84
filter.wide$Subject[row.names(filter.wide) %in% c(67, 84)]

# take subs 7 and 87 out of filter.wide
filter.wide = filter(filter.wide, !(Subject %in% c(7, 87)))

# refit MPT
pdpMPT = fit.mpt(filter.wide[,6:9], textConnection(modelw), fia = fia.samples)

# examine the fit
pdpMPT[["goodness.of.fit"]][2:3] #for sum and aggregate fits
pdpMPT[["goodness.of.fit"]][1] #for individual fits
# fits well in aggregate
# doesn't fit well for some rows
badfit.rows.w = c(7, 23, 38, 58)
# match rows up with subs
badfit.subs.w = filter.wide$Subject[row.names(filter.wide) %in% badfit.rows]


# put individual estimates from both models into data frame form
ind.estimates.MPT = rbind(data.frame(Row = 1:95,
                                     Subject = filter.wide$Subject,
                                     Cw.MPT = round(pdpMPT[["parameters"]][["individual"]][1:95*6-4], digits=2),
                                     Aw.MPT = round(pdpMPT[["parameters"]][["individual"]][1:95*6-5], digits=2)))

# merge with algebraic PDP estimates

PDP$Cw.MPT = NA
PDP$Aw.MPT = NA

for (i in unique(ind.estimates.MPT$Subject)) {
  PDP$Cw.MPT[PDP$Subject == i] = ind.estimates.MPT$Cw.MPT[ind.estimates.MPT$Subject == i]
  PDP$Aw.MPT[PDP$Subject == i] = ind.estimates.MPT$Aw.MPT[ind.estimates.MPT$Subject == i]
}

# compare PDP and MPT estimates
cor(PDP$C_black, PDP$Cb.MPT, use="complete.obs")
cor(PDP$A_black, PDP$Ab.MPT, use="complete.obs")

cor(PDP$C_white, PDP$Cw.MPT, use="complete.obs")
cor(PDP$A_gun_white, PDP$Aw.MPT, use="complete.obs")


# Do all estimates together (Black and White) -----------------------------


model2 = "
Cb + (1-Cb)*(1-Ab)
(1-Cb)*Ab

(1-Cb)*(1-Ab)
Cb + (1-Cb)*Ab

Cw + (1-Cw)*(1-Aw)
(1-Cw)*Aw

(1-Cw)*(1-Aw)
Cw + (1-Cw)*Aw"

check.mpt(textConnection(model2), restrictions.filename = NULL)

fia.samples = 200000
pdpMPT = fit.mpt(wide[,2:9], textConnection(model2), fia = fia.samples)
# didn't converge for 36, 68, 73, 75, 87
didntconverge.subs = wide$Subject[row.names(wide) %in% c(36, 68, 73, 75, 87)]

# check if didn't converge because of large number of misses, but doesn't seem to be it
nomiss.avg = select(numTrials, Subject, Miss) %>% 
  group_by(Subject) %>% 
  summarise_all(funs(mean)) %>% 
  as.data.frame()

# # refit without those subjects
# filter.wide = filter(wide, !(Subject %in% didntconverge.subs))
# pdpMPT = fit.mpt(filter.wide[2:9], textConnection(model2), fia = fia.samples)
# # no problems with converging
# 
# # examine the fit
# pdpMPT[["goodness.of.fit"]][2:3] #for sum and aggregate fits
# pdpMPT[["goodness.of.fit"]][1] #for individual fits
# # fits well in aggregate
# # doesn't fit well for some rows
# badfit.rows = c(23, 27, 36, 38, 52, 57, 58, 64, 74, 78)
# # match rows up with subs
# badfit.subs = filter.wide$Subject[row.names(filter.wide) %in% badfit.rows]

# put individual estimates from both models into data frame form
ind.estimates.MPT = rbind(data.frame(Row = 1:100,
                                     Subject = wide$Subject,
                                     Cb.pdpMPT = round(pdpMPT[["parameters"]][["individual"]][1:100*12-11], digits=2),
                                     Ab.pdpMPT = round(pdpMPT[["parameters"]][["individual"]][1:100*12-10], digits=2),
                                     Cw.pdpMPT = round(pdpMPT[["parameters"]][["individual"]][1:100*12-9], digits=2),
                                     Aw.pdpMPT = round(pdpMPT[["parameters"]][["individual"]][1:100*12-8], digits=2)))

# merge with algebraic PDP estimates

PDP = read.delim("Study1_pdpEstimates_WIT.txt")

PDP$Cb.MPT = NA
PDP$Ab.MPT = NA
PDP$Cw.MPT = NA
PDP$Aw.MPT = NA

for (i in unique(ind.estimates.MPT$Subject)) {
  PDP$Cb.MPT[PDP$Subject == i] = ind.estimates.MPT$Cb.pdpMPT[ind.estimates.MPT$Subject == i]
  PDP$Ab.MPT[PDP$Subject == i] = ind.estimates.MPT$Ab.pdpMPT[ind.estimates.MPT$Subject == i]
  PDP$Cw.MPT[PDP$Subject == i] = ind.estimates.MPT$Cw.pdpMPT[ind.estimates.MPT$Subject == i]
  PDP$Aw.MPT[PDP$Subject == i] = ind.estimates.MPT$Aw.pdpMPT[ind.estimates.MPT$Subject == i]
}

cor(PDP$C_black, PDP$Cb.MPT, use="complete.obs")
cor(PDP$A_black, PDP$Ab.MPT, use="complete.obs")

cor(PDP$C_white, PDP$Cw.MPT, use="complete.obs")
cor(PDP$A_gun_white, PDP$Aw.MPT, use="complete.obs")

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
ggsave("./MPT plots/BlackC_Subs1-50.jpg", height = 5, width = 12)

ggplot(forPlot, aes(Subject, Black_C, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=50:101, limits=c(51, 101))
ggsave("./MPT plots/BlackC_Subs51-101.jpg", height = 5, width = 12)

ggplot(forPlot, aes(Subject, Black_A, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=1:50, limits=c(1,50))
ggsave("./MPT plots/BlackA_Subs1-50.jpg", height = 5, width = 12)

ggplot(forPlot, aes(Subject, Black_A, color = Method)) +
  geom_point(size = 4) +
  geom_line() +
  scale_x_continuous(breaks=50:101, limits=c(51, 101))
ggsave("./MPT plots/BlackA_Subs51-101.jpg", height = 5, width = 12)



