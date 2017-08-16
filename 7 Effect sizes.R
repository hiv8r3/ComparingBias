

# 1. Comparison of error rates within each task ---------------------------


# STUDY 1 -----------------------------------------------------------------

# For analyses with accuracy as DV, need trials to be grouped into conditions (ie number of errors in each condition)
s1.acc = read.delim("Study1_errCountLong.txt")

# separate by task
s1.acc.AP = s1.acc[s1.acc$Task == "AP",]
s1.acc.WIT = s1.acc[s1.acc$Task == "WIT",]

s1.acc.AP$TargetType = factor(s1.acc.AP$TargetType)
s1.acc.WIT$TargetType = factor(s1.acc.WIT$TargetType)
s1.acc.AP$Subject = factor(s1.acc.AP$Subject)
s1.acc.WIT$Subject = factor(s1.acc.WIT$Subject)


# WIT ---------------------------------------------------------------------


# Race x Valence on accuracy (WIT)
sum = aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = s1.acc.WIT) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
2883/(2883+2719) # partial eta-sq = .51

# calculate error rates for table
dat.cond.WIT = mutate(s1.acc.WIT, errRate = numErr/48)
# means
print(model.tables(aov(errRate ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT),
                   "means"), se = TRUE, digits=3)

# standard deviations 
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "black" & 
                          dat.cond.WIT$TargetType == "gun"], na.rm = T)
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "black" & 
                          dat.cond.WIT$TargetType == "tool"], na.rm = T)
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "white" & 
                          dat.cond.WIT$TargetType == "gun"], na.rm = T)
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "white" & 
                          dat.cond.WIT$TargetType == "tool"], na.rm = T)


# Look at simple contrasts

#Following black faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s1.acc.WIT, PrimeType == "black")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
3206/(3206+2576) # partial eta-sq = .55

#Following white faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s1.acc.WIT, PrimeType == "white")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
373/(373+4043) # partial eta-sq = .08

# APT ---------------------------------------------------------------------

# Race x Valence on accuracy (APT)
sum = aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = s1.acc.AP) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
1794/(1794+4145) # partial eta-sq = .30

# calculate error rates for table
dat.cond.AP = mutate(s1.acc.AP, errRate = numErr/48)
# means
print(model.tables(aov(errRate ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP),
                   "means"), se = TRUE, digits=3)

# standard deviations 
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "black" & 
                          dat.cond.AP$TargetType == "positive"], na.rm = T)
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "black" & 
                          dat.cond.AP$TargetType == "negative"], na.rm = T)
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "white" & 
                          dat.cond.AP$TargetType == "positive"], na.rm = T)
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "white" & 
                          dat.cond.AP$TargetType == "negative"], na.rm = T)


# Look at simple contrasts

#Following black faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s1.acc.AP, PrimeType == "black")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
260/(260+3450) # partial eta-sq = .07

#Following white faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s1.acc.AP, PrimeType == "white")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
1916/(1916+4624) # partial eta-sq = .29


# Calculate Cohen's D to examine effect size of differences
# x and y are vectors
# Cohen's D is mean(x) - mean(y)/common variance
require(lsr)

# Effect size of difference between Black gun and tool trials in WIT
x = dat.cond.WIT$numErr[dat.cond.WIT$GenType == "black_con" & !is.na(dat.cond.WIT$numErr)]
y = dat.cond.WIT$numErr[dat.cond.WIT$GenType == "black_incon" & !is.na(dat.cond.WIT$numErr)]
cohensD(x,y)   # Cohen's D = 1.24
t.test(x,y)

# Effect size of difference between White gun and tool trials in WIT
x = dat.cond.WIT$numErr[dat.cond.WIT$GenType == "white_con" & !is.na(dat.cond.WIT$numErr)]
y = dat.cond.WIT$numErr[dat.cond.WIT$GenType == "white_incon" & !is.na(dat.cond.WIT$numErr)]
cohensD(x,y)   # Cohen's D = .42
t.test(x,y)

# Effect size of difference between Black positive and negative trials in AP
x = dat.cond.AP$numErr[dat.cond.AP$GenType == "black_con" & !is.na(dat.cond.AP$numErr)]
y = dat.cond.AP$numErr[dat.cond.AP$GenType == "black_incon" & !is.na(dat.cond.AP$numErr)]
cohensD(x,y)   # Cohen's D = .33
t.test(x,y)

# Effect size of difference between White positive and negative trials in AP
x = dat.cond.AP$numErr[dat.cond.AP$GenType == "white_con" & !is.na(dat.cond.AP$numErr)]
y = dat.cond.AP$numErr[dat.cond.AP$GenType == "white_incon" & !is.na(dat.cond.AP$numErr)]
cohensD(x,y)   # Cohen's D = .87
t.test(x,y)


# STUDY 2 -----------------------------------------------------------------

# For analyses with accuracy as DV, need trials to be grouped into conditions (ie number of errors in each condition)
s2.acc = read.delim("Study2_errCountLong.txt")

# separate by task
s2.acc.AP = s2.acc[s2.acc$Task == "AP",]
s2.acc.WIT = s2.acc[s2.acc$Task == "WIT",]

s2.acc.AP$TargetType = factor(s2.acc.AP$TargetType)
s2.acc.WIT$TargetType = factor(s2.acc.WIT$TargetType)
s2.acc.AP$Subject = factor(s2.acc.AP$Subject)
s2.acc.WIT$Subject = factor(s2.acc.WIT$Subject)


# WIT ---------------------------------------------------------------------

# Race x Valence on accuracy (WIT)
sum = aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = s2.acc.WIT) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
3861/(3861+6331) # partial eta-sq = .38

# calculate error rates for table
dat.cond.WIT = mutate(s2.acc.WIT, errRate = numErr/48)
# means
print(model.tables(aov(errRate ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.WIT),
                   "means"), se = TRUE, digits=3)

# standard deviations 
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "black" & 
                          dat.cond.WIT$TargetType == "gun"], na.rm = T)
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "black" & 
                          dat.cond.WIT$TargetType == "tool"], na.rm = T)
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "white" & 
                          dat.cond.WIT$TargetType == "gun"], na.rm = T)
sd(dat.cond.WIT$errRate[dat.cond.WIT$PrimeType == "white" & 
                          dat.cond.WIT$TargetType == "tool"], na.rm = T)


# Look at simple contrasts

#Following black faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s2.acc.WIT, PrimeType == "black")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
4593/(4593+7469) # partial eta-sq = .38

#Following white faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s2.acc.WIT, PrimeType == "white")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
404/(404+8905) # partial eta-sq = .04

# APT ---------------------------------------------------------------------

# Race x Valence on accuracy (APT)
aov(numErr ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = s2.acc.AP) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
2478/(2478+8900) # partial eta-sq = .22

# calculate error rates for table
dat.cond.AP = mutate(s2.acc.AP, errRate = numErr/48)
# means
print(model.tables(aov(errRate ~ PrimeType*TargetType + Error(Subject/(PrimeType*TargetType)), data = dat.cond.AP),
                   "means"), se = TRUE, digits=3)

# standard deviations 
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "black" & 
                         dat.cond.AP$TargetType == "positive"], na.rm = T)
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "black" & 
                         dat.cond.AP$TargetType == "negative"], na.rm = T)
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "white" & 
                         dat.cond.AP$TargetType == "positive"], na.rm = T)
sd(dat.cond.AP$errRate[dat.cond.AP$PrimeType == "white" & 
                         dat.cond.AP$TargetType == "negative"], na.rm = T)


# Look at simple contrasts

#Following black faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s2.acc.AP, PrimeType == "black")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
1/(1+9274) # partial eta-sq = .00

#Following white faces
aov(numErr ~ TargetType + Error(Subject/(TargetType)), data = filter(s2.acc.AP, PrimeType == "white")) %>% 
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
4837/(4837+9385) # partial eta-sq = .34


# 2. Comparison across tasks ----------------------------------------------


# STUDY 1 ------------------------------------------------------------------

# Look just at subjects that have data for both tasks, otherwise throws error ("Error() model is singular")
# Probably because some subjects don't have data across both levels of task
s1.bsWIT = read.delim("Study1_badsubsWIT.txt")
s1.bsAP = read.delim("Study1_badsubsAP.txt")
s1.acc.nobs = s1.acc[!(s1.acc$Subject %in% s1.bsWIT$Subject) & !(s1.acc$Subject %in% s1.bsAP$Subject),]
s1.acc.nobs$Subject = factor(s1.acc.nobs$Subject)

aov(numErr ~ (PrimeType*ConType*Task)+Error(Subject/(PrimeType*ConType*Task)), data = s1.acc.nobs) %>%
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
992/(992+3830) # partial eta-sq = .21

# STUDY 2 -----------------------------------------------------------------

# Look just at subjects that have data for both tasks, otherwise throws error ("Error() model is singular")
# Probably because some subjects don't have data across both levels of task
s2.bsWIT = read.delim("Study2_badsubsWIT.txt")
s2.bsAP = read.delim("Study2_badsubsAP.txt")
s2.acc.nobs = s2.acc[!(s2.acc$Subject %in% s2.bsWIT$Subject) & !(s2.acc$Subject %in% s2.bsAP$Subject),]
s2.acc.nobs$Subject = factor(s2.acc.nobs$Subject)

aov(numErr ~ (PrimeType*ConType*Task)+Error(Subject/(PrimeType*ConType*Task)), data = s2.acc.nobs) %>%
  summary()

# Calculate partial etq-squared for 2 way interaction
# SS.effect/(SS.effect + SS.total)
3123/(3123+6378) # partial eta-sq = .33
