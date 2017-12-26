require(dplyr)
require(tidyr)
require(lme4)
require(lmerTest)


questDat = read.delim("Study1_questDat.txt")
# Add post-task questions
questDat$blockName = as.character(questDat$blockName)
questDat$blockName[questDat$blockName == "PostWITquestions"] = "WIT"
questDat$blockName[questDat$blockName == "PostAPquestions"] = "AP"
questDat$blockName = as.factor(questDat$blockName)

names(questDat)[3] = "Resp"

dat = data.frame(Subject = rep(unique(questDat$Subject), each = 2),
                 blockName = rep(c("WIT", "AP"), length(unique(questDat$Subject))))
for (i in unique(questDat$Subject)) {
  for (j in c("WIT", "AP")) {
    dat$Frust[dat$Subject == i & dat$blockName == j] = questDat$Resp[questDat$Subject == i & 
                                                                     questDat$SubTrial == 1 &
                                                                     questDat$blockName == j]
    dat$Anx[dat$Subject == i & dat$blockName == j] = questDat$Resp[questDat$Subject == i & 
                                                                   questDat$SubTrial == 2 &
                                                                   questDat$blockName == j]
    dat$Unpleas[dat$Subject == i & dat$blockName == j] = questDat$Resp[questDat$Subject == i & 
                                                                       questDat$SubTrial == 3 &
                                                                       questDat$blockName == j]
    dat$Attend[dat$Subject == i & dat$blockName == j] = questDat$Resp[questDat$Subject == i & 
                                                                      questDat$SubTrial == 4 &
                                                                      questDat$blockName == j]
    dat$Effort[dat$Subject == i & dat$blockName == j] = questDat$Resp[questDat$Subject == i & 
                                                                      questDat$SubTrial == 5 &
                                                                      questDat$blockName == j]
  }
}

dat$Err_comp = (dat$Frust + dat$Anx + dat$Unpleas)/3

PDPdat = read.delim("Study1_PDP_long.txt")

for (i in unique(questDat$Subject)) {
  for (j in c("WIT", "AP")) {
    PDPdat$Err_comp[PDPdat$Subject == i & PDPdat$Task == j] = dat$Err_comp[dat$Subject == i & dat$blockName == j]
  }
}

# Means and SDs for error-related distress
PDPdat %>% 
  select(Task, Observer, Err_comp) %>% 
  group_by(Task, Observer) %>% 
  summarise_each(funs(mean))

PDPdat %>% 
  select(Task, Observer, Err_comp) %>% 
  group_by(Task, Observer) %>% 
  summarise_each(funs(sd))

# Effect of error-related distress composite on PDP-A estimates
lm(value ~ scale(Err_comp)*Task, data = PDPdat[PDPdat$Type == "AResid",]) %>% 
  summary()

# Effect of error-related distress composite on PDP-C estimates
lm(value ~ scale(Err_comp)*Task, data = PDPdat[PDPdat$Type == "MeanC",]) %>% 
  summary()





# Observer x IMS interactions ---------------------------------------------

WIT = select(s1.widePDP, Subject, Observer, WIT_MeanC, WIT_AResid) %>% 
  rename(MeanC = WIT_MeanC, AResid = WIT_AResid)
WIT$Task = "WIT"

APT = select(s1.widePDP, Subject, Observer, APT_MeanC, APT_AResid) %>% 
  rename(MeanC = APT_MeanC, AResid = APT_AResid)
APT$Task = "APT"

# put them together
s1.wide = rbind(WIT, APT)

# add IMS data
IMS = read.delim("Study1_experimentalTrials.txt") %>% 
  filter(blockName == "WIT", SubTrial == 1) %>% 
  select(Subject, IMS)

s1.wide$IMS = NA
for (k in IMS$Subject[!(is.na(IMS$IMS))]) {
  s1.wide$IMS[s1.wide$Subject == k] = IMS$IMS[IMS$Subject == k]
}

# add response accuracy data
s1.wide$perfBias = NA
for (k in unique(s1.perfBias$Subject)) {
  s1.wide$perfBias[s1.wide$Subject == k & s1.wide$Task == "WIT"] = s1.perfBias$WITperfBias[s1.perfBias$Subject == k]
  s1.wide$perfBias[s1.wide$Subject == k & s1.wide$Task == "APT"] = s1.perfBias$APperfBias[s1.perfBias$Subject == k]  
}

# Perf bias
ggplot(s1.wide, aes(IMS, perfBias, color=Observer)) +
  facet_wrap(~Task) +
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_manual(values=c("darkgreen", "green")) +
  ylab("Performance bias") +
  theme(plot.title=element_text(hjust=.5))

ggsave("Figures/ObsXIMS_perfBias_Study1.tiff", width = 8, height = 4)

# PDP control
ggplot(s1.wide, aes(IMS, MeanC, color=Observer)) +
  facet_wrap(~Task) +
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_manual(values=c("purple4", "orchid2")) +
  ylab("PDP control estimate") +
  theme(plot.title=element_text(hjust=.5))

ggsave("Figures/ObsXIMS_PDPcontrol_Study1.tiff", width = 8, height = 4)

# PDP auto
ggplot(s1.wide, aes(IMS, AResid, color=Observer)) +
  facet_wrap(~Task)+
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_manual(values=c("red4", "tomato")) +
  ylab("PDP auto estimate") +
  theme(plot.title=element_text(hjust=.5))

ggsave("Figures/ObsXIMS_PDPauto_Study1.tiff", width = 8, height = 4)

m1 = lm(perfBias ~ scale(IMS)*Observer*Task, data = s1.wide) %>% 
  summary()
m2 = lm(MeanC ~ scale(IMS)*Observer*Task, data = s1.wide) %>% 
  summary()
m3 = lm(AResid ~ scale(IMS)*Observer*Task, data = s1.wide) %>% 
  summary()

# Look at each level of observer separately
## Present
lm(AResid ~ scale(IMS)*Task, data = s1.wide[s1.wide$Observer == "Present",]) %>%
  summary()
## Absent
lm(AResid ~ scale(IMS)*Task, data = s1.wide[s1.wide$Observer == "Absent",]) %>%
  summary()

# simple slopes
lm(scale(AResid) ~ scale(IMS), data = s1.wide[s1.wide$Observer == "Present" & s1.wide$Task == "APT",]) %>%
  summary()
lm(scale(AResid) ~ scale(IMS), data = s1.wide[s1.wide$Observer == "Present" & s1.wide$Task == "WIT",]) %>%
  summary()


