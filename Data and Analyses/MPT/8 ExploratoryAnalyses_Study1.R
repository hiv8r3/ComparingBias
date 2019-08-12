require(dplyr)
require(tidyr)
require(lme4)
require(lmerTest)


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


