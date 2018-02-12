# Look at social anxiety

# add SA data
SA = read.delim("Study2_experimentalTrials.txt") %>% 
  filter(blockName == "WIT_1", SubTrial == 1) %>% 
  select(Subject, SA)

s2.wide$SA = NA
for (k in SA$Subject[!(is.na(SA$SA))]) {
  s2.wide$SA[s2.wide$Subject == k] = SA$SA[SA$Subject == k]
}

ggplot(s2.wide, aes(scale(SA), Anx_composite, color = Observer, shape = Observer)) +
  facet_wrap(~Task) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("red", "darkred")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x = "Trait social anxiety", y = "Anxiety composite score") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

lmer(scale(Anx_composite) ~ scale(SA)*Observer*Task + (1|Subject), data = s2.wide) %>% summary()

noMissing = s2.wide[!(s2.wide$Subject %in% c(111, 189, 201)),]
cor.test(noMissing$Anx_composite[noMissing$Task == "WIT"], noMissing$SA[noMissing$Task == "WIT"])
cor.test(noMissing$Anx_composite[noMissing$Task == "APT"], noMissing$SA[noMissing$Task == "APT"])

# Look very similar to IMS results, but SA and IMS are not highly correlated, r = .007, p = .899.  
# 
# SA and the anxiety composite are significantly correlated though, r = .13, p = .013. So, wanted to test if 

ggplot(s2.wide, aes(scale(SA), MeanC, color = Task, shape = Task)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("black", "forestgreen")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x="Trait social anxiety", y = "PDP-C") +
  #  ggtitle("Anxiety predicting PDP-C") +  
  theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

lmer(MeanC ~ scale(SA)*Task + (1|Subject), data = s2.wide) %>% summary()

ggplot(s2.wide, aes(scale(SA), AResid, color = Task, shape = Task)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("black", "darkred")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x="Trait social anxiety", y = "PDP-A") +
  #  ggtitle("Anxiety predicting PDP-A") +  
  theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

lmer(AResid ~ scale(SA)*Task + (1|Subject), data = s2.wide) %>% summary()







# Observer x IMS interactions ---------------------------------------------

# read in PDP estimates in wide format

WIT = select(s2.widePDP, Subject, Observer, WIT_MeanC, WIT_AResid) %>% 
  rename(MeanC = WIT_MeanC, AResid = WIT_AResid)
WIT$Task = "WIT"

APT = select(s2.widePDP, Subject, Observer, APT_MeanC, APT_AResid) %>% 
  rename(MeanC = APT_MeanC, AResid = APT_AResid)
APT$Task = "APT"

# put them together
s2.wide = rbind(WIT, APT)

# add IMS data
IMS = read.delim("Study2_experimentalTrials.txt") %>% 
  filter(blockName == "WIT_1", SubTrial == 1) %>% 
  select(Subject, IMS)

s2.wide$IMS = NA
for (k in IMS$Subject[!(is.na(IMS$IMS))]) {
  s2.wide$IMS[s2.wide$Subject == k] = IMS$IMS[IMS$Subject == k]
}

# add response accuracy data
s2.wide$perfBias = NA
for (k in unique(s2.perfBias$Subject)) {
  s2.wide$perfBias[s2.wide$Subject == k & s2.wide$Task == "WIT"] = s2.perfBias$WITperfBias[s2.perfBias$Subject == k]
  s2.wide$perfBias[s2.wide$Subject == k & s2.wide$Task == "APT"] = s2.perfBias$APperfBias[s2.perfBias$Subject == k]  
}

# Perf bias
ggplot(s2.wide, aes(IMS, perfBias, color=Observer)) +
  facet_wrap(~Task) +
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_manual(values=c("darkgreen", "green")) +
  ylab("Performance bias") +
  theme(plot.title=element_text(hjust=.5))

ggsave("Figures/ObsXIMS_perfBias_Study2.tiff", width = 8, height = 4)


# PDP control
ggplot(s2.wide, aes(IMS, MeanC, color=Observer)) +
  facet_wrap(~Task) +
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_manual(values=c("purple4", "orchid2")) +
  ylab("PDP control estimate") +
  theme(plot.title=element_text(hjust=.5))

ggsave("Figures/ObsXIMS_PDPcontrol_Study2.tiff", width = 8, height = 4)

# PDP auto
ggplot(s2.wide, aes(IMS, AResid, color=Observer)) +
  facet_wrap(~Task)+
  geom_point()+
  geom_smooth(method="lm") +
  theme_bw() +
  scale_color_manual(values=c("red4", "tomato")) +
  ylab("PDP auto estimate") +
  theme(plot.title=element_text(hjust=.5))

ggsave("Figures/ObsXIMS_PDPauto_Study2.tiff", width = 8, height = 4)

m4 = lm(perfBias ~ scale(IMS)*Observer*Task + (1|Subject), data = s2.wide) %>% 
  summary()
m5 = lm(MeanC ~ scale(IMS)*Observer*Task + (1|Subject), data = s2.wide) %>% 
  summary()
m6 = lm(AResid ~ scale(IMS)*Observer*Task + (1|Subject), data = s2.wide) %>% 
  summary()


ggplot(s2.wide, aes(scale(IMS), Anx_composite, color = Observer, shape = Observer)) +
  facet_wrap(~Task) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("dodgerblue", "darkblue")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x = "IMS", y = "Anxiety composite score") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave("./Figures/IMSxObsPredictingAnx.tiff", width=8, height=6, unit="in")

lmer(scale(Anx_composite) ~ scale(IMS)*Observer*Task + (1|Subject), data = s2.wide) %>% summary()

# add IMS data
EMS = read.delim("Study2_experimentalTrials.txt") %>% 
  filter(blockName == "WIT_1", SubTrial == 1) %>% 
  select(Subject, EMS)

s2.wide$EMS = NA
for (k in EMS$Subject[!(is.na(EMS$EMS))]) {
  s2.wide$EMS[s2.wide$Subject == k] = EMS$EMS[EMS$Subject == k]
}

ggplot(s2.wide, aes(scale(EMS), Anx_composite, color = Observer, shape = Observer)) +
  facet_wrap(~Task) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("red", "darkred")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x = "EMS", y = "Anxiety composite score") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))
ggsave("./Figures/EMSxObsPredictingAnx.tiff", width=8, height=6, unit="in")

lmer(scale(Anx_composite) ~ scale(EMS)*Observer*Task + (1|Subject), data = s2.wide) %>% summary()


s2.wide$IMS.EMS.diff = s2.wide$IMS - s2.wide$EMS

hist(s2.wide$IMS.EMS.diff)

ggplot(s2.wide, aes(scale(IMS.EMS.diff), Anx_composite, color = Observer, shape = Observer)) +
  facet_wrap(~Task) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("orchid", "purple")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x = "IMS minus EMS", y = "Anxiety composite score") +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        strip.text.x = element_text(face = "bold", size = 14),
        strip.background = element_rect(fill = "grey98"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))

lmer(scale(Anx_composite) ~ scale(IMS.EMS.diff)*Observer*Task + (1|Subject), data = s2.wide) %>% summary()



