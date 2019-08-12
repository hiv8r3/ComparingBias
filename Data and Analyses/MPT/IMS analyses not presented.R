### 4. Test Observer x IMS interaction on three criterion (response accuracy bias, PDP-C, and PDP-A)  
#**IMS is standardized for models, although not in plots. Models give unstandardized estimates.**  
  #####Study 1:  

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



# Study 2
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


### 9. Look at relationship between IMS and anxiety, as a function of observer



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


### 10. Look at relationship between EMS and anxiety, as a function of observer

# add EMS data (Study 1)
EMS = read.delim("Study1_experimentalTrials.txt") %>% 
  filter(blockName == "WIT", SubTrial == 1) %>% 
  select(Subject, EMS)

s1.wide$EMS = NA
for (k in EMS$Subject[!(is.na(EMS$EMS))]) {
  s1.wide$EMS[s1.wide$Subject == k] = EMS$EMS[EMS$Subject == k]
}


# add EMS data (Study 2)
EMS = read.delim("Study2_experimentalTrials.txt") %>% 
  filter(blockName == "WIT_1", SubTrial == 1) %>% 
  select(Subject, EMS)

s2.wide$EMS = NA
for (k in EMS$Subject[!(is.na(EMS$EMS))]) {
  s2.wide$EMS[s2.wide$Subject == k] = EMS$EMS[EMS$Subject == k]
}

# correlations (Study 1)
cor.test(s1.wide$IMS[s1.wide$Task == "WIT"], s1.wide$perfBias[s1.wide$Task == "WIT"])
cor.test(s1.wide$IMS[s1.wide$Task == "APT"], s1.wide$perfBias[s1.wide$Task == "APT"])

cor.test(s1.wide$IMS[s1.wide$Task == "WIT"], s1.wide$AResid[s1.wide$Task == "WIT"])
cor.test(s1.wide$IMS[s1.wide$Task == "APT"], s1.wide$AResid[s1.wide$Task == "APT"])

cor.test(s1.wide$IMS[s1.wide$Task == "WIT"], s1.wide$MeanC[s1.wide$Task == "WIT"])
cor.test(s1.wide$IMS[s1.wide$Task == "APT"], s1.wide$MeanC[s1.wide$Task == "APT"])


cor.test(s1.wide$EMS[s1.wide$Task == "WIT"], s1.wide$perfBias[s1.wide$Task == "WIT"])
cor.test(s1.wide$EMS[s1.wide$Task == "APT"], s1.wide$perfBias[s1.wide$Task == "APT"])

cor.test(s1.wide$EMS[s1.wide$Task == "WIT"], s1.wide$AResid[s1.wide$Task == "WIT"])
cor.test(s1.wide$EMS[s1.wide$Task == "APT"], s1.wide$AResid[s1.wide$Task == "APT"])

cor.test(s1.wide$EMS[s1.wide$Task == "WIT"], s1.wide$MeanC[s1.wide$Task == "WIT"])
cor.test(s1.wide$EMS[s1.wide$Task == "APT"], s1.wide$MeanC[s1.wide$Task == "APT"])

# correlations (Study 2)
cor.test(s2.wide$IMS[s2.wide$Task == "WIT"], s2.wide$perfBias[s2.wide$Task == "WIT"])
cor.test(s2.wide$IMS[s2.wide$Task == "APT"], s2.wide$perfBias[s2.wide$Task == "APT"])

cor.test(s2.wide$IMS[s2.wide$Task == "WIT"], s2.wide$AResid[s2.wide$Task == "WIT"])
cor.test(s2.wide$IMS[s2.wide$Task == "APT"], s2.wide$AResid[s2.wide$Task == "APT"])

cor.test(s2.wide$IMS[s2.wide$Task == "WIT"], s2.wide$MeanC[s2.wide$Task == "WIT"])
cor.test(s2.wide$IMS[s2.wide$Task == "APT"], s2.wide$MeanC[s2.wide$Task == "APT"])


cor.test(s2.wide$EMS[s2.wide$Task == "WIT"], s2.wide$perfBias[s2.wide$Task == "WIT"])
cor.test(s2.wide$EMS[s2.wide$Task == "APT"], s2.wide$perfBias[s2.wide$Task == "APT"])

cor.test(s2.wide$EMS[s2.wide$Task == "WIT"], s2.wide$AResid[s2.wide$Task == "WIT"])
cor.test(s2.wide$EMS[s2.wide$Task == "APT"], s2.wide$AResid[s2.wide$Task == "APT"])

cor.test(s2.wide$EMS[s2.wide$Task == "WIT"], s2.wide$MeanC[s2.wide$Task == "WIT"])
cor.test(s2.wide$EMS[s2.wide$Task == "APT"], s2.wide$MeanC[s2.wide$Task == "APT"])


# correlations (Study 2)
both.wide = rbind(s1.wide, s2.wide)

cor.test(both.wide$IMS[both.wide$Task == "WIT"], both.wide$perfBias[both.wide$Task == "WIT"])
cor.test(both.wide$IMS[both.wide$Task == "APT"], both.wide$perfBias[both.wide$Task == "APT"])

cor.test(both.wide$IMS[both.wide$Task == "WIT"], both.wide$AResid[both.wide$Task == "WIT"])
cor.test(both.wide$IMS[both.wide$Task == "APT"], both.wide$AResid[both.wide$Task == "APT"])

cor.test(both.wide$IMS[both.wide$Task == "WIT"], both.wide$MeanC[both.wide$Task == "WIT"])
cor.test(both.wide$IMS[both.wide$Task == "APT"], both.wide$MeanC[both.wide$Task == "APT"])


cor.test(both.wide$EMS[both.wide$Task == "WIT"], both.wide$perfBias[both.wide$Task == "WIT"])
cor.test(both.wide$EMS[both.wide$Task == "APT"], both.wide$perfBias[both.wide$Task == "APT"])

cor.test(both.wide$EMS[both.wide$Task == "WIT"], both.wide$AResid[both.wide$Task == "WIT"])
cor.test(both.wide$EMS[both.wide$Task == "APT"], both.wide$AResid[both.wide$Task == "APT"])

cor.test(both.wide$EMS[both.wide$Task == "WIT"], both.wide$MeanC[both.wide$Task == "WIT"])
cor.test(both.wide$EMS[both.wide$Task == "APT"], both.wide$MeanC[both.wide$Task == "APT"])

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


```

### 11. Look at relationship between IMS-EMS and anxiety, as a function of observer

``` {r IMS-EMS, echo=F}

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


```


