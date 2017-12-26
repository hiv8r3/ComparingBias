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



# Look at effect of experimenter

dat = read.delim("Study2_experimentalTrials.txt")

selectDat = NULL
for (i in unique(dat$Subject)) {
  temp = dat[dat$Subject == i,]
  selectDat = rbind(selectDat, temp[1,])
}

selectDat = select(selectDat, Subject, Observer, IMS, Exp_ID, Exp_gender, Exp_race)
selectDat$Observer = as.character(selectDat$Observer)
selectDat$Exp_race = as.character(selectDat$Exp_race)
selectDat$Exp_gender = as.character(selectDat$Exp_gender)


perfBias = read.delim("Study2_perfBias.txt")

melt = gather(select(perfBias, Subject, WITperfBias, APperfBias), Task, perfBias, WITperfBias, APperfBias)
melt$Task[melt$Task == "WITperfBias"] = "WIT"
melt$Task[melt$Task == "APperfBias"] = "APT"

for (i in unique(melt$Subject)) {
  melt$Observer[melt$Subject == i] = selectDat$Observer[selectDat$Subject == i]
  melt$IMS[melt$Subject == i] = selectDat$IMS[selectDat$Subject == i]
  melt$Exp_ID[melt$Subject == i] = selectDat$Exp_ID[selectDat$Subject == i]
  melt$Exp_gender[melt$Subject == i] = selectDat$Exp_gender[selectDat$Subject == i]
  melt$Exp_race[melt$Subject == i] = selectDat$Exp_race[selectDat$Subject == i]
}

melt$Exp_race2 = "Nonwhite"
melt$Exp_race2[melt$Exp_race == "White"] = "White"

melt$Subject = as.factor(melt$Subject)
melt$Task = as.factor(melt$Task)
melt$Observer = as.factor(melt$Observer)
melt$Exp_ID = as.factor(melt$Exp_ID)
melt$Exp_race = as.factor(melt$Exp_race)
melt$Exp_gender = as.factor(melt$Exp_gender)

lmer(perfBias ~ Observer*Exp_race2*Task+(1|Subject), data = melt) %>% 
  summary()

