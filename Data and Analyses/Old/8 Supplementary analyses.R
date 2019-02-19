require(dplyr)
require(tidyr)


# 1. Race-specific PDP estimates ------------------------------------------

# Study 1

s1.longPDP = read.delim("Study1_PDP_long.txt") %>% 
  filter(!(is.na(PrimeType)))

# WIT
s1.longWIT = filter(s1.longPDP, Task == "WIT")
s1.longWIT$PrimeType = factor(s1.longWIT$PrimeType)
s1.longWIT$Type = factor(s1.longWIT$Type)
s1.longWIT$Subject = factor(s1.longWIT$Subject)

aov(value ~ PrimeType*Estimate + Error(Subject/(PrimeType*Estimate)), data = s1.longWIT) %>% 
  summary()

tapply(s1.longWIT$value, s1.longWIT$Type, mean)
tapply(s1.longWIT$value, s1.longWIT$Type, sd)

# pair wise contrasts
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s1.longWIT, Estimate == "A")) %>% 
  summary()

aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s1.longWIT, Estimate == "C")) %>% 
  summary()


# APT
s1.longAP = filter(s1.longPDP, Task == "AP")
s1.longAP$PrimeType = factor(s1.longAP$PrimeType)
s1.longAP$Type = factor(s1.longAP$Type)
s1.longAP$Subject = factor(s1.longAP$Subject)

aov(value ~ PrimeType*Estimate + Error(Subject/(PrimeType*Estimate)), data = s1.longAP) %>% 
  summary()

tapply(s1.longAP$value, s1.longAP$Type, mean)
tapply(s1.longAP$value, s1.longAP$Type, sd)

# pair wise contrasts
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s1.longAP, Estimate == "A")) %>% 
  summary()

aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s1.longAP, Estimate == "C")) %>% 
  summary()

# Study 2

s2.longPDP = read.delim("Study2_PDP_long.txt") %>% 
  filter(!(is.na(PrimeType)))

# WIT
s2.longWIT = filter(s2.longPDP, Task == "WIT")
s2.longWIT$PrimeType = factor(s2.longWIT$PrimeType)
s2.longWIT$Type = factor(s2.longWIT$Type)
s2.longWIT$Subject = factor(s2.longWIT$Subject)

aov(value ~ PrimeType*Estimate + Error(Subject/(PrimeType*Estimate)), data = s2.longWIT) %>% 
  summary()

tapply(s2.longWIT$value, s2.longWIT$Type, mean)
tapply(s2.longWIT$value, s2.longWIT$Type, sd)

# pair wise contrasts
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s2.longWIT, Estimate == "A")) %>% 
  summary()

aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s2.longWIT, Estimate == "C")) %>% 
  summary()


# APT
s2.longAP = filter(s2.longPDP, Task == "AP")
s2.longAP$PrimeType = factor(s2.longAP$PrimeType)
s2.longAP$Type = factor(s2.longAP$Type)
s2.longAP$Subject = factor(s2.longAP$Subject)

aov(value ~ PrimeType*Estimate + Error(Subject/(PrimeType*Estimate)), data = s2.longAP) %>% 
  summary()

tapply(s2.longAP$value, s2.longAP$Type, mean)
tapply(s2.longAP$value, s2.longAP$Type, sd)

# pair wise contrasts
aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s2.longAP, Estimate == "A")) %>% 
  summary()

aov(value ~ PrimeType + Error(Subject/(PrimeType)), data = filter(s2.longAP, Estimate == "C")) %>% 
  summary()

# 2. PDP-A as difference score ------------------------------------------

# Comparing across tasks

# Study 1
s1.widePDP = read.delim("Study1_PDP_wide.txt")

lm(WIT_DiffA.stand ~ AP_DiffA.stand, data = s1.widePDP) %>% summary()

ggplot(s1.widePDP, aes(WIT_DiffA.stand, AP_DiffA.stand)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "WIT PDP-A", y = "AP PDP-A") +
  theme_bw()+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

s2.widePDP = read.delim("Study2_PDP_wide.txt")

# Study 2

lm(WIT_DiffA.stand ~ AP_DiffA.stand, data = s2.widePDP) %>% summary()

ggplot(s2.widePDP, aes(WIT_DiffA.stand, AP_DiffA.stand)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "WIT PDP-A", y = "AP PDP-A") +
  theme_bw()+
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        title = element_text(size=20)
        #axis.text.x  = element_text(angle=90, vjust=0.5, size=16)
  )

# Looking at Task x Estimate interaction across tasks

# Study 1
# Compare C and A resid 
tempC = select(s1.widePDP, Subject, WIT_MeanC.stand, AP_MeanC.stand) %>% 
  rename(WITestimate = WIT_MeanC.stand, APTestimate = AP_MeanC.stand)
tempC$Type = "PDP-C"

tempA = select(s1.widePDP, Subject, WIT_DiffA.stand, AP_DiffA.stand) %>% 
  rename(WITestimate = WIT_DiffA.stand, APTestimate = AP_DiffA.stand)
tempA$Type = "PDP-A"

compareAC = rbind(tempC, tempA)
compareAC$Type = factor(compareAC$Type)

ggplot(compareAC, aes(WITestimate, APTestimate, pch = Type)) +
  geom_point(aes(shape = Type), size = 2.5, alpha = .7) +
  scale_shape_manual(values=c(1,17)) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  theme_bw() +
  geom_smooth(method = "lm", aes(linetype=Type), color = "black") +
  labs(x = "PDP estimates for WIT", y = "PDP estimates for APT") +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.title = element_blank(),
        legend.key.size = unit(1.2, "cm"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  coord_cartesian(ylim=c(-2.5,3), xlim=c(-2.5,3))

ggsave("./Figures/CompareAdiffC_SM_Study1.tiff")

lm(WITestimate ~ APTestimate*Type, data = compareAC) %>% summary()

# Study 2
# Compare C and A resid 
tempC = select(s2.widePDP, Subject, WIT_MeanC.stand, AP_MeanC.stand) %>% 
  rename(WITestimate = WIT_MeanC.stand, APTestimate = AP_MeanC.stand)
tempC$Type = "PDP-C"

tempA = select(s2.widePDP, Subject, WIT_DiffA.stand, AP_DiffA.stand) %>% 
  rename(WITestimate = WIT_DiffA.stand, APTestimate = AP_DiffA.stand)
tempA$Type = "PDP-A"

compareAC = rbind(tempC, tempA)
compareAC$Type = factor(compareAC$Type)

ggplot(compareAC, aes(WITestimate, APTestimate, pch = Type)) +
  geom_point(aes(shape = Type), size = 2.5, alpha = .7) +
  scale_shape_manual(values=c(1,17)) +
  scale_linetype_manual(values=c("solid", "dashed")) +
  theme_bw() +
  geom_smooth(method = "lm", aes(linetype=Type), color = "black") +
  labs(x = "PDP estimates for WIT", y = "PDP estimates for APT") +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        legend.title = element_blank(),
        legend.key.size = unit(1.2, "cm"),
        axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text.x = element_text(size=16),
        axis.text.y = element_text(size=16))+
  coord_cartesian(ylim=c(-2.5,3), xlim=c(-2.5,3))

ggsave("./Figures/CompareAdiffC_SM_Study2.tiff")

lm(WITestimate ~ APTestimate*Type, data = compareAC) %>% summary()


# Effect of Observer
# Study 1
s1.test = select(s1.widePDP, Subject, Observer, contains("DiffA.stand")) %>% 
  gather(Task, DiffA, 3:4)
s1.test$Subject = factor(s1.test$Subject)
s1.test$Task = factor(s1.test$Task)

aov(DiffA ~ Observer*Task + Error(Subject/(Task)), data = s1.test) %>% 
  summary()

# Study 2
# Study 1
s2.test = select(s2.widePDP, Subject, Observer, contains("DiffA.stand")) %>% 
  gather(Task, DiffA, 3:4)
s2.test$Subject = factor(s2.test$Subject)
s2.test$Task = factor(s2.test$Task)

aov(DiffA ~ Observer*Task + Error(Subject/(Task)), data = s2.test) %>% 
  summary()




# 3. Anxiety stuff --------------------------------------------------------

anxDat = read.delim("Study2_questDat.txt")
qplot(x=Var1, y=Var2, data=melt(cor(filter(anxDat, Subject != 87)[c(3:6, 8:18)])), fill=value, geom="tile") # sub 87 doesn't have data for the last four questions

hist(anxDat$Anx_composite, main = "Histogram for anxiety composite")


anxDat$Task = "APT"
anxDat$Task[grep("WIT", anxDat$blockName)] = "WIT"

block1 = anxDat[grep(1, anxDat$blockName),]
block2 = anxDat[grep(2, anxDat$blockName),]

block1 = select(block1, Subject, Task, Anx_composite) %>% 
  rename(Anx_composite1 = Anx_composite)
block2 = select(block2, Subject, Task, Anx_composite) %>% 
  rename(Anx_composite2 = Anx_composite)

sepBlock = cbind(block1, select(block2, Anx_composite2))

ggplot(sepBlock, aes(Anx_composite1, Anx_composite2, color = Task)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_smooth(method="lm") +
  labs(x = "Block 1", y = "Block 2")

# have to take sub 87 out for correlation, has missing data


# create error-related distress
questDat = read.delim("Study1_questDat.txt")
# Add post-task questions
questDat$blockName = as.character(questDat$blockName)
questDat$blockName[questDat$blockName == "PostWITquestions"] = "WIT"
questDat$blockName[questDat$blockName == "PostAPquestions"] = "APT"
questDat$blockName = as.factor(questDat$blockName)

names(questDat)[3] = "Resp"

dat = data.frame(Subject = rep(unique(questDat$Subject), each = 2),
                 blockName = rep(c("WIT", "APT"), length(unique(questDat$Subject))))
for (i in unique(questDat$Subject)) {
  for (j in c("WIT", "APT")) {
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

#Moderation of observer effect by anxiety:**
#  Uses a multiple regression because of continuous nature of anxiety.

  lm(perfBias ~ Observer*Anx_composite, data = s2.wide) %>% 
  summary()

lm(MeanC ~ Observer*Anx_composite, data = s2.wide) %>% 
  summary()

lm(AResid ~ Observer*Anx_composite, data = s2.wide) %>% 
  summary()


### 6. Look at relationship between anxiety and PDP-A/PDP-C
#Anxiety composite scores were averaged across blocks, so each participant has one anxiety score per task.


ggplot(s2.wide, aes(scale(Anx_composite), MeanC, color = Task, shape = Task)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("black", "forestgreen")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x="Anxiety composite score", y = "PDP-C") +
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
ggsave("./Figures/AnxPredictingPDPC.tiff")

lmer(MeanC ~ scale(Anx_composite)*Task + (1|Subject), data = s2.wide) %>% summary()

ggplot(s2.wide, aes(scale(Anx_composite), AResid, color = Task, shape = Task)) +
  geom_point() +
  geom_smooth(method="lm") +
  scale_color_manual(values=c("black", "forestgreen")) +
  scale_shape_manual(values=c(1,2)) +
  labs(x="Anxiety composite score", y = "PDP-A") +
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
ggsave("./Figures/AnxPredictingPDPA.tiff")

lmer(AResid ~ scale(Anx_composite)*Task + (1|Subject), data = s2.wide) %>% summary()



