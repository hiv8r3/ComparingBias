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
