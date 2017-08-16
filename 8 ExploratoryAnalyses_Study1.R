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
