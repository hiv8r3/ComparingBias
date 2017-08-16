# Calculate alphas for self-report questionnaires in each study
# Need to make sure ggplot2 is not attached in order for this to work
require(psych)

# Study 1 -----------------------------------------------------------------

MTCP = read.delim("./Data/Study I/MTCPscores.txt")
# includes 5 subjects added after semester was over: 37, 45, 70, 73, 85

# reverse score IMS_1
MTCP$IMS_1.rev = 10 - MTCP$IMS_1

# create composite scores for IMS and EMS
MTCP = mutate(MTCP, IMS = (MTCP$IMS_1.rev + MTCP$IMS_2 + MTCP$IMS_3 + MTCP$IMS_4 + MTCP$IMS_5)/5)
MTCP = mutate(MTCP, EMS = (MTCP$EMS_1 + MTCP$EMS_2 + MTCP$EMS_3 + MTCP$EMS_4 + MTCP$EMS_5)/5)
# manually add IMS/EMS scores for subject 15 and 59 because of missing data on one item
MTCP$EMS[MTCP$Subject == 15] = 1.8
MTCP$IMS[MTCP$Subject == 59] = 2.3

# calculate alphas for IMS and EMS
IMS = select(MTCP, starts_with("IMS")) %>%
  select(-IMS) %>%
  select(-IMS_1)

EMS = select(MTCP, starts_with("EMS")) %>%
  select(-EMS)

alpha(IMS)
alpha(EMS)


# Study 2 -----------------------------------------------------------------

qualDat = read.delim("./Data/Study II/QualtricsData.txt", stringsAsFactors = F)
qualDat = qualDat[order(qualDat$Subject),]

# Calc IMS/EMS scores -----------------------------------------------------

# reverse score IMS_1
qualDat$IMS_1.rev = 10 - qualDat$IMS_1

# create composite scores for IMS and EMS
qualDat = mutate(qualDat, IMS = (qualDat$IMS_1.rev + qualDat$IMS_2 + qualDat$IMS_3 + qualDat$IMS_4 + qualDat$IMS_5)/5)
qualDat = mutate(qualDat, EMS = (qualDat$EMS_1 + qualDat$EMS_2 + qualDat$EMS_3 + qualDat$EMS_4 + qualDat$EMS_5)/5)

# Make IMS-EMS diff score
qualDat = mutate(qualDat, IMS.EMS.diff = IMS - EMS)

# calculate alphas for IMS and EMS
IMS = select(qualDat, starts_with("IMS")) %>%
  select(-IMS) %>%
  select(-IMS_1) %>% 
  select(-IMS.EMS.diff)

EMS = select(qualDat, starts_with("EMS")) %>%
  select(-EMS)

alpha(IMS) # .85
alpha(EMS) # .80

# correlation between IMS and EMS
cor.test(qualDat$IMS, qualDat$EMS)

# Calc social anxiety scores ----------------------------------------------

# reverse score SA_3
qualDat$SA_3.rev = 7 - qualDat$SA_3

# create composite scores for SA
qualDat = mutate(qualDat, SA = (qualDat$SA_1 + qualDat$SA_2 + qualDat$SA_3.rev + qualDat$SA_4 + qualDat$SA_5)/5)

# calculate alpha for SA
SA = select(qualDat, starts_with("SA")) %>%
  select(-SA_3) %>%
  select(-SA) 

alpha(SA) # .81

