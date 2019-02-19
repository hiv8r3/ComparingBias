require(dplyr)

pdpWIT = read.delim("Study1_pdpEstimates_WIT.txt")
pdpAPT = read.delim("Study1_pdpEstimates_APT.txt")


# WIT ---------------------------------------------------------------------

# check number of subs w/o IMS/EMS
pdpWIT[is.na(pdpWIT$IMS),] #7 subs
# subs 3, 27, 82, 88, 89, 90, 101
noIMS = unique(pdpWIT$Subject[is.na(pdpWIT$IMS)])

# subs not included in submission
bs = read.delim("Study1_badSubsWIT.txt")
subBS = bs$Subject

# not native english speakers
notNativeEng = bs$Subject[1:5]

# definite exclusions
defExSs = bs$Subject[6:7]

 
# Correlation tables ------------------------------------------------------

# taking people with no IMS out
all.WIT = 
round(cor(filter(pdpWIT, !(Subject %in% noIMS)) %>% 
            select(-Subject, -Task, -Observer)), digits = 2)

# original bad subs
original.WIT = 
  round(cor(filter(pdpWIT, !(Subject %in% noIMS) & !(Subject %in% subBS)) %>% 
            select(-Subject, -Task, -Observer)), digits = 2)

# only definite exclusions
defEx.WIT = 
  round(cor(filter(pdpWIT, !(Subject %in% noIMS) & !(Subject %in% defExSs)) %>% 
            select(-Subject, -Task, -Observer), method = "pearson"), digits = 2)


# Specific tests ---------------------------------------------------

# cor.test automatically drops rows with NA in one of the values, so this is equivalent to without subsetting out subs with not IMS

# check difference between A_white/A_black estimates in original vs defEx
cor.test(pdpWIT$A_black[!(pdpWIT$Subject %in% subBS)], 
         pdpWIT$A_gun_white[!(pdpWIT$Subject %in% subBS)], method = "pearson")

cor.test(pdpWIT$A_black[!(pdpWIT$Subject %in% defExSs)], 
         pdpWIT$A_gun_white[!(pdpWIT$Subject %in% defExSs)], method = "pearson")

# why does this not match up with correlation table??????


# APT ---------------------------------------------------------------------

# check number of subs w/o IMS/EMS
pdpAPT[is.na(pdpAPT$IMS),] #7 subs
# subs 3, 27, 82, 88, 89, 90, 101
noIMS = unique(pdpAPT$Subject[is.na(pdpAPT$IMS)])

# subs not included in submission
bs = read.delim("Study1_badSubsAP.txt")
subBS = bs$Subject

# not native english speakers
notNativeEng = bs$Subject[1:5]

# definitely exclude
defExSs = bs$Subject[7]

# Correlation tables ------------------------------------------------------

# taking people with no IMS out
all.APT = 
round(cor(filter(pdpAPT, !(Subject %in% noIMS)) %>% 
            select(-Subject, -Task, -Observer)), digits = 2)

# original bad subs
original.APT = 
round(cor(filter(pdpAPT, !(Subject %in% noIMS) & !(Subject %in% defEx)) %>% 
            select(-Subject, -Task, -Observer)), digits = 2)

# only definite exclusions
defEx.APT = 
round(cor(filter(pdpAPT, !(Subject %in% noIMS) & !(Subject %in% defEx)) %>% 
            select(-Subject, -Task, -Observer)), digits = 2)


# Specific tests ---------------------------------------------------

# original bad subs
cor.test(pdpAPT$C_black[!(pdpAPT$Subject %in% subBS)], 
         pdpAPT$IMS[!(pdpAPT$Subject %in% subBS)])

# only definite exclusions
cor.test(pdpAPT$C_black[!(pdpAPT$Subject %in% defExSs)], 
         pdpAPT$IMS[!(pdpAPT$Subject %in% defExSs)])

cor.test(pdpAPT$C_black[!(pdpAPT$Subject %in% defExSs)], 
         pdpAPT$EMS[!(pdpAPT$Subject %in% defExSs)])

cor.test(pdpAPT$A_neg_black[!(pdpAPT$Subject %in% defExSs)], 
         pdpAPT$EMS[!(pdpAPT$Subject %in% defExSs)])
