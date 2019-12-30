library(dplyr)

# Creates performance bias scores (errors|incongruent - errors|congruent)

# Study 1 -----------------------------------------------------------------

dat = read.delim("Study1_accuracy.txt")

APTdat = filter(dat, Task == "APT")
WITdat = filter(dat, Task == "WIT")

perfBiasdat = NULL
for (i in unique(APTdat$Subject)) {
  # APT
  temp = APTdat[APTdat$Subject == i,]
  perfBiasAPT = mean(temp$Accuracy[temp$ConType == "incongruent"]) - mean(temp$Accuracy[temp$ConType == "congruent"])
  # WIT
  temp = WITdat[WITdat$Subject == i,]
  perfBiasWIT = mean(temp$Accuracy[temp$ConType == "incongruent"]) - mean(temp$Accuracy[temp$ConType == "congruent"])
  # add to data frame
  perfBiasdat = rbind(perfBiasdat, 
                      data.frame(Subject = i,
                                 APperfBias = perfBiasAPT,
                                 WITperfBias = perfBiasWIT))

}

write.table(perfBiasdat, "Study1_perfBias.txt", sep="\t", row.names=F)


# Study 2 -----------------------------------------------------------------


dat = read.delim("Study2_accuracy.txt")

APTdat = filter(dat, Task == "APT")
WITdat = filter(dat, Task == "WIT")

perfBiasdat = NULL
for (i in unique(APTdat$Subject)) {
  # APT
  temp = APTdat[APTdat$Subject == i,]
  perfBiasAPT = mean(temp$Accuracy[temp$ConType == "incongruent"]) - mean(temp$Accuracy[temp$ConType == "congruent"])
  # WIT
  temp = WITdat[WITdat$Subject == i,]
  perfBiasWIT = mean(temp$Accuracy[temp$ConType == "incongruent"]) - mean(temp$Accuracy[temp$ConType == "congruent"])
  # add to data frame
  perfBiasdat = rbind(perfBiasdat, 
                      data.frame(Subject = i,
                                 APperfBias = perfBiasAPT,
                                 WITperfBias = perfBiasWIT))
  
}

write.table(perfBiasdat, "Study2_perfBias.txt", sep="\t", row.names=F)




