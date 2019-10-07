# Adapted from Jimmy Calanchini

install.packages(c("devtools", "runjags", "hypergeo",
                   "rjags", "RcppArmadillo", "logspline"))
devtools::install_github("denis-arnold/TreeBUGS", build_vignettes = TRUE)

#http://www.dwheck.de/files/TreeBUGS_1.3.2.zip

setwd("C:/users/calanchi/Desktop/Evaluative Learning Contest/TreeBUGS")

require("TreeBUGS")

set.seed(6789)
cond<-read.csv("Conditions.csv")
cond$condition <- as.factor(cond$condition)
cond$covariate <- rnorm(nrow(cond))

fitQUAD <- traitMPT(eqnfile="quad.eqn",
                          data="Errors.csv",
                          covData = cond,
                          n.adapt=2000,
                          n.iter=30000,
                          n.burnin=5000,
                          n.thin=2,
                          n.chains=3,
                          predStructure = list("ACwg1 ACbb1 D1 G1 OB1; condition"),
                          predType = list("f", "c")
)

#Once the analysis coverges, this command saves the MCMC chains so you can re-load them later for 
#future analyses without having to re-run the analysis. 
save(fitQUAD, file="MCMC.RData")
load("MCMC.RData")

summary(fitQUAD)

#######################################################
##Plotting the MCMC traceplot to ensure MCMC converged properly
##Click "back" on the quartz display to view all parameters
##Traceplot should look relatively random and not get "stuck" in patches of space
##Density plot should show a normal distribution for each of the model parameters
plot(fitQUAD, parameter="mean")

##Auto-correlation plot
##Autocorrelation should approach 0 over time
plot(fitQUAD, parameter="mean", type="acf")

##Gelman-Rubin plot
##Median and upper bound (97.5%) should converge on 1
##Sensitive if the thinning factor is too high
plot(fitQUAD, parameter="mean", type="gelman")

##Generates plot of fixed parameter estimates and credible intervals
##and individual point estimates
plotParam(fitQUAD,includeIndividual=TRUE)

##Distribution/histograms of individual-level parameters
plotDistribution(fitQUAD)

#######################################################
##T1 and T2 statistics for mean and covariance structure - see Klauer 2010
PPP(fitQUAD)

#######################################################
##Individual-parameter estimates
getParam(fitQUAD, parameter="theta", file="IndividualParameters.csv")

################
#Group-level parameter means

getGroupMeans(fitQUAD, factor = "condition",probit = FALSE)

groups <- getGroupMeans(fitQUAD, factor = "condition",probit = FALSE)

install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")

library("ggplot2")
library("tidyr")
library("dplyr")
as.data.frame(groups) %>% 
  mutate(names = rownames(groups)) %>% 
  separate(names, c("par", "condition"), "\\_") %>% 
  ggplot(aes(y = Mean, x = par, ymin = `2.5%`, ymax = `97.5%`, col = condition))+
  geom_point(position = position_dodge(w = .5))+
  geom_errorbar(position = position_dodge(w = .5), width = .2) +
  theme_bw()

#### on probability scale
groups <- getGroupMeans(fitQUAD, factor = "condition",probit = FALSE, mcmc=TRUE)
summarizeMCMC(groups)

# compute differences in group means (for each MCMC sample)
tp <- transformedParameters(groups, transformedParameters =
                              list(
                                "diffAb12=ACbb1_condition[1] - ACbb1_condition[2]",
                                "diffAw12=ACwg1_condition[1] - ACwg1_condition[2]",
                                "diffD12=D1_condition[1] - D1_condition[2]",
                                "diffOB12=OB1_condition[1] - OB1_condition[2]",
                                "diffG12=G1_condition[1] - G1_condition[2]"
                                ))

# get BCI etc. for the difference in group means
summarizeMCMC(tp)

# PPP values
ppp <- PPP(fitQUADretry3, M = 50)
ppp  # prints p-values for individual



