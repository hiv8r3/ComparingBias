# Adapted from Jimmy Calanchini 10/1/2019

# install.packages(c("devtools", "runjags", "hypergeo",
#                    "rjags", "RcppArmadillo", "logspline"))
# #devtools::install_github("denis-arnold/TreeBUGS", build_vignettes = TRUE)
# install.packages("TreeBUGS")


#http://www.dwheck.de/files/TreeBUGS_1.3.2.zip

#setwd("C:/users/calanchi/Desktop/Evaluative Learning Contest/TreeBUGS")

library("TreeBUGS")

# I think this is for between subs manip
# set.seed(6789)
# cond = read.csv("./MPT/TreeBUGS/Conditions.csv")
# cond$condition = as.factor(cond$condition)
# cond$covariate = rnorm(nrow(cond))

#check model
library(MPTinR)
check.mpt("./MPT/TreeBUGS/ABCmodel.eqn", restrictions.filename = NULL)

#check data
dat = read.delim("./MPT/TreeBUGS/Study1_bothTasks.csv", sep=",")


# 1. Study 1 --------------------------------------------------------------


#fit model
fitABC.s1 <- traitMPT(eqnfile="./MPT/TreeBUGS/ABCmodel.eqn",
                      data="./MPT/TreeBUGS/Study1_bothTasks_selectSubs.csv",
                      #covData = cond,
                      n.adapt=10000,
                      n.iter=210000,
                      n.burnin=30000,
                      n.thin=2,
                      n.chains=3,
                      #predStructure = list("Abapt Abwit Awapt Awwit Capt Cwit Gapt Gwit"),
                      #predType = list("c")
)

#Once the analysis coverges, this command saves the MCMC chains so you can re-load them later for 
#future analyses without having to re-run the analysis. 
save(fitABC.s1, file="ABC_Study1.RData")
load("ABC_Study1.RData")

summary(fitABC.s1)

#######################################################
##Plotting the MCMC traceplot to ensure MCMC converged properly
##Click "back" on the quartz display to view all parameters
##Traceplot should look relatively random and not get "stuck" in patches of space
##Density plot should show a normal distribution for each of the model parameters
plot(fitABC.s1, parameter="mean")

##Auto-correlation plot
##Autocorrelation should approach 0 over time
plot(fitABC.s1, parameter="mean", type="acf")

##Gelman-Rubin plot
##Median and upper bound (97.5%) should converge on 1
##Sensitive if the thinning factor is too high
plot(fitABC.s1, parameter="mean", type="gelman")

##Generates plot of fixed parameter estimates and credible intervals
##and individual point estimates
plotParam(fitABC.s1,includeIndividual=TRUE)

##Distribution/histograms of individual-level parameters
plotDistribution(fitABC.s1)

#######################################################
##T1 and T2 statistics for mean and covariance structure - see Klauer 2010
PPP(fitABC.s1)

#######################################################
##Individual-parameter estimates
getParam(fitABC.s1, parameter="theta", file="IndividualParameters.csv")

################
#Group-level parameter means

getGroupMeans(fitQUAD, factor = "condition",probit = FALSE)

groups <- getGroupMeans(fitQUAD, factor = "condition",probit = FALSE)



# 2. Study 2 --------------------------------------------------------------

#fit model
fitABC.s2 <- traitMPT(eqnfile="./MPT/TreeBUGS/ABCmodel.eqn",
                      data="./MPT/TreeBUGS/Study2_bothTasks_selectSubs.csv",
                      #covData = cond,
                      n.adapt=10000,
                      n.iter=210000,
                      n.burnin=30000,
                      n.thin=2,
                      n.chains=3,
                      #predStructure = list("Abapt Abwit Awapt Awwit Capt Cwit Gapt Gwit"),
                      #predType = list("c")
)

#Once the analysis coverges, this command saves the MCMC chains so you can re-load them later for 
#future analyses without having to re-run the analysis. 
save(fitABC.s2, file="ABC_Study2.RData")
load("ABC_Study2.RData")

summary(fitABC.s1)



# 3. Fit ABC model with A constrained to be the same across primes --------

#check model
library(MPTinR)
check.mpt("./MPT/TreeBUGS/ABCmodel_constrainA.eqn", restrictions.filename = NULL)


## Study 1 ----------
#fit model
fitABC.s1.constrainA <- traitMPT(eqnfile="./MPT/TreeBUGS/ABCmodel_constrainA.eqn",
                      data="./MPT/TreeBUGS/Study1_bothTasks_selectSubs.csv",
                      #covData = cond,
                      n.adapt=2000,
                      n.iter=30000,
                      n.burnin=5000,
                      n.thin=2,
                      n.chains=3,
                      #predStructure = list("Abapt Abwit Awapt Awwit Capt Cwit Gapt Gwit"),
                      #predType = list("c")
)

#Once the analysis coverges, this command saves the MCMC chains so you can re-load them later for 
#future analyses without having to re-run the analysis. 
save(fitABC.s1.constrainA, file="ABC_Study1_constrainA.RData")
load("ABC_Study1_constraingA.RData")

summary(fitABC.s1.constrainA)

## Study 2 ----------
#fit model
fitABC.s2.constrainA <- traitMPT(eqnfile="./MPT/TreeBUGS/ABCmodel_constrainA.eqn",
                                 data="./MPT/TreeBUGS/Study2_bothTasks_selectSubs.csv",
                                 #covData = cond,
                                 n.adapt=2000,
                                 n.iter=30000,
                                 n.burnin=5000,
                                 n.thin=2,
                                 n.chains=3,
                                 #predStructure = list("Abapt Abwit Awapt Awwit Capt Cwit Gapt Gwit"),
                                 #predType = list("c")
)

#Once the analysis coverges, this command saves the MCMC chains so you can re-load them later for 
#future analyses without having to re-run the analysis. 
save(fitABC.s2.constrainA, file="ABC_Study2_constrainA.RData")
load("ABC_Study2_constraingA.RData")

summary(fitABC.s2.constrainA)


