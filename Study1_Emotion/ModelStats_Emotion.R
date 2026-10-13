###Model comparisons

library(BEST)
library(hBayesDM)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(ggstatsplot)
library(tidyverse)
library(R.matlab)
library(pastecs)
library(rstatix)
library(emmeans)
library(afex)
library(pastecs)
library(bayesplot)
library(ggdist)

###load parameters 
setwd("~/data/RLHabituation/RLVarBayes")

numsubs = 137

###comparison of model fitting 

load("looQakF80")
looQakF80 <- looQakF
load("looQarF80")
looQarF80 <- looQarF

print(looQakF80, simplify = FALSE)
print(looQarF80, simplify = FALSE)

load("looQakF40")
looQakF40 <- looQakF

print(looQakF40, simplify = FALSE)

looQakF40$estimates[1,1]/40
looQakF80$estimates[1,1]/80

## test for individual participants having better loo

looTrialsakDF <- looQakF80$pointwise[,1] - looQarF80$pointwise[,1]
colSums(looTrialsakdfABR > 0)


##test differences in beta as a function of trial numbers

parFCovar <- matrix(data=NA, nrow=length(numTrials), ncol=20000)

for (nt in 1:length(numTrials)) {
  par <- readMat(paste0("Qparams_AK_Fam_Covar", numTrials[nt], ".mat"))
  parFCovar[nt,] <- par$beta.rating.QakF
  }
  
##rating
samples <- list(par1=parFCovar[1,], par2=parFCovar[2,], par3=parFCovar[3,], par4=parFCovar[4,], par5=parFCovar[5,], 
                par6=parFCovar[6,], par7=parFCovar[7,], par8=parFCovar[8,], par9=parFCovar[9,])
nt <- length(samples)

max_val <- max(abs(unlist(samples)))

x_limits <- c(-max_val, max_val)

samples_df <- data.frame(Trials_40 = parFCovar[1,], Trials_45 =parFCovar[2,], Trials_50 =parFCovar[3,], Trials_55 =parFCovar[4,], 
                         Trials_60 =parFCovar[5,], Trials_65 =parFCovar[6,], Trials_70 =parFCovar[7,], Trials_75 =parFCovar[8,], Trials_80 =parFCovar[9,])

mcmc_areas(samples_df, prob=.95) + 
  coord_cartesian(xlim = x_limits) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") 



###INFERENTIAL ANALYSES

### load explicit ratings

ratings <- read.csv("/Users/emolab/data/RLHabituation/runningData/RatingData/RatingsAverages.csv", header=FALSE)

ePosDat <- data.frame(ratings[2], "Positive")
names(ePosDat) <- c("value", "param")
eNeuDat <- data.frame(ratings[3], "Neutral")
names(eNeuDat) <- c("value", "param")
eNegDat <- data.frame(ratings[4], "Negative")
names(eNegDat) <- c("value", "param")

eMVDiff <- ePosDat$value - eNegDat$value

eAllDat <- rbind(ePosDat, eNeuDat, eNegDat)

stat.desc(ePosDat)
stat.desc(eNeuDat)
stat.desc(eNegDat)

t.test(ePosDat$value, eNeuDat$value, paired=TRUE, var.equal = TRUE)
t.test(eNeuDat$value, eNegDat$value, paired=TRUE, var.equal = TRUE)

t.test(ePosDat$value[ratings[5] ==1], ePosDat$value[ratings[5] == 2], var.equal = TRUE)
t.test(eNeuDat$value[ratings[5] ==1], eNeuDat$value[ratings[5] == 2], var.equal = TRUE)
t.test(eNegDat$value[ratings[5] ==1], eNegDat$value[ratings[5] == 2], var.equal = TRUE)

##plots of explicit ratings

ePosNegDat <- ePosDat
ePosNegDat <- rbind(ePosNegDat, eNegDat)

ggplot(ePosNegDat, aes(x = param, y = value, fill=param)) +
  geom_violin(position = position_dodge(0.9)) +
  geom_boxplot(width = 0.2, position = position_dodge(0.9)) +
  labs(x = "Valence", y = "Explicit ratings") +
  theme_classic()+
  theme(legend.position = "none")



### HDI on parameter differences

#############################################################
#MAKE SURE YOU GET THE RIGHT SET OF PARAMS HERE FOR 40 VS. 80 TRIAL VERSION

#80 trial version
parF <- readMat("Qparams_AK_Fam80.mat")
parU <- readMat("Qparams_AK_UnFam80.mat")
parFCovar <- readMat("Qparams_AK_Fam_Covar80.mat")

#40 trial version
parF <- readMat("Qparams_AK_Fam40.mat")
parU <- readMat("Qparams_AK_UnFam40.mat")
parFCovar <- readMat("Qparams_AK_Fam_Covar40.mat")
############################################################

PosNegFdiff <- parF$mu.kPos.QakF - parF$mu.kNeg.QakF
HDIofMCMC(PosNegFdiff)
p_direction(PosNegFdiff)
HDIofMCMC(parF$mu.kPos.QakF)
p_direction(parF$mu.kPos.QakF)
HDIofMCMC(parF$mu.kNeg.QakF)
p_direction(parF$mu.kNeg.QakF)

PosNegUdiff <- parU$mu.kPos.QakU - parU$mu.kNeg.QakU
HDIofMCMC(PosNegUdiff)
p_direction(PosNegFdiff)
HDIofMCMC(parU$mu.kPos.QakU)
p_direction(parU$mu.kPos.QakU)
HDIofMCMC(parU$mu.kNeg.QakU)
p_direction(parU$mu.kNeg.QakU)

HDIofMCMC(parFCovar$beta.rating.QakF)
p_direction(parFCovar$beta.rating.QakF)


##PLOT cRVs

samples_cRV <- data.frame("PosF" = parF$mu.kPos.QakF, "NegF" = parF$mu.kNeg.QakF, "PosU" = parU$mu.kPos.QakU, "NegU" = parU$mu.kNeg.QakU)

cRV_long <- samples_cRV %>%
  pivot_longer(cols = c(PosF, NegF, PosU, NegU), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(
    Valence = case_when(
      parameter %in% c("PosF", "PosU") ~ "Positive",
      parameter %in% c("NegF", "NegU") ~ "Negative"
    ),
    Familiarity = case_when(
      parameter %in% c("PosF", "NegF") ~ "Familiar",
      parameter %in% c("PosU", "NegU") ~ "Novel"
    )
  )


ggplot(cRV_long, aes(x = Familiarity, y = value, fill=Valence)) +
  stat_halfeye(
    point_interval = median_hdi, 
    .width = 0.95, 
    alpha = 0.7, 
    position = position_dodge(width = 0.5) 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(x = "Familiarity", y = "cRV") +
  theme_classic()



