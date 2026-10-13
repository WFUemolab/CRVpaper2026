
library(rjags)
library(BEST)
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(ggstatsplot)
library(tidyverse)
library(R.matlab)
library(loo)
library(hBayesDM)
library(rstantools)
library(PMCMRplus)
library(bayestestR)
library(ggdist)
library(patchwork)

setwd("~/data/RLFaces")

### load explicit ratings

ExpDat <- read.csv('ExplicitRatings.csv', header=TRUE)

ExpDatF <- ExpDat[attFIndx,]
ExpDatM <- ExpDat[attMIndx,]

ExpDatL <- ExpDat

ExpDatL <- pivot_longer(
  ExpDatL,
  cols = c("HAttvF", "HAttvM", "MAttvF", "MAttvM", "LAttvF", "LAttvM"),
  names_to = c("Attract", "Gender"),
  names_sep = "Attv",
)

## Explicit ratings Anovas

Exp.aov2 <- aov_ez("X", "value", ExpDatL, within = c("Attract", "Gender"), between = "F1M2", anova_table=list(es="pes"))
Exp.aov2
Exp.mn1 <- emmeans(Exp.aov2, ~Attract)
Exp.mn1
Exp.mn2 <- emmeans(Exp.aov2, ~Attract +Gender +F1M2)
Exp.mn2

attConHM <- c(1, -1, 0, 1, -1, 0, 1, -1, 0, 1, -1, 0)
attConML <- c(0, 1, -1, 0, 1, -1, 0, 1, -1, 0, 1, -1)

attHM <- contrast(Exp.mn2, list("interaction" = attConHM))
attML <- contrast(Exp.mn2, list("interaction" = attConML))

## separate ANOVAs for each face gender


ExpF.aov2 <- aov_ez("X", "value", subset(ExpDatL, Gender == "F"), within = "Attract", between = "F1M2", anova_table=list(es="pes"))
ExpM.aov2 <- aov_ez("X", "value", subset(ExpDatL, Gender == "M"), within = "Attract", between = "F1M2", anova_table=list(es="pes"))

## follow up with male-faces (where there was an interaction)

ExpM.mn <- emmeans(ExpM.aov2, ~Attract +F1M2)
conMale <- contrast(ExpM.mn, method="trt.vs.ctrl", simple="F1M2")

confint(conMale)

effectsize(t.test(ExpDatF$HAttvM, ExpDatM$HAttvM, var.equal=TRUE))
effectsize(t.test(ExpDatF$MAttvM, ExpDatM$MAttvM, var.equal=TRUE))
effectsize(t.test(ExpDatF$LAttvM, ExpDatM$LAttvM, var.equal=TRUE))


###load cRV parameters 
##pick from the 80 trial or 40 trial version

# ##80 trial
# parF <- readMat("Qparams_ak_F80.mat")
# parM <- readMat("Qparams_ak_M80.mat")
# 
# ##40 trial
# parF <- readMat("Qparams_ak_F40.mat")
# parM <- readMat("Qparams_ak_M40.mat")

### Trying new master-stan models

load("results_base_M")
load("results_base_F")
load("results_base_M_group")
load("results_base_M_group")


### testing models (40 trials)
### female faces

loo_compare(results_base_F$loo, results_base_F_group$loo)

FHiLowDiff <- results_base_F$pars$mu_kHi - results_base_F$pars$mu_kLow
HDIofMCMC(FHiLowDiff)
p_direction(FHiLowDiff)

HDIofMCMC(results_base_F$pars$mu_kHi)
p_direction(results_base_F$pars$mu_kHi)
HDIofMCMC(results_base_F$pars$mu_kLow)
p_direction(results_base_F$pars$mu_kLow)

##separated by groups
#female attracted = 1, male attracted = 2

FFHiLowDiff <- results_base_F_group$pars$mu_kHi[,1] - results_base_F_group$pars$mu_kLow[,1]
HDIofMCMC(FFHiLowDiff)
p_direction(FFHiLowDiff)

HDIofMCMC(results_base_F_group$pars$mu_kHi[,1])
p_direction(results_base_F_group$pars$mu_kHi[,1])
HDIofMCMC(results_base_F_group$pars$mu_kLow[,1])
p_direction(results_base_F_group$pars$mu_kLow[,1])

FMHiLowDiff <- results_base_F_group$pars$mu_kHi[,2] - results_base_F_group$pars$mu_kLow[,2]
HDIofMCMC(FMHiLowDiff)
p_direction(FMHiLowDiff)

HDIofMCMC(results_base_F_group$pars$mu_kHi[,2])
p_direction(results_base_F_group$pars$mu_kHi[,2])
HDIofMCMC(results_base_F_group$pars$mu_kLow[,2])
p_direction(results_base_F_group$pars$mu_kLow[,2])

### testing models
### Male faces

loo_compare(results_base_M$loo, results_base_M_group$loo)

MHiLowDiff <- results_base_M$pars$mu_kHi - results_base_M$pars$mu_kLow
HDIofMCMC(MHiLowDiff)
p_direction(MHiLowDiff)

HDIofMCMC(rowMeans(results_base_M$pars$mu_kHi))
p_direction(results_base_M$pars$mu_kHi)
HDIofMCMC(results_base_M$pars$mu_kLow)
p_direction(results_base_M$pars$mu_kLow)

##separated by groups
#female attracted = 1, male attracted = 2

MFHiLowDiff <- results_base_M_group$pars$mu_kHi[,1] - results_base_M_group$pars$mu_kLow[,1]
HDIofMCMC(MFHiLowDiff)
p_direction(MFHiLowDiff)

HDIofMCMC(results_base_M_group$pars$mu_kHi[,1])
p_direction(results_base_M_group$pars$mu_kHi[,1])
HDIofMCMC(results_base_M_group$pars$mu_kLow[,1])
p_direction(results_base_M_group$pars$mu_kLow[,1])

MMHiLowDiff <- results_base_M_group$pars$mu_kHi[,2] - results_base_M_group$pars$mu_kLow[,2]
HDIofMCMC(MMHiLowDiff)
p_direction(MMHiLowDiff)

HDIofMCMC(results_base_M_group$pars$mu_kHi[,2])
p_direction(results_base_M_group$pars$mu_kHi[,2])
HDIofMCMC(results_base_M_group$pars$mu_kLow[,2])
p_direction(results_base_M_group$pars$mu_kLow[,2])


### betas of explicit ratings on crvs

load("results_covar_M")
load("results_covar_F")

HDIofMCMC(results_covar_F$pars$beta_exp)
p_direction(results_covar_F$pars$beta_exp)

HDIofMCMC(results_covar_M$pars$beta_exp)
p_direction(results_covar_M$pars$beta_exp)


###Graphs of CRVs

## Coding system -- GENDER FACE - GENDER Attraction
## female faces

samples_cRV_F <- data.frame("HiFF" = results_base_F_group$pars$mu_kHi[,1], "LoFF" = results_base_F_group$pars$mu_kLow[,1], "HiFM" = results_base_F_group$pars$mu_kHi[,2], "LoFM" = results_base_F_group$pars$mu_kLow[,2])

cRV_long_F <- samples_cRV_F %>%
  pivot_longer(cols = c(HiFF, LoFF, HiFM, LoFM), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(
    Attraction = case_when(
      parameter %in% c("HiFF", "LoFF") ~ "Gynephilic",
      parameter %in% c("HiFM", "LoFM") ~ "Androphilic"
    ),
    FaceAttractiveness = case_when(
      parameter %in% c("HiFF", "HiFM") ~ "High",
      parameter %in% c("LoFF", "LoFM") ~ "Low"
    )
  )

cRV_long_F$FaceAttractiveness <- factor(
  cRV_long_F$FaceAttractiveness, 
  levels = c("Low", "High") 
)

FFplot <- ggplot(cRV_long_F, aes(x = FaceAttractiveness, y = value, fill=Attraction)) +
  stat_halfeye(
    point_interval = median_hdi, 
    .width = 0.95, 
    alpha = 0.7, 
    position = position_dodge(width = 0.5) 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Female Faces", x = "Face Attractiveness", y = "cRV") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


##male faces
samples_cRV_M <- data.frame("HiMF" = results_base_M_group$pars$mu_kHi[,1], "LoMF" = results_base_M_group$pars$mu_kLow[,1], "HiMM" = results_base_M_group$pars$mu_kHi[,2], "LoMM" = results_base_M_group$pars$mu_kLow[,2])


cRV_long_M <- samples_cRV_M %>%
  pivot_longer(cols = c(HiMF, LoMF, HiMM, LoMM), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(
    Attraction = case_when(
      parameter %in% c("HiMF", "LoMF") ~ "Gynephilic",
      parameter %in% c("HiMM", "LoMM") ~ "Androphilic"
    ),
    FaceAttractiveness = case_when(
      parameter %in% c("HiMF", "HiMM") ~ "High",
      parameter %in% c("LoMF", "LoMM") ~ "Low"
    )
  )

cRV_long_M$FaceAttractiveness <- factor(
  cRV_long_M$FaceAttractiveness, 
  levels = c("Low", "High")
)

MMplot <- ggplot(cRV_long_M, aes(x = FaceAttractiveness, y = value, fill=Attraction)) +
  stat_halfeye(
    point_interval = median_hdi, 
    .width = 0.95, 
    alpha = 0.7, 
    position = position_dodge(width = 0.5) 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Male Faces", x = "Face Attractiveness", y = "cRV") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


combined_plots <- FFplot + MMplot +
  plot_layout(guides = "collect")

combined_plots


##########80 trials for SM

load("results80_base_M")
load("results80_base_F")
load("results80_base_M_group")
load("results80_base_M_group")

### testing models
### female faces


FHiLowDiff <- results80_base_F$pars$mu_kHi - results80_base_F$pars$mu_kLow
HDIofMCMC(FHiLowDiff)
p_direction(FHiLowDiff)

HDIofMCMC(results80_base_F$pars$mu_kHi)
p_direction(results80_base_F$pars$mu_kHi)
HDIofMCMC(results80_base_F$pars$mu_kLow)
p_direction(results80_base_F$pars$mu_kLow)

##separated by groups
#female attracted = 1, male attracted = 2

FFHiLowDiff <- results80_base_F_group$pars$mu_kHi[,1] - results80_base_F_group$pars$mu_kLow[,1]
HDIofMCMC(FFHiLowDiff)
p_direction(FFHiLowDiff)

HDIofMCMC(results80_base_F_group$pars$mu_kHi[,1])
p_direction(results80_base_F_group$pars$mu_kHi[,1])
HDIofMCMC(results80_base_F_group$pars$mu_kLow[,1])
p_direction(results80_base_F_group$pars$mu_kLow[,1])

FMHiLowDiff <- results80_base_F_group$pars$mu_kHi[,2] - results80_base_F_group$pars$mu_kLow[,2]
HDIofMCMC(FMHiLowDiff)
p_direction(FMHiLowDiff)

HDIofMCMC(results80_base_F_group$pars$mu_kHi[,2])
p_direction(results80_base_F_group$pars$mu_kHi[,2])
HDIofMCMC(results80_base_F_group$pars$mu_kLow[,2])
p_direction(results80_base_F_group$pars$mu_kLow[,2])

### testing models
### Male faces

MHiLowDiff <- results80_base_M$pars$mu_kHi - results80_base_M$pars$mu_kLow
HDIofMCMC(MHiLowDiff)
p_direction(MHiLowDiff)

HDIofMCMC(rowMeans(results80_base_M$pars$mu_kHi))
p_direction(results80_base_M$pars$mu_kHi)
HDIofMCMC(results80_base_M$pars$mu_kLow)
p_direction(results80_base_M$pars$mu_kLow)

##separated by groups
#female attracted = 1, male attracted = 2

MFHiLowDiff <- results80_base_M_group$pars$mu_kHi[,1] - results80_base_M_group$pars$mu_kLow[,1]
HDIofMCMC(MFHiLowDiff)
p_direction(MFHiLowDiff)

HDIofMCMC(results80_base_M_group$pars$mu_kHi[,1])
p_direction(results80_base_M_group$pars$mu_kHi[,1])
HDIofMCMC(results80_base_M_group$pars$mu_kLow[,1])
p_direction(results80_base_M_group$pars$mu_kLow[,1])

MMHiLowDiff <- results80_base_M_group$pars$mu_kHi[,2] - results80_base_M_group$pars$mu_kLow[,2]
HDIofMCMC(MMHiLowDiff)
p_direction(MMHiLowDiff)

HDIofMCMC(results80_base_M_group$pars$mu_kHi[,2])
p_direction(results80_base_M_group$pars$mu_kHi[,2])
HDIofMCMC(results80_base_M_group$pars$mu_kLow[,2])
p_direction(results80_base_M_group$pars$mu_kLow[,2])


### betas of explicit ratings on crvs
load("results80_covar_M")
load("results80_covar_F")

HDIofMCMC(results80_covar_F$pars$beta_exp)
p_direction(results80_covar_F$pars$beta_exp)

HDIofMCMC(results80_covar_M$pars$beta_exp)
p_direction(results80_covar_M$pars$beta_exp)



