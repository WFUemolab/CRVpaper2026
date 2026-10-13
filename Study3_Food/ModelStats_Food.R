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
library(effectsize)
library(psych)
library(mediation)
library(ggdist)
library(patchwork)

###load parameters 
setwd("/Users/emolab/data/RLFood")



#### Get Subject Info


IDs <- read.csv("subNamesData.csv")
IDs <- IDs$x
expData <- read.csv("TaskFinalData.csv")
conds <- expData$Condition


###checking models without and without condition

load("results_base_F")
load("results_base_E")
load("results_gropu_F")
load("results_group_E")

loo_compare(results_base_F$loo, results_group_F$loo)
loo_compare(results_base_E$loo, results_group_E$loo)

##### CRV of food and emo

### 1 = Disgust, 2 = Popcorn, 3 = No Smell

### Food
### difference between groups with follow-ups per group

### Hi vs. Low
HiLowFooddiff <- results_base_F$pars$mu_kHi - results_base_F$pars$mu_kLow
HDIofMCMC(HiLowFooddiff)
p_direction(HiLowFooddiff)

HiLowFooddiffg1 <- results_group_F$pars$mu_kHi[,1] - results_group_F$pars$mu_kLow[,1] 
HiLowFooddiffg2 <- results_group_F$pars$mu_kHi[,2] - results_group_F$pars$mu_kLow[,2] 
HiLowFooddiffg3 <- results_group_F$pars$mu_kHi[,3] - results_group_F$pars$mu_kLow[,3] 

HDIofMCMC(HiLowFooddiffg1)
p_direction(HiLowFooddiffg1)
HDIofMCMC(HiLowFooddiffg2)
p_direction(HiLowFooddiffg2)
HDIofMCMC(HiLowFooddiffg3)
p_direction(HiLowFooddiffg3)

## Hi/Low vs. mid

HDIofMCMC(results_base_F$pars$mu_kHi)
p_direction(results_base_F$pars$mu_kHi)
HDIofMCMC(results_base_F$pars$mu_kLow)
p_direction(results_base_F$pars$mu_kLow)


### HvL as different b/n groups:

HiLowFooddiffg1g2 <- HiLowFooddiffg1 - HiLowFooddiffg2
HiLowFooddiffg1g3 <- HiLowFooddiffg1 - HiLowFooddiffg3
HiLowFooddiffg2g3 <- HiLowFooddiffg2 - HiLowFooddiffg3

HDIofMCMC(HiLowFooddiffg1g2)
p_direction(HiLowFooddiffg1g2)
HDIofMCMC(HiLowFooddiffg1g3)
p_direction(HiLowFooddiffg1g3)
HDIofMCMC(HiLowFooddiffg2g3)
p_direction(HiLowFooddiffg2g3)

### High and low (relative to 0) by different groups

HiFooddiffg1g2 <- results_group_F$pars$mu_kHi[,1] - results_group_F$pars$mu_kHi[,2] 
HiFooddiffg1g3 <- results_group_F$pars$mu_kHi[,1] - results_group_F$pars$mu_kHi[,3] 
HiFooddiffg2g3 <- results_group_F$pars$mu_kHi[,2] - results_group_F$pars$mu_kHi[,3] 

HDIofMCMC(HiFooddiffg1g2)
p_direction(HiFooddiffg1g2)
HDIofMCMC(HiFooddiffg1g3)
p_direction(HiFooddiffg1g3)
HDIofMCMC(HiFooddiffg2g3)
p_direction(HiFooddiffg2g3)

LowFooddiffg1g2 <- results_group_F$pars$mu_kLow[,1] - results_group_F$pars$mu_kLow[,2] 
LowFooddiffg1g3 <- results_group_F$pars$mu_kLow[,1] - results_group_F$pars$mu_kLow[,3] 
LowFooddiffg2g3 <- results_group_F$pars$mu_kLow[,2] - results_group_F$pars$mu_kLow[,3] 

HDIofMCMC(LowFooddiffg1g2)
p_direction(LowFooddiffg1g2)
HDIofMCMC(LowFooddiffg1g3)
p_direction(LowFooddiffg1g3)
HDIofMCMC(LowFooddiffg2g3)
p_direction(LowFooddiffg2g3)


### Possible Initial V differences?

MidFooddiffg1g2 <- results_group_F$pars$mu_kMid[,1] - results_group_F$pars$mu_kMid[,2]
MidFooddiffg1g3 <- results_group_F$pars$mu_kMid[,1] - results_group_F$pars$mu_kMid[,3]
MidFooddiffg2g3 <- results_group_F$pars$mu_kMid[,2] - results_group_F$pars$mu_kMid[,3]

HDIofMCMC(MidFooddiffg1g2)
p_direction(MidFooddiffg1g2)
HDIofMCMC(MidFooddiffg1g3)
p_direction(MidFooddiffg1g3)
HDIofMCMC(MidFooddiffg2g3)
p_direction(MidFooddiffg2g3)

### difference between groups with follow-ups per group
### Emo
### Hi vs. Low
HiLowEmodiff <- results_base_E$pars$mu_kHi - results_base_E$pars$mu_kLow
HDIofMCMC(HiLowEmodiff)
p_direction(HiLowEmodiff)

HiLowEmodiffg1 <- results_group_E$pars$mu_kHi[,1] - results_group_E$pars$mu_kLow[,1] 
HiLowEmodiffg2 <- results_group_E$pars$mu_kHi[,2] - results_group_E$pars$mu_kLow[,2] 
HiLowEmodiffg3 <- results_group_E$pars$mu_kHi[,3] - results_group_E$pars$mu_kLow[,3] 

HDIofMCMC(HiLowEmodiffg1)
p_direction(HiLowEmodiffg1)
HDIofMCMC(HiLowEmodiffg2)
p_direction(HiLowEmodiffg2)
HDIofMCMC(HiLowEmodiffg3)
p_direction(HiLowEmodiffg3)

### HvL as different b/n groups:

HiLowEmodiffg1g2 <- HiLowEmodiffg1 - HiLowEmodiffg2
HiLowEmodiffg1g3 <- HiLowEmodiffg1 - HiLowEmodiffg3
HiLowEmodiffg2g3 <- HiLowEmodiffg2 - HiLowEmodiffg3

HDIofMCMC(HiLowEmodiffg1g2)
p_direction(HiLowEmodiffg1g2)
HDIofMCMC(HiLowEmodiffg1g3)
p_direction(HiLowEmodiffg1g3)
HDIofMCMC(HiLowEmodiffg2g3)
p_direction(HiLowEmodiffg2g3)

### High and low (relative to 0) by different groups

HiEmodiffg1g2 <- results_group_E$pars$mu_kHi[,1] - results_group_E$pars$mu_kHi[,2] 
HiEmodiffg1g3 <- results_group_E$pars$mu_kHi[,1] - results_group_E$pars$mu_kHi[,3] 
HiEmodiffg2g3 <- results_group_E$pars$mu_kHi[,2] - results_group_E$pars$mu_kHi[,3] 

HDIofMCMC(HiEmodiffg1g2)
p_direction(HiEmodiffg1g2)
HDIofMCMC(HiEmodiffg1g3)
p_direction(HiEmodiffg1g3)
HDIofMCMC(HiEmodiffg2g3)
p_direction(HiEmodiffg2g3)

LowEmodiffg1g2 <- results_group_E$pars$mu_kLow[,1] - results_group_E$pars$mu_kLow[,2] 
LowEmodiffg1g3 <- results_group_E$pars$mu_kLow[,1] - results_group_E$pars$mu_kLow[,3] 
LowEmodiffg2g3 <- results_group_E$pars$mu_kLow[,2] - results_group_E$pars$mu_kLow[,3] 

HDIofMCMC(LowEmodiffg1g2)
p_direction(LowEmodiffg1g2)
HDIofMCMC(LowEmodiffg1g3)
p_direction(LowEmodiffg1g3)
HDIofMCMC(LowEmodiffg2g3)
p_direction(LowEmodiffg2g3)

###Graphs of CRVs


samples_cRV_Food <- data.frame("DisHiFood" = results_group_F$pars$mu_kHi[,1], "DisLowFood" = results_group_F$pars$mu_kLow[,1], 
                               "PopHiFood" = results_group_F$pars$mu_kHi[,2], "PopLowFood" = results_group_F$pars$mu_kLow[,2],
                               "NoHiFood" = results_group_F$pars$mu_kHi[,3], "NoLowFood" = results_group_F$pars$mu_kLow[,3])

cRV_long_Food <- samples_cRV_Food %>%
  pivot_longer(cols = c(DisHiFood, DisLowFood, PopHiFood, PopLowFood, NoHiFood, NoLowFood), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(
    SmellCond = case_when(
      parameter %in% c("DisHiFood", "DisLowFood") ~ "Disgust",
      parameter %in% c("PopHiFood", "PopLowFood") ~ "Pleasant",
      parameter %in% c("NoHiFood", "NoLowFood") ~ "Control",
    ),
    FoodDesirability = case_when(
      parameter %in% c("DisHiFood", "PopHiFood", "NoHiFood") ~ "High",
      parameter %in% c("DisLowFood", "PopLowFood", "NoLowFood") ~ "Low"
    )
  )

cRV_long_Food$FoodDesirability <- factor(
  cRV_long_Food$FoodDesirability, 
  levels = c("Low", "High") 
)

Foodplot <- ggplot(cRV_long_Food, aes(x = FoodDesirability, y = value, fill=SmellCond)) +
  stat_halfeye(
    point_interval = median_hdi, 
    .width = 0.95, 
    alpha = 0.7, 
    position = position_dodge(width = 0.5) 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Food task", x = "Food Desirability", y = "cRV") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


samples_cRV_Emo <- data.frame("DisHiEmo" = results_group_E$pars$mu_kHi[,1], "DisLowEmo" = results_group_E$pars$mu_kLow[,1], 
                              "PopHiEmo" = results_group_E$pars$mu_kHi[,2], "PopLowEmo" = results_group_E$pars$mu_kLow[,2],
                              "NoHiEmo" = results_group_E$pars$mu_kHi[,3], "NoLowEmo" = results_group_E$pars$mu_kLow[,3])

cRV_long_Emo <- samples_cRV_Emo %>%
  pivot_longer(cols = c(DisHiEmo, DisLowEmo, PopHiEmo, PopLowEmo, NoHiEmo, NoLowEmo), 
               names_to = "parameter", 
               values_to = "value") %>%
  mutate(
    SmellCond = case_when(
      parameter %in% c("DisHiEmo", "DisLowEmo") ~ "Disgust",
      parameter %in% c("PopHiEmo", "PopLowEmo") ~ "Pleasant",
      parameter %in% c("NoHiEmo", "NoLowEmo") ~ "Control",
    ),
    Valence = case_when(
      parameter %in% c("DisHiEmo", "PopHiEmo", "NoHiEmo") ~ "High",
      parameter %in% c("DisLowEmo", "PopLowEmo", "NoLowEmo") ~ "Low"
    )
  )

cRV_long_Emo$Valence <- factor(
  cRV_long_Emo$Valence, 
  levels = c("Low", "High") 
)

Emoplot <- ggplot(cRV_long_Emo, aes(x = Valence, y = value, fill=SmellCond)) +
  stat_halfeye(
    point_interval = median_hdi, 
    .width = 0.95, 
    alpha = 0.7, 
    position = position_dodge(width = 0.5) 
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Emotion task", x = "Valence", y = "cRV") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))


combined_plots <- Foodplot + Emoplot +
  plot_layout(guides = "collect")

combined_plots


####Check on hunger and food variables

alpha(data.frame(expData$PreGold1, expData$PreGold2, expData$PreGold3,expData$PreGold4, expData$PreGold5, expData$PreGold6, expData$PreGold7))
alpha(data.frame(expData$PrePret1, expData$PrePret2, expData$PrePret3,expData$PrePret4, expData$PrePret5, expData$PrePret6, expData$PrePret7))
alpha(data.frame(expData$PreChip1, expData$PreChip2, expData$PreChip3,expData$PreChip4, expData$PreChip5, expData$PreChip6, expData$PreChip7))

expData$PreGoldTot <- rowMeans(cbind(expData$PreGold1, expData$PreGold2, expData$PreGold3,expData$PreGold4, expData$PreGold5, expData$PreGold6, expData$PreGold7))
expData$PrePretTot <- rowMeans(cbind(expData$PrePret1, expData$PrePret2, expData$PrePret3,expData$PrePret4, expData$PrePret5, expData$PrePret6, expData$PrePret7))
expData$PreChipTot <- rowMeans(cbind(expData$PreChip1, expData$PreChip2, expData$PreChip3,expData$PreChip4, expData$PreChip5, expData$PreChip6, expData$PreChip7))

alpha(data.frame(expData$PreHunger1, expData$PreHunger2, expData$PreHunger3))
expData$PreHunger <- rowMeans(cbind(expData$PreHunger1, expData$PreHunger2, expData$PreHunger3))

alpha(data.frame(expData$PostGold1, expData$PostGold2, expData$PostGold3,expData$PostGold4, expData$PostGold5, expData$GoldRate))
alpha(data.frame(expData$PostPret1, expData$PostPret2, expData$PostPret3,expData$PostPret4, expData$PostPret5, expData$PretRate))
alpha(data.frame(expData$PostChip1, expData$PostChip2, expData$PostChip3,expData$PostChip4, expData$PostChip5, expData$ChipRate))

expData$PostGoldEnjoy <- rowMeans(cbind(expData$PostGold1, expData$PostGold2, expData$PostGold3,expData$PostGold4, expData$PostGold5, expData$GoldRate))
expData$PostPretEnjoy <- rowMeans(cbind(expData$PostPret1, expData$PostPret2, expData$PostPret3,expData$PostPret4, expData$PostPret5, expData$PretRate))
expData$PostChipEnjoy <- rowMeans(cbind(expData$PostChip1, expData$PostChip2, expData$PostChip3,expData$PostChip4, expData$PostChip5, expData$ChipRate))

####CRVs with covariates

### covariates are Hi > Low contrast
load(results_beh_F)
load(results_covar_F)
load(results_beh_E)

HDIofMCMC(results_beh_F$pars$beta_beh_crv) ##crv of Food predicting food eaten
p_direction(results_beh_F$pars$beta_beh_crv)

HDIofMCMC(results_covar_F$pars$beta_between) ##hunger predicting crv of Food
p_direction(results_covar_F$pars$beta_between)

HDIofMCMC(results_beh_E$pars$beta_beh_crv) ##crv of Emotion predicting food eaten
p_direction(results_beh_E$pars$beta_beh_crv)

### covariates (hi > low) controlling for explicit ratings
load(results_behCovar_F)

HDIofMCMC(results_behCovar_F$pars$beta_beh_crv)
p_direction(results_behCovar_F$pars$beta_beh_crv)

HDIofMCMC(results_behCovar_F$pars$beta_beh_exp)
p_direction(results_behCovar_F$pars$beta_beh_exp)


### Analyses again with untransformed food eaten re: prereg
load(results_behOG_F)

HDIofMCMC(results_behOG_F$pars$beta_beh_crv) ##crv of Food predicting food eaten
p_direction(results_behOG_F$pars$beta_beh_crv)


#### calculate which food they ate the most of in case we want to use that to analyze data by (in preregistration)

expData <- expData %>%
  rowwise() %>% # Process one participant at a time
  mutate(
    # 1. Find the highest amount eaten for this person
    MaxVal = max(c(GoldWeightChange, ChipWeightChange, PretWeightChange), na.rm = TRUE),
    
    # 2. Flag which foods equal that max (handles ties)
    isGoldMax = (GoldWeightChange == MaxVal),
    isChipMax = (ChipWeightChange == MaxVal),
    isPretMax = (PretWeightChange == MaxVal),
    
    # 3. Check FavSnack for keywords (Case Insensitive)
    # This looks for "gold" inside "Goldfish", "chip" inside "Chips", etc.
    likesGold = grepl("gold", FavSnack, ignore.case = TRUE),
    likesChip = grepl("chip", FavSnack, ignore.case = TRUE),
    likesPret = grepl("pret", FavSnack, ignore.case = TRUE),
    
    # 4. Determine the Winner
    MostEaten = case_when(
      # --- PRIORITY: It is the Max AND matches their Favorite ---
      isGoldMax & likesGold ~ "Gold",
      isChipMax & likesChip ~ "Chip",
      isPretMax & likesPret ~ "Pret",
      
      # --- FALLBACK: Favorite didn't help (or no tie existed) ---
      # Just pick the one that is the max. 
      # (If there is still a tie but they wrote "Pizza", this picks Gold first)
      isGoldMax ~ "Gold",
      isChipMax ~ "Chip",
      isPretMax ~ "Pret",
      
      TRUE ~ NA_character_ # Catch-all for missing data
    )
  ) %>%
  ungroup() # Switch back to normal dataframe mode

expData <- expData %>%
  mutate(
    FavCrave = case_when(
      MostEaten == "Gold" ~ PreGoldTot,  # If Gold was winner, take value from x
      MostEaten == "Chip" ~ PrePretTot,  # If Chip was winner, take value from y
      MostEaten == "Pret" ~ PreChipTot,  # If Pret was winner, take value from z
      TRUE ~ NA_real_           # If MostEaten is NA, make AA NA
    )
  )

expData <- expData %>%
  mutate(
    FavEnjoy = case_when(
      MostEaten == "Gold" ~ PostGoldEnjoy,  # If Gold was winner, take value from x
      MostEaten == "Chip" ~ PostPretEnjoy,  # If Chip was winner, take value from y
      MostEaten == "Pret" ~ PostChipEnjoy,  # If Pret was winner, take value from z
      TRUE ~ NA_real_           # If MostEaten is NA, make AA NA
    )
  )



#### ANOVAS of explicit hunger

hunger.aov <- aov_ez("IDs", "PreHunger", expData, between = "Condition", anova_table=list(es="pes"))
hunger.aov
hunger.mn <- emmeans(hunger.aov, ~Condition)
hunger.mn

conhunger <- contrast(hunger.mn, method="trt.vs.ctrl")
conhunger
confint(conhunger)


effectsize(t.test(expData$PreHunger[expData$Condition == 3], expData$PreHunger[expData$Condition == 2], var.equal=TRUE, paired=FALSE))
effectsize(t.test(expData$PreHunger[expData$Condition == 1], expData$PreHunger[expData$Condition == 2], var.equal=TRUE, paired=FALSE))
effectsize(t.test(expData$PreHunger[expData$Condition == 3], expData$PreHunger[expData$Condition == 1], var.equal=TRUE, paired=FALSE))

foodManova <- manova(cbind(PreGoldTot, PrePretTot, PreChipTot) ~ Condition, data = expData)
summary(foodManova)

#### ANOVAS of food eaten

eat.aov <- aov_ez("IDs", "TotalEatenLog", expData, between = "Condition", anova_table=list(es="pes"))
eat.aov
eat.mn <- emmeans(eat.aov, ~Condition)
eat.mn

conEat <- contrast(eat.mn, method="trt.vs.ctrl")
conEat
confint(conEat)


effectsize(t.test(expData$TotalEatenLog[expData$Condition == 3], expData$TotalEatenLog[expData$Condition == 2], var.equal=TRUE, paired=FALSE))
effectsize(t.test(expData$TotalEatenLog[expData$Condition == 1], expData$TotalEatenLog[expData$Condition == 2], var.equal=TRUE, paired=FALSE))
effectsize(t.test(expData$TotalEatenLog[expData$Condition == 3], expData$TotalEatenLog[expData$Condition == 1], var.equal=TRUE, paired=FALSE))


#### ANOVAS of food eaten (untransformed)

eat2.aov <- aov_ez("IDs", "TotalEaten", expData, between = "Condition", anova_table=list(es="pes"))
eat2.aov
eat2.mn <- emmeans(eat.aov, ~Condition)
eat2.mn

conEat2 <- contrast(eat2.mn, method="trt.vs.ctrl")
conEat2
confint(conEat2)


effectsize(t.test(expData$TotalEaten[expData$Condition == 3], expData$TotalEaten[expData$Condition == 2], var.equal=TRUE, paired=FALSE))
effectsize(t.test(expData$TotalEaten[expData$Condition == 1], expData$TotalEaten[expData$Condition == 2], var.equal=TRUE, paired=FALSE))
effectsize(t.test(expData$TotalEaten[expData$Condition == 3], expData$TotalEaten[expData$Condition == 1], var.equal=TRUE, paired=FALSE))


#### ANOVAS of food cravings

cravGold.aov <- aov_ez("IDs", "PreGoldTot", expData, between = "Condition", anova_table=list(es="pes"))
cravGold.aov
cravChip.aov <- aov_ez("IDs", "PreChipTot", expData, between = "Condition", anova_table=list(es="pes"))
cravChip.aov
cravPret.aov <- aov_ez("IDs", "PrePretTot", expData, between = "Condition", anova_table=list(es="pes"))
cravPret.aov
cravFav.aov <- aov_ez("IDs", "FavCrave", expData, between = "Condition", anova_table=list(es="pes"))
cravFav.aov

#### ANOVAS of food enjoyment

enjoyGold.aov <- aov_ez("IDs", "PostGoldEnjoy", expData, between = "Condition", anova_table=list(es="pes"))
enjoyGold.aov
enjoyChip.aov <- aov_ez("IDs", "PostChipEnjoy", expData, between = "Condition", anova_table=list(es="pes"))
enjoyChip.aov
enjoyPret.aov <- aov_ez("IDs", "PostPretEnjoy", expData, between = "Condition", anova_table=list(es="pes"))
enjoyPret.aov
enjoyFav.aov <- aov_ez("IDs", "FavEnjoy", expData, between = "Condition", anova_table=list(es="pes"))
enjoyFav.aov


### food hunger and food eaten
## transformed

cor.test(expData$PreHunger, expData$TotalEatenLog)

## untransformed

cor.test(expData$PreHunger, expData$TotalEaten)
