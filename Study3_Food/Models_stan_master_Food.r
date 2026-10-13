library(rstan)
library(loo)
library(tidyverse)

### TEMPLATES FOR RUNNING RL MODELS ON CHOICE DATA
### ADD THIS AND THE Qlearning_master.stan FILE TO STUDY DIRECTORY

### DATA FORMAT:
### DATA (PER PARTICIPANT) ARE IN A FORMAT WITH LABELS FOR: 
### CHOICE DATA (REQUIRED), 
### OUTCOME (REQUIRED; 1 = HI, 0 = MID, -1 = LOW),
### GROUP (OPTIONAL)
### ANY FILTER (EXAMPLE, SEPARATE SETS OF CHOICES FOR DIFFERENT TYPES OF STIMULI)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

setwd("/Users/emolab/data/RLFood")

### MODEL ASSUMPTIONS
### ESTIMATES ALPHA, KHI, KMID, KLOW
### INITIALIZES Q WITH KMID PARAMETER AND SETS MID REWARD VALUES TO 0 TO SERVE
### AS A REFERENCE POINT FOR HI AND LOW K VALUES
### RATINGS AS PREDICTORS ARE HIvsMID and LOvsMID, BUT USING ONE BETA TO PREDICT
### HI VS. LOW K VALUES. 
### HI VS. LOW K VALUES ARE ALSO USED TO PREDICT BEHAVIOR

source("/Users/emolab/data/Scripts/RLFiles/rl_pipeline_function.r")

### THESE ARE JUST TEMPLATES. REPLACE THE INPUTS WITH YOUR STUDY'S INFORMATION

### FOOD base model (model_type = "base")
results_base_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_base_F", results_base_F)

### EMOTION base model (model_type = "base")
results_base_E <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_base_E", results_base_E)

### FOOD group model (model_type = "group")
results_group_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "group",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = "cond"
)

save(file="results_group_F", results_group_F)

### EMOTION base model (model_type = "base")
results_group_E <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "group",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = "cond"
)

save(file="results_group_E", results_group_E)


### FOOD between covariate (hunger) model (model_type = "base")
results_covar_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = "hunger",
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_covar_F", results_covar_F)

### EMOTION between covariate (hunger) model (model_type = "base")
results_covar_E <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = "hunger",
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_covar_E", results_covar_E)

### FOOD behavior (food eaten) model (model_type = "behavior")
results_beh_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "behavior",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = "foodEaten",
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_beh_F", results_beh_F)

### EMOTION behavior (food eaten) model (model_type = "behavior")
results_beh_E <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "behavior",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = "foodEaten",
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_beh_E", results_beh_E)


### FOOD between covariate (hunger) model (model_type = "base") but with Hi and
### Low both being predicted by same covariate (not the difference)
results_covarMatch_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = "hunger",
  behavior_var = NULL,
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HandL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_covarMatch_F", results_covarMatch_F)


### FOOD behavior (food eaten) model (model_type = "behavior") but with Hi and Low
### both predicting food eaten, not difference between them.
results_behMatch_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "behavior",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = "foodEaten",
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HandL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_behMatch_F", results_behMatch_F)

### FOOD behavior (food eaten) model (model_type = "behavior") with hunger as covariate
results_behCovar_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "behavior",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = "hunger",
  behavior_var = "foodEaten",
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_behCovar_F", results_behCovar_F)

### FOOD behavior (food eaten) model (model_type = "behavior") untransformed
results_behOG_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFood/PreppedData",
  num_trials = 40,
  model_type = "behavior",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "OneOrTwo",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  between_var = NULL,
  behavior_var = "foodEatenOG",
  
  #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
  Hi_Low_var = "HvL",
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results_behOG_F", results_behOG_F)

### WHEN RUNNING MODELS, JUST ADD GROUP VARIABLE INFORMATION 
### TO ANY OTHER BASE, COVAR, BEHAVIOR MODEL IF DESIRED
### THE BEHAVIOR MODEL WILL ALSO ADD IN COVARIATE PREDICTORS ALONG WITH K VALUES 
### IF COVARIATES ARE SPECIFIED


###

