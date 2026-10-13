library(rstan)
library(loo)
library(tidyverse)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("/Users/emolab/data/Scripts/RLFiles/rl_pipeline_function.r")


### example base model (model_type = "base")
results80_base_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFaces/PreppedData",
  num_trials = 80,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FaceGender",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  behavior_var = NULL,
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results80_base_F", results80_base_F)

### example base model (model_type = "base")
results80_base_M <- run_rl_master(
  data_path = "/Users/emolab/data/RLFaces/PreppedData",
  num_trials = 80,
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FaceGender",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  behavior_var = NULL,
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results80_base_M", results80_base_M)

### example group base model 
### adding group keeps the same model specifications (e.g., "base"), but no group
### just sets group = 1

results80_base_F_group <- run_rl_master(
  data_path = "/Users/emolab/data/RLFaces/PreppedData",
  num_trials = 80,
  model_type = "base", 
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FaceGender",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  behavior_var = NULL,
  
  # Optional: If there are groups
  group_var = "group"
)

save(file="results80_base_F_group", results80_base_F_group)

results80_base_M_group <- run_rl_master(
  data_path = "/Users/emolab/data/RLFaces/PreppedData",
  num_trials = 80,
  model_type = "base", 
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FaceGender",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = NULL,
  behavior_var = NULL,
  
  # Optional: If there are groups
  group_var = "group"
)

save(file="results80_base_M_group", results80_base_M_group)

### example covariate model (model_type = "covar")
results80_covar_F <- run_rl_master(
  data_path = "/Users/emolab/data/RLFaces/PreppedData",
  num_trials = 80,
  model_type = "covar",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FaceGender",
  filter_val = 1,
  
  # Optional: Predictors and Outcomes
  rating_var = "rating",
  behavior_var = NULL,
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results80_covar_F", results80_covar_F)

### example covariate model (model_type = "covar")
results80_covar_M <- run_rl_master(
  data_path = "/Users/emolab/data/RLFaces/PreppedData",
  num_trials = 80,
  model_type = "covar",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FaceGender",
  filter_val = 2,
  
  # Optional: Predictors and Outcomes
  rating_var = "rating",
  behavior_var = NULL,
  
  # Optional: If there are groups
  group_var = NULL
)

save(file="results80_covar_M", results80_covar_M)
