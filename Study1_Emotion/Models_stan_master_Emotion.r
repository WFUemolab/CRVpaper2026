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

setwd("/Users/emolab/data/RLHabituation")
cwd = getwd()

### MODEL ASSUMPTIONS
### ESTIMATES ALPHA, KHI, KMID, KLOW
### INITIALIZES Q WITH KMID PARAMETER AND SETS MID REWARD VALUES TO 0 TO SERVE
### AS A REFERENCE POINT FOR HI AND LOW K VALUES
### RATINGS AS PREDICTORS ARE HIvsMID and LOvsMID, BUT USING ONE BETA TO PREDICT
### HI VS. LOW K VALUES. 
### HI VS. LOW K VALUES ARE ALSO USED TO PREDICT BEHAVIOR

source("/Users/emolab/data/RLFiles/rl_pipeline_function.r")

### THESE ARE JUST TEMPLATES. REPLACE THE INPUTS WITH YOUR STUDY'S INFORMATION

###Emotion model with just alpha (all trials) with just familiar task it's the alpha model
### but uses 'rating' as the outcome instead of the categorical outcome (1,0,-1)

results_alpha_F <- run_rl_master(
  data_path = paste0(cwd, "/PreppedData"),
  num_trials = 80,
  model_type = "base",
  
  ## which parameters. default = alpha plus k (AK), but can do just alpha (A) or 
  ## use prescribed rewards (A, plus outcome_var = prescribed rewards) or
  ## weighting of prescribed rewards "W"
  run_params = "A", 
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  # Optional: Only use Familiar trials (== 1)
  filter_var = "FamUnFam",
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

save(file="results_alpha_F", results_alpha_F)


###Emotion model with just alpha (all trials) with just familiar task it's the alpha model
### but uses 'rating' as the outcome instead of the categorical outcome (1,0,-1)

results_ratingReward_F <- run_rl_master(
  data_path = paste0(cwd, "/PreppedData"),
  num_trials = 80,
  model_type = "base",
  
  ## which parameters. default = alpha plus k (AK), but can do just alpha (A) or 
  ## use prescribed rewards (A, plus outcome_var = prescribed rewards) or
  ## weighting of prescribed rewards "W"
  run_params = "A", 
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "rating",
  
  # Optional: Only use Familiar trials (== 1)
  filter_var = "FamUnFam",
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

save(file="results_ratingReward_F", results_ratingReward_F)


### Emotion base model with kappas (model_type = "base") but need to cycle through 
### different numbers of trials

numTrials = c(40, 45, 50, 55, 60, 65, 70, 75, 80)

for (nt in 1:length(numTrials)) {
  
results_base_F <- run_rl_master(
  data_path = paste0(cwd, "/PreppedData"),
  num_trials = numTrials[nt],
  model_type = "base",
  
  # Variable Mapping that is specific to that data file/experiment
  choice_var = "choice",
  outcome_var = "outcome",
  
  ## which parameters. default = alpha plus k (AK), but can do just alpha (A) or 
  ## use prescribed rewards (A, plus outcome_var = prescribed rewards) or
  ## weighting of prescribed rewards "W"
  run_params = "AK", 
  
  # Optional: Only use trials where Condition == 1
  filter_var = "FamUnFam",
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

save(file=paste0("results_base_F", numTrials[nt]), results_base_F)
}

### Unfamiliar Emotion base model with kappas (model_type = "base") but need to
### estimate both 40 and 80 trials

numTrials = c(40, 80)

for (nt in 1:length(numTrials)) {
  
  results_base_U <- run_rl_master(
    data_path = paste0(cwd, "/PreppedData"),
    num_trials = numTrials[nt],
    model_type = "base",
    
    # Variable Mapping that is specific to that data file/experiment
    choice_var = "choice",
    outcome_var = "outcome",
    
    ## which parameters. default = alpha plus k (AK), but can do just alpha (A) or 
    ## use prescribed rewards (A, plus outcome_var = prescribed rewards) or
    ## weighting of prescribed rewards "W"
    run_params = "AK", 
    
    # Optional: Only use unfamiliar trials
    filter_var = "FamUnFam",
    filter_val = -1,
    
    # Optional: Predictors and Outcomes
    rating_var = NULL,
    between_var = NULL,
    behavior_var = NULL,
    
    #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
    Hi_Low_var = "HvL",
    
    # Optional: If there are groups
    group_var = NULL
  )
  
  save(file=paste0("results_base_U", numTrials[nt]), results_base_U)
}


### Emotion covariate model (familiar trials) with kappas (model_type = "covar") but need to
### estimate both 40 and 80 trials

numTrials = c(40, 45, 50, 55, 60, 65, 70, 75, 80)

for (nt in 1:length(numTrials)) {
  
  results_covar_F <- run_rl_master(
    data_path = paste0(cwd, "/PreppedData"),
    num_trials = numTrials[nt],
    model_type = "covar",
    
    # Variable Mapping that is specific to that data file/experiment
    choice_var = "choice",
    outcome_var = "outcome",
    
    ## which parameters. default = alpha plus k (AK), but can do just alpha (A) or 
    ## use prescribed rewards (A, plus outcome_var = prescribed rewards) or
    ## weighting of prescribed rewards "W"
    run_params = "AK", 
    
    # Optional: Only use unfamiliar trials
    filter_var = "FamUnFam",
    filter_val = 1,
    
    # Optional: Predictors and Outcomes
    rating_var = "rating",
    between_var = NULL,
    behavior_var = NULL,
    
    #Optional: Test Hi > Low ("HvL") or Hi = Low ("HandL")for covariates/behavior
    Hi_Low_var = "HvL",
    
    # Optional: If there are groups
    group_var = NULL
  )
  
  save(file=paste0("results_covar_F", numTrials[nt]), results_covar_F)
}




