run_rl_master <- function(data_path, 
                          num_trials, 
                          model_type = "base",
                          choice_var = "choice",      
                          outcome_var = "outcome",    
                          filter_var = NULL,          
                          filter_val = 1,             
                          rating_var = NULL,
                          between_var = NULL,
                          behavior_var = NULL,
                          group_var = NULL,
                          Hi_Low_var = "HvL",
                          run_params = "AK") {         # NEW: Column name for group assignment
  
  parent_dir <- dirname(data_path)
  setwd(parent_dir)
  
  namesFiles <- list.files(data_path)
  N <- length(namesFiles)
  
  # Initialize standard matrices
  optionsTot <- matrix(data = NA, nrow = N, ncol = 2)
  choice <- matrix(data = NA, nrow = N, ncol = num_trials)
  outcome <- matrix(data = NA, nrow = N, ncol = num_trials)
  slen <- vector(length = N)
  raw_groups <- vector(length = N) # Array to hold raw group labels
  
  # Initialize conditional matrices
  rating <- matrix(data = NA, nrow = N, ncol = num_trials)
  HiMidRating <- vector(length = N)
  LoMidRating <- vector(length = N)
  behavior_outcome <- vector(length = N) 
  between_covar <- vector(length = N)
  
  # 1. Dynamic Data Extraction Loop
  for (i in 1:N) {
    dataTemp <- read.csv(file.path(data_path, namesFiles[i]), header = TRUE)
    
    if (!is.null(filter_var) && filter_var %in% names(dataTemp)) {
      dataTemp <- dataTemp[dataTemp[[filter_var]] == filter_val, ]
    }
    
    dataTemp <- dataTemp[1:min(nrow(dataTemp), num_trials), ]
    slen[i] <- nrow(dataTemp)
    
    # Extract Group
    if (!is.null(group_var) && group_var %in% names(dataTemp)) {
      raw_groups[i] <- as.character(dataTemp[[group_var]][1])
    }
    
    # Extract Dynamic Vectors
    choices_temp <- dataTemp[[choice_var]]
    outcomes_temp <- dataTemp[[outcome_var]]
    
    opts <- unique(na.omit(choices_temp))
    if (length(opts) < 2) opts <- c(opts, 999)
    optionsTot[i, ] <- opts[1:2]
    
    length(choices_temp) <- num_trials
    choices_temp[is.na(choices_temp)] <- 999
    choice[i, ] <- choices_temp
    
    length(outcomes_temp) <- num_trials
    outcomes_temp[is.na(outcomes_temp)] <- 0 
    outcome[i, ] <- outcomes_temp
    
    # Extract Conditional Data
    if (model_type %in% c("covar", "behavior") && !is.null(rating_var)) {
      ratings_temp <- dataTemp[[rating_var]]
      length(ratings_temp) <- num_trials
      rating[i, ] <- ratings_temp
      
      valid_idx <- 1:slen[i]
      HiMidRating[i] <- mean(ratings_temp[valid_idx][outcomes_temp[valid_idx] == 1], na.rm = TRUE) - 
        mean(ratings_temp[valid_idx][outcomes_temp[valid_idx] == 0], na.rm = TRUE)
      
      LoMidRating[i] <- mean(ratings_temp[valid_idx][outcomes_temp[valid_idx] == -1], na.rm = TRUE) - 
        mean(ratings_temp[valid_idx][outcomes_temp[valid_idx] == 0], na.rm = TRUE)
    }
    
    if (model_type == "behavior" && !is.null(behavior_var)) {
      behavior_outcome[i] <- mean(dataTemp[[behavior_var]], na.rm = TRUE)
    }
    
    # Extract the single between-subject constant
    if (!is.null(between_var) && between_var %in% names(dataTemp)) {
      between_covar[i] <- dataTemp[[between_var]][1] 
    }
  }
  
  # 2. Process Group Indexing
  if (!is.null(group_var)) {
    group_factor <- as.factor(raw_groups)
    group_id <- as.integer(group_factor)
    num_groups <- max(group_id)
    
    # Create a dictionary dataframe to return to the user
    group_mapping <- data.frame(
      Stan_ID = 1:num_groups,
      Group_Name = levels(group_factor)
    )
  } else {
    group_id <- rep(1, N)
    num_groups <- 1
    group_mapping <- data.frame(Stan_ID = 1, Group_Name = "All")
  }
  
  # Safely Scale Conditional Data
  if (model_type %in% c("covar", "behavior") && !is.null(rating_var)) {
    HiMidRating_scaled <- scale(HiMidRating)[, 1]
    LoMidRating_scaled <- scale(LoMidRating)[, 1]
    
    # Neutralize any true NAs generated during scaling
    HiMidRating_scaled[is.na(HiMidRating_scaled)] <- 0
    LoMidRating_scaled[is.na(LoMidRating_scaled)] <- 0
  } else {
    # If no ratings provided, pass safe dummy zeros to satisfy Stan's data checks
    HiMidRating_scaled <- rep(0, N)
    LoMidRating_scaled <- rep(0, N)
  }
  
  if (model_type == "behavior" && !is.null(behavior_var)) {
    behavior_outcome_scaled <- scale(behavior_outcome)[, 1]
    behavior_outcome_scaled[is.na(behavior_outcome_scaled)] <- 0
  } else {
    behavior_outcome_scaled <- rep(0, N)
  }
  
  # Safely scale the between-subject covariate
  if (!is.null(between_var)) {
    between_covar_scaled <- scale(between_covar)[, 1]
    between_covar_scaled[is.na(between_covar_scaled)] <- 0
  } else {
    between_covar_scaled <- rep(0, N)
  }
  
  run_params_int <- switch(run_params,
                           "A"  = 0,  # Alpha only (Raw rewards)
                           "AK"  = 1,  # cRVs as rewards
                           "W" = 2)  # cRVs as weighting of rewards
  
  # 3. Build the Master Data List
  stan_dat <- list(
    N = N,
    numTrials = num_trials,
    option1 = optionsTot[, 1],
    option2 = optionsTot[, 2],
    choice = choice,
    choiceIndex = ifelse(choice == optionsTot[, 1], 1, 2),
    outcome = outcome,
    slen = slen,
    HiMidRating = HiMidRating_scaled,
    LoMidRating = LoMidRating_scaled,
    between_covar = between_covar_scaled,
    run_between = ifelse(model_type == 'covar' && !is.null(between_var), 1, 0),
    behavior_outcome = behavior_outcome_scaled,
    
    run_covar = ifelse(model_type == "covar" && !is.null(rating_var), 1, 0),
    run_behavior = ifelse(model_type == "behavior", 1, 0),
    run_match = ifelse(Hi_Low_var == "HandL", 1, 0),
    run_params = run_params_int,
    
    # New Group Data
    num_groups = num_groups,
    group_id = group_id
  )
  
  # 4. Variational Bayes Initialization
  Qmodel_arg <- stan_model("../RLFiles/Qlearning_master.stan")
  Q_init <- list()
  for (i in 1:4) { 
    fitQ_vb <- vb(Qmodel_arg, data = stan_dat, init = 'random')
    Qm_vb <- colMeans(as.data.frame(fitQ_vb))
    
    Qlist <- list(
      mu_pr = as.vector(Qm_vb[startsWith(names(Qm_vb), "mu_pr")]),
      sigma = as.vector(Qm_vb[startsWith(names(Qm_vb), "sigma")])
    )
    for (p in c("alpha", "kLow", "kHi", "kMid")) {
      if (any(startsWith(names(Qm_vb), paste0(p, "_pr")))) {
        Qlist[[paste0(p, "_pr")]] <- as.vector(Qm_vb[startsWith(names(Qm_vb), paste0(p, "_pr"))])
      }
    }
    Q_init[[i]] <- Qlist
  }
  
  # 5. Full MCMC Fit
  fit <- stan(file = "../RLFiles/Qlearning_master.stan", data = stan_dat,
              iter = 7000, warmup = 2000, chains = 4, cores = 4,
              init = Q_init, control = list(adapt_delta = 0.95))
  
  # 6. Extract LOO
  loglik <- extract_log_lik(fit, merge_chains = FALSE)
  r_eff <- relative_eff(exp(loglik), chain_id = rep(1:4, each = nrow(loglik) / 4))
  model_loo <- loo(loglik, r_eff = r_eff, save_psis = TRUE)
  
  pars <- rstan::extract(fit)
  
  return(list(fit = fit, loo = model_loo, data = stan_dat, pars = pars, groups = group_mapping))
}