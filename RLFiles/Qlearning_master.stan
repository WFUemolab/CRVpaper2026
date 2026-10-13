data {
  int<lower=1> N;
  int<lower=1> numTrials;
  array[N] int option1;
  array[N] int option2;
  array[N, numTrials] int choice;
  array[N, numTrials] int choiceIndex;
  array[N, numTrials] real outcome; 
  array[N] int slen;
  
  // Conditional Vectors
  vector[N] HiMidRating;
  vector[N] LoMidRating;
  vector[N] behavior_outcome;
  vector[N] between_covar;
 
  // Toggle Flags 
  int<lower=0, upper=1> run_covar;
  int<lower=0, upper=1> run_behavior;
  int<lower=0, upper=1> run_between;
  int<lower=0, upper=1> run_match;
  int<lower=0, upper=2> run_params;
  
  // Group Indexing
  int<lower=1> num_groups;
  array[N] int<lower=1, upper=num_groups> group_id;
}

parameters {
  // Core population-level means (Now indexed by group)
  vector[num_groups] mu_alpha;
  vector[num_groups] mu_kHi;
  vector[num_groups] mu_kLow;
  vector[num_groups] mu_kMid; 
  
  // Core population-level standard deviations (Pooled across groups)
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_kHi;
  real<lower=0> sigma_kLow;
  real<lower=0> sigma_kMid; 
  
  // Participant-level raw deviations
  vector[N] alpha_pr;
  vector[N] kHi_pr;
  vector[N] kLow_pr;
  vector[N] kMid_pr; 
  
  // Conditional parameters 
  vector[run_covar] beta_exp; 
  vector[run_behavior] beta_beh_crv;
  vector[run_behavior] beta_beh_exp;
  vector[run_behavior] mu_beh;
  vector<lower=0>[run_behavior] sigma_beh;
  vector[run_between] beta_between;
}

transformed parameters {
  vector[N] alpha;
  vector[N] kHi;
  vector[N] kLow;
  vector[N] kMid; 
  
  for (i in 1:N) {
    // Check group_id[i] to apply the correct baseline mean
    alpha[i] = Phi_approx(mu_alpha[group_id[i]] + sigma_alpha * alpha_pr[i]);
    kMid[i] = mu_kMid[group_id[i]] + sigma_kMid * kMid_pr[i]; 
    
   // Base hierarchical values
    real base_kHi = mu_kHi[group_id[i]] + sigma_kHi * kHi_pr[i];
    real base_kLow = mu_kLow[group_id[i]] + sigma_kLow * kLow_pr[i];
    
    // Apply the shared between-subject beta to both baselines
    if (run_between == 1) {
      if (run_match == 1) {
        // MATCH: The covariate pushes BOTH reward sensitivities in the same direction
        base_kHi  += beta_between[1] * between_covar[i];
        base_kLow += beta_between[1] * between_covar[i];
      } else {
        // DIFFERENCE: The covariate widens or narrows the gap between them
        base_kHi  += beta_between[1] * between_covar[i];
        base_kLow -= beta_between[1] * between_covar[i];
      }
    }
    
    // Now apply the within-subject explicit ratings on top of that base
    if (run_covar == 1) {
      kHi[i] = base_kHi + beta_exp[1] * HiMidRating[i];
      kLow[i] = base_kLow + beta_exp[1] * LoMidRating[i];
    } else {
      kHi[i] = base_kHi;
      kLow[i] = base_kLow;
    }
  }
}

model {
  // 1. Priors (Stan automatically applies these normal(0,1) priors to every group in the vector)
  mu_alpha ~ normal(0, 1);
  mu_kHi ~ normal(0, 1);
  mu_kLow ~ normal(0, 1);
  mu_kMid ~ normal(0, 1); 
  
  sigma_alpha ~ normal(0, 1);
  sigma_kHi ~ normal(0, 1);
  sigma_kLow ~ normal(0, 1);
  sigma_kMid ~ normal(0, 1); 
  
  alpha_pr ~ normal(0, 1);
  kHi_pr ~ normal(0, 1);
  kLow_pr ~ normal(0, 1);
  kMid_pr ~ normal(0, 1); 
  
  // 2. Conditional Priors
  if (run_covar == 1) {
    beta_exp[1] ~ normal(0, 1);
  }
  if (run_behavior == 1) {
    beta_beh_crv[1] ~ normal(0, 1);
    beta_beh_exp[1] ~ normal(0, 1);
    mu_beh[1] ~ normal(0, 1);
    sigma_beh[1] ~ normal(0, 1);
  }
  
  if (run_between == 1) {
    beta_between[1] ~ normal(0, 1);
  }
  
  // 3. Subject Loop
  for (i in 1:N) {
    vector[2] Q;
    
    if (run_params == 1) {
      Q = rep_vector(kMid[i], 2); // The others estimate the baseline
    } else {
      Q = rep_vector(0.0, 2); // Raw model uses true 0 as baseline
    }
  
    
    for (t in 1:slen[i]) {
      target += categorical_logit_lpmf(choiceIndex[i, t] | Q);
      
      real subjective_reward;
      
     if (run_params == 0) {
       
      subjective_reward = outcome[i,t];
      
      } else if (run_params == 2) {
        // WEIGHTING MODEL: Parameters act as multipliers on the quantitative outcome
        if (outcome[i, t] > 0) {
          subjective_reward = kHi[i] * outcome[i, t];
        } else if (outcome[i, t] < 0) {
          subjective_reward = kLow[i] * outcome[i, t];
        } else {
          subjective_reward = 0.0; 
        }
      } else {
        // ABSOLUTE MODEL: Parameters act as fixed categorical substitutes
        if (outcome[i, t] == 1) { 
          subjective_reward = kHi[i];
        } else if (outcome[i, t] == -1) {
          subjective_reward = kLow[i];
        } else {
          subjective_reward = 0.0; 
        }
      }
      
      real PE = subjective_reward - Q[choiceIndex[i, t]];
      Q[choiceIndex[i, t]] += alpha[i] * PE;
    }
    
    if (run_behavior == 1) {
      if (run_match == 1) {
        behavior_outcome[i] ~ normal(mu_beh[1] + 
                                   beta_beh_crv[1] * (kHi[i] + kLow[i]) + 
                                   beta_beh_exp[1] * between_covar[i], sigma_beh[1]);
      } else {
        behavior_outcome[i] ~ normal(mu_beh[1] + 
                                   beta_beh_crv[1] * (kHi[i] - kLow[i]) + 
                                   beta_beh_exp[1] * between_covar[i], sigma_beh[1]);
    }
    }
  }
}

generated quantities {
  array[N, numTrials] real log_lik;
  
  for (i in 1:N) {
    vector[2] Q = rep_vector(kMid[i], 2); 
    
    for (t in 1:numTrials) {
      if (t <= slen[i]) {
        log_lik[i, t] = categorical_logit_lpmf(choiceIndex[i, t] | Q);
        
        real subjective_reward;
        
        
      if (run_params == 0) {
       
      subjective_reward = outcome[i,t];
      
      } else if (run_params == 2) {
        // WEIGHTING MODEL: Parameters act as multipliers on the quantitative outcome
        if (outcome[i, t] > 0) {
          subjective_reward = kHi[i] * outcome[i, t];
        } else if (outcome[i, t] < 0) {
          subjective_reward = kLow[i] * outcome[i, t];
        } else {
          subjective_reward = 0.0; 
        }
      } else {
        // ABSOLUTE MODEL: Parameters act as fixed categorical substitutes
        if (outcome[i, t] == 1) { 
          subjective_reward = kHi[i];
        } else if (outcome[i, t] == -1) {
          subjective_reward = kLow[i];
        } else {
          subjective_reward = 0.0; 
        }
      }
        
        real PE = subjective_reward - Q[choiceIndex[i, t]];
        Q[choiceIndex[i, t]] += alpha[i] * PE;
      } else {
        log_lik[i, t] = 0.0; 
      }
    }
  }
}