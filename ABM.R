library(tidyverse)
set.seed(111)

###############  variables ##########
t <- 300
n_banks <- 10
n_households <- 1000
n_borrowers <- 2000
loan_periods <- 1
equity <- 40
interest <- 0.0125
CAR <- 0.08
MRR <- 0.035
depo_margin <- 0.1
loan_margin <- 0.9
recovery_rate <- 0.4
withdrawal_prob_min <- 0.01
withdrawal_prob_max <- 0.08
default_prob_min <- 0.04
default_prob_max <- 0.08
reserve_fire_sell <- 0.8
bankruptcy_fire_sell <- 0.5
income_tax <- 0.18
target_reserves <- 2
u_bound_CAR <- 2
safety_effect <- 0.04

## shocks variables
CAR_shock_TF <- F
p_change_CAR <- 40
CAR_delta <- 0.02

PD_shock_TF <- F
p_change_PD <- 30
max_PD_delta <- 0.07
PD_decrease_rate <- 0.05

MRR_shock_TF <- F
p_change_MRR <- 50
MRR_delta <- 0.03

interest_shock_TF <- F
p_change_interest <- 50
interest_delta <- 0.005

spread_shock_TF <- F
p_change_spread <- 50
l_margin_delta <- 0.02
d_margin_delta <- -0.01

############### agent matrices ############
households <- list(depo_value = matrix(0, ncol = n_households, nrow = t),
                   which_bank = matrix(0, ncol = n_households, nrow = t),
                   withdrawal_prob = matrix(0, ncol = n_households, nrow = t))

borrowers <- list(loan_value = matrix(0, ncol = n_borrowers, nrow = t),
                  which_bank = matrix(0, ncol = n_borrowers, nrow = t),
                  pd = matrix(0, ncol = n_borrowers, nrow = t),
                  recovery_rate = matrix(0, ncol = n_borrowers, nrow = t),
                  risk_weight = matrix(0, ncol = n_borrowers, nrow = t),
                  loan_end = matrix(0, ncol = n_borrowers, nrow = t))

banks <- list(equity = matrix(0, ncol = n_banks, nrow = t),
              sum_deposits = matrix(0, ncol = n_banks, nrow = t),
              loan_portfolio = matrix(0, ncol = n_banks, nrow = t),
              reserves = matrix(0, ncol = n_banks, nrow = t),
              exp_loss_provision = matrix(0, ncol = n_banks, nrow = t),
              income = matrix(0, ncol = n_banks, nrow = t),
              dividends = matrix(0, ncol = n_banks, nrow = t),
              bankruptcy = matrix(0, ncol = n_banks, nrow = t))

############## initial values #############
households$depo_value[1,] <- 1
households$which_bank[1,] <- sample(1:n_banks, size = n_households, replace = TRUE)
households$withdrawal_prob[1,] <- runif(n_households, withdrawal_prob_min, withdrawal_prob_max)
borrowers$pd[1:t,] <- runif(n_borrowers, default_prob_min, default_prob_max)
borrowers$recovery_rate[1,] <- recovery_rate
borrowers$which_bank[1,] <- sample(1:n_banks, size = n_borrowers, replace = TRUE)
banks$equity[1,] <- equity

########## simulation ##########
for (p in 2:t) {
  households$which_bank[p,] <- households$which_bank[p-1,]
  households$depo_value[p,] <- households$depo_value[p-1,]
  
  ## Update banks' deposits
  for (e in 1:n_banks) {
    banks$sum_deposits[p, e] <- sum(households$depo_value[p, households$which_bank[p,] == e])
  }
  
  ## Loan defaults
  borrowers$loan_value[p,] <- borrowers$loan_value[p-1,]
  defaults <- runif(n_borrowers) < borrowers$pd[p,]
  borrowers$loan_value[p, defaults] <- 0
  
  ## Update banks' loan portfolios
  for (r in 1:n_banks) {
    banks$loan_portfolio[p, r] <- sum(borrowers$loan_value[p, borrowers$which_bank[p,] == r])
  }
  
  ## Reserves and capital adequacy
  banks$reserves[p,] <- banks$sum_deposits[p,] - banks$loan_portfolio[p,]
  for (d in which(banks$reserves[p,] < banks$sum_deposits[p,] * MRR)) {
    banks$loan_portfolio[p, d] <- banks$loan_portfolio[p, d] * reserve_fire_sell
  }
  banks$reserves[p,] <- banks$sum_deposits[p,] - banks$loan_portfolio[p,]
  
  ## Income and equity update
  for (y in 1:n_banks) {
    interest_income <- sum(borrowers$loan_value[p, borrowers$which_bank[p,] == y]) * interest
    deposit_costs <- sum(households$depo_value[p, households$which_bank[p,] == y]) * interest * (1 + depo_margin)
    banks$income[p, y] <- interest_income - deposit_costs
    banks$equity[p, y] <- banks$equity[p-1, y] + banks$income[p, y]
  }
  
  ## Dividends
  for (g in 1:n_banks) {
    if (banks$equity[p, g] > banks$sum_deposits[p, g] * CAR * u_bound_CAR) {
      banks$dividends[p, g] <- banks$income[p, g] * 0.5
      banks$equity[p, g] <- banks$equity[p, g] - banks$dividends[p, g]
    }
  }
  
  ## Bankruptcy check
  banks$bankruptcy[p,] <- banks$equity[p,] <= 0
  if (any(banks$bankruptcy[p,])) {
    for (k in which(banks$bankruptcy[p,]==1)) {
      banks$sum_deposits[p, k] <- 0
      households$which_bank[p, households$which_bank[p,] == k] <- sample(setdiff(1:n_banks, k), sum(households$which_bank[p,] == k), replace = TRUE)
    }
  }
}

############## End of Simulation ##############
print("Simulation completed!")

