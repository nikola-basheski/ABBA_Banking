library(tidyverse)
set.seed(111)

# Parameters
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
recovery_rate <- 0.1
withdrawal_prob_min <- 0.01
withdrawal_prob_max <- 0.08
default_prob_min <- 0.001
default_prob_max <- 0.5
reserve_fire_sell <- 0.8
bankruptcy_fire_sell <- 0.5
safety_effect <- 0.04
concentration_spread <- function(x) { 0.15 * x^2 }
risk_weight <- function(x) { 0.5 + 5 * x }

# Interest rate shock parameters
interest_shock_TF <- TRUE
p_change_interest <- 50
interest_delta <- 0.005

# Agent matrices
households <- list(
  depo_value = matrix(1, nrow = t, ncol = n_households),
  which_bank = matrix(sample(1:n_banks, n_households, replace = TRUE), nrow = t, ncol = n_households, byrow = TRUE),
  withdrawal_prob = matrix(runif(n_households, withdrawal_prob_min, withdrawal_prob_max), nrow = t, ncol = n_households, byrow = TRUE)
)

borrowers <- list(
  loan_value = matrix(0, nrow = t, ncol = n_borrowers),
  which_bank = matrix(sample(1:n_banks, n_borrowers, replace = TRUE), nrow = t, ncol = n_borrowers, byrow = TRUE),
  pd = matrix(runif(n_borrowers, default_prob_min, default_prob_max), nrow = t, ncol = n_borrowers, byrow = TRUE),
  recovery_rate = matrix(recovery_rate, nrow = t, ncol = n_borrowers, byrow = TRUE),
  risk_weight = matrix(0, nrow = t, ncol = n_borrowers),
  cost_capital = matrix(0, nrow = t, ncol = n_borrowers),
  loan_end = matrix(sample(1:loan_periods, n_borrowers, replace = TRUE), nrow = t, ncol = n_borrowers, byrow = TRUE)
)

banks <- list(
  equity = matrix(equity, nrow = t, ncol = n_banks, byrow = TRUE),
  sum_deposits = matrix(0, nrow = t, ncol = n_banks),
  loan_portfolio = matrix(0, nrow = t, ncol = n_banks),
  reserves = matrix(0, nrow = t, ncol = n_banks),
  exp_loss_provision = matrix(0, nrow = t, ncol = n_banks),
  revenue = matrix(0, nrow = t, ncol = n_banks),
  cost = matrix(0, nrow = t, ncol = n_banks),
  income = matrix(0, nrow = t, ncol = n_banks),
  risk_weighted_assets = matrix(0, nrow = t, ncol = n_banks),
  dividends = matrix(0, nrow = t, ncol = n_banks),
  bankruptcy = matrix(0, nrow = t, ncol = n_banks),
  undercapitalized = matrix(0, nrow = t, ncol = n_banks),
  liquidity_assistance = matrix(0, nrow = t, ncol = n_banks)
)



# Initial values
borrowers$risk_weight[1, ] <- risk_weight(borrowers$pd[1, ])
for (q in 1:n_borrowers) {
  borrowers$cost_capital[1, q] <- (1 + ((loan_margin + 1) * interest) - borrowers$pd[1, q] * recovery_rate) / (1 - borrowers$pd[1, q])
}

for (w in 1:n_banks) {
  banks$sum_deposits[1, w] <- sum(households$depo_value[1, households$which_bank[1, ] == w])
  RW_assets <- borrowers$risk_weight[1, borrowers$which_bank[1, ] == w]
  potential_loans <- min(floor(banks$sum_deposits[1, w] * (1 - MRR)), floor((banks$equity[1, w] / CAR) / mean(RW_assets)))
  borrowers$loan_value[1, borrowers$which_bank[1, ] == w] <- c(rep(1, potential_loans), rep(0, sum(borrowers$which_bank[1, ] == w) - potential_loans))
  banks$loan_portfolio[1, w] <- sum(borrowers$loan_value[1, borrowers$which_bank[1, ] == w])
  banks$risk_weighted_assets[1, w] <- sum(borrowers$risk_weight[1, borrowers$which_bank[1, ] == w] * borrowers$loan_value[1, borrowers$which_bank[1, ] == w])
  banks$exp_loss_provision[1, w] <- sum(borrowers$pd[1, borrowers$which_bank[1, ] == w] * borrowers$loan_value[1, borrowers$which_bank[1, ] == w] * (1 - recovery_rate))
  banks$reserves[1, w] <- banks$sum_deposits[1, w] - banks$loan_portfolio[1, w]
}

# Interest rate shock
if (interest_shock_TF) {
  interest <- c(rep(interest, p_change_interest - 1), rep(interest + interest_delta, t - p_change_interest + 1))
}

# Simulation
for (p in 2:t) {
  for (u in 1:n_households) {
    if (runif(1) < households$withdrawal_prob[p - 1, u]) {
      households$which_bank[p, u] <- sample(which(banks$bankruptcy[p - 1, ] == 0), 1)
    } else {
      households$which_bank[p, u] <- households$which_bank[p - 1, u]
    }
  }
  
  for (b in 1:n_banks) {
    banks$sum_deposits[p, b] <- sum(households$depo_value[p, households$which_bank[p, ] == b])
  }
  
  borrowers$loan_value[p, ] <- borrowers$loan_value[p - 1, ]
  for (i in which(borrowers$loan_value[p, ] == 1)) {
    if (runif(1) < borrowers$pd[p, i]) {
      borrowers$loan_value[p, i] <- 0
    }
  }
  
  for (r in 1:n_banks) {
    banks$loan_portfolio[p, r] <- sum(borrowers$loan_value[p, borrowers$which_bank[p, ] == r])
  }
  
  for (a in 1:n_banks) {
    banks$exp_loss_provision[p, a] <- sum(borrowers$pd[p, borrowers$which_bank[p, ] == a] * borrowers$loan_value[p, borrowers$which_bank[p, ] == a] * (1 - recovery_rate))
  }
  
  for (l in 1:n_borrowers) {
    borrowers$cost_capital[p, l] <- (1 + ((loan_margin + 1) * interest[p]) - borrowers$pd[p, l] * recovery_rate) / (1 - borrowers$pd[p, l])
  }
  
  for (y in 1:n_banks) {
    banks$revenue[p, y] <- sum(borrowers$cost_capital[p, borrowers$which_bank[p, ] == y] * borrowers$loan_value[p, borrowers$which_bank[p, ] == y]) + banks$reserves[p - 1, y] * interest[p - 1]
    banks$cost[p, y] <- sum(households$depo_value[p - 1, households$which_bank[p, ] == y] * interest[p] * (1 + depo_margin))
    banks$income[p, y] <- banks$revenue[p, y] - banks$cost[p, y] - (banks$exp_loss_provision[p - 1, y] - banks$exp_loss_provision[p, y])
    banks$equity[p, y] <- banks$equity[p - 1, y] + banks$income[p, y]
    banks$reserves[p, y] <- banks$sum_deposits[p, y] - banks$loan_portfolio[p, y]
  }
  
  for (k in which(banks$equity[p, ] <= 0)) {
    banks$bankruptcy[p, k] <- 1
    for (s in which(households$which_bank[p, ] == k)) {
      households$which_bank[p, s] <- sample(which(banks$bankruptcy[p, ] == 0), 1)
    }
  }
}

# Results Visualization
bank_liquidity <- as.data.frame(banks$reserves)
bank_bankruptcy <- as.data.frame(banks$bankruptcy)

par(mfrow = c(2, 1))
plot(bank_liquidity[, 1], type = 'l', col = 'blue', ylim = c(min(bank_liquidity), max(bank_liquidity)), ylab = 'Liquidity', xlab = 'Time', main = 'Bank Liquidity Over Time')
for (i in 2:n_banks) {
  lines(bank_liquidity[, i], col = 'blue')
}

plot(rowSums(bank_bankruptcy), type = 'l', col = 'red', ylim = c(0, n_banks), ylab = 'Number of Bankruptcies', xlab = 'Time', main = 'Bank Bankruptcies Over Time')

print("Simulation completed!")














library(tidyverse)
set.seed(111)

# Parameters
t <- 300
n_banks <- 10
n_households <- 1000
n_borrowers <- 2000
loan_periods <- 1
equity <- 10
interest <- 0.0125
CAR <- 0.01
MRR <- 0.035
depo_margin <- 0.1
loan_margin <- 0.9
recovery_rate <- 0.05
withdrawal_prob_min <- 0.01
withdrawal_prob_max <- 0.1
default_prob_min <- 0.001
default_prob_max <- 0.5
reserve_fire_sell <- 0.8
bankruptcy_fire_sell <- 0.5
safety_effect <- 0.04
concentration_spread <- function(x) { 0.15 * x^2 }
risk_weight <- function(x) { 0.5 + 5 * x }

# Interest rate shock parameters
interest_shock_TF <- TRUE
p_change_interest <- 50
interest_delta <- 0.005

# Agent matrices
households <- list(
  depo_value = matrix(1, nrow = t, ncol = n_households),
  which_bank = matrix(sample(1:n_banks, n_households, replace = TRUE), nrow = t, ncol = n_households, byrow = TRUE),
  withdrawal_prob = matrix(runif(n_households, withdrawal_prob_min, withdrawal_prob_max), nrow = t, ncol = n_households, byrow = TRUE)
)

borrowers <- list(
  loan_value = matrix(0, nrow = t, ncol = n_borrowers),
  which_bank = matrix(sample(1:n_banks, n_borrowers, replace = TRUE), nrow = t, ncol = n_borrowers, byrow = TRUE),
  pd = matrix(runif(n_borrowers, default_prob_min, default_prob_max), nrow = t, ncol = n_borrowers, byrow = TRUE),
  recovery_rate = matrix(recovery_rate, nrow = t, ncol = n_borrowers, byrow = TRUE),
  risk_weight = matrix(0, nrow = t, ncol = n_borrowers),
  cost_capital = matrix(0, nrow = t, ncol = n_borrowers),
  loan_end = matrix(sample(1:loan_periods, n_borrowers, replace = TRUE), nrow = t, ncol = n_borrowers, byrow = TRUE)
)

banks <- list(
  equity = matrix(equity, nrow = t, ncol = n_banks, byrow = TRUE),
  sum_deposits = matrix(0, nrow = t, ncol = n_banks),
  loan_portfolio = matrix(0, nrow = t, ncol = n_banks),
  reserves = matrix(0, nrow = t, ncol = n_banks),
  exp_loss_provision = matrix(0, nrow = t, ncol = n_banks),
  revenue = matrix(0, nrow = t, ncol = n_banks),
  cost = matrix(0, nrow = t, ncol = n_banks),
  income = matrix(0, nrow = t, ncol = n_banks),
  risk_weighted_assets = matrix(0, nrow = t, ncol = n_banks),
  dividends = matrix(0, nrow = t, ncol = n_banks),
  bankruptcy = matrix(FALSE, nrow = t, ncol = n_banks),
  undercapitalized = matrix(0, nrow = t, ncol = n_banks),
  liquidity_assistance = matrix(0, nrow = t, ncol = n_banks)
)


# Initial values
borrowers$risk_weight[1, ] <- risk_weight(borrowers$pd[1, ])
for (q in 1:n_borrowers) {
  borrowers$cost_capital[1, q] <- (1 + ((loan_margin + 1) * interest) - borrowers$pd[1, q] * recovery_rate) / (1 - borrowers$pd[1, q])
}

for (w in 1:n_banks) {
  banks$sum_deposits[1, w] <- sum(households$depo_value[1, households$which_bank[1, ] == w])
  RW_assets <- borrowers$risk_weight[1, borrowers$which_bank[1, ] == w]
  potential_loans <- min(floor(banks$sum_deposits[1, w] * (1 - MRR)), floor((banks$equity[1, w] / CAR) / mean(RW_assets)))
  borrowers$loan_value[1, borrowers$which_bank[1, ] == w] <- c(rep(1, potential_loans), rep(0, sum(borrowers$which_bank[1, ] == w) - potential_loans))
  banks$loan_portfolio[1, w] <- sum(borrowers$loan_value[1, borrowers$which_bank[1, ] == w])
  banks$risk_weighted_assets[1, w] <- sum(borrowers$risk_weight[1, borrowers$which_bank[1, ] == w] * borrowers$loan_value[1, borrowers$which_bank[1, ] == w])
  banks$exp_loss_provision[1, w] <- sum(borrowers$pd[1, borrowers$which_bank[1, ] == w] * borrowers$loan_value[1, borrowers$which_bank[1, ] == w] * (1 - recovery_rate))
  banks$reserves[1, w] <- banks$sum_deposits[1, w] - banks$loan_portfolio[1, w]
}

# Interest rate shock
if (interest_shock_TF) {
  interest <- c(rep(interest, p_change_interest - 1), rep(interest + interest_delta, t - p_change_interest + 1))
}

# Simulation
for (p in 2:t) {
  for (u in 1:n_households) {
    if (runif(1) < households$withdrawal_prob[p - 1, u]) {
      available_banks <- which(banks$bankruptcy[p - 1, ] == FALSE) # Only consider non-bankrupt banks
      if (length(available_banks) == 0) {
        households$which_bank[p, u] <- sample(1:n_banks, 1) # If all banks are bankrupt, choose randomly
      } else {
        households$which_bank[p, u] <- sample(available_banks, 1)
      }
    } else {
      households$which_bank[p, u] <- households$which_bank[p - 1, u]
    }
  }
  
  for (b in 1:n_banks) {
    banks$sum_deposits[p, b] <- sum(households$depo_value[p, households$which_bank[p, ] == b])
  }
  
  borrowers$loan_value[p, ] <- borrowers$loan_value[p - 1, ]
  for (i in which(borrowers$loan_value[p, ] == 1)) {
    if (runif(1) < borrowers$pd[p, i]) {
      borrowers$loan_value[p, i] <- 0
    }
  }
  
  for (r in 1:n_banks) {
    banks$loan_portfolio[p, r] <- sum(borrowers$loan_value[p, borrowers$which_bank[p, ] == r])
  }
  
  for (a in 1:n_banks) {
    banks$exp_loss_provision[p, a] <- sum(borrowers$pd[p, borrowers$which_bank[p, ] == a] * borrowers$loan_value[p, borrowers$which_bank[p, ] == a] * (1 - recovery_rate))
  }
  
  for (l in 1:n_borrowers) {
    borrowers$cost_capital[p, l] <- (1 + ((loan_margin + 1) * interest[p]) - borrowers$pd[p, l] * recovery_rate) / (1 - borrowers$pd[p, l])
  }
  
  for (y in 1:n_banks) {
    banks$revenue[p, y] <- sum(borrowers$cost_capital[p, borrowers$which_bank[p, ] == y] * borrowers$loan_value[p, borrowers$which_bank[p, ] == y]) + banks$reserves[p - 1, y] * interest[p - 1]
    banks$cost[p, y] <- sum(households$depo_value[p - 1, households$which_bank[p, ] == y] * interest[p] * (1 + depo_margin))
    banks$income[p, y] <- banks$revenue[p, y] - banks$cost[p, y] - (banks$exp_loss_provision[p - 1, y] - banks$exp_loss_provision[p, y])
    banks$equity[p, y] <- banks$equity[p - 1, y] + banks$income[p, y]
    banks$reserves[p, y] <- banks$sum_deposits[p, y] - banks$loan_portfolio[p, y]
  }
  
  for (k in which(banks$equity[p, ] <= 0)) {
    banks$bankruptcy[p, k] <- 1
    for (s in which(households$which_bank[p, ] == k)) {
      households$which_bank[p, s] <- sample(which(banks$bankruptcy[p, ] == 0), 1)
    }
  }
}

# Results Visualization
bank_liquidity <- as.data.frame(banks$reserves)
bank_bankruptcy <- as.data.frame(banks$bankruptcy)

# Increase plot margins and save to file
png("bank_liquidity.png", width = 800, height = 600)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(bank_liquidity[, 1], type = 'l', col = 'blue', ylim = c(min(bank_liquidity), max(bank_liquidity)), ylab = 'Liquidity', xlab = 'Time', main = 'Bank Liquidity Over Time')
for (i in 2:n_banks) {
  lines(bank_liquidity[, i], col = 'blue')
}
dev.off()

png("bank_bankruptcy.png", width = 800, height = 600)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(rowSums(bank_bankruptcy), type = 'l', col = 'red', ylim = c(0, n_banks), ylab = 'Number of Bankruptcies', xlab = 'Time', main = 'Bank Bankruptcies Over Time')
dev.off()

print("Simulation completed!")








# Parameters
t <- 300
n_banks <- 10
n_households <- 1000
n_borrowers <- 2000
loan_periods <- 1
equity <- 40
interest <- 0.0125
CAR <- 0.01
MRR <- 0.035
depo_margin <- 0.1
loan_margin <- 0.9
recovery_rate <- 0.05
withdrawal_prob_min <- 0.01
withdrawal_prob_max <- 0.1
default_prob_min <- 0.05
default_prob_max <- 0.5
reserve_fire_sell <- 0.8
bankruptcy_fire_sell <- 0.5
safety_effect <- 0.04
concentration_spread <- function(x) { 0.15 * x^2 }
risk_weight <- function(x) { 0.5 + 5 * x }
operational_cost <- 0.01  # Operational cost per unit of deposit

# Interest rate shock parameters
interest_shock_TF <- TRUE
p_change_interest <- 50
interest_delta <- 0.005

# Agent matrices
households <- list(
  depo_value = matrix(1, nrow = t, ncol = n_households),
  which_bank = matrix(sample(1:n_banks, n_households, replace = TRUE), nrow = t, ncol = n_households, byrow = TRUE),
  withdrawal_prob = matrix(runif(n_households, withdrawal_prob_min, withdrawal_prob_max), nrow = t, ncol = n_households, byrow = TRUE)
)

borrowers <- list(
  loan_value = matrix(0, nrow = t, ncol = n_borrowers),
  which_bank = matrix(sample(1:n_banks, n_borrowers, replace = TRUE), nrow = t, ncol = n_borrowers, byrow = TRUE),
  pd = matrix(runif(n_borrowers, default_prob_min, default_prob_max), nrow = t, ncol = n_borrowers, byrow = TRUE),
  recovery_rate = matrix(recovery_rate, nrow = t, ncol = n_borrowers, byrow = TRUE),
  risk_weight = matrix(0, nrow = t, ncol = n_borrowers),
  cost_capital = matrix(0, nrow = t, ncol = n_borrowers),
  loan_end = matrix(sample(1:loan_periods, n_borrowers, replace = TRUE), nrow = t, ncol = n_borrowers, byrow = TRUE)
)

banks <- list(
  equity = matrix(equity, nrow = t, ncol = n_banks, byrow = TRUE),
  sum_deposits = matrix(0, nrow = t, ncol = n_banks),
  loan_portfolio = matrix(0, nrow = t, ncol = n_banks),
  reserves = matrix(0, nrow = t, ncol = n_banks),
  exp_loss_provision = matrix(0, nrow = t, ncol = n_banks),
  revenue = matrix(0, nrow = t, ncol = n_banks),
  cost = matrix(0, nrow = t, ncol = n_banks),
  income = matrix(0, nrow = t, ncol = n_banks),
  risk_weighted_assets = matrix(0, nrow = t, ncol = n_banks),
  dividends = matrix(0, nrow = t, ncol = n_banks),
  bankruptcy = matrix(0, nrow = t, ncol = n_banks),
  undercapitalized = matrix(0, nrow = t, ncol = n_banks),
  liquidity_assistance = matrix(0, nrow = t, ncol = n_banks)
)

# Initial values
borrowers$risk_weight[1, ] <- risk_weight(borrowers$pd[1, ])
for (q in 1:n_borrowers) {
  borrowers$cost_capital[1, q] <- (1 + ((loan_margin + 1) * interest) - borrowers$pd[1, q] * recovery_rate) / (1 - borrowers$pd[1, q])
}

for (w in 1:n_banks) {
  banks$sum_deposits[1, w] <- sum(households$depo_value[1, households$which_bank[1, ] == w])
  RW_assets <- borrowers$risk_weight[1, borrowers$which_bank[1, ] == w]
  potential_loans <- min(floor(banks$sum_deposits[1, w] * (1 - MRR)), floor((banks$equity[1, w] / CAR) / mean(RW_assets)))
  borrowers$loan_value[1, borrowers$which_bank[1, ] == w] <- c(rep(1, potential_loans), rep(0, sum(borrowers$which_bank[1, ] == w) - potential_loans))
  banks$loan_portfolio[1, w] <- sum(borrowers$loan_value[1, borrowers$which_bank[1, ] == w])
  banks$risk_weighted_assets[1, w] <- sum(borrowers$risk_weight[1, borrowers$which_bank[1, ] == w] * borrowers$loan_value[1, borrowers$which_bank[1, ] == w])
  banks$exp_loss_provision[1, w] <- sum(borrowers$pd[1, borrowers$which_bank[1, ] == w] * borrowers$loan_value[1, borrowers$which_bank[1, ] == w] * (1 - recovery_rate))
  banks$reserves[1, w] <- banks$sum_deposits[1, w] - banks$loan_portfolio[1, w]
}

# Interest rate shock
if (interest_shock_TF) {
  interest <- c(rep(interest, p_change_interest - 1), rep(interest + interest_delta, t - p_change_interest + 1))
}

# Simulation
for (p in 2:t) {
  for (u in 1:n_households) {
    if (runif(1) < households$withdrawal_prob[p - 1, u]) {
      available_banks <- which(banks$bankruptcy[p - 1, ] == 0)
      if (length(available_banks) == 0) {
        households$which_bank[p, u] <- sample(1:n_banks, 1)
      } else {
        households$which_bank[p, u] <- sample(available_banks, 1)
      }
    } else {
      households$which_bank[p, u] <- households$which_bank[p - 1, u]
    }
  }
  
  for (b in 1:n_banks) {
    banks$sum_deposits[p, b] <- sum(households$depo_value[p, households$which_bank[p, ] == b])
  }
  
  borrowers$loan_value[p, ] <- borrowers$loan_value[p - 1, ]
  for (i in which(borrowers$loan_value[p, ] == 1)) {
    if (runif(1) < borrowers$pd[p, i]) {
      borrowers$loan_value[p, i] <- 0
    }
  }
  
  for (r in 1:n_banks) {
    banks$loan_portfolio[p, r] <- sum(borrowers$loan_value[p, borrowers$which_bank[p, ] == r])
  }
  
  for (a in 1:n_banks) {
    banks$exp_loss_provision[p, a] <- sum(borrowers$pd[p, borrowers$which_bank[p, ] == a] * borrowers$loan_value[p, borrowers$which_bank[p, ] == a] * (1 - recovery_rate))
  }
  
  for (l in 1:n_borrowers) {
    borrowers$cost_capital[p, l] <- (1 + ((loan_margin + 1) * interest[p]) - borrowers$pd[p, l] * recovery_rate) / (1 - borrowers$pd[p, l])
  }
  
  for (y in 1:n_banks) {
    banks$revenue[p, y] <- sum(borrowers$cost_capital[p, borrowers$which_bank[p, ] == y] * borrowers$loan_value[p, borrowers$which_bank[p, ] == y]) + banks$reserves[p - 1, y] * interest[p - 1]
    banks$cost[p, y] <- sum(households$depo_value[p - 1, households$which_bank[p, ] == y] * interest[p] * (1 + depo_margin)) + operational_cost * banks$sum_deposits[p - 1, y]
    banks$income[p, y] <- banks$revenue[p, y] - banks$cost[p, y] - (banks$exp_loss_provision[p - 1, y] - banks$exp_loss_provision[p, y])
    banks$equity[p, y] <- banks$equity[p - 1, y] + banks$income[p, y]
    banks$reserves[p, y] <- banks$sum_deposits[p, y] - banks$loan_portfolio[p, y]
  }
  
  for (k in which(banks$equity[p, ] <= 0)) {
    banks$bankruptcy[p, k] <- TRUE
    for (s in which(households$which_bank[p, ] == k)) {
      available_banks <- which(banks$bankruptcy[p, ] == FALSE)
      if (length(available_banks) == 0) {
        households$which_bank[p, s] <- sample(1:n_banks, 1)
      } else {
        households$which_bank[p, s] <- sample(available_banks, 1)
      }
    }
  }
}

# Results Visualization
bank_liquidity <- as.data.frame(banks$reserves)
bank_bankruptcy <- as.data.frame(banks$bankruptcy)

# Increase plot margins and save to file
png("bank_liquidity.png", width = 800, height = 600)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(bank_liquidity[, 1], type = 'l', col = 'blue', ylim = c(min(bank_liquidity), max(bank_liquidity)), ylab = 'Liquidity', xlab = 'Time', main = 'Bank Liquidity Over Time')
for (i in 2:n_banks) {
  lines(bank_liquidity[, i], col = 'blue')
}
dev.off()

png("bank_bankruptcy.png", width = 800, height = 600)
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
plot(rowSums(bank_bankruptcy), type = 'l', col = 'red', ylim = c(0, n_banks), ylab = 'Number of Bankruptcies', xlab = 'Time', main = 'Bank Bankruptcies Over Time')
dev.off()

print("Simulation completed!")
