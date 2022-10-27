# BASE MODEL --------------------------------------------------------------

#set working directory, load source code, libraries
setwd(system("pwd", intern = T))
source("functions.R")

#set parameters
pop_size <- 10000
t <- 100
neg_costs <- seq(0, 5, 0.5)
n_moves <- seq(2, 12, 1)

#store params in data frame
params <- data.frame(neg_costs = rep(neg_costs, length(n_moves)),
                     n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))

#iterate through one simulation per combo of params
base_model <- lapply(1:nrow(params), function(x){
  model(pop_size, t, priors = c(5, 0, 0, 0), neg_cost = params$neg_costs[x], n_moves = params$n_moves[x], phi = 0.5, delta = 0, kappa = 0, lambda = 1, cores = 47)
})

#save simulations
save(base_model, file = "base_model.RData")
