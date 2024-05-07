# CONF MODEL ---------------------------------------------------------------

#set working directory, load source code, libraries
setwd("/gpfs/home/myoungblood/ConservatismModel")
source("code/functions.R")

#store required packages
pkgs <- unique(getParseData(parse("code/functions.R"))$text[getParseData(parse("code/functions.R"))$token == "SYMBOL_PACKAGE"])

#set parameters
pop_size <- 10000
t <- 2000
neg_costs <- seq(0, 1, 0.125)
n_moves <- seq(2, 10, 1)

#store params in data frame
params <- data.frame(neg_costs = rep(neg_costs, length(n_moves)),
                     n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))

#run conformist version of model and save output
conf_model <- parallel::mclapply(1:nrow(params), function(x){
  model(pop_size = pop_size, t = t, neg_cost = params$neg_costs[x], n_moves = params$n_moves[x], gamma = 0.25, f = 2.5, last_output = TRUE)
}, mc.cores = parallel::detectCores() - 1)
save(conf_model, file = "data/conf_model.RData")
