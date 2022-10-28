# BASE MODEL --------------------------------------------------------------

#set working directory, load source code, libraries
setwd(system("pwd", intern = T))
source("functions.R")

#store required packages
pkgs <- unique(getParseData(parse("functions.R"))$text[getParseData(parse("functions.R"))$token == "SYMBOL_PACKAGE"])

#set parameters
neg_costs <- seq(0, 5, 0.5)
n_moves <- seq(2, 12, 1)

#store params in data frame
params <- data.frame(neg_costs = rep(neg_costs, length(n_moves)),
                     n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))

#wrap model function for slurm
model_slurm <- function(neg_costs, n_moves){
  model(pop_size = 5000, t = 100, priors = c(5, 0, 0, 0), neg_cost = neg_costs, n_moves = n_moves, phi = 0.5, delta = 0, kappa = 0, lambda = 1, cores = 1)
}

#run simulations
rslurm::slurm_apply(model_slurm, params, jobname = "base_model",
                    nodes = 1, cpus_per_node = 39, pkgs = pkgs,
                    global_objects = objects(), slurm_options = list(mem = 0))
