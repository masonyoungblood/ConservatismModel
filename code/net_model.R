# NET MODEL ---------------------------------------------------------------

#set working directory, load source code, libraries
setwd(system("pwd", intern = T))
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

#run networked version of model and save
net_model <- parallel::mclapply(1:nrow(params), function(x){
  model(pop_size = pop_size, t = t, neg_cost = params$neg_costs[x], n_moves = params$n_moves[x], networked = TRUE, last_output = TRUE)
}, mc.cores = parallel::detectCores() - 1)
save(net_model, file = "data/net_model.RData")
