# DISRUPTION SIMULATION ---------------------------------------------------

#set working directory, load source code, libraries
setwd("/gpfs/home/myoungblood/ConservatismModel")
source("code/functions.R")

#store required packages
pkgs <- unique(getParseData(parse("code/functions.R"))$text[getParseData(parse("code/functions.R"))$token == "SYMBOL_PACKAGE"])

#set parameters
n <- 10000
t <- 2000
t_2 <- 200
cost <- c(0.5, 0.5, 0.5, 0.5)
moves <- c(2, 2, 2, 2)
gamma <- c(0, 0.25, 0, 0.25)
f <- c(0, 2.5, 0, 2.5)
networked <- c(FALSE, FALSE, TRUE, TRUE)
props <- seq(0.1, 0.9, by = 0.1)

#put parameters into data frame
base_params <- data.frame(cost = cost, moves = moves, gamma = gamma, f = f, networked = networked)
disrupt_params <- data.frame(model = rep(1:length(cost), each = length(props)), props = rep(props, length(cost)), cost = rep(cost, each = length(props)), moves = rep(moves, each = length(props)), gamma = rep(gamma, each = length(props)), f = rep(f, each = length(props)), networked = rep(networked, each = length(props)))

#remove original parameter objects
rm(list = c("cost", "moves", "gamma", "f", "networked", "props"))

#set seed
set.seed(123)

#run base model code
base_model <- parallel::mclapply(1:nrow(base_params), function(x){
  #collect the full output
  full_output <- model(pop_size = n, t = t, neg_cost = base_params$cost[x], n_moves = base_params$moves[x], gamma = base_params$gamma[x], f = base_params$f[x], networked = base_params$networked[x])
  
  #compute the frequency of status quo moves in each timestep and combine into a matrix
  sq_freqs <- do.call(rbind, lapply(1:length(full_output), function(m){as.numeric(table(factor(full_output[[m]]$status_quo, levels = 1:base_params$moves[x])))}))
  
  #return the frequencies and the final timestep of agents
  return(list(sq_freqs, full_output[[t]]))
}, mc.cores = parallel::detectCores() - 1)

#run disrupt model code
disrupt_model <- parallel::mclapply(1:nrow(disrupt_params), function(x){
  #store base model and compute status quo
  base <- base_model[[disrupt_params$model[x]]]
  status_quo <- which.max(base[[1]][t, ])
  
  #get frequency of previous status quo
  base_freqs <- base[[1]][, status_quo]/n
  
  #store new status quo
  object <- base[[2]]
  if(disrupt_params$moves[x] == 2){
    new_status_quo <- c(1:disrupt_params$moves[x])[-status_quo]
  } else{
    new_status_quo <- sample(c(1:disrupt_params$moves[x])[-status_quo], 1)
  }
  
  #create table of fresh agents
  pop_size <- n*disrupt_params$props[x]
  agents <- data.table::data.table(pref = NA,
                                   status_quo = new_status_quo,
                                   advertisement = 0,
                                   negotiation = 0,
                                   payoffs = lapply(1:pop_size, function(y){payoff_matrix_constructor(n_moves = disrupt_params$moves[x], out_of = 1)}),
                                   power = rnorm(pop_size, mean = 0, sd = 1),
                                   outcome = NA,
                                   a_status_quo = lapply(1:pop_size, function(y){c(0, rep(0, disrupt_params$moves[x] - 1))}),
                                   a_advertisement = lapply(1:pop_size, function(y){c(0, 0)}),
                                   a_negotiation = lapply(1:pop_size, function(y){c(0, 0)}),
                                   n_status_quo = lapply(1:pop_size, function(y){rep(1, disrupt_params$moves[x])}),
                                   n_advertisement = lapply(1:pop_size, function(y){c(1, 1)}),
                                   n_negotiation = lapply(1:pop_size, function(y){c(1, 1)}),
                                   cum_status_quo = lapply(1:pop_size, function(y){rep(0, disrupt_params$moves[x])}),
                                   cum_advertisement = lapply(1:pop_size, function(y){c(0, 0)}),
                                   cum_negotiation = lapply(1:pop_size, function(y){c(0, 0)}),
                                   lifetime_payoff = 0)
  agents$pref <- sapply(1:pop_size, function(y){which.max(diag(agents$payoffs[[y]]))})
  
  #overwrite proportion of existing agents with new agents
  object <- rbind(object[1:(n-pop_size), ], agents)
  
  #run new simulations and store simplified output
  output <- model(pop_size = n, t = t_2, neg_cost = disrupt_params$cost[x], n_moves = disrupt_params$moves[x], supply_agents = object, gamma = disrupt_params$gamma[x], f = disrupt_params$f[x], networked = disrupt_params$networked[x])
  c(base_freqs, sapply(1:t_2, function(y){length(which(output[[y]]$status_quo == status_quo))/n}))
}, mc.cores = parallel::detectCores() - 1)

#save output in named list
disrupt_model <- list(base = base_model, disrupt = disrupt_model)
save(disrupt_model, file = "data/disrupt_model.RData")
