# DISRUPTION SIMULATION ---------------------------------------------------

#set working directory, load source code, libraries
setwd(system("pwd", intern = T))
source("functions.R")

#store required packages
pkgs <- unique(getParseData(parse("functions.R"))$text[getParseData(parse("functions.R"))$token == "SYMBOL_PACKAGE"])

#set parameters
n <- 10
t <- 2000
t_2 <- 200
cost <- c(0.5, 0.5, 0.5, 0.5)
moves <- c(2, 2, 2, 2)
gamma <- c(0, 0.25, 0, 0.25)
f <- c(0, 2.5, 0, 2.5)
networked <- c(FALSE, FALSE, TRUE, TRUE)
props <- seq(0.1, 0.9, by = 0.1)

#put parameters into data frame format for slurm
base_params <- data.frame(cost = cost, moves = moves, gamma = gamma, f = f, networked = networked)
disrupt_params <- data.frame(base = rep(1:length(cost), each = length(props)), props = rep(props, length(cost)), cost = rep(cost, each = length(props)), moves = rep(moves, each = length(props)), gamma = rep(gamma, each = length(props)), f = rep(f, each = length(props)), networked = rep(networked, each = length(props)))

#remove original parameter objects
rm(list = c("cost", "moves", "gamma", "f", "networked", "props"))

#wrarp base model for slurm
base_model_slurm <- function(cost, moves, gamma, f, networked){model(pop_size = pop_size, t = t, neg_cost = cost, n_moves = moves, gamma = gamma, f = f, networked = networked)}

#wrap distruption model for slurm
disrupt_model_slurm <- function(base, props, cost, moves, gamma, f, networked){
  #store base model and compute status quo
  base <- base_output[[base]]
  status_quo <- which.max(as.numeric(table(factor(base[[t]]$status_quo, levels = 1:moves))))

  #get frequency of previous status quo
  base_freqs <- sapply(1:t, function(y){length(which(base[[y]]$status_quo == status_quo))/n})

  #store new status quo
  object <- base[[t]]
  if(moves == 2){
    new_status_quo <- c(1:moves)[-which.max(as.numeric(table(factor(object$status_quo, levels = 1:moves))))]
  } else{
    new_status_quo <- sample(c(1:moves)[-which.max(as.numeric(table(factor(object$status_quo, levels = 1:moves))))], 1)
  }

  #create table of fresh agents
  pop_size <- n*props
  agents <- data.table::data.table(pref = NA,
                                   status_quo = new_status_quo,
                                   advertisement = 0,
                                   negotiation = 0,
                                   payoffs = lapply(1:pop_size, function(x){payoff_matrix_constructor(n_moves = moves, out_of = 1)}),
                                   power = rnorm(pop_size, mean = 0, sd = 1),
                                   outcome = NA,
                                   a_status_quo = lapply(1:pop_size, function(x){c(0, rep(0, moves - 1))}),
                                   a_advertisement = lapply(1:pop_size, function(x){c(0, 0)}),
                                   a_negotiation = lapply(1:pop_size, function(x){c(0, 0)}),
                                   n_status_quo = lapply(1:pop_size, function(x){rep(1, moves)}),
                                   n_advertisement = lapply(1:pop_size, function(x){c(1, 1)}),
                                   n_negotiation = lapply(1:pop_size, function(x){c(1, 1)}),
                                   cum_status_quo = lapply(1:pop_size, function(x){rep(0, moves)}),
                                   cum_advertisement = lapply(1:pop_size, function(x){c(0, 0)}),
                                   cum_negotiation = lapply(1:pop_size, function(x){c(0, 0)}))
  agents$pref <- sapply(1:pop_size, function(x){which.max(diag(agents$payoffs[[x]]))})

  #overwrite proportion of existing agents with new agents
  object <- rbind(object[1:(n-pop_size), ], agents)

  #run new simulations and store simplified output
  output <- model(pop_size = n, t = t_2, neg_cost = cost, n_moves = moves, supply_agents = object, gamma = gamma, f = f, networked = networked, simple_output = TRUE)
  list(base_freqs = base_freqs, disrupt_freqs = do.call(rbind, lapply(1:length(output), function(y){output[[y]][[1]]})))
}

#set seed
set.seed(123)

#run base model
base_job <- rslurm::slurm_apply(model_slurm, params, jobname = "base_model",
                                nodes = 1, cpus_per_node = 4, pkgs = pkgs,
                                global_objects = objects(), slurm_options = list(mem = "100G"))

#store base output for disruption model
base_output <- get_slurm_out(base_job)

#run disruption model
disrupt_job <- rslurm::slurm_apply(model_slurm, params, jobname = "disrupt_model",
                                   nodes = 1, cpus_per_node = 36, pkgs = pkgs,
                                   global_objects = objects(), slurm_options = list(mem = "100G"))
