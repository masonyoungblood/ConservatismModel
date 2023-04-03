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

#set seed
set.seed(123)

#run simulations
disruption_sims <- lapply(1:length(gamma), function(z){
  #first run base model and compute previous status quo
  base <- model(pop_size = n, t = t, neg_cost = cost[z], n_moves = moves[z], gamma = gamma[z], f = f[z], networked = networked[z])
  status_quo <- which.max(as.numeric(table(factor(base[[t]]$status_quo, levels = 1:moves[z]))))

  #get frequency of previous status quo
  base_freqs <- sapply(1:t, function(y){length(which(base[[y]]$status_quo == status_quo))/n})

  #hard reset proportion of population with alternate status quo
  disrupt_freqs <- parallel::mclapply(1:length(props), function(x){
    object <- base[[t]]
    if(moves[z] == 2){
      new_status_quo <- c(1:moves[z])[-which.max(as.numeric(table(factor(object$status_quo, levels = 1:moves[z]))))]
    } else{
      new_status_quo <- sample(c(1:moves[z])[-which.max(as.numeric(table(factor(object$status_quo, levels = 1:moves[z]))))], 1)
    }
    pop_size <- n*props[x]

    #create table of fresh agents
    agents <- data.table::data.table(pref = NA,
                                     status_quo = new_status_quo,
                                     advertisement = 0,
                                     negotiation = 0,
                                     payoffs = lapply(1:pop_size, function(x){payoff_matrix_constructor(n_moves = moves[z], out_of = 1)}),
                                     power = rnorm(pop_size, mean = 0, sd = 1),
                                     outcome = NA,
                                     a_status_quo = lapply(1:pop_size, function(x){c(0, rep(0, moves[z] - 1))}),
                                     a_advertisement = lapply(1:pop_size, function(x){c(0, 0)}),
                                     a_negotiation = lapply(1:pop_size, function(x){c(0, 0)}),
                                     n_status_quo = lapply(1:pop_size, function(x){rep(1, moves[z])}),
                                     n_advertisement = lapply(1:pop_size, function(x){c(1, 1)}),
                                     n_negotiation = lapply(1:pop_size, function(x){c(1, 1)}),
                                     cum_status_quo = lapply(1:pop_size, function(x){rep(0, moves[z])}),
                                     cum_advertisement = lapply(1:pop_size, function(x){c(0, 0)}),
                                     cum_negotiation = lapply(1:pop_size, function(x){c(0, 0)}))
    agents$pref <- sapply(1:pop_size, function(x){which.max(diag(agents$payoffs[[x]]))})

    #overwrite proportion of existing agents with new agents
    object <- rbind(object[1:(n-pop_size), ], agents)

    #run new simulations and store simplified output
    output <- model(pop_size = n, t = t_2, neg_cost = cost[z], n_moves = moves[z], supply_agents = object, gamma = gamma[z], f = f[z], networked = networked[z], simple_output = TRUE)
    return(do.call(rbind, lapply(1:length(output), function(y){output[[y]][[1]]})))
  }, mc.cores = 9)

  #return all simplified output
  return(list(base_freqs = base_freqs, disrupt_freqs = disrupt_freqs))
})

#save everything
save(disruption_sims, file = "disruption_sims.RData")
