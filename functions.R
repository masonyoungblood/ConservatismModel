# FUNCTIONS ---------------------------------------------------------------

#advertisement is whether an agent advertises their preferred move (0) or the status quo move (1)
#negotiation is whether an agent tries to initiate negotiation (1) or not (0)

#function for payoff matrix construction (row is self, column is other)
payoff_matrix_constructor <- function(n_moves, out_of, off_diag = 0){
  #generate matrix n_moves*n_moves with value off_diag
  payoffs <- matrix(off_diag, nrow = n_moves, ncol = n_moves)
  
  #replace diagonal with sample from uniform distribution
  diag(payoffs) <- runif(n = n_moves, min = off_diag, max = out_of)
  
  #return the matrix
  return(payoffs)
}

#play coordination game with an opponent (b) using an arbitrary strategy and preference
#strat is a 2L vector with a 0/1 for advertisement and negotiation
#return character with actual negotiation outcome ("W" or "L") when actual == TRUE
coord_game <- function(strat, pref, a, b, status_quo, neg_cost, data, actual = FALSE, power_weighted = FALSE){
  #create output objects
  payoff_outcome <- NA
  neg_outcome <- NA
  
  #get advertised move
  advertised_a <- ifelse(strat[1] == 0, pref, status_quo)
  advertised_b <- ifelse(data$advertisement[b] == 0, data$pref[b], status_quo)
  
  #if they match, return payoff of advertised move
  if(advertised_a == advertised_b){payoff_outcome <- diag(data$payoffs[[a]])[advertised_a]}
  
  #if they do not match and both are negotiators, return payoff of move from agent with highest power
  if(advertised_a != advertised_b & strat[2] == 1 & data$negotiation[b] == 1){
    #if power is used to weight probability
    if(power_weighted){
      #convert power values to probability with softmax
      prob_a <- exp(data$power[a])/sum(exp(data$power[a]), exp(data$power[b]))
      
      #base win on power values
      win <- sample(c(TRUE, FALSE), 1, prob = c(prob_a, 1 - prob_a))
    } else{
      #base win on simply who is higher in value
      win <- data$power[a] > data$power[b]
    }
    
    #if it's a win and actual == TRUE, supply both "W" and the actual strategy
    #otherwise return the payoff of winner's move (from a's payoff matrix)
    neg_outcome <- ifelse(win, "W", "L")
    payoff_outcome <- ifelse(win, diag(data$payoffs[[a]])[advertised_a], diag(data$payoffs[[a]])[advertised_b])-neg_cost
  }
  
  #if they do not match and only one or neither are negotiators
  if(advertised_a != advertised_b & (strat[2] == 0 | data$negotiation[b] == 0)){payoff_outcome <- data$payoffs[[a]][advertised_a, advertised_b]}
  
  #return appropriate output
  return(ifelse(actual, neg_outcome, payoff_outcome))
}

#ewa observation function, to update the number of previous rounds of experience
observation <- function(n, phi = 0, kappa = 0){(phi*(1 - kappa)*n) + 1}

#ewa attraction function, with optional loss aversion
attraction <- function(a, n, phi, delta, i, pi, kappa, loss_averse = FALSE, out_of){
  #if loss_averse == TRUE
  if(loss_averse){
    #then set delta to 0 when pi above average
    ifelse(pi < out_of/2,
           (phi*n*a + (delta + (1 - delta)*i)*pi)/observation(n, phi, kappa),
           (phi*n*a + i*pi)/observation(n, phi, kappa))
  } else{
    (phi*n*a + (delta + (1 - delta)*i)*pi)/observation(n, phi, kappa)
  }
}

#ewa attraction function
tuning_attraction <- function(a, n, phi, delta, i, pi, kappa){(phi*n*a + (delta + (1 - delta)*i)*pi)/observation(n, phi, kappa)}

#ewa softmax function
softmax <- function(a, a_all, lambda){exp(lambda*a)/sum(exp(lambda*a_all))}

#full ewa function, which requires current attraction and payoff of both strategies
#when delta != 0, a, strat_used, and pi are vectors (strat_used is 0s and 1s, where 1 denotes which was used)
ewa <- function(a, n, phi, delta, strat_used, pi, kappa, lambda, loss_averse = FALSE, out_of){
  #if strat_used is a single number, convert to vector
  if(length(strat_used) == 1){
    temp <- rep(0, length(a))
    temp[strat_used] <- 1
    strat_used <- temp
  }
  
  #update attractions based on payoffs (pi)
  a_new <- sapply(1:length(a), function(x){attraction(a[x], n, phi, delta, strat_used[x], pi[x], kappa, loss_averse, out_of)})

  #update (global) n
  n_new <- observation(n, phi, kappa)
  
  #get probabilities based on new attractions
  probs <- sapply(1:length(a_new), function(x){softmax(a_new[x], a_new, lambda)})
  
  #return everything named list
  return(list(a = a_new, n = n_new, probs = probs))
}

#10.1016/j.jet.2005.12.008
tuning_ewa <- function(a, n, strat_used, pi, kappa, lambda, cum, imm){
  #if strat_used is a single number, convert to vector (0 if wasn't played, 1 if was played)
  if(length(strat_used) == 1){
    temp <- rep(0, length(a))
    temp[strat_used] <- 1
    strat_used <- temp
  }
  
  #calculating tuning phi, where imm is binary vector and cum is frequency of previous plays
  phi <- 1 - (0.5*sum((cum-imm)^2))
  
  #calculating tuning delta
  deltas <- sapply(1:length(a), function(x){ifelse(pi[x] >= pi[which(strat_used == 1)], 1, 0)})
  
  #update attractions based on payoffs (pi)
  a_new <- sapply(1:length(a), function(x){tuning_attraction(a[x], n[x], phi, deltas[x], strat_used[x], pi[x], kappa)})
  
  #update (local) n's
  n_new <- sapply(1:length(a), function(x){observation(n[x], phi, kappa)})

  #get probabilities based on new attractions
  probs <- sapply(1:length(a), function(x){softmax(a_new[x], a_new, lambda)})
  
  #return everything in a named list
  return(list(a = a_new, n = n_new, probs = probs))
}

#function for sampling unique rows from network
network_sampler <- function(pop_structure){
  #sample first edge
  sampled_edges <- sample(nrow(pop_structure), 1)
  
  #overwrite remaining rows with unsampled nodes
  remaining <- c(1:nrow(pop_structure))[-which(pop_structure[, 1] %in% c(pop_structure[sampled_edges, ]) | pop_structure[, 2] %in% c(pop_structure[sampled_edges, ]))]
  
  #while edges are available to be sampled
  while(length(remaining) > 0){
    #sample from remaining rows
    sampled_edges <- c(sampled_edges, ifelse(length(remaining) > 1, sample(remaining, 1), remaining))
    
    #overwrite remaining rows with unsampled nodes
    remaining <- c(1:nrow(pop_structure))[-which(pop_structure[, 1] %in% c(pop_structure[sampled_edges, ]) | pop_structure[, 2] %in% c(pop_structure[sampled_edges, ]))]
  }
  
  #return edges to be sampled
  return(sampled_edges)
}

#full model function
#ewa parameters:
  #n (prior for n)
  #phi (weight given to new experience) (0-1)
  #delta (whether only used strategy is updated (0) or used and unused strategies are updated (1)) (0-1)
  #kappa (rate at which previous information is depreciated) (0-1)
  #lambda (randomness in softmax, where 0 is completely random and larger values are more deterministic)
#special conditions:
  #average reinforcement learning: delta = 0, kappa = 0, n = 1... (simplified: (phi*a + i*pi)/(phi + 1))
  #cumulative reinforcement learning: delta = 0, kappa = 1, n = 1... (simplifed: phi*a + i*pi)
  #weighted fictitious play: delta = 1, kappa = 0, n = 1... (simplified: (phi*a + pi)/(phi + 1))
  #fictitious play: phi = 1, delta = 1, kappa = 0, n = 1... (simplified: (a + pi)/2))
  #cournot best response: phi = 0, delta = 1, kappa = 0, n = 1... (simplified: pi)
model <- function(pop_size, t, status_quo = 1, priors = c(0, 0, 0),
                  n_moves = 4, default_strat = c(0, 0), out_of = 1,
                  neg_cost = 0, n = 1, phi = 1, delta = 0, kappa = 0, lambda = 50,
                  pref_payoff = FALSE, loss_averse = FALSE, static_prefs = FALSE, networked = FALSE, power_weighted = FALSE){
  #set initial move probabilities (was initially allowed to be customized in the model definition)
  #init_move_probs <- c(1, 1, 1, 1)
  init_move_probs <- rep(1, n_moves)
  
  #initialize population of agents with 
  agents <- data.table::data.table(pref = sample(n_moves, pop_size, replace = TRUE, prob = init_move_probs),
                                   advertisement = default_strat[1], negotiation = default_strat[2],
                                   payoffs = lapply(1:pop_size, function(x){payoff_matrix_constructor(n_moves = n_moves, out_of = out_of)}),
                                   power = rnorm(pop_size, mean = 0, sd = 1), neg_outcome = NA,
                                   a_moves = lapply(1:pop_size, function(x){c(priors[1], rep(0, n_moves - 1))}),
                                   a_advertisement = lapply(1:pop_size, function(x){c(priors[2], 0)}),
                                   a_negotiation = lapply(1:pop_size, function(x){c(priors[3], 0)}))
  
  #if agents prefer their highest payoff move, then overwrite accordingly
  if(pref_payoff){agents$pref <- sapply(1:pop_size, function(x){which.max(diag(agents$payoffs[[x]]))})}
  
  #create output object
  output <- list()
  output[[1]] <- agents
  
  #if networked = TRUE, then generate a scale free network (m = 2 leads to mean degree of 4), and set seed so network is consistent across iterations
  if(networked){
    set.seed(12345)
    pop_structure <- igraph::as_edgelist(igraph::sample_pa(pop_size, directed = FALSE, m = 2))
  }
  
  for(i in 2:t){
    #generate data frame of duos to coordinate (fully-connected or networked population)
    if(networked){
      duos <- as.data.frame(pop_structure[network_sampler(pop_structure), ])
      colnames(duos) <- c("x", "y")
    } else{
      duos <- data.frame(x = sample(1:pop_size, pop_size/2), y = NA)
      duos$y <- sample(c(1:pop_size)[-duos$x])
    }
    
    #iterate through duos
    coord_game_results <- lapply(1:nrow(duos), function(j){
      #get actually played outcomes for a and b
      neg_outcome_a <- coord_game(as.numeric(agents[duos[j, 1], 2:3]), agents$pref[duos[j, 1]], duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, actual = TRUE, power_weighted = power_weighted)
      neg_outcome_b <- coord_game(as.numeric(agents[duos[j, 2], 2:3]), agents$pref[duos[j, 2]], duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, actual = TRUE, power_weighted = power_weighted)
      
      if(!static_prefs){
        #calculate payoffs from different moves
        move_payoffs_a <- sapply(1:n_moves, function(x){coord_game(as.numeric(agents[duos[j, 1], 2:3]), x, duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, power_weighted = power_weighted)})
        move_payoffs_b <- sapply(1:n_moves, function(x){coord_game(as.numeric(agents[duos[j, 2], 2:3]), x, duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, power_weighted = power_weighted)})
        
        #solve ewa for moves
        move_ewa_a <- ewa(a = agents$a_moves[[duos[j, 1]]], n = n, phi = phi, delta = delta, strat_used = agents$pref[duos[j, 1]], 
                          pi = move_payoffs_a, kappa = kappa, lambda = lambda, loss_averse = loss_averse, out_of = out_of)
        move_ewa_b <- ewa(a = agents$a_moves[[duos[j, 2]]], n = n, phi = phi, delta = delta, strat_used = agents$pref[duos[j, 2]], 
                          pi = move_payoffs_b, kappa = kappa, lambda = lambda, loss_averse = loss_averse, out_of = out_of)
      }
      
      #calculate payoffs for advertisement
      advertisement_payoffs_a <- sapply(c(0, 1), function(x){coord_game(c(x, agents$negotiation[duos[j, 1]]),
                                                                        agents$pref[duos[j, 1]], duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      advertisement_payoffs_b <- sapply(c(0, 1), function(x){coord_game(c(x, agents$negotiation[duos[j, 2]]),
                                                                        agents$pref[duos[j, 2]], duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      
      #solve ewa for advertisement
      advertisement_ewa_a <- ewa(a = agents$a_advertisement[[duos[j, 1]]], n = n, phi = phi, delta = delta, strat_used = agents$advertisement[duos[j, 1]] + 1, 
                                 pi = advertisement_payoffs_a, kappa = kappa, lambda = lambda, loss_averse = loss_averse, out_of = out_of)
      advertisement_ewa_b <- ewa(a = agents$a_advertisement[[duos[j, 2]]], n = n, phi = phi, delta = delta, strat_used = agents$advertisement[duos[j, 2]] + 1, 
                                 pi = advertisement_payoffs_b, kappa = kappa, lambda = lambda, loss_averse = loss_averse, out_of = out_of)
      
      #calculate payoffs for negotiation
      negotiation_payoffs_a <- sapply(c(0, 1), function(x){coord_game(c(agents$advertisement[duos[j, 1]], x),
                                                                      agents$pref[duos[j, 1]], duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      negotiation_payoffs_b <- sapply(c(0, 1), function(x){coord_game(c(agents$advertisement[duos[j, 2]], x),
                                                                      agents$pref[duos[j, 2]], duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      
      #solve ewa for negotiation
      negotiation_ewa_a <- ewa(a = agents$a_negotiation[[duos[j, 1]]], n = n, phi = phi, delta = delta, strat_used = agents$negotiation[duos[j, 1]] + 1, 
                               pi = negotiation_payoffs_a, kappa = kappa, lambda = lambda, loss_averse = loss_averse, out_of = out_of)
      negotiation_ewa_b <- ewa(a = agents$a_negotiation[[duos[j, 2]]], n = n, phi = phi, delta = delta, strat_used = agents$negotiation[duos[j, 2]] + 1, 
                               pi = negotiation_payoffs_b, kappa = kappa, lambda = lambda, loss_averse = loss_averse, out_of = out_of)
      
      #sample new preference and strategies for agent a
      if(!static_prefs){
        pref_strats_a <- c(sample(n_moves, 1, prob = move_ewa_a$probs), sample(c(0, 1), 1, prob = advertisement_ewa_a$probs),
                           sample(c(0, 1), 1, prob = negotiation_ewa_a$probs))
      } else{
        pref_strats_a <- c(sample(c(0, 1), 1, prob = advertisement_ewa_a$probs),
                           sample(c(0, 1), 1, prob = negotiation_ewa_a$probs))
      }
      
      #sample new preference and strategies for agent b
      if(!static_prefs){
        pref_strats_b <- c(sample(n_moves, 1, prob = move_ewa_b$probs), sample(c(0, 1), 1, prob = advertisement_ewa_b$probs),
                           sample(c(0, 1), 1, prob = negotiation_ewa_b$probs))
      } else{
        pref_strats_b <- c(sample(c(0, 1), 1, prob = advertisement_ewa_b$probs),
                           sample(c(0, 1), 1, prob = negotiation_ewa_b$probs))
      }
      
      #return objects
      if(!static_prefs){
        return(list(a = list(pref_strats = pref_strats_a,
                             neg_outcome = neg_outcome_a,
                             move_a = move_ewa_a$a,
                             advertisement_a = advertisement_ewa_a$a,
                             negotiation_a = negotiation_ewa_a$a),
                    b = list(pref_strats = pref_strats_b,
                             neg_outcome = neg_outcome_b,
                             move_a = move_ewa_b$a,
                             advertisement_a = advertisement_ewa_b$a,
                             negotiation_a = negotiation_ewa_b$a)))
      } else{
        return(list(a = list(pref_strats = pref_strats_a,
                             neg_outcome = neg_outcome_a,
                             advertisement_a = advertisement_ewa_a$a,
                             negotiation_a = negotiation_ewa_a$a),
                    b = list(pref_strats = pref_strats_b,
                             neg_outcome = neg_outcome_b,
                             advertisement_a = advertisement_ewa_b$a,
                             negotiation_a = negotiation_ewa_b$a)))
      }
    })
    
    #get preferred strategies of everyone in column 1 of duos, and then everyone in column 2
    pref_strats <- do.call(rbind, c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$pref_strats}), 
                                    lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$pref_strats})))
    
    #overwrite current preferences and strategies
    if(!static_prefs){
      agents$pref[c(duos$x, duos$y)] <- pref_strats[, 1]
      agents$advertisement[c(duos$x, duos$y)] <- pref_strats[, 2]
      agents$negotiation[c(duos$x, duos$y)] <- pref_strats[, 3]
    } else{
      agents$advertisement[c(duos$x, duos$y)] <- pref_strats[, 1]
      agents$negotiation[c(duos$x, duos$y)] <- pref_strats[, 2]
    }
    
    #overwrite a values for preferences and strategies
    if(!static_prefs){
      agents$a_moves[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$move_a}),
                                             lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$move_a}))
    }
    agents$a_advertisement[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$advertisement_a}),
                                                   lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$advertisement_a}))
    agents$a_negotiation[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$negotiation_a}),
                                                 lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$negotiation_a}))
    
    #overwrite actually played outcomes
    agents$neg_outcome[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$neg_outcome}),
                                               lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$neg_outcome}))
    
    #overwrite n
    n <- observation(n, phi, kappa)
    
    #remove objects
    rm(list = c("duos", "coord_game_results", "pref_strats"))
    
    #store output
    output[[i]] <- agents
  }
  
  #return output
  return(output)
  
  #garbage collection
  gc()
}

# pop_size <- 10
# t <- 5
# status_quo <- 1
# priors <- c(0, 0)
# n_moves <- 4
# default_strat <- c(0, 0)
# out_of <- 1
# neg_cost <- 0
# n <- 1
# kappa <- 0
# lambda <- 5
# networked <- FALSE
# power_weighted <- FALSE

tuning_model <- function(pop_size, t, status_quo = 1, priors = c(0, 0),
                         n_moves = 4, default_strat = c(0, 0), out_of = 1,
                         neg_cost = 0, n = 1, kappa = 0, lambda = 50,
                         networked = FALSE, power_weighted = FALSE){
  #set initial move probabilities (was initially allowed to be customized in the model definition)
  init_move_probs <- rep(1, n_moves)
  
  #initialize population of agents with 
  agents <- data.table::data.table(pref = NA,
                                   advertisement = default_strat[1], 
                                   negotiation = default_strat[2],
                                   payoffs = lapply(1:pop_size, function(x){payoff_matrix_constructor(n_moves = n_moves, out_of = out_of)}),
                                   power = rnorm(pop_size, mean = 0, sd = 1), 
                                   neg_outcome = NA,
                                   a_advertisement = lapply(1:pop_size, function(x){c(priors[1], 0)}),
                                   a_negotiation = lapply(1:pop_size, function(x){c(priors[2], 0)}),
                                   n_advertisement = lapply(1:pop_size, function(x){c(n, n)}),
                                   n_negotiation = lapply(1:pop_size, function(x){c(n, n)}),
                                   cum_advertisement = lapply(1:pop_size, function(x){c(0, 0)}),
                                   cum_negotiation = lapply(1:pop_size, function(x){c(0, 0)}))
  
  #overwrite preferred moves to be highest payoff moves
  agents$pref <- sapply(1:pop_size, function(x){which.max(diag(agents$payoffs[[x]]))})
  
  #create output object
  output <- list()
  output[[1]] <- agents
  
  #if networked = TRUE, then generate a scale free network (m = 2 leads to mean degree of 4), and set seed so network is consistent across iterations
  if(networked){
    set.seed(12345)
    pop_structure <- igraph::as_edgelist(igraph::sample_pa(pop_size, directed = FALSE, m = 2))
  }
  
  for(i in 2:t){
    #generate data frame of duos to coordinate (fully-connected or networked population)
    if(networked){
      duos <- as.data.frame(pop_structure[network_sampler(pop_structure), ])
      colnames(duos) <- c("x", "y")
    } else{
      duos <- data.frame(x = sample(1:pop_size, pop_size/2), y = NA)
      duos$y <- sample(c(1:pop_size)[-duos$x])
    }
    
    #iterate through duos
    coord_game_results <- lapply(1:nrow(duos), function(j){
      #get actually played outcomes for a and b
      neg_outcome_a <- coord_game(as.numeric(agents[duos[j, 1], 2:3]), agents$pref[duos[j, 1]], duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, actual = TRUE, power_weighted = power_weighted)
      neg_outcome_b <- coord_game(as.numeric(agents[duos[j, 2], 2:3]), agents$pref[duos[j, 2]], duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, actual = TRUE, power_weighted = power_weighted)
      
      #calculate payoffs for advertisement
      advertisement_payoffs_a <- sapply(c(0, 1), function(x){coord_game(c(x, agents$negotiation[duos[j, 1]]), agents$pref[duos[j, 1]], duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      advertisement_payoffs_b <- sapply(c(0, 1), function(x){coord_game(c(x, agents$negotiation[duos[j, 2]]), agents$pref[duos[j, 2]], duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      
      #solve ewa for advertisement (note that cum and imm come from opponent, so opposite index of a, n, and strat_used)
      advertisement_ewa_a <- tuning_ewa(a = agents$a_advertisement[[duos[j, 1]]], n = agents$n_advertisement[[duos[j, 1]]], strat_used = agents$advertisement[duos[j, 1]] + 1, pi = advertisement_payoffs_a, kappa = kappa, lambda = lambda, cum = agents$cum_advertisement[[duos[j, 2]]]/(i - 1), imm = `if`(agents$advertisement[duos[j, 2]] == 0, c(1, 0), c(0, 1)))
      advertisement_ewa_b <- tuning_ewa(a = agents$a_advertisement[[duos[j, 2]]], n = agents$n_advertisement[[duos[j, 2]]], strat_used = agents$advertisement[duos[j, 2]] + 1, pi = advertisement_payoffs_b, kappa = kappa, lambda = lambda, cum = agents$cum_advertisement[[duos[j, 1]]]/(i - 1), imm = `if`(agents$advertisement[duos[j, 1]] == 0, c(1, 0), c(0, 1)))
      
      #calculate payoffs for negotiation
      negotiation_payoffs_a <- sapply(c(0, 1), function(x){coord_game(c(agents$advertisement[duos[j, 1]], x), agents$pref[duos[j, 1]], duos[j, 1], duos[j, 2], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      negotiation_payoffs_b <- sapply(c(0, 1), function(x){coord_game(c(agents$advertisement[duos[j, 2]], x), agents$pref[duos[j, 2]], duos[j, 2], duos[j, 1], status_quo, neg_cost, agents, power_weighted = power_weighted)})
      
      #solve ewa for negotiation (note that cum and imm come from opponent, so opposite index of a, n, and strat_used)
      negotiation_ewa_a <- tuning_ewa(a = agents$a_negotiation[[duos[j, 1]]], n = agents$n_negotiation[[duos[j, 1]]], strat_used = agents$negotiation[duos[j, 1]] + 1, pi = negotiation_payoffs_a, kappa = kappa, lambda = lambda, cum = agents$cum_negotiation[[duos[j, 2]]]/(i - 1), imm = `if`(agents$negotiation[duos[j, 2]] == 0, c(1, 0), c(0, 1)))
      negotiation_ewa_b <- tuning_ewa(a = agents$a_negotiation[[duos[j, 2]]], n = agents$n_negotiation[[duos[j, 2]]], strat_used = agents$negotiation[duos[j, 2]] + 1, pi = negotiation_payoffs_b, kappa = kappa, lambda = lambda, cum = agents$cum_negotiation[[duos[j, 1]]]/(i - 1), imm = `if`(agents$negotiation[duos[j, 1]] == 0, c(1, 0), c(0, 1)))
      
      #sample new strategies for agent a
      pref_strats_a <- c(sample(c(0, 1), 1, prob = advertisement_ewa_a$probs),
                         sample(c(0, 1), 1, prob = negotiation_ewa_a$probs))
      
      #sample new strategies for agent b
      pref_strats_b <- c(sample(c(0, 1), 1, prob = advertisement_ewa_b$probs),
                         sample(c(0, 1), 1, prob = negotiation_ewa_b$probs))
      
      #return objects
      return(list(a = list(pref_strats = pref_strats_a,
                           neg_outcome = neg_outcome_a,
                           advertisement_a = advertisement_ewa_a$a,
                           negotiation_a = negotiation_ewa_a$a,
                           advertisement_n = advertisement_ewa_a$n,
                           negotiation_n = negotiation_ewa_a$n),
                  b = list(pref_strats = pref_strats_b,
                           neg_outcome = neg_outcome_b,
                           advertisement_a = advertisement_ewa_b$a,
                           negotiation_a = negotiation_ewa_b$a,
                           advertisement_n = advertisement_ewa_b$n,
                           negotiation_n = negotiation_ewa_b$n)))
    })
    
    #add 1 to each position of the cumulative history vectors for advertisement and negotiation
    agents$cum_advertisement <- lapply(1:pop_size, function(x){
      temp <- agents$cum_advertisement[[x]]
      temp[[agents$advertisement[[x]] + 1]] <- temp[[agents$advertisement[[x]] + 1]] + 1
      return(temp)
    })
    agents$cum_negotiation <- lapply(1:pop_size, function(x){
      temp <- agents$cum_negotiation[[x]]
      temp[[agents$negotiation[[x]] + 1]] <- temp[[agents$negotiation[[x]] + 1]] + 1
      return(temp)
    })
    
    #get preferred strategies of everyone in column 1 of duos, and then everyone in column 2
    pref_strats <- do.call(rbind, c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$pref_strats}), 
                                    lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$pref_strats})))
    
    #overwrite current preferences and strategies
    agents$advertisement[c(duos$x, duos$y)] <- pref_strats[, 1]
    agents$negotiation[c(duos$x, duos$y)] <- pref_strats[, 2]
    
    #overwrite a values for strategies
    agents$a_advertisement[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$advertisement_a}),
                                                   lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$advertisement_a}))
    agents$a_negotiation[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$negotiation_a}),
                                                 lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$negotiation_a}))
    
    #overwrite n values for strategies
    agents$n_advertisement[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$advertisement_n}),
                                                   lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$advertisement_n}))
    agents$n_negotiation[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$negotiation_n}),
                                                 lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$negotiation_n}))
    
    #overwrite actually played outcomes
    agents$neg_outcome[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$neg_outcome}),
                                               lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$neg_outcome}))
    
    #remove objects
    rm(list = c("duos", "coord_game_results", "pref_strats"))
    
    #store output
    output[[i]] <- agents
  }
  
  #return output
  return(output)
  
  #garbage collection
  gc()
}


#model plotting function, strategies or moves (strats = TRUE/FALSE)
plot_model <- function(output, colors = NULL, strats = TRUE, xlim = NULL){
  if(strats){
    #get all possible combinations of strategies
    poss_patterns <- expand.grid(rep(list(c(0, 1)), 2))
    poss_patterns <- sapply(1:nrow(poss_patterns), function(x){paste0(poss_patterns[x, ], collapse = "")})
    
    #store pop_size and t from output
    t <- ifelse(is.null(xlim), length(output), xlim)
    pop_size <- nrow(output[[1]])
    
    #convert output to matrix
    output_mat <- sapply(1:t, function(x){as.numeric(table(factor(sapply(1:pop_size, function(y){paste0(output[[x]][y, 2:3], collapse = "")}), levels = poss_patterns)))})
    
    #generate colors for plotting
    if(is.null(colors)){colors <- c("black", randomcoloR::distinctColorPalette(length(poss_patterns) - 1))}
    
    #plot
    par(mar = c(4, 4, 1, 1))
    plot(1:t, output_mat[1,]/pop_size, ylim = c(0, 1), ylab = "Proportion", xlab = "Time", type = "l", col = colors[1], lwd = 2)
    for(i in 2:nrow(output_mat)){lines(1:t, output_mat[i,]/pop_size, col = colors[i], lwd = 2)}
    legend("topright", legend = poss_patterns, col = colors, pch = 19, ncol = 2)
  } else{
    #get number of moves
    n_moves <- length(output[[1]]$a_moves[[1]])
    
    #store pop_size and t from output
    t <- ifelse(is.null(xlim), length(output), xlim)
    pop_size <- nrow(output[[1]])
    
    #convert output to matrix
    output_mat <- sapply(1:t, function(x){as.numeric(table(factor(output[[x]]$pref, levels = 1:n_moves)))})
    
    #generate colors for plotting
    colors <- c("black", distinctColorPalette(n_moves - 1))
    
    #plot
    par(mar = c(4, 4, 1, 1))
    plot(1:t, output_mat[1,]/pop_size, ylim = c(0, 1), ylab = "Proportion", xlab = "Time", type = "l", col = colors[1], lwd = 2)
    for(i in 2:nrow(output_mat)){lines(1:t, output_mat[i,]/pop_size, col = colors[i], lwd = 2)}
    legend("topright", legend = 1:n_moves, col = colors, pch = 19, ncol = 2)
  }
}
