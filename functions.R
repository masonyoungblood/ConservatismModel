# FUNCTIONS ---------------------------------------------------------------

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
coord_game <- function(strat, pref, status_quo, a, b, neg_cost, data, power_weighted = FALSE){
  #create output objects
  actual_outcome <- NA
  payoff_outcome <- NA
  
  #get advertised move
  advertised_a <- ifelse(strat[1] == 0, pref, status_quo)
  advertised_b <- ifelse(data$advertisement[b] == 0, data$pref[b], data$status_quo[b])
  
  #if they match, return payoff of advertised move
  if(advertised_a == advertised_b){
    actual_outcome <- advertised_a
    payoff_outcome <- diag(data$payoffs[[a]])[advertised_a]
  }
  
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
    
    #return results
    actual_outcome <- ifelse(win, advertised_a, advertised_b)
    payoff_outcome <- ifelse(win, diag(data$payoffs[[a]])[advertised_a], diag(data$payoffs[[a]])[advertised_b])-neg_cost
  }
  
  #if they do not match and only one or neither are negotiators
  if(advertised_a != advertised_b & (strat[2] == 0 | data$negotiation[b] == 0)){
    actual_outcome <- 0
    payoff_outcome <- data$payoffs[[a]][advertised_a, advertised_b]
  }
  
  #return output
  return(c(actual_outcome, payoff_outcome))
}

#EWA observation function, to update the number of previous rounds of experience
observation <- function(n, phi = 0, kappa = 0){(phi*(1 - kappa)*n) + 1}

#EWA attraction function
attraction <- function(a, n, phi, delta, i, pi, kappa){(phi*n*a + (delta + (1 - delta)*i)*pi)/observation(n, phi, kappa)}

#EWA softmax function
softmax <- function(a, a_all, lambda){exp(lambda*a)/sum(exp(lambda*a_all))}

#self-tuning EWA function, which requires current attraction and payoff of both strategies: 10.1016/j.jet.2005.12.008
#modified to include social information, when frequencies are supplied (only for status quo moves): 10.1098/rstb.2008.0131
ewa <- function(a, n, strat_used, pi, kappa, lambda, cum, imm, gamma = NULL, freqs = NULL){
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
  
  #update attractions based on payoffs (pi), using social information if frequencies are supplied
  if(is.null(freqs)){a_new <- sapply(1:length(a), function(x){attraction(a[x], n[x], phi, deltas[x], strat_used[x], pi[x], kappa)})}
  if(!is.null(freqs)){a_new <- sapply(1:length(a), function(x){((1 - gamma)*attraction(a[x], n[x], phi, deltas[x], strat_used[x], pi[x], kappa)) + (gamma*freqs[x])})}
  
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
#self-tuning EWA parameters:
  #n (prior for n) (fixed at 1 for self-tuning EWA)
  #kappa (rate at which previous information is depreciated) (0-1, but fixed at 1 for self-tuning EWA)
  #lambda (randomness in softmax, where 0 is completely random and larger values are more deterministic)
model <- function(pop_size, t, priors = c(0, 0, 0),
                  n_moves = 4, default_strat = c(0, 0), out_of = 1,
                  neg_cost = 0, n = 1, kappa = 0, lambda = 50, gamma = 0.5,
                  networked = FALSE, power_weighted = FALSE){
  #initialize population of agents with 
  agents <- data.table::data.table(pref = NA,
                                   status_quo = sample(n_moves, pop_size, replace = TRUE),
                                   advertisement = default_strat[1], 
                                   negotiation = default_strat[2],
                                   payoffs = lapply(1:pop_size, function(x){payoff_matrix_constructor(n_moves = n_moves, out_of = out_of)}),
                                   power = rnorm(pop_size, mean = 0, sd = 1), 
                                   outcome = NA,
                                   a_status_quo = lapply(1:pop_size, function(x){c(priors[1], rep(0, n_moves - 1))}),
                                   a_advertisement = lapply(1:pop_size, function(x){c(priors[2], 0)}),
                                   a_negotiation = lapply(1:pop_size, function(x){c(priors[3], 0)}),
                                   n_status_quo = lapply(1:pop_size, function(x){rep(n, n_moves)}),
                                   n_advertisement = lapply(1:pop_size, function(x){c(n, n)}),
                                   n_negotiation = lapply(1:pop_size, function(x){c(n, n)}),
                                   cum_status_quo = lapply(1:pop_size, function(x){rep(0, n_moves)}),
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
    
    #store frequency table of status quo moves in case it's needed
    freq_table <- as.numeric(table(factor(agents$status_quo, levels = c(1:n_moves))))
    freq_table <- freq_table/sum(freq_table)
    
    #iterate through duos
    coord_game_results <- lapply(1:nrow(duos), function(j){
      #get actually played outcomes for a and b
      outcome_a <- coord_game(c(agents$advertisement[duos[j, 1]], agents$negotiation[duos[j, 1]]), agents$pref[duos[j, 1]], agents$status_quo[duos[j, 1]], duos[j, 1], duos[j, 2], neg_cost, agents, power_weighted = power_weighted)[1]
      outcome_b <- coord_game(c(agents$advertisement[duos[j, 2]], agents$negotiation[duos[j, 2]]), agents$pref[duos[j, 2]], agents$status_quo[duos[j, 2]], duos[j, 2], duos[j, 1], neg_cost, agents, power_weighted = power_weighted)[1]
      
      #if person a is an advertiser, calculate payoffs and solve ewa for status quo move
      if(agents$advertisement[duos[j, 1]] == 1){
        status_quo_payoffs_a <- sapply(1:n_moves, function(x){coord_game(c(agents$advertisement[duos[j, 1]], agents$negotiation[duos[j, 1]]), agents$pref[duos[j, 1]], x, duos[j, 1], duos[j, 2], neg_cost, agents, power_weighted = power_weighted)[2]})
        status_quo_ewa_a <- ewa(a = agents$a_status_quo[[duos[j, 1]]], n = agents$n_status_quo[[duos[j, 1]]], strat_used = agents$status_quo[duos[j, 1]], pi = status_quo_payoffs_a, kappa = kappa, lambda = lambda, 
                                cum = agents$cum_status_quo[[duos[j, 2]]]/(i - 1), imm = c(rep(0, agents$status_quo[duos[j, 2]] - 1), 1, rep(0, n_moves - agents$status_quo[duos[j, 2]])), gamma = gamma, freqs = freq_table)
      }
      
      #if person b is an advertiser, calculate payoffs and solve ewa for status quo move
      if(agents$advertisement[duos[j, 2]] == 1){
        status_quo_payoffs_b <- sapply(1:n_moves, function(x){coord_game(c(agents$advertisement[duos[j, 2]], agents$negotiation[duos[j, 2]]), agents$pref[duos[j, 2]], x, duos[j, 2], duos[j, 1], neg_cost, agents, power_weighted = power_weighted)[2]})
        status_quo_ewa_b <- ewa(a = agents$a_status_quo[[duos[j, 2]]], n = agents$n_status_quo[[duos[j, 2]]], strat_used = agents$status_quo[duos[j, 2]], pi = status_quo_payoffs_b, kappa = kappa, lambda = lambda, 
                                cum = agents$cum_status_quo[[duos[j, 1]]]/(i - 1), imm = c(rep(0, agents$status_quo[duos[j, 1]] - 1), 1, rep(0, n_moves - agents$status_quo[duos[j, 1]])), gamma = gamma, freqs = freq_table)
      }
      
      #calculate payoffs for advertisement
      advertisement_payoffs_a <- sapply(c(0, 1), function(x){coord_game(c(x, agents$negotiation[duos[j, 1]]), agents$pref[duos[j, 1]], agents$status_quo[duos[j, 1]], duos[j, 1], duos[j, 2], neg_cost, agents, power_weighted = power_weighted)[2]})
      advertisement_payoffs_b <- sapply(c(0, 1), function(x){coord_game(c(x, agents$negotiation[duos[j, 2]]), agents$pref[duos[j, 2]], agents$status_quo[duos[j, 2]], duos[j, 2], duos[j, 1], neg_cost, agents, power_weighted = power_weighted)[2]})
      
      #solve EWA for advertisement (note that cum and imm come from opponent, so opposite index of a, n, and strat_used)
      advertisement_ewa_a <- ewa(a = agents$a_advertisement[[duos[j, 1]]], n = agents$n_advertisement[[duos[j, 1]]], strat_used = agents$advertisement[duos[j, 1]] + 1, pi = advertisement_payoffs_a, kappa = kappa, lambda = lambda, cum = agents$cum_advertisement[[duos[j, 2]]]/(i - 1), imm = `if`(agents$advertisement[duos[j, 2]] == 0, c(1, 0), c(0, 1)))
      advertisement_ewa_b <- ewa(a = agents$a_advertisement[[duos[j, 2]]], n = agents$n_advertisement[[duos[j, 2]]], strat_used = agents$advertisement[duos[j, 2]] + 1, pi = advertisement_payoffs_b, kappa = kappa, lambda = lambda, cum = agents$cum_advertisement[[duos[j, 1]]]/(i - 1), imm = `if`(agents$advertisement[duos[j, 1]] == 0, c(1, 0), c(0, 1)))
      
      #calculate payoffs for negotiation
      negotiation_payoffs_a <- sapply(c(0, 1), function(x){coord_game(c(agents$advertisement[duos[j, 1]], x), agents$pref[duos[j, 1]], agents$status_quo[duos[j, 1]], duos[j, 1], duos[j, 2], neg_cost, agents, power_weighted = power_weighted)[2]})
      negotiation_payoffs_b <- sapply(c(0, 1), function(x){coord_game(c(agents$advertisement[duos[j, 2]], x), agents$pref[duos[j, 2]], agents$status_quo[duos[j, 2]], duos[j, 2], duos[j, 1], neg_cost, agents, power_weighted = power_weighted)[2]})
      
      #solve EWA for negotiation (note that cum and imm come from opponent, so opposite index of a, n, and strat_used)
      negotiation_ewa_a <- ewa(a = agents$a_negotiation[[duos[j, 1]]], n = agents$n_negotiation[[duos[j, 1]]], strat_used = agents$negotiation[duos[j, 1]] + 1, pi = negotiation_payoffs_a, kappa = kappa, lambda = lambda, cum = agents$cum_negotiation[[duos[j, 2]]]/(i - 1), imm = `if`(agents$negotiation[duos[j, 2]] == 0, c(1, 0), c(0, 1)))
      negotiation_ewa_b <- ewa(a = agents$a_negotiation[[duos[j, 2]]], n = agents$n_negotiation[[duos[j, 2]]], strat_used = agents$negotiation[duos[j, 2]] + 1, pi = negotiation_payoffs_b, kappa = kappa, lambda = lambda, cum = agents$cum_negotiation[[duos[j, 1]]]/(i - 1), imm = `if`(agents$negotiation[duos[j, 1]] == 0, c(1, 0), c(0, 1)))
      
      #sample new status quo and strategies for agent a
      pref_strats_a <- c(ifelse(agents$advertisement[duos[j, 1]] == 1, sample(1:n_moves, 1, prob = status_quo_ewa_a$probs), agents$status_quo[duos[j, 1]]),
                         sample(c(0, 1), 1, prob = advertisement_ewa_a$probs),
                         sample(c(0, 1), 1, prob = negotiation_ewa_a$probs))
      
      #sample new status quo and strategies for agent b
      pref_strats_b <- c(ifelse(agents$advertisement[duos[j, 2]] == 1, sample(1:n_moves, 1, prob = status_quo_ewa_b$probs), agents$status_quo[duos[j, 2]]),
                         sample(c(0, 1), 1, prob = advertisement_ewa_b$probs),
                         sample(c(0, 1), 1, prob = negotiation_ewa_b$probs))
      
      #return objects
      return(list(a = list(pref_strats = pref_strats_a,
                           outcome = outcome_a,
                           status_quo_a = `if`(agents$advertisement[duos[j, 1]] == 1, status_quo_ewa_a$a, agents$a_status_quo[[duos[j, 1]]]),
                           advertisement_a = advertisement_ewa_a$a,
                           negotiation_a = negotiation_ewa_a$a,
                           status_quo_n = `if`(agents$advertisement[duos[j, 1]] == 1, status_quo_ewa_a$n, agents$n_status_quo[[duos[j, 1]]]),
                           advertisement_n = advertisement_ewa_a$n,
                           negotiation_n = negotiation_ewa_a$n),
                  b = list(pref_strats = pref_strats_b,
                           outcome = outcome_b,
                           status_quo_a = `if`(agents$advertisement[duos[j, 2]] == 1, status_quo_ewa_b$a, agents$a_status_quo[[duos[j, 2]]]),
                           advertisement_a = advertisement_ewa_b$a,
                           negotiation_a = negotiation_ewa_b$a,
                           status_quo_n = `if`(agents$advertisement[duos[j, 2]] == 1, status_quo_ewa_b$n, agents$n_status_quo[[duos[j, 2]]]),
                           advertisement_n = advertisement_ewa_b$n,
                           negotiation_n = negotiation_ewa_b$n)))
    })
    
    #add 1 to each position of the cumulative history vectors for status quo, advertisement, and negotiation
    agents$cum_status_quo <- lapply(1:pop_size, function(x){
      temp <- agents$cum_status_quo[[x]]
      temp[[agents$status_quo[[x]]]] <- temp[[agents$status_quo[[x]]]] + 1
      return(temp)
    })
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
    agents$status_quo[c(duos$x, duos$y)] <- pref_strats[, 1]
    agents$advertisement[c(duos$x, duos$y)] <- pref_strats[, 2]
    agents$negotiation[c(duos$x, duos$y)] <- pref_strats[, 3]
    
    #overwrite a values
    agents$a_status_quo[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$status_quo_a}),
                                                lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$status_quo_a}))
    agents$a_advertisement[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$advertisement_a}),
                                                   lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$advertisement_a}))
    agents$a_negotiation[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$negotiation_a}),
                                                 lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$negotiation_a}))
    
    #overwrite n values
    agents$n_status_quo[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$status_quo_n}),
                                                lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$status_quo_n}))
    agents$n_advertisement[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$advertisement_n}),
                                                   lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$advertisement_n}))
    agents$n_negotiation[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$negotiation_n}),
                                                 lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$negotiation_n}))
    
    #overwrite actually played outcomes
    agents$outcome[c(duos$x, duos$y)] <- c(lapply(1:nrow(duos), function(x){coord_game_results[[x]]$a$outcome}),
                                           lapply(1:nrow(duos), function(x){coord_game_results[[x]]$b$outcome}))
    
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
plot_model <- function(output, colors = NULL, xlim = NULL){
  #get all possible combinations of strategies
  poss_patterns <- expand.grid(rep(list(c(0, 1)), 2))
  poss_patterns <- sapply(1:nrow(poss_patterns), function(x){paste0(poss_patterns[x, ], collapse = "")})
  
  #store pop_size and t from output
  t <- ifelse(is.null(xlim), length(output), xlim)
  pop_size <- nrow(output[[1]])
  
  #convert output to matrix
  output_mat <- sapply(1:t, function(x){as.numeric(table(factor(sapply(1:pop_size, function(y){paste0(output[[x]][y, 3:4], collapse = "")}), levels = poss_patterns)))})
  
  #generate colors for plotting
  if(is.null(colors)){colors <- c("black", randomcoloR::distinctColorPalette(length(poss_patterns) - 1))}
  
  #plot
  par(mar = c(4, 4, 1, 1))
  plot(1:t, output_mat[1,]/pop_size, ylim = c(0, 1), ylab = "Proportion", xlab = "Time", type = "l", col = colors[1], lwd = 2)
  for(i in 2:nrow(output_mat)){lines(1:t, output_mat[i,]/pop_size, col = colors[i], lwd = 2)}
  legend("topright", legend = poss_patterns, col = colors, pch = 19, ncol = 2)
}
