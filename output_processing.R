# OUTPUT PROCESSING -------------------------------------------------------

#set working directory, load source code, libraries
setwd(system("pwd", intern = T))

#set models to loop through
models <- c("base", "base_net", "loss", "loss_net")

#set parameters
neg_costs <- seq(0, 10, 1)
n_moves <- seq(2, 12, 1)
pop_size <- 500
t <- 100

#loop through models
for(i in 1:length(models)){
  #load in output file
  output <- readRDS(paste0("_rslurm_", models[i], "_model/results_0.RDS"))
  
  #store params in data frame
  params <- data.frame(neg_costs = rep(neg_costs, length(n_moves)),
                       n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))
  
  #store proportion of each strategy in params data frame
  params$conservative <- sapply(1:nrow(params), function(x){
    median(sapply(1:t, function(y){
      length(which(output[[x]][[y]]$advertisement == 1 & output[[x]][[y]]$negotiation == 0 & output[[x]][[y]]$matching == 0))/pop_size
    }))
  })
  params$advertisement <- sapply(1:nrow(params), function(x){
    median(sapply(1:t, function(y){
      length(which(output[[x]][[y]]$advertisement == 1))/pop_size
    }))
  })
  params$negotiation <- sapply(1:nrow(params), function(x){
    median(sapply(1:t, function(y){
      length(which(output[[x]][[y]]$negotiation == 1))/pop_size
    }))
  })
  params$matching <- sapply(1:nrow(params), function(x){
    median(sapply(1:t, function(y){
      length(which(output[[x]][[y]]$matching == 1))/pop_size
    }))
  })
  
  #store median a value for status quo move and arbitrary other move in params data frame
  attractive <- list(
    status_quo = lapply(1:nrow(params), function(x){
      sapply(1:t, function(y){
        median(sapply(1:pop_size, function(z){output[[x]][[y]]$a_moves[[z]][1]}))
      })
    }),
    arbitrary = lapply(1:nrow(params), function(x){
      sapply(1:t, function(y){
        median(sapply(1:pop_size, function(z){output[[x]][[y]]$a_moves[[z]][2]}))
      })
    })
  )
  
  #save simplified output
  save(params, file = paste0("output/", models[i], "_output.RData"))
  
  #save a values
  save(attractive, file = paste0("output/", models[i], "_attr.RData"))
  
  #save final timesteps as separate object
  final <- lapply(1:length(output), function(x){output[[x]][[t]]})
  save(final, file = paste0("output/", models[i], "_final.RData"))
  
  #remove objects
  rm(list = c("output", "params", "attractive"))
}
