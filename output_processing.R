# OUTPUT PROCESSING -------------------------------------------------------

#set working directory, load source code, libraries
setwd(system("pwd", intern = T))
library(data.table)

#set models to loop through
models <- c("full", "net")

#set parameters
pop_size <- 5000
t <- 100
neg_costs <- seq(0, 1, 0.1)
n_moves <- seq(2, 12, 1)

#loop through models
for(i in 1:length(models)){
  #load in output file
  output <- readRDS(paste0("_rslurm_", models[i], "_model/results_0.RDS"))
  
  #store params in data table
  params <- data.table(neg_costs = rep(neg_costs, length(n_moves)),
                       n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))
  
  #store proportion of each strategy in params data frame
  params$conservative <- lapply(1:nrow(params), function(x){
    sapply(1:t, function(y){
      length(which(output[[x]][[y]]$advertisement == 1 & output[[x]][[y]]$negotiation == 0))/pop_size
    })
  })
  params$advertisement <- lapply(1:nrow(params), function(x){
    sapply(1:t, function(y){
      length(which(output[[x]][[y]]$advertisement == 1))/pop_size
    })
  })
  params$negotiation <- lapply(1:nrow(params), function(x){
    sapply(1:t, function(y){
      length(which(output[[x]][[y]]$negotiation == 1))/pop_size
    })
  })
  
  #save simplified output
  save(params, file = paste0("output/", models[i], "_output.RData"))
  
  #save final timesteps as separate object
  final <- lapply(1:length(output), function(x){output[[x]][[t]]})
  save(final, file = paste0("output/", models[i], "_final.RData"))
  
  #remove objects
  rm(list = c("output", "params", "attractive"))
}
