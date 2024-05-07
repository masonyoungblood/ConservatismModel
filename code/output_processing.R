# PROPORTION HEATMAPS -----------------------------------------------------

#set workspace
setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")

#load libraries
library(parallel)
library(randomcoloR)
library(ggplot2)
library(cowplot)
library(compiler)
library(profvis)

#loop through models
for(i in 1:3){
  if(i == 1){load("data/full_model.RData"); target_obj <- full_model; rm(full_model)}
  if(i == 2){load("data/net_model.RData"); target_obj <- net_model; rm(net_model)}
  if(i == 3){load("data/conf_model.RData"); target_obj <- conf_model; rm(conf_model)}
  
  #set parameters
  pop_size <- 10000
  neg_costs <- seq(0, 1, 0.125)
  n_moves <- seq(2, 10, 1)
  params <- data.frame(neg_costs = rep(neg_costs, length(n_moves)),
                       n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))
  
  #run through actual outcomes and replace 0 positions (landing off the diagonal) with the played move that landed them off the diagonal
  for(x in 1:length(target_obj)){
    temp <- unlist(target_obj[[x]]$outcome)
    for(y in which(temp == 0)){temp[y] <- ifelse(target_obj[[x]]$advertisement[y] == 1, 1, target_obj[[x]]$pref[y])}
    target_obj[[x]]$outcome <- temp
  }
  
  #return proportions and ranks of each strategy
  obj_adv_props <- sapply(1:length(target_obj), function(x){as.numeric(table(factor(target_obj[[x]]$advertisement, levels = c(0, 1))))[2]})/pop_size
  obj_neg_props <- sapply(1:length(target_obj), function(x){as.numeric(table(factor(target_obj[[x]]$negotiation, levels = c(0, 1))))[2]})/pop_size
  obj_00_props <- sapply(1:length(target_obj), function(x){length(which(target_obj[[x]]$advertisement == 0 & target_obj[[x]]$negotiation == 0))})/pop_size
  obj_10_props <- sapply(1:length(target_obj), function(x){length(which(target_obj[[x]]$advertisement == 1 & target_obj[[x]]$negotiation == 0))})/pop_size
  obj_01_props <- sapply(1:length(target_obj), function(x){length(which(target_obj[[x]]$advertisement == 0 & target_obj[[x]]$negotiation == 1))})/pop_size
  obj_11_props <- sapply(1:length(target_obj), function(x){length(which(target_obj[[x]]$advertisement == 1 & target_obj[[x]]$negotiation == 1))})/pop_size
  obj_ranks <- factor(sapply(1:length(target_obj), function(x){as.numeric(which.max(table(factor(paste0(target_obj[[x]]$advertisement, target_obj[[x]]$negotiation), levels = c("00", "10", "01", "11")))))}), levels = c(1, 2, 3, 4))
  
  #return z-score of the status quo move
  obj_quo <- sapply(1:length(target_obj), function(x){
    #max(table(factor(target_obj[[x]]$status_quo, levels = 1:params$n_moves[[x]])))/(pop_size/params$n_moves[[x]])
    #(max(table(factor(target_obj[[x]]$status_quo, levels = 1:params$n_moves[[x]])))-median(table(factor(target_obj[[x]]$status_quo, levels = 1:params$n_moves[[x]]))))/pop_size
    obs <- max(table(factor(target_obj[[x]]$status_quo, levels = 1:params$n_moves[[x]])))/pop_size
    
    #code to convert to a z-score
    pred <- 1/params$n_moves[[x]]
    (obs-pred)/sqrt(pred*(1-pred)*(1/pop_size)) #https://stats.stackexchange.com/questions/49945/finding-the-test-statistic-for-a-majority-hypothesis #https://vitalflux.com/one-proportion-z-test-formula-examples/
  })
  
  #combine into object for plotting
  obj_data <- data.frame(neg_costs = params$neg_costs, n_moves = params$n_moves, adv = obj_adv_props, neg = obj_neg_props, mav = obj_00_props, pure_cons = obj_10_props, pure_neg = obj_01_props, flex = obj_11_props, rank = obj_ranks, quo = obj_quo)
  
  #create plot for advertisement
  a <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = adv)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "Prop.", colours = c("#0072B2", "white", "#D55E00"),
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1)) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("1X: Signaling Status Quo")
  
  #create plot for negotiation
  b <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = neg)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "Prop.", colours = c("#0072B2", "white", "#D55E00"),
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1)) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("X1: Negotiation")
  
  #create plot for mavericks
  c <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = mav)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "Prop.", colours = c("#0072B2", "white", "#D55E00"),
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1)) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("00: Maverick")
  
  #create plot for pure conservatism
  d <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = pure_cons)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "Prop.", colours = c("#0072B2", "white", "#D55E00"),
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1)) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("10: Conservative")
  
  #create plot for pure negotiation
  e <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = pure_neg)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "Prop.", colours = c("#0072B2", "white", "#D55E00"),
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1)) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("01: Negotiator")
  
  #create plot for flexible
  f <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = flex)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "Prop.", colours = c("#0072B2", "white", "#D55E00"),
                         limits = c(0, 1),
                         breaks = c(0, 0.5, 1),
                         labels = c(0, 0.5, 1)) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("11: Flexible")
  
  #create plot for ranks
  obj_data_dummy <- rbind(c(0, 2, 1, 1, 1, 1, 1, 1, 1, 1), obj_data) #required for plotting the missing data
  g <- ggplot(obj_data_dummy, aes(x = neg_costs, y = n_moves, fill = rank)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
    theme_linedraw(base_size = 8) + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + 
    theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("Most Common Strategy")
  
  #create plot for status quo
  h <- ggplot(obj_data, aes(x = neg_costs, y = n_moves, fill = quo)) + geom_tile() + xlab("Negotiation Cost") + ylab("Number of Moves") +
    scale_fill_gradientn(name = "z-score", colours = c("white", "#CC79A7"),
                         breaks = c(min(obj_data$quo), round(max(obj_data$quo)/2), max(obj_data$quo)),
                         labels = c(0, round(max(obj_data$quo)/2), round(max(obj_data$quo)))) +
    theme_linedraw(base_size = 8) + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0), breaks = c(2, 4, 6, 8, 10, 12), labels = c(2, 4, 6, 8, 10, 12)) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
    ggtitle("Playing Status Quo")
  
  #export cowplot
  if(i == 1){png("plots/full_heatmaps.png", width = 4.6, height = 4, units = "in", res = 1200)}
  if(i == 2){png("plots/net_heatmaps.png", width = 4.6, height = 4, units = "in", res = 1200)}
  if(i == 3){png("plots/conf_heatmaps.png", width = 4.6, height = 4, units = "in", res = 1200)}
  print({plot_grid(a + theme(legend.position = "none"), get_legend(a), b + theme(legend.position = "none"), get_legend(b),
                   h + theme(legend.position = "none"), get_legend(h), g + theme(legend.position = "none"), get_legend(g),
                   nrow = 2, ncol = 4, rel_widths = c(1, 0.26, 1, 0.26))})
  dev.off()
  
  #export cowplot
  if(i == 1){png("plots/full_strat_heatmaps.png", width = 4.2, height = 4, units = "in", res = 1200)}
  if(i == 2){png("plots/net_strat_heatmaps.png", width = 4.2, height = 4, units = "in", res = 1200)}
  if(i == 3){png("plots/conf_strat_heatmaps.png", width = 4.2, height = 4, units = "in", res = 1200)}
  print({plot_grid(
    plot_grid(c + theme(legend.position = "none"), d + theme(legend.position = "none"), ncol = 1),
    plot_grid(e + theme(legend.position = "none"), f + theme(legend.position = "none"), ncol = 1),
    plot_grid(get_legend(c)),
    nrow = 1, ncol = 3, rel_widths = c(1, 1, 0.24)
  )})
  dev.off()
  
  while(!is.null(dev.list())){dev.off()}
}

# DENSITY PLOTS -----------------------------------------------------------

#set workspace
setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")

#load libraries
library(parallel)
library(randomcoloR)
library(ggplot2)
library(cowplot)
library(compiler)
library(profvis)

#loop through models
for(i in 1:3){
  if(i == 1){load("data/full_model.RData"); target_obj <- full_model; rm(full_model)}
  if(i == 2){load("data/net_model.RData"); target_obj <- net_model; rm(net_model)}
  if(i == 3){load("data/conf_model.RData"); target_obj <- conf_model; rm(conf_model)}
  
  #compute density of expected vs. played preferred and status quo moves
  dens_data <- as.data.frame(do.call(rbind, lapply(1:length(target_obj), function(x){
    #get proportions
    p_obs_pref <- length(which(target_obj[[x]]$outcome == target_obj[[x]]$pref))/pop_size
    p_obs_sq <- length(which(target_obj[[x]]$outcome == as.numeric(which.max(table(factor(target_obj[[x]]$status_quo, levels = 1:params$n_moves[x]))))))/pop_size
    p_exp <- 1/params$n_moves[x]
    
    ##https://stats.stackexchange.com/questions/49945/finding-the-test-statistic-for-a-majority-hypothesis #https://vitalflux.com/one-proportion-z-test-formula-examples/
    z_pref <- (p_obs_pref - p_exp)/sqrt(p_exp*(1 - p_exp)*(1/pop_size))
    z_sq <- (p_obs_sq - p_exp)/sqrt(p_exp*(1 - p_exp)*(1/pop_size))
    
    #return values
    return(c(z_pref, z_sq))
  })))
  colnames(dens_data) <- c("pref", "sq")
  
  #combine into object for plotting
  dens_plot_data <- data.frame(x = c(density(dens_data$pref, n = 1000)$x, density(dens_data$sq, n = 1000)$x),
                               y = c(density(dens_data$pref, n = 1000)$y, density(dens_data$sq, n = 1000)$y),
                               sep = factor(rep(1:2, each = 1000)))
  
  #export cowplot
  if(i == 1){png("plots/full_density.png", width = 3.4, height = 2.4, units = "in", res = 1200)}
  if(i == 2){png("plots/net_density.png", width = 3.4, height = 2.4, units = "in", res = 1200)}
  if(i == 3){png("plots/conf_density.png", width = 3.4, height = 2.4, units = "in", res = 1200)}
  print({ggplot(dens_plot_data, aes(x = x, y = y, color = sep)) + geom_line() + xlab("z-score (Observed vs. Expected Prop. w/Random Play)") + ylab("Density") +
      geom_vline(xintercept = 0, lty = "dashed") +
      theme_linedraw(base_size = 8) + theme(plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next"), legend.position = c(0.805, 0.84), legend.background = element_blank(), legend.key = element_rect(fill = NA)) +
      scale_color_manual(name = NULL, values = c("#0072B2", "#D55E00"), labels = c("Preference", "Status Quo")) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
      scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0), limits = c(0, max(dens_plot_data$y)+(max(dens_plot_data$y)*0.05)))})
  dev.off()
  
  while(!is.null(dev.list())){dev.off()}
}

# COMPARE PLOTS -----------------------------------------------------------

#set workspace
setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")

#load libraries
library(parallel)
library(randomcoloR)
library(ggplot2)
library(cowplot)
library(compiler)
library(profvis)

#loop through models
for(i in 1:3){
  if(i == 1){load("data/full_model.RData"); target_obj <- full_model; rm(full_model)}
  if(i == 2){load("data/net_model.RData"); target_obj <- net_model; rm(net_model)}
  if(i == 3){load("data/conf_model.RData"); target_obj <- conf_model; rm(conf_model)}
  
  #set parameters
  pop_size <- 10000
  neg_costs <- seq(0, 1, 0.125)
  n_moves <- seq(2, 10, 1)
  params <- data.frame(neg_costs = rep(neg_costs, length(n_moves)),
                       n_moves = unlist(lapply(1:length(n_moves), function(x){rep(n_moves[x], length(neg_costs))})))
  
  #combined negotation and moves into single parameter for plot grouping
  params$combined <- paste0(params$neg_costs, params$n_moves)
  
  #format for constructing plots
  plot_data <- do.call("rbind", lapply(1:length(target_obj), function(y){
    data.frame(adv = target_obj[[y]]$advertisement, neg = target_obj[[y]]$negotiation, power = target_obj[[y]]$power,
               n_moves = params$n_moves[y], neg_costs = params$neg_costs[y],
               combined = params$combined[y])
  }))
  plot_data$neg_costs <- factor(plot_data$neg_costs)
  plot_data$strategy <- 0
  plot_data$strategy[which(plot_data$adv == 0 & plot_data$neg == 0)] <- 1
  plot_data$strategy[which(plot_data$adv == 1 & plot_data$neg == 0)] <- 2
  plot_data$strategy[which(plot_data$adv == 0 & plot_data$neg == 1)] <- 3
  plot_data$strategy[which(plot_data$adv == 1 & plot_data$neg == 1)] <- 4
  plot_data$combined_2 <- paste0(plot_data$strategy, plot_data$combined)
  
  #choose parameter combinations to plot
  target_moves <- c(2, 3, 4)
  target_cost <- c(0.125, 0.25, 0.375, 0.5)
  
  #set up y-labelling
  ylabel <- function(stuff){
    ggplot() + ylab(stuff) + xlab(NULL) +
      theme_minimal(base_size = 8) +
      theme(axis.title = element_text(face = "bold"), text = element_text(family = "Avenir Next"))
  }
  
  #set up x-labelling
  xlabel <- function(stuff){
    ggplot() + ylab(NULL) + xlab(stuff) +
      theme_minimal(base_size = 8) +
      theme(axis.title = element_text(face = "bold"), text = element_text(family = "Avenir Next"))
  }
  
  #set alpha transparency of points
  alpha <- 0.025
  
  #construct list of list of plots
  grid <- lapply(1:length(target_moves), function(i){
    lapply(1:length(target_cost), function(j){
      ggplot(plot_data[which(plot_data$n_moves == target_moves[i] & plot_data$neg_costs == target_cost[j]), ], aes(x = factor(strategy), y = power, fill = factor(strategy))) +
        geom_hline(yintercept = 0, lty = "dashed", size = 0.2) +
        geom_jitter(size = 0.5, width = 0.35, height = 0.15, aes(color = factor(strategy))) +
        geom_boxplot(width = 0.35, outlier.color = "transparent") +
        theme_linedraw(base_size = 8) + xlab("Strategy") + ylab("Power") +
        scale_fill_manual(values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), drop = FALSE) + theme(legend.position = "none", plot.title = element_text(face = "bold")) +
        scale_color_manual(values = scales::alpha(c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), alpha)) + scale_y_continuous(position = "right", limits = c(-3, 3)) +
        scale_x_discrete(labels = c("00", "10", "01", "11")) +
        theme(plot.margin = unit(c(0,0,0,0), "cm"), text = element_text(family = "Avenir Next"))
    })
  })
  
  #export cowplot
  if(i == 1){png("plots/full_power_compare.png", width = 6.8, height = 4, units = "in", res = 1200)}
  if(i == 2){png("plots/net_power_compare.png", width = 6.8, height = 4, units = "in", res = 1200)}
  if(i == 3){png("plots/conf_power_compare.png", width = 6.8, height = 4, units = "in", res = 1200)}
  print({plot_grid(plot_grid(ylabel(""), xlabel(paste0("COST: ", target_cost[1], "\n")), xlabel(paste0("COST: ", target_cost[2], "\n")), xlabel(paste0("COST: ", target_cost[3], "\n")), xlabel(paste0("COST: ", target_cost[4], "\n")), ylabel(""),
                             ylabel(paste0("MOVES: ", target_moves[1])), grid[[1]][[1]] + xlab(NULL) + ylab(NULL) + theme(axis.text = element_blank()), grid[[1]][[2]] + xlab(NULL) + ylab(NULL) + theme(axis.text = element_blank()), grid[[1]][[3]] + xlab(NULL) + ylab(NULL) + theme(axis.text = element_blank()), grid[[1]][[4]] + xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_blank()), ylabel(""),
                             ylabel(paste0("MOVES: ", target_moves[2])), grid[[2]][[1]] + xlab(NULL) + ylab(NULL) + theme(axis.text = element_blank()), grid[[2]][[2]] + xlab(NULL) + ylab(NULL) + theme(axis.text = element_blank()), grid[[2]][[3]] + xlab(NULL) + ylab(NULL) + theme(axis.text = element_blank()), grid[[2]][[4]] + xlab(NULL) + ylab(NULL) + theme(axis.text.x = element_blank()), ylabel("Power") + scale_y_continuous(position = "right") + theme(axis.title = element_text(face = "plain")),
                             ylabel(paste0("   MOVES: ", target_moves[3])), grid[[3]][[1]] + xlab(NULL) + ylab(NULL) + theme(axis.text.y = element_blank()), grid[[3]][[2]] + xlab(NULL) + ylab(NULL) + theme(axis.text.y = element_blank()), grid[[3]][[3]] + xlab(NULL) + ylab(NULL) + theme(axis.text.y = element_blank()), grid[[3]][[4]] + xlab(NULL) + ylab(NULL), ylabel(""),
                             nrow = 4, ncol = 6, rel_widths = c(0.15, 1, 1, 1, 1, 0.15), rel_heights = c(0.18, 1, 1, 1.05)),
                   xlabel("Strategy") + theme(axis.title = element_text(face = "plain")),
                   nrow = 2, rel_heights = c(sum(c(0.15, 1, 1, 1)), 0.15))})
  dev.off()
  
  while(!is.null(dev.list())){dev.off()}
}

# TIME DYNAMICS -----------------------------------------------------------

#set working directory
setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")

#load libraries
library(parallel)
library(randomcoloR)
library(ggplot2)
library(cowplot)
library(compiler)
library(profvis)
library(gifski)
library(gganimate)
library(ggsankey)
library(dplyr)

#source the base functions
source("code/functions.R")

#set up y-labelling
ylabel <- function(stuff){
  ggplot() + ylab(stuff) + xlab(NULL) +
    theme_minimal(base_size = 8) + scale_x_continuous(expand = c(0, 0)) + theme(text = element_text(family = "Avenir Next"))
}

#set up x-labelling
xlabel <- function(stuff){
  ggplot() + ylab(NULL) + xlab(stuff) +
    theme_minimal(base_size = 8) + theme(text = element_text(family = "Avenir Next"))
}

#set parameters
pop_size <- 200
t <- 15
neg_cost <- 0.5
n_moves <- 7

#get model output
output <- model(pop_size = pop_size, t = t, neg_cost = neg_cost, n_moves = n_moves)

#each row is an individual, columns are timesteps
plot_data <- do.call("rbind",
                     lapply(1:t, function(i){
                       data.frame(time = i,
                                  ind = factor(1:pop_size),
                                  strat = factor(paste0(output[[i]]$advertisement, output[[i]]$negotiation), levels = c("00", "10", "01", "11")),
                                  x = sapply(1:nrow(output[[i]]), function(j){output[[i]]$a_advertisement[[j]][2]-output[[i]]$a_advertisement[[j]][1]}),
                                  y = sapply(1:nrow(output[[i]]), function(j){output[[i]]$a_negotiation[[j]][2]-output[[i]]$a_negotiation[[j]][1]}))
                     }))

#set axis limits
xlims <- c(min(plot_data$x), max(plot_data$x))
ylims <- c(min(plot_data$y), max(plot_data$y))

#create plot of first batch of timesteps
a <- ggplot(plot_data[which(plot_data$time %in% 1:5), ], aes(x = x, y = y, group = ind, color = strat)) +
  geom_hline(yintercept = 0, lty = "dashed", size = 0.5) +
  geom_vline(xintercept = 0, lty = "dashed", size = 0.5) +
  #geom_point(size = 0.2, aes(alpha = time)) +
  #geom_path(size = 0.5, aes(alpha = time)) +
  geom_point(size = 0.2) +
  geom_path(size = 0.5) +
  theme_linedraw(base_size = 8) + xlim(xlims) + ylim(ylims) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
  scale_alpha(guide = "none", range = c(0, 0.8)) + ggtitle("Timesteps: 1-5") +
  scale_color_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  scale_fill_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  xlab(expression(A["1X"]-A["0X "](Advertisement))) + ylab(expression(A["X1"]-A["X0 "](Negotiation)))

#create plot of second batch of timesteps
b <- ggplot(plot_data[which(plot_data$time %in% 6:10), ], aes(x = x, y = y, group = ind, color = strat)) +
  geom_hline(yintercept = 0, lty = "dashed", size = 0.5) +
  geom_vline(xintercept = 0, lty = "dashed", size = 0.5) +
  #geom_point(size = 0.2, aes(alpha = time)) +
  #geom_path(size = 0.5, aes(alpha = time)) +
  geom_point(size = 0.2) +
  geom_path(size = 0.5) +
  theme_linedraw(base_size = 8) + xlim(xlims) + ylim(ylims) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
  scale_alpha(guide = "none", range = c(0, 0.8)) + ggtitle("Timesteps: 6-10") +
  scale_color_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  scale_fill_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  xlab(expression(A["1X"]-A["0X "](Advertisement))) + ylab(expression(A["X1"]-A["X0 "](Negotiation)))

#create plot of third batch of timesteps
c <- ggplot(plot_data[which(plot_data$time %in% 11:15), ], aes(x = x, y = y, group = ind, color = strat)) +
  geom_hline(yintercept = 0, lty = "dashed", size = 0.5) +
  geom_vline(xintercept = 0, lty = "dashed", size = 0.5) +
  #geom_point(size = 0.2, aes(alpha = time)) +
  #geom_path(size = 0.5, aes(alpha = time)) +
  geom_point(size = 0.2) +
  geom_path(size = 0.5) +
  theme_linedraw(base_size = 8) + xlim(xlims) + ylim(ylims) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
  scale_alpha(guide = "none", range = c(0, 0.8)) + ggtitle("Timesteps: 11-15") +
  scale_color_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  scale_fill_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  xlab(expression(A["1X"]-A["0X "](Advertisement))) + ylab(expression(A["X1"]-A["X0 "](Negotiation)))

#prepare data for sankey plot
ind_arcs <- lapply(1:pop_size, function(x){sapply(1:t, function(y){paste0(as.numeric(unlist(c(output[[y]][x, 3:4]))), collapse = "")})})
ind_arcs_sankey <- do.call(rbind, lapply(1:pop_size, function(x){do.call(rbind, lapply(1:(t), function(y){data.frame(x = y, next_x = y+1, node = ind_arcs[[x]][y], next_node = ind_arcs[[x]][y+1])}))}))
ind_arcs_sankey$node <- factor(ind_arcs_sankey$node, levels = c("00", "10", "01", "11"))
ind_arcs_sankey$next_node <- factor(ind_arcs_sankey$next_node, levels = c("00", "10", "01", "11"))

#combine timesteps into top panel
top <- plot_grid(plot_grid(a + theme(legend.position = "none") + theme(axis.title.x = element_text(color = "white")),
                           b + theme(legend.position = "none") + ylab(NULL),
                           c + theme(legend.position = "none") + theme(axis.title.x = element_text(color = "white")) + ylab(NULL),
                           nrow = 1, rel_widths = c(1.05, 1, 1)),
                 get_plot_component(a, "guide-box-bottom", return_all = TRUE), nrow = 2, rel_heights = c(1, 0.12))

#create sankey plot
sankey <- ggplot(ind_arcs_sankey, aes(x = x, next_x = next_x,
                                      node = node, next_node = next_node,
                                      fill = node, label = node)) +
  geom_sankey(flow.alpha = 0.6, space = 30, width = 0.1) +
  theme_linedraw(base_size = 8) +
  theme(legend.position = "none", panel.background = element_blank(), panel.border = element_blank(),
        panel.grid = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), text = element_text(family = "Avenir Next")) +
  scale_fill_manual(values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
  xlab("Timestep") + scale_x_continuous(breaks = c(1:15), limits = c(0.95, 15.05)) + scale_y_continuous(expand = c(0.01, 0.01)) +
  guides(fill = guide_legend(title = "Strategy")) + theme(legend.position = "none", plot.title = element_text(face = "bold")) +
  ggtitle("                       Strategies Over Time")

#combine and save plot
png("plots/time_dynamics.png", width = 6.8, height = 4.2, units = "in", res = 1200)
plot_grid(top, sankey + theme(plot.margin = margin(0.05, -0.28, 0.05, -0.28, "in")),
          nrow = 2, rel_heights = c(1, 0.6))
dev.off()

# LAMBDA CHOICE -----------------------------------------------------------

#create plot that justifies our choice of lambda
lambda_plot <- data.frame(x = seq(0, 0.1, length.out = 1000),
                          y = sapply(seq(0, 0.1, length.out = 1000), function(x){exp(50*x)/sum(exp(50*0), exp(50*x))}))

#save plot
png("plots/lambda_choice.png", width = 3.4, height = 2.4, units = "in", res = 1200)
ggplot(lambda_plot, aes(x = x, y = y)) + geom_line() +
  theme_linedraw(base_size = 8) + theme(legend.position = "bottom", plot.title = element_text(face = "bold"), text = element_text(family = "Avenir Next")) +
  ylab(expression(Probability~of~A["1X"]~Over~A["0X"])) + xlab(expression(Percent~Differential~(A["1X"]/A["0X"]))) +
  scale_x_continuous(breaks = c(0, 0.025, 0.05, 0.075, 0.1), labels = c("+0%", "+2.5%", "+5%", "+7.5%", "+10%"), limits = c(0, 0.1)) +
  scale_y_continuous(breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0), expand = c(0, 0), limits = c(0.5, 1))
dev.off()

# ANIMATION ---------------------------------------------------------------

# #set working directory
# setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")
# 
# #load libraries
# library(parallel)
# library(randomcoloR)
# library(ggplot2)
# library(cowplot)
# library(compiler)
# library(profvis)
# library(gifski)
# library(gganimate)
# 
# #source base functions
# source("code/functions.R")
# 
# #set parameters
# pop_size <- 200
# t <- 400
# neg_cost <- 0.5
# n_moves <- 2
# 
# #get model output
# output <- model(pop_size = pop_size, t = t, neg_cost = neg_cost, n_moves = n_moves)
# 
# #each row is an individual, columns are timesteps
# plot_data <- do.call("rbind",
#                      lapply(1:t, function(i){
#                        data.frame(time = i,
#                                   ind = factor(1:pop_size),
#                                   strat = factor(paste0(output[[i]]$advertisement, output[[i]]$negotiation), levels = c("00", "10", "01", "11")),
#                                   x = sapply(1:nrow(output[[i]]), function(j){output[[i]]$a_advertisement[[j]][2]-output[[i]]$a_advertisement[[j]][1]}),
#                                   y = sapply(1:nrow(output[[i]]), function(j){output[[i]]$a_negotiation[[j]][2]-output[[i]]$a_negotiation[[j]][1]}))
#                      }))
# 
# #produce static version of plot to then animate
# static_plot <- ggplot(plot_data, aes(x = x, y = y, group = ind, color = strat)) +
#   geom_hline(yintercept = 0, lty = "dashed", size = 0.5) +
#   geom_vline(xintercept = 0, lty = "dashed", size = 0.5) +
#   geom_point(size = 0.5) +
#   theme_linedraw(base_size = 8) +
#   scale_color_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
#   scale_fill_manual(name = "Strategy", values = c("#CC79A7", "#D55E00", "#009E73", "#0072B2"), labels = c("00", "10", "01", "11"), drop = FALSE) +
#   xlab(expression(A["1X"]-A["0X "](Advertisement))) + ylab(expression(A["X1"]-A["X0 "](Negotiation)))
# 
# #create dynamic form and save
# dynamic_plot <- static_plot + transition_time(time) + shadow_wake(wake_length = 4/t, wrap = FALSE) + ease_aes("linear") + ggtitle("Timestep: {frame_time}")
# animate(dynamic_plot, nframes = t*3, detail = 10, fps = 10, height = 3, width = 3.3, units = "in", res = 200)

# DISRUPTION PLOTS --------------------------------------------------------

#set working directory
setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")

#load libraries
library(parallel)
library(randomcoloR)
library(ggplot2)
library(cowplot)
library(compiler)
library(profvis)

#read in disruption results
load("data/disrupt_model.RData")

#set parameters
t <- 2000
t_2 <- 200

#create data for plotting
plot_data <- lapply(1:4, function(x){
  if(x == 1){iters <- 1:9}
  if(x == 2){iters <- 10:18}
  if(x == 3){iters <- 19:27}
  if(x == 4){iters <- 28:36}
  data.frame(f = c(disrupt_model[[2]][[iters[1]]][1:t], unlist(lapply(iters, function(y){disrupt_model[[2]][[y]][(t + 1):(t + t_2)]}))),
             t = c(1:t, rep((t + 1):(t + t_2), 9)),
             group = factor(c(rep(1, t), rep(2:10, each = t_2))))
})

#create plot of asocial and fully-connected condition
a <- ggplot(plot_data[[1]]) + geom_line(aes(x = t, y = f, group = group, color = group)) +
  xlab("Timestep") + ylab("Prop. w/Status Quo = 1") +
  scale_color_manual(name = "Prop.\nReplaced\nw/SQ = 2", labels = c("Base", seq(0.1, 0.9, by = 0.1)),
                     values = c("black", "gray80", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "gray40")) +
  scale_x_continuous(expand = c(0, 0), limits = c(t-50, t+t_2+10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_vline(xintercept = t, lty = "dashed") +
  ggtitle("Asocial + Fully-Connected") +
  theme_linedraw(base_size = 8) + theme(text = element_text(family = "Avenir Next"), plot.title = element_text(face = "bold"))

#create plot of social and fully-connected condition
b <- ggplot(plot_data[[2]]) + geom_line(aes(x = t, y = f, group = group, color = group)) +
  xlab("Timestep") + ylab("Prop. w/Status Quo = 1") +
  scale_color_manual(name = "Prop.\nReplaced\nw/SQ = 2", labels = c("Base", seq(0.1, 0.9, by = 0.1)),
                     values = c("black", "gray80", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "gray40")) +
  scale_x_continuous(expand = c(0, 0), limits = c(t-50, t+t_2+10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_vline(xintercept = t, lty = "dashed") +
  ggtitle("Social + Fully-Connected") +
  theme_linedraw(base_size = 8) + theme(text = element_text(family = "Avenir Next"), plot.title = element_text(face = "bold"))

#create plot of asocial and networked condition
c <- ggplot(plot_data[[3]]) + geom_line(aes(x = t, y = f, group = group, color = group)) +
  xlab("Timestep") + ylab("Prop. w/Status Quo = 1") +
  scale_color_manual(name = "Prop.\nReplaced\nw/SQ = 2", labels = c("Base", seq(0.1, 0.9, by = 0.1)),
                     values = c("black", "gray80", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "gray40")) +
  scale_x_continuous(expand = c(0, 0), limits = c(t-50, t+t_2+10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_vline(xintercept = t, lty = "dashed") +
  ggtitle("Asocial + Networked") +
  theme_linedraw(base_size = 8) + theme(text = element_text(family = "Avenir Next"), plot.title = element_text(face = "bold"))

#create plot of social and networked condition
d <- ggplot(plot_data[[4]]) + geom_line(aes(x = t, y = f, group = group, color = group)) +
  xlab("Timestep") + ylab("Prop. w/Status Quo = 1") +
  scale_color_manual(name = "Prop.\nReplaced\nw/SQ = 2", labels = c("Base", seq(0.1, 0.9, by = 0.1)),
                     values = c("black", "gray80", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "gray40")) +
  scale_x_continuous(expand = c(0, 0), limits = c(t-50, t+t_2+10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  geom_vline(xintercept = t, lty = "dashed") +
  ggtitle("Social + Networked") +
  theme_linedraw(base_size = 8) + theme(text = element_text(family = "Avenir Next"), plot.title = element_text(face = "bold"))

#combined and save
png("plots/disruption.png", width = 6.8, height = 4, units = "in", res = 1200)
plot_grid(plot_grid(a + theme(legend.position = "none"), c + theme(legend.position = "none"), nrow = 2), plot_grid(b + theme(legend.position = "none"), d + theme(legend.position = "none"), nrow = 2), get_legend(a), nrow = 1, rel_widths = c(1, 1, 0.2))
dev.off()

# GINI COMPARISON ---------------------------------------------------------

#set working directory
setwd("~/Documents/Work/Fall 2022/Conservatism/ConservatismModel")

#load libraries
library(parallel)
library(ggplot2)
library(cowplot)

#source base code
source("code/functions.R")

# #run simulations
# a <- mclapply(1:100, function(x){model(100, 2000, n_moves = 6, neg_cost = 0.25, last_output = TRUE)$lifetime_payoff}, mc.cores = 7)
# b <- mclapply(1:100, function(x){model(100, 2000, n_moves = 6, neg_cost = 0.75, last_output = TRUE)$lifetime_payoff}, mc.cores = 7)
# gini_median_compare <- list(a, b)
# save(gini_median_compare, file = "gini_median_compare.RData")

#load simulations
load("data/gini_median_compare.RData")
a <- gini_median_compare[[1]]
b <- gini_median_compare[[2]]

#format for gini plotting
gini_data <- data.frame(x = c(density(sapply(1:length(a), function(x){DescTools::Gini(abs(a[[x]]))}), n = 1000, from = 0, to = 1)$x,
                              density(sapply(1:length(b), function(x){DescTools::Gini(abs(b[[x]]))}), n = 1000, from = 0, to = 1)$x),
                        y = c(density(sapply(1:length(a), function(x){DescTools::Gini(abs(a[[x]]))}), n = 1000, from = 0, to = 1)$y,
                              density(sapply(1:length(b), function(x){DescTools::Gini(abs(b[[x]]))}), n = 1000, from = 0, to = 1)$y),
                        group = factor(c(rep(1, 1000), rep(2, 1000))))

#create plot of gini coefficient
gini_plot <- ggplot(gini_data, aes(x = x, y = y, color = group)) + geom_line() +
  scale_x_continuous(expand = c(0, 0), breaks = c(0.1, 0.2, 0.3, 0.4), limits = c(0, 0.5)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 30)) +
  scale_color_manual(name = "Negotiation\nCost", values = c("#009E73", "#D55E00"), labels = c("0.25", "0.75")) +
  theme_linedraw(base_size = 8) + xlab("Gini Coefficient of Lifetime Payoff") + ylab("Density\n") +
  guides(color = guide_legend(override.aes = list(shape = 18))) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), text = element_text(family = "Avenir Next"))

#format for median plotting
median_data <- data.frame(x = c(density(sapply(1:length(a), function(x){median(a[[x]])}), n = 1000, from = 500, to = 1500)$x,
                                density(sapply(1:length(b), function(x){median(b[[x]])}), n = 1000, from = 500, to = 1500)$x),
                          y = c(density(sapply(1:length(a), function(x){median(a[[x]])}), n = 1000, from = 500, to = 1500)$y,
                                density(sapply(1:length(b), function(x){median(b[[x]])}), n = 1000, from = 500, to = 1500)$y),
                          group = factor(c(rep(1, 1000), rep(2, 1000))))

#create plot of median values
median_plot <- ggplot(median_data, aes(x = x, y = y, color = group)) + geom_line() +
  scale_x_continuous(expand = c(0, 0), limits = c(600, 1300)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.013)) +
  scale_color_manual(name = "Negotiation\nCost", values = c("#009E73", "#D55E00"), labels = c("0.25", "0.75")) +
  theme_linedraw(base_size = 8) + xlab("Median Lifetime Payoff") + ylab("Density\n") +
  guides(color = guide_legend(override.aes = list(shape = 18))) + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), text = element_text(family = "Avenir Next"))

#combine and save
png("plots/gini_median_compare.png", width = 6.8, height = 1.8, units = "in", res = 1200)
plot_grid(gini_plot + theme(legend.position = "none"), median_plot + theme(legend.position = "none"), get_legend(median_plot), nrow = 1, rel_widths = c(1, 1, 0.2))
dev.off()

# EXAMPLE NETWORK ---------------------------------------------------------

#plot example of network structure
png("plots/example_net.png", width = 6, height = 6, units = "in", res = 1200)
par(mar=c(0,0,0,0)+.1)
plot(igraph::sample_pa(50, directed = FALSE, m = 5), vertex.label = "", vertex.color = "black", vertex.size = 4)
dev.off()
