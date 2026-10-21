source("tech_trees.R")

### EDITABLE PARAMETERS ###

starting_techs <- 5 # number of techs in tier 1
max_tier <- 8 # number to tiers in tree
tree_growth_rate <- 2 # rate at with tiers get wider
p_extra_dependency <- 0.001 # probability a given extra dependency is realized
max_tier_gap_for_dependencies <- 99 # how many tiers extra dependencies can cross
p_keystone_tech <- 0.00 # probability a tech is a keystone tech
keystone_multiplier <- 10 # how much more likely keystone techs are to have extra dependencies

generations <- 20000  # how many goes
p_innovate <- 0.1  # probability a tech that can be innovated, is innovated
can_innovate_from_path <- TRUE # whether innovations can be made anywhere along the path leading to a tech
greediness <- 1  # how aggressively do you exploit high paying techs?
p_forget <- 0.005 # probability a tech is forgotten
can_forget_used_techs <- FALSE # can you forget a tech exploited on that turn
can_forget_stem_techs <- FALSE # can you forget a tech than another known tehc depends on


### FUNCTIONS ###

# a single mind explores the tree
run_simulation <- function() {

  tier_1_techs <- techs$id[techs$tier == 1]
  known_techs <- tier_1_techs  # vector of known techs by ID
  sampled_techs <- rep(NA, generations)  # vector for which tech is exploited on each step
  n_known_techs <- rep(NA, generations)
  payoff <- rep(NA, generations)
  expected_payoff <- rep(NA, generations)
  
  # innovation loop
  for (i in 1:generations) {
    gen_percent <- max(1, floor(generations / 100))
    if (i %% gen_percent == 0 || i == generations) {
      cat(sprintf("\rProgress: %.0f%%", 100 * i / generations))
      flush.console()
    }
    
    ### EXPLOIT ###
    known_payoffs <- techs$payoff[known_techs]
    known_probs <- known_payoffs^greediness/(sum(known_payoffs^greediness))
    sampled_tech <- sample(known_techs, 1, prob=known_probs)
    payoff[i] <- techs[sampled_tech,]$payoff
    sampled_techs[i] <- sampled_tech
    expected_payoff[i] <- sum(known_payoffs*known_probs)
    
    ### INNOVATE ###
    
    # which nodes might you innovate from
    if (can_innovate_from_path) {
      # the exploited tech and all other techs it depends on
      points_of_innovation <- c(sampled_tech, techs$id[dependencies[sampled_tech,] < 0])
    } else {
      # or just the exploited tech
      points_of_innovation <- c(sampled_tech)
    }
    
    # discover innovations
    for (p in points_of_innovation) { # for each point of innovation
      # which techs does it lead to
      connected_techs <- techs$id[dependencies[p,] %in% c(1, 2)]
      
      # remove those we already know
      unknown_connected_techs <- connected_techs[!connected_techs %in% known_techs]
      
      # for each of those, they can be innovated if all dependencies are known, and with a bit of luck
      for (t in unknown_connected_techs) {
        required_techs <- techs$id[dependencies[t,] < 0]
        if (all(required_techs %in% known_techs)) {
          if (runif(1, 0, 1) < p_innovate) {
            known_techs <- c(known_techs, t)
          }
        }
      }
    }
    n_known_techs[i] <- length(known_techs)
    
    ### FORGET ###
    
    # which techs might be forgotten
    forgettable_techs <- known_techs
    
    # cannot forget tier 1 techs!
    forgettable_techs <- forgettable_techs[!forgettable_techs %in% tier_1_techs]
    
    # can you forgets techs you just used?
    if (!can_forget_used_techs) {
      forgettable_techs <- forgettable_techs[!forgettable_techs %in% points_of_innovation]
    }
    
    # can you forget techs that other known techs depend on?
    if (!can_forget_stem_techs) {
      for (t in forgettable_techs) {
        dependers <- techs$id[dependencies[t,] > 0]
        if (any(dependers %in% known_techs)) {
          forgettable_techs <- forgettable_techs[forgettable_techs != t]
        }
      }
    }
    
    # forget them with probability
    for (t in forgettable_techs) {
      if (runif(1, 0, 1) < p_forget) {
        techs_to_forget <- c(t, techs$id[dependencies[t,] > 0])
        known_techs <- known_techs[!known_techs %in% techs_to_forget]
      }
    }
    
  }
  
  output = list(
    sampled_techs=sampled_techs,
    n_known_techs=n_known_techs,
    payoff=payoff,
    expected_payoff=expected_payoff
  )
  return(output)
}


plot_simulation <- function(output) {
    
  sampled_techs <- output$sampled_techs
  n_known_techs <- output$n_known_techs
  payoff <- output$payoff
  expected_payoff <- output$expected_payoff
  
  par(mfrow=c(2, 3))
  
  # panel 1
  plot_tech_tree()
  
  # panel 2
  incoming <- techs$direct_incoming_dependencies
  outgoing <- techs$direct_outgoing_dependencies
  
  breaks <- seq(min(c(incoming, outgoing)) - 0.5,
                max(c(incoming, outgoing)) + 0.5, by = 1)
  
  h_in <- hist(incoming, breaks = breaks, plot = FALSE)
  h_out <- hist(outgoing, breaks = breaks, plot = FALSE)
  
  plot(h_in, col = adjustcolor("maroon", alpha.f = 0.5),
       main = "Number of dependencies", xlab = "Dependencies",
       ylim = c(0, max(h_in$counts, h_out$counts)))
  plot(h_out, col = adjustcolor("cornflowerblue", alpha.f = 0.5),
       add = TRUE)
  
  legend("topright", legend = c("Incoming", "Outgoing"),
         fill = adjustcolor(c("maroon", "cornflowerblue"), alpha.f = 0.5))
  
  
  # panel 3
  incoming <- techs$all_incoming_dependencies/nrow(techs)
  outgoing <- techs$all_outgoing_dependencies/nrow(techs)
  
  breaks <- seq(0,
                1, by = 0.05)
  
  h_in <- hist(incoming, breaks = breaks, plot = FALSE)
  h_out <- hist(outgoing, breaks = breaks, plot = FALSE)
  
  plot(h_in, col = adjustcolor("maroon", alpha.f = 0.5),
       main = "Proportion of tree indirectly depending", xlab = "Proportion",
       ylim = c(0, max(h_in$counts, h_out$counts)))
  plot(h_out, col = adjustcolor("cornflowerblue", alpha.f = 0.5),
       add = TRUE)
  
  legend("topright", legend = c("Incoming", "Outgoing"),
         fill = adjustcolor(c("maroon", "cornflowerblue"), alpha.f = 0.5))
  
  
  # plot 4
  plot(n_known_techs, ylim=c(0, max(n_known_techs)), type="l", xlab="Round", ylab="Number of techs known")
  
  # plot 5
  plot(expected_payoff, col="maroon", type="l", xlab="Round", ylab="Expected payoff")
  
  # plot 6
  total_payoff <- cumsum(payoff)
  plot(total_payoff, type="l", xlab="Round", ylab="Total accumulated payoff")
  abline(0,sum(payoff)/length(payoff), lty=2)
}

techs <- create_techs()
dependencies <- create_tech_tree()
output <- run_simulation()
plot_simulation(output)

