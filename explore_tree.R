library(av)
source("tech_trees.R")

### EDITABLE PARAMETERS ###

starting_techs <- 5 # number of techs in tier 1
max_tier <- 8 # number to tiers in tree
tree_growth_rate <- 2 # rate at with tiers get wider
payoff_growth_rate <- 1.1 # rate at which average payoff grows with tier (multiplicative)
p_extra_dependency <- 0.001 # probability a given extra dependency is realized
max_tier_gap_for_dependencies <- 2 # how many tiers extra dependencies can cross
p_keystone_tech <- 0.00 # probability a tech is a keystone tech
keystone_multiplier <- 10 # how much more likely keystone techs are to have extra dependencies

generations <- 20000  # how many goes
p_innovate <- 1  # probability a tech that can be innovated, is innovated
can_innovate_from_path <- FALSE # whether innovations can be made anywhere along the path leading to a tech
greediness <- 1  # how aggressively do you exploit high paying techs?
n_samples <- 5 # how many techs do you exploit per round
p_forget <- 0.01 # probability a tech that can be forgotten, is forgotten
can_forget_used_techs <- FALSE # can you forget a tech exploited on that turn
can_forget_stem_techs <- TRUE # can you forget a tech than another known tech depends on
can_forget_tier_1_techs <- TRUE # can you forget techs right at the bottom of the tree
forgetting_cascades <- FALSE # does forgetting a tech cause you to forget all techs that depend on it


### FUNCTIONS ###

# a single mind explores the tree
run_simulation <- function() {

  tier_1_techs <- techs$id[techs$tier == 1]
  known_techs <- tier_1_techs  # vector of known techs by ID
  sampled_techs <- vector("list", generations)  # list for which techs were exploited on each step
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
    sampled_tech <- sample(known_techs, n_samples, prob=known_probs, replace=F)
    payoff[i] <- sum(techs[sampled_tech,]$payoff)
    sampled_techs[[i]] <- sampled_tech
    expected_payoff[i] <- sum(known_payoffs*known_probs)
    
    ### INNOVATE ###
    
    # which nodes might you innovate from
    points_of_innovation <- c(sampled_tech)
    if (can_innovate_from_path) {
      # the exploited tech and all other techs it depends on
      for (t in sampled_tech) {
        points_of_innovation <- c(points_of_innovation, techs$id[dependencies[t,] < 0])
      }
    }
    points_of_innovation <- unique(points_of_innovation)
    
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
  # panel 3
  plot_tech_tree(unique(unlist(sampled_techs)))
  title(main = "Ever Exploited tree")
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
create_video <- function(output) {
  n_timesteps <- length(output$knowledge_history)
  av::av_capture_graphics(
    {
      for (t in 1:n_timesteps) {
        gen_percent <- max(1, floor(n_timesteps / 50))
        if (t %% gen_percent == 0 || t == n_timesteps) {
          cat(sprintf("\rProgress: %.0f%%", 100 * t / n_timesteps))
          flush.console()
        }
        plot_tech_tree(techs_to_paint = output$knowledge_history[[t]])
        title(main = paste("Timestep", t))
      }
    },
    output = "tech_tree.mp4",
    width = 800,
    height = 800,
    framerate = 60
  )
}


run_simulations_across_trees <- function(trees, n_repeats = 10) {
  n_trees <- length(trees)
  n_runs <- n_trees * n_repeats
  
  file <- paste0(n_trees, "x", n_repeats, "_results.rds")
  if (file.exists(file)) {
    results <- readRDS(file)
  } else {
    results <- vector("list", n_runs)
  }
  
  run_id <- 0
  for (i in seq_len(n_trees)) {
    techs <<- trees[[i]]$techs
    dependencies <<- trees[[i]]$dependencies
    
    for (j in seq_len(n_repeats)) {
      run_id <- run_id + 1
      
      if (!is.null(results[[run_id]])) {
        next
      }
      
      output <- run_simulation()
      results[[run_id]] <- list(
        tree_id = i,
        repeat_id = j,
        output = output
      )
      
      cat(paste0("\rCompleted ", run_id, "/", n_runs,
                 " simulations (tree ", i, ", repeat ", j, ").\n"))
      flush.console()
      
      # Write a temporary file first to protect the existing checkpoint.
      if ((run_id %% 100) == 0 || run_id == n_runs) {
        print("Writing to file...")
        saveRDS(results, file = paste0(file, ".tmp"))
        
        if (!file.rename(paste0(file, ".tmp"), file)) {
          stop("Could not replace the results file.")
        }
      }
    }
  }
}

plot_multiple_simulations(results) {
  runs <- length(results)
  gens <- length(results[[1]]$output$n_known_techs)
  plot(c(0, 1),c(0,1), col="white", xlim=c(0, gens), ylim=c(0, 20), xlab="Round", ylab="N known techs")
  n_known <- rep(0, gens)
  for (i in 1:runs) {
    y <- results[[i]]$output$n_known_techs
    if (runif(1, 0, 1) < 0.1) {
      lines(y~c(1:gens), col=rgb(0, 0, 0, 0.1))
    }
    n_known <- n_known+y
  }
  n_known <- n_known/runs
  lines(n_known~c(1:gens), lwd=3, col="black")
  
  
  plot(c(0, 1),c(0,1), col="white", xlim=c(0, gens), ylim=c(0, 5), xlab="Round", ylab="Expected payoff")
  e_payoff <- rep(0, gens)
  for (i in 1:runs) {
    y <- results[[i]]$output$expected_payoff
    if (runif(1, 0, 1) < 0.05) {
      lines(y~c(1:gens), col=rgb(0.3, 0, 0.05, 0.1))
    }
    e_payoff <- e_payoff+y
  }
  e_payoff <- e_payoff/runs
  lines(e_payoff~c(1:gens), lwd=3, col="maroon")
}


# techs <- create_techs()
# dependencies <- create_tech_tree()
# output <- run_simulation()
# plot_simulation(output)

# generate_trees()
# trees <- readRDS("1000_trees.rds")
# run_simulations_across_trees(trees, n_repeats=3)
results <- readRDS("1000x3_results.rds")
plot_multiple_simulations(results)



