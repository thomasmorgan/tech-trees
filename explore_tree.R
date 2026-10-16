source("tech_trees.R")

# make the tree
techs <- create_techs()
dependencies <- create_tech_tree()
plot_tech_tree()

generations <- 100  # how many goes
p_innovate <- 0.1  # probaiblity a tech that can be innovated, is innovated
can_innovate_from_path <- TRUE # whether innovations can be made anywhere along the path leading to a tech
greediness <- 3  # how aggressively do you exploit high paying techs?
p_forget <- 0.001 # probability a tech is forgotten
can_forget_used_techs <- FALSE # can you forget a tech exploited on that turn

tier_1_techs <- techs$id[techs$tier == 1]
known_techs <- tier_1_techs  # vector of known techs by ID
sampled_techs <- vector()  # vector for which tech is exploited on each step
n_known_techs <- vector()
payoff <- vector()

# innovation loop
for (i in 1:generations) {
  # pick a tech to exploit
  known_payoffs <- techs$payoff[known_techs]
  sampled_tech <- sample(known_techs, 1, prob=known_payoffs^greediness)
  
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
    
    # for each of those, they can be innovated if...
    for (t in unknown_connected_techs) {
      required_techs <- techs$id[dependencies[t,] < 0]
      if (all(required_techs %in% known_techs)) {  # all dependencies must be known
        if (runif(1, 0, 1) < p_innovate) {
          known_techs <- c(known_techs, t)
        }
      }
    }
  }
  sampled_techs <- c(sampled_techs, sampled_tech)
  n_known_techs <- c(n_known_techs, length(known_techs))
  payoff <- c(payoff, techs[sampled_tech,]$payoff)
  
  ### FORGET ###
  
  # which techs might be forgotten
  if (can_forget_used_techs) {
    forgettable_techs <- known_techs
  } else {
    forgettable_techs <- known_techs[!known_techs %in% points_of_innovation]
  }
  # cannot forget tier 1 techs!
  forgettable_techs <- forgettable_techs[!forgettable_techs %in% tier_1_techs]
  
  # forget them with probability
  for (t in forgettable_techs) {
    if (runif(1, 0, 1) < p_forget) {
      techs_to_forget <- c(t, techs$id[dependencies[t,] > 0])
      known_techs <- known_techs[!known_techs %in% techs_to_forget]
    }
  }
  
}
sampled_techs
plot(n_known_techs, ylim=c(0, max(n_known_techs)))
abline(0, max(n_known_techs)/generations)
scaled_payoff <- payoff / max(payoff) * max(n_known_techs)
smoothed_scaled_payoff <- stats::filter(scaled_payoff, rep(1 / 50, 50), sides = 1)
total_payoff <- cumsum(payoff)
scaled_total_payoff <- total_payoff/max(total_payoff) * max(n_known_techs)
lines(smoothed_scaled_payoff, col="cornflowerblue")
lines(scaled_total_payoff, col="firebrick")

