source("tech_trees.R")

# make the tree
techs <- create_techs()
dependencies <- create_tech_tree()
plot_tech_tree()

generations <- 100  # how many goes
p_innovate <- 0.1  # probaiblity a tech that can be innovated, is innovated
can_innovate_from_path <- TRUE # whether innovations can be made anywhere along the path leading to a tech

known_techs <- techs$id[techs$tier == 1]  # vector of known techs by ID
sampled_techs <- vector()  # vector for which tech is exploited on each step

# innovation loop
for (i in 1:generations) {
  # pick a tech to exploit
  known_payoffs <- techs$payoff[known_techs]
  sampled_tech <- sample(known_techs, 1, prob=known_payoffs)
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
}
